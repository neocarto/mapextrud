library(sf)
library(scales)

# get frame
  
  getframe <- function(x){
    bb <- st_bbox(x)
    d <- (bb[3] - bb[1]) / 50
    bb <- bb + c(-d, -d, d, d)
    frame <- st_as_sfc(bb, crs = st_crs(x))
    frame <- st_sf(geometry = frame)
    return(frame)
  }
  
  
  # centroid
  
  center <- function(x){
    x <- st_as_sfc(st_bbox(x))
    x <- st_sf(geometry = st_centroid(x))
    x$x <- st_coordinates(x)[1]
    x$y <- st_coordinates(x)[2]
    x <- x[,c("x","y","geometry")]
    return(x)
   }
  
  # Rotate
  
  rotate <- function(x, deg){
  geom <- st_geometry(x) 
  rot = function(a) matrix(c(cos(a), sin(a), -sin(a), cos(a)), 2, 2)   
  ctr <- st_centroid(st_as_sfc(st_bbox(geom)))
  geom <- (geom - ctr) * rot(deg * pi/180) + ctr                     
  st_geometry(x) <- geom
  return(x)
  }
  
 # Deform
  
  deform <- function(x, flatten = 0.8, rescale = c(1.3,2)){

  ymin <- as.numeric(st_bbox(x)[2]) # prendre en compte le frame ?
  ymax <- as.numeric(st_bbox(x)[4]) # prendre en compte le frame ?
  
  # Centering
  cx <- as.numeric((st_bbox(x)[3] + st_bbox(x)[1]) / 2)
  cy <- as.numeric((st_bbox(x)[4] + st_bbox(x)[2]) / 2)
  geom <- st_geometry(x)
  geom <- geom - c(cx, cy)
  st_geometry(x) <- geom
  
  # translate nodes
  
  for (i in 1:nrow(x)){
  
  poly <- st_geometry(x)[[i]]
  pts <- st_coordinates(poly)
  idx <- unique(pts[, colnames(pts) %in% c("L1", "L2", "L3")])
 
        # For each polygons in multipolygons
        for(k in 1:nrow(idx)) {

          newpts <- pts[pts[, "L1"] == idx[k, "L1"] & pts[, "L2"] == idx[k, "L2"], c("X", "Y")]
          
          # ---------------------------------------------
          
          for (n in 1:nrow(newpts)){

            Y <- newpts[n,"Y"]
            X <- newpts[n,"X"]

            # transformation en Y
            
            # Y <- Y + 100000
            Y <- Y * flatten
            newpts[n,"Y"] <- Y
            
            # transformation en X
            v <- c(ymin, Y, ymax)
            Y2 <- rescale(v, c(rescale[2], rescale[1]))[2]
            X <- X * Y2
            newpts[n,"X"] <- X
          } 
            
          
        # ---------------------------------------------
          
          # points to polygons
          if (sf::st_geometry_type(sf::st_geometry(x)[[i]]) == "POLYGON"){
            sf::st_geometry(x)[[i]][[idx[k, "L1"]]] <- newpts
          } else {
            sf::st_geometry(x)[[i]][[idx[k, "L2"]]][[idx[k, "L1"]]] <- newpts
          }
        
        }
  }
  
    return(x)
}
  
# ----------------------------------------
states <- readRDS("us.rds")
x <- deform(states)
var = "pop2019"
k = 1
col = "red"
#  


world <- readRDS("worldpopgrid.rds")
View(world)
x <- world[!is.na(world$pop2020),]
x <- x[x$pop2020 > median(x$pop2020),]
View(x)
var = "pop2020"
k = 1
col = "yellow"
regular = TRUE

world <- readRDS("world.rds")
x <- deform(world)
var = "pop"
k = 1
col = "yellow"

extrude <- function(x, var, k = 1, col = "red", regular = FALSE, add = FALSE) {

tmp <- x[is.na(x[,var]),]  
ids <- row.names(tmp)  
x[rownames(x) %in% ids,var] <- 0

h <- st_bbox(x)[4]-st_bbox(x)[2]
m <- max(x[,var] %>% st_drop_geometry())
k = k * 0.1 * h / m  
  
x$id <- row.names(x)
x[,"height"] <- x[,var] %>% st_drop_geometry() * k
x[is.na(x[,var]),"height"] <- 0
n1 <- dim(x)[1]  
x <- st_cast(x, "POLYGON", warn = FALSE)
n2 <- dim(x)[1]
if(n2 > n1){
  message("Splitting multi-part polygon into single polygons. The same value is assigned to each splitted polygon.")
  x$id <- row.names(x)
  }

nodes <- st_cast(x,"POINT", warn = FALSE)

# Faces

message("Starting extrusion")

nodes$first <- !duplicated(nodes$id)
nodes$last <- !duplicated(nodes$id, fromLast = TRUE)
dots1 <- nodes[!nodes$last,]
dots2 <- nodes[!nodes$first,]  
p1x <- st_coordinates(dots1)[,1]
p1y <- st_coordinates(dots1)[,2]
p2x <- st_coordinates(dots2)[,1]
p2y <- st_coordinates(dots2)[,2]
p3x <- p2x
p3y <- p2y + dots2$height
p4x <- p1x
p4y <- p1y + dots1$height

faces <- dots1
faces$ang <- atan((p2y - p1y) / ( p2x - p1x))*180/pi
faces$pos <- (p1y + p2y)/2

st_geometry(faces) <- st_as_sfc(paste0("POLYGON((",p1x," ",p1y,", ",p2x," ",p2y,", ",p3x," ",p3y,", ",p4x," ",p4y,", ",p1x," ",p1y,"))"))
faces <- faces[order(faces$pos, decreasing = TRUE),]

# Colors

if(length(col) == 1){
    if(col == "white"){
      fill <- c("white","white","white") 
    } else {
      pal <- colorRampPalette(c("white",col,"black"))(11)
      fill <- c(col, pal[3],pal[7]) 
    }
}

if(length(col)==2){
  fill <- c(col[1],col[2],col[2])
  }

if(length(col)==3){
  fill <- col
  }

faces$fill <- fill[2]
faces[faces$ang > 0,"fill"] <- fill[3]   

# Tops

tops <- x

for (i in 1:dim(tops)[1])
{
  st_geometry(tops[i,]) <- st_geometry(tops[i,]) + c(0, as.numeric(x[i,var])[1] * k)
}
tops <- tops[order(tops$height, decreasing = FALSE),]
st_crs(tops) <- NA

# faces <- st_difference(faces,st_buffer(st_union(st_geometry(tops)),1))

# Opérations géométriques

# 1 - Faces cleaning


if(regular == FALSE){

message("Topological issues (1/3)")

faces$xpos <- round(st_coordinates(st_centroid(st_geometry(faces)))[,1],0)
faces$dup1 <- as.numeric(duplicated(faces$pos))
faces$dup2 <- as.numeric(duplicated(faces$pos, fromLast = TRUE))
faces$dup3 <- as.numeric(duplicated(faces$xpos))
faces$dup4 <- as.numeric(duplicated(faces$xpos, fromLast = TRUE))
faces$test1 <- faces$dup1 + faces$dup3
faces$test2 <- faces$dup2 + faces$dup4
faces$test3 <- faces$dup1 + faces$dup2 + faces$dup3 + faces$dup4
f1 <- faces[faces$test1 == 2,]
f2 <- faces[faces$test2 == 2,]
f3 <- faces[faces$test3 == 0,]

nb <- dim(f2)[1]
f <- f1

for(i in 1:nb){

  if (f1$height[i] == max(f1$heigh[i],f2$heigh[i])){
    st_geometry(f[i,]) <- st_difference(st_geometry(f1[i,]),st_buffer(st_geometry(f2[i,]),1) )
    f$id[i] <- f1$id[i]
  } else {
    st_geometry(f[i,]) <- st_difference(st_geometry(f2[i,]),st_buffer(st_geometry(f1[i,]),1) )
    f$id[i] <- f2$id[i]
  }
}

faces <- f
faces <- faces[order(faces$pos, decreasing = TRUE),]


# alltops <- st_buffer(st_union(st_geometry(tops)),1)
# faces <- st_difference(faces,alltops)


# 2 -Tops cleaning

message("Topological issues (2/3)")

blocs <- x
for (i in ids){
  #st_geometry(blocs[i,]) <- st_buffer(st_union(st_union(st_geometry(faces[faces$id==i,]),st_geometry(tops[tops$id==i,]))),1)
  st_geometry(blocs[i,]) <- st_buffer(st_union(st_union(st_geometry(faces[faces$id==i,]),st_geometry(tops[tops$id==i,]))),1)
}

message("Topological issues (3/3)")

for(i in ids){
  v <- as.numeric(tops[tops$id == i,var] %>% st_drop_geometry())
  tmp <- tops %>% st_drop_geometry()
  ids2 <- tmp[tmp[,var] > v & tmp$id != i,"id"]
  geom1 <- st_geometry(tops[tops$id == i,])
  # plot(geom1)
  geom2 <- st_geometry(blocs[blocs$id %in% ids2,])
  # plot(geom2)
  geom <- st_difference(geom1, st_union(geom2))
  
  if(length(geom) > 0) {
    st_geometry(tops[tops$id == i,]) <- geom
  } else {
    # Supprimer les geometries vides (TODO)
  }
  
  
}

# 3  - bind & sort faces

# faces2 <- st_difference(f3,st_buffer(st_union(st_geometry(tops)),1))
f <- rbind(faces, f3)
f <- f[order(f$pos, decreasing = TRUE),]


# Display

plot(st_geometry(x), add=add)
for(i in tops$id)
{
  plot(st_geometry(f[f$id == i,]), col=f$fill[f$id ==i], add=T)
  plot(st_geometry(tops[tops$id == i,]), col=fill[1], add=T)
}


} else {
  
 # f <- faces
  
  # Sort
  
  message("Sort")
  
  
  f <- faces[,c("id","pos","fill")]
  tpos <- min(f$pos) - 1
  tops$fill <- fill[1]
  t <- tops[,c("id","fill")]
  t$pos <- tpos
  geom <- rbind(f, t)
  
  xextent <- as.numeric(st_bbox(geom)[3] - st_bbox(geom)[1])
  sorty <- data.frame(id=x$id,posy = st_coordinates(st_centroid(st_geometry(x)))[,2])
  sortx <- data.frame(id=x$id,posx = st_coordinates(st_centroid(st_geometry(x)))[,1])
  if(xextent > 10000) {sorty$posy <- round(sorty$posy,0)}
  sortx$posx <- 1/sortx$posx
  geom <- merge(x=geom, y=sortx, by = "id")
  geom <- merge(x=geom, y=sorty, by = "id")
  geom <- geom[order(geom$posy, geom$posx, geom$pos, decreasing = TRUE),]
  
  # Display

  plot(st_geometry(x), add=add)
  plot(st_geometry(geom), col=geom$fill, add=T)

  }



message("Done")


}

# ------------------------

# Example 1  
  
states <- readRDS("us.rds")
basemap <- deform(states)
extrude(basemap, var = "pop2019" , k = 1, col = "red", add = FALSE)
  
# Example 2

states <- readRDS("us.rds")
frame <- deform(getframe(x))
basemap <- deform(states)
plot(st_geometry(frame))
extrude(basemap, "pop2019", k = 1, col = "white", add = TRUE)

# Example 3

world <- readRDS("worldpopgrid.rds")
world2 <- deform(world)
basemap <- world2[!is.na(world$pop2020),]
basemap <- basemap[basemap$pop2020 > quantile(basemap$pop2020, 0.9),]
plot(st_geometry(world2),col="#CCCCCC", border="white")
extrude(basemap, "pop2020", k = 2, col = "red", regular = TRUE, add=T)

# Example 4 (World countries extrud pop height)

world <- readRDS("world.rds")
basemap <- deform(world)
frame <- deform(getframe(world))
plot(st_geometry(frame), col="lightblue")
plot(st_geometry(basemap), col="white", add=T)
extrude(basemap, "pop", k = 1, col = "white", add=T)

# Example 5 (World countries extrud pop Volume)

# Example 6 (Buffer cenroid)


# Example 5 (Martinique)



# ------------------------------------------------------------------------
 

