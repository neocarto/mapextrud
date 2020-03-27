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
   st_geometry(x) <- st_crop(st_geometry(x), st_bbox(st_union(x)))
    return(x)
  }
  
# -------------------------------------------------
  
  library(SpatialPosition)
  data("hospital")
  pot <- quickStewart(x = hospital,
                      var = "capacity",
                      span = 1000,
                      beta = 2, mask = paris, 
                      returnclass = "sf")
  x <- deform(pot)
  states <- readRDS("us.rds")
  var = "center"
  k = 3
  col = "white"
  add = F
  regular = F  

  
extrude <- function(x, var, k = 1, col = "red", regular = FALSE, add = FALSE) {


xraw <- x  
x[is.na(x[,var]),var]  <- 0
x <- x[x[,var] %>% st_drop_geometry() > 0,]

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

x <- st_cast(x, "POLYGON", warn = FALSE) # !!!!!!!!!!!!!!!!!!!!! PB ICI !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

n2 <- dim(x)[1]
if(n2 > n1){
  message("Splitting multi-part polygon into single polygons. The same value is assigned to each splitted polygon.")
  x$id <- row.names(x)
  }

nodes <- st_cast(x,"POINT", warn = FALSE)

if(dim(nodes)[1]> 2000 & regular == FALSE){
    if (interactive()){
      cat(paste0("Sorry, the script is not optimized...\nThe basemap is not enough generalized (",dim(nodes)[1]," nodes) and the computation will take time.\nDo you still want to continue ? [y/n]"))
      z <- readLines(con = stdin(), n = 1) 
      while (!z %in% c("n","y")){
        cat ("Enter y or n")
        z <- readLines(con = stdin(), n = 1)  
      }
      if (z == "y"){
        cat("Okay!\nGo get a coffee and come back later to see the result.")
      } else {
        stop("Computation aborted",
             call. = F)
      }
    } else {
      stop("Computation aborted", 
           call. = F)
    }
}

L1 <- data.frame(st_coordinates(x))
nodes <- st_sf(cbind(data.frame(nodes),L1= L1$L1))
nodes$id2 <- paste(nodes$id,nodes$L1,sep="_")

# Faces

nodes$first <- !duplicated(nodes$id2)
nodes$last <- !duplicated(nodes$id2, fromLast = TRUE)
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




if(regular == FALSE){


message(paste0("Topological detection (",dim(nodes)[1]," nodes)"))

  
  lines <- faces
  st_geometry(lines) <- st_as_sfc(paste0("LINESTRING(",p1x," ",p1y,", ",p2x," ",p2y,")"))
  nb = dim(lines)[1]
  
  span <- round(nb/10,0)
  init <- span
  pct <- 10
  
  for(i in 1:nb){
    r <- st_contains(lines[i,], lines)
    faces$i1[i] <- i
    if(r[[1]][1] == i){faces$i2[i] <- r[[1]][2]} else {faces$i2[i] <-r[[1]][1] }
    
    if(i == span){
      cat(paste0("[",pct,"%]"))
      span <- span + init
      pct <- pct + 10
    }
  }
  cat("[100%]")
  
  fext <- faces[is.na(faces$i2),]
  f <- faces[!is.na(faces$i2),]

message("")
message("Extrude faces")
  
  span <- round(length(f$i1)/10,0)
  init <- span
  pct <- 10
  count <- 1

  f$none <- 0

  for(i in f$i1){
    i2 <- f$i2[f$i1 == i]
    poly1 <- st_geometry(f[f$i1 == i,])
    poly2 <- st_geometry(f[f$i1 == i2,])
    height1 <- f$height[f$i1 == i]
    height2 <- f$height[f$i1 == i2]
    #if(height2 == 0){poly2 <- st_buffer(poly2,1)}
    if(height1 > height2){
      st_geometry(f[f$i1==i,]) <- st_difference(poly1,poly2)
    } else {
      f$none[f$i1==i] <- 1
    }
    
    if(count == span){
      cat(paste0("[",pct,"%]"))
      span <- span + init
      pct <- pct + 10
    }  
    count <- count +1
  }
  cat("[100%]")
  
  f <- f[f$none == 0,]
  f <- f[order(f$pos, decreasing = TRUE),]

  
  message("")
  message("Cleaning")

  
f <- f[,c("id","height","fill","pos")]
fext <- fext[,c("id","height","fill","pos")]
f$type <- "inter"
fext$type <- "ext"
faces <- rbind(fext,f)

faces <- faces[order(faces$pos,decreasing = TRUE),]  

nb <- dim(faces)[1]
faces$none <- 0
faces$i <- 1:nb

span <- round(nb/10,0)
init <- span
pct <- 10

for (i in 1:nb){
  id <- (faces[i,"id"] %>% st_drop_geometry())[,1]
  poly1 <- st_geometry(faces[i,])
  poly2 <- st_union(st_geometry(faces[faces$i > i, ])) 
  poly2 <- st_union(poly2,st_geometry(tops[tops$id==id,]))
  geom <- st_difference(poly1,st_buffer(poly2,1))
  if (length(geom) > 0){
    st_geometry(faces[i,]) <- geom
  } else {
    faces[i,"none"] <- 1
  }
  
  if(i == span){
    cat(paste0("[",pct,"%]"))
    span <- span + init
    pct <- pct + 10
  }
}
cat("[100%]")
faces <- faces[faces$none == 0,]  

# Plot

plot(st_geometry(xraw),col=fill[1], add=add)

for(i in tops$id)
{
  plot(st_geometry(faces[faces$id == i,]), col=faces$fill[faces$id ==i], add=T)
  plot(st_geometry(tops[tops$id == i,]), col=fill[1], add=T)
}
message("")
message("Done")
} else {
  
 # f <- faces
  
  # Sort
  
  message("Extrusion et ordering")
  

  faces <- faces[,c("id","pos","fill")]
  tops$id[1]
  for (i in tops$id){
  tops[tops$id == i,"pos"] <- max(faces[faces$id == i,"pos"] %>% st_drop_geometry()) - 1000
  }  
  tops$fill <- fill[1]
  tops <- tops[,c("id","pos","fill")]
  
  geom <- rbind(faces, tops)
  geom <- geom[order(geom$pos, decreasing = TRUE),]

  # Display

  plot(st_geometry(xraw), add=add)
  plot(st_geometry(geom), col=geom$fill, add=T)

  }
}

# ------------------------

# Deform
v1 = 1
v2 = 1
v3 = 2.5  
states <- readRDS("us.rds")
frame <- getframe(states)
basemap <- deform(states, flatten = v1, rescale = c(v2,v3))
frame <- deform(frame, flatten = v1, rescale = c(v2,v3))
plot(st_geometry(frame))
plot(st_geometry(basemap), add=T)


# Example 1  
  
states <- readRDS("us.rds")
basemap <- deform(states)
extrude(basemap, var = "pop2019" , k = 1, col = "red")
  
# Example 2

states <- readRDS("us.rds")
frame <- deform(getframe(states))
basemap <- deform(states)
plot(st_geometry(frame))
extrude(basemap, "pop2019", k = 1, col = "white", add = TRUE)

# Example 3

world <- readRDS("worldpopgrid.rds")
world2 <- deform(world)
basemap <- world2[!is.na(world$pop2020),]
basemap <- basemap[basemap$pop2020 > quantile(basemap$pop2020, 0.9),]
plot(st_geometry(world2),col="#CCCCCC", border="white")
extrude(basemap, "pop2020", k = 10, col = "red", regular = TRUE, add=T)

# Example 4 (World countries extrud pop height)
world <- readRDS("world.rds")
frame <- deform(getframe(world))
basemap <- deform(world)
plot(st_geometry(frame))
plot(st_geometry(basemap), add=T)
extrude(basemap, "pop", k = 1.5, col = "white", add=T)

# Example 5 (Martinique)

library(cartography)
mtq <- st_read(system.file("gpkg/mtq.gpkg", package="cartography"))
basemap <- deform(rotate(mtq,-40))
extrude(basemap, "POP", k = 1, col = "white")

# Example 6 (Paris smooth)

library(SpatialPosition)
data("hospital")
# Compute potentials
pot <- quickStewart(x = hospital,
                    var = "capacity",
                    span = 1000,
                    beta = 2, mask = paris, 
                    returnclass = "sf")
basemap <- deform(pot)
extrude(basemap, "center", k = 3, col = "white", regular = FALSE)

# ------------------------------------------------------------------------
 

