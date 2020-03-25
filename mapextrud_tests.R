library(sf)
library(scales)

  # original sf
  
  # vintage frame
  
  frame <- function(x){
    bb <- st_bbox(x)
    d <- (bb[3] - bb[1]) / 50
    bb <- bb + c(-d, -d, d, d)
    frame <- st_as_sfc(bb, crs = st_crs(x))
    f <- st_segmentize(frame, d)
    f <- st_cast(f, "POINT")
    plot(f, pch=3)
  }
  
  # frame(states)
  # plot(st_geometry(states), add=T)
  
  # get frame
  
  getframe <- function(x){
    bb <- st_bbox(x)
    d <- (bb[3] - bb[1]) / 50
    bb <- bb + c(-d, -d, d, d)
    frame <- st_as_sfc(bb, crs = st_crs(x))
    frame <- st_sf(geometry = frame)
    return(frame)
  }
  
  
  
  # f <- getframe(states)
  # plot(st_geometry(states))
  # plot(st_geometry(f), add=T)
  
  
  # centroid
  
  center <- function(x){
    x <- st_as_sfc(st_bbox(x))
    x <- st_sf(geometry = st_centroid(x))
    x$x <- st_coordinates(x)[1]
    x$y <- st_coordinates(x)[2]
    x <- x[,c("x","y","geometry")]
    return(x)
   }
  
  # c <- center(states)
  # plot(st_geometry(states))
  # plot(st_geometry(c), add=TRUE, pch=20, lwd = 10)
  
  
  # Rotate
  
  rotate <- function(x, deg){
  geom <- st_geometry(x) 
  rot = function(a) matrix(c(cos(a), sin(a), -sin(a), cos(a)), 2, 2)   
  ctr <- st_centroid(st_as_sfc(st_bbox(geom)))
  geom <- (geom - ctr) * rot(deg * pi/180) + ctr                     
  st_geometry(x) <- geom
  return(x)
  }
  
  # x <- rotate(states, 0)
  # plot(st_geometry(x))
    # Deform -----------------------------------------------------------------------------------
  
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
  
# EXTRUDE

x <- deform(world)
var = "pop2019"
plot(st_geometry(world))
k = 1
col = "red"

plot(st_geometry(x))

extrude <- function(x, var, k = 1, col = "red", add = FALSE) {

  h <- st_bbox(x)[4]-st_bbox(x)[2]
  m <- max(x[,var] %>% st_drop_geometry())
  k = k * 0.1 * h / m

  n1 <- dim(x)[1]  
  x <- st_cast(x, "POLYGON", warn = FALSE)
  n2 <- dim(x)[1]
  if(n2 > n1){message("Splitting multi-part polygon into single polygons. The same value is assigned to each splitted polygon.")}
  
  x$id <- row.names(x)
  x <- x[,c("id",var)]
  v <- x[,var] %>% st_drop_geometry()
  x$height <- as.numeric(v[,var] * k)

  # Polygons to dots
    
  nodes <- st_cast(x,"POINT", warn = FALSE)
  #nodes <- nodes[nodes$id %in% c(1:1),] # enlever
  nb <- dim(nodes)[1] - 1
  
  
message("Topology Detection")  
pct <- 0
for (i in 1:nb){
  n <- seq(1, nb, by = round(nb/10,0))

  if (i %in% n) {
    pct <- pct + 10
    message(paste0("[",pct,"%]"), appendLF = FALSE)
    }

  id1 <- nodes[i,"id"] %>% st_drop_geometry()
  id2 <- nodes[i+1,"id"] %>% st_drop_geometry()  
  x1 <- st_coordinates(nodes[i,])[1]
  y1 <- st_coordinates(nodes[i,])[2]
  x2 <- st_coordinates(nodes[i+1,])[1]
  y2 <- st_coordinates(nodes[i+1,])[2]
  pos <- (y1 + y2) / 2
  c <- st_sfc(st_point(c(X = (x1 + x2) / 2, Y = (y1 + y2) / 2)))
  
  if (i == 1){
  dots <- st_sf(x1 = x1, x2 = x2, y1 = y1, y2 = y2, pos = pos,  geometry=c)
  } else {
      if (id1 == id2) {
       dots <- rbind(dots,st_sf(x1 = x1, x2 = x2, y1 = y1, y2 = y2, pos = pos, geometry=c))  
      }
  }
  
}



  # duplicate suppression

  dots$dbl <- duplicated(dots$geometry)
  dots <- dots[!dots$dbl,]  

  # Sort
  
  dots <- dots[order(dots[,"pos"] %>% st_drop_geometry() , decreasing = TRUE),]

  # Dots -> faces 
  test <- st_is_within_distance(dots,x,1)
  
  message("")    
  message("Extrusion")  
  
  pct <- 0

  for (i in 1 : dim(dots)[1]){
    
    
    n <- seq(1, dim(dots)[1], by = round(dim(dots)[1]/10,0))
    n <- n[1:length(n)-1]
    
    if (i %in% n) {
      pct <- pct + 10
      message(paste0("[",pct,"%]"), appendLF = FALSE)
    }
    
    
    tmp1 <- as.numeric(x[test[[i]][1],"height"] %>% st_drop_geometry())
    tmp2 <- as.numeric(x[test[[i]][2],"height"] %>% st_drop_geometry())
    height1 <- min(tmp1, tmp2)
    height2 <- max(tmp1, tmp2, na.rm=TRUE)
    
    if (tmp1 > tmp2 | is.na(tmp2)){
      dots$id[i]  <- as.numeric(x[test[[i]][1],"id"])[1]
    } else {
      dots$id[i] <- as.numeric(x[test[[i]][2],"id"])[1]
    }
    
    if(is.na(height1)){height1 <- 0}
    dot1 <- c(dots$x1[i],dots$y1[i] + height1)
    dot2 <- c(dots$x2[i],dots$y2[i] + height1)
    dot3 <- c(dots$x2[i],dots$y2[i] + height2)
    dot4 <- c(dots$x1[i],dots$y1[i] + height2)
    dots[i,"ang"] <- atan((dot2[2] - dot1[2]) / ( dot2[1] - dot1[1]))*180/pi
    st_geometry(dots[i,]) <- st_sfc(st_polygon(list(rbind(dot1, dot2, dot3, dot4, dot1))))
  }
  
  faces <- dots 
  
  plot(st_geometry(faces))
  # colors
  
  if(col == "white"){
    fill <- c("white","white","white") 
    border <- c("black","black","black")
  } else {
    pal <- colorRampPalette(c("white",col,"black"))(11)
    fill <- c(col, pal[3],pal[7]) 
    border <- c(pal[9], pal[4],pal[8])
  }
  
  faces$fill <- fill[2]
  faces[faces$ang > 0,"fill"] <- fill[3]   
  faces$border <- border[2]
  faces[faces$ang > 0,"border"] <- border[3]  

  # Tops
  tops <- x
  for (i in 1:dim(tops)[1])
  {
    st_geometry(tops[i,]) <- st_geometry(tops[i,]) + + c(0, as.numeric(x[i,var])[1] * k)
  }

# Sort & plot
  
  # faces <-  faces[order(faces[,"pos"])]
  
  ids <- tops[order(tops[,var] %>% st_drop_geometry(), decreasing = FALSE),"id"] %>% st_drop_geometry()
  ids <- ids$id

  plot(st_geometry(x), add = add)


  for (i in ids){
    #plot(st_geometry(faces[faces$id == i,]), col=faces$fill[faces$id == i], border = faces$border, add = TRUE)      
    plot(st_geometry(faces[faces$id == i,]), col=faces$fill[faces$id == i], border = "black", add = TRUE)      
    plot(st_geometry(tops[i,]), col = fill[1], add = TRUE)
  }
  
  message("")    
  message("Done")  
  
  return(list(faces,tops))

}


# ----------------------------------------
# Regular extrud

# states <- readRDS("us.rds")
# x <- deform(states)
# var = "pop2019"
# k = 1
# col = "red"

extrude2 <- function(x, var, k = 1, col = "red", add = FALSE) {

h <- st_bbox(x)[4]-st_bbox(x)[2]
m <- max(x[,var] %>% st_drop_geometry())
k = k * 0.1 * h / m  
  
x$id <- row.names(x)
x[,"height"] <- x[,var] %>% st_drop_geometry() * k
x[is.na(x[,var]),"height"] <- 0
n1 <- dim(x)[1]  
x <- st_cast(x, "POLYGON", warn = FALSE)
n2 <- dim(x)[1]
if(n2 > n1){message("Splitting multi-part polygon into single polygons. The same value is assigned to each splitted polygon.")}
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

if(col == "white"){
  fill <- c("white","white","white") 
  border <- c("black","black","black")
} else {
  pal <- colorRampPalette(c("white",col,"black"))(11)
  fill <- c(col, pal[3],pal[7]) 
  border <- c(pal[9], pal[4],pal[8])
}
if(length(col)==2){fill <- c(col[1],col[2],col[2])}
if(length(col)==3){fill <- col}

faces$fill <- fill[2]
faces[faces$ang > 0,"fill"] <- fill[3]   

# Tops

tops <- x

for (i in 1:dim(tops)[1])
{
  st_geometry(tops[i,]) <- st_geometry(tops[i,]) + c(0, as.numeric(x[i,var])[1] * k)
}
tops <- tops[order(tops$height, decreasing = FALSE),]

# faces <- st_difference(faces,st_buffer(st_union(st_geometry(tops)),1))

# plot(st_geometry(faces), col="red")

# Opérations géométriques

# 1 - Faces cleaning

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
    st_geometry(f[i,]) <- st_difference(st_geometry(f1[i,]),st_geometry(f2[i,]) )
    f$id[i] <- f1$id[i]
  } else {
    st_geometry(f[i,]) <- st_difference(st_geometry(f2[i,]),st_geometry(f1[i,]) )
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


plot(st_geometry(x))
plot(st_geometry(f), col = f$fill, add=T)
plot(st_geometry(tops), col=fill[1], add=T)


# for(i in tops$id)
# {
#   plot(st_geometry(f[f$id == i,]), col="red", add=T)
#  plot(st_geometry(tops[tops$id == i,]), col="yellow", add=T)
# }

message("Done")

}

# ------------------------

states <- readRDS("us.rds")
basemap <- deform(states)
extrude2(basemap, var = "pop2019" , k = 2, col = "orange", add = FALSE)
  

# ------------------------------------------------------------------------
 
# TEST

states <- readRDS("us.rds")
x <- deform(states)
tmp <- extrude(x, "pop2019", k = 1, col = "red")
faces <- tmp[[1]]
tops <- tmp[[2]]


tops <- tops[order(tops$height, decreasing = FALSE),]

faces <- faces[order(faces$pos, decreasing = TRUE),]
plot(st_geometry(faces), col ="red")

tops <- tops[order(tops$height, decreasing = FALSE),]
plot(st_geometry(tops[1,]), col ="blue", add=T)
plot(st_geometry(tops), col ="blue", add=T)

View(faces)
View(tops)




# EXAMPLE 1
# Population in Us States (Height)

View(states)

states <- readRDS("us.rds")
x <- deform(states)
extrude(x, "pop2019", k = 1, col = "white")

# EXAMPLE 2 (with frame)
# Population in Us States (Height)

states <- readRDS("us.rds")
f <- deform(getframe(x))
x <- deform(states)
plot(st_geometry(f))
extrude(x, "pop2019", k = 1, col = "white", add = TRUE)

# EXAMPLE 3
# Population in Us States (Height)

states <- readRDS("us.rds")
x <- deform(states)
extrude(x, "pop2019", k = 1, col = "red")


# EXAMPLE 2 (en cours)
# Population in Us States (Volume)

states <- readRDS("us.rds")
states <- st_cast(states, "POLYGON", warn = FALSE)
states$area <- st_area(states)
x <- deform(states)

# EXAMPLE 3
# World Population

world <- readRDS("worldpopgrid.rds")
# x <- deform(world)
# extrude(x, "pop2020", k = 1, col = "white")
