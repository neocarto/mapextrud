library(sf)
library(scales)


  # original sf
  
  states <- readRDS("us.rds")
  crs <- "+proj=aea +lat_1=29.5 +lat_2=45.5 +lat_0=37.5 +lon_0=-96 +x_0=0 +y_0=0 +ellps=GRS80 +datum=NAD83 +units=m +no_defs "
  states <- st_transform(states, crs)
  plot(st_geometry(states))
  
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
  
deform <- function(x, flatten = 0.8, rescale = c(1.3,2), rotate = 0){

# x = states # enlever  
  ymin <- as.numeric(st_bbox(x)[2]) # prendre en compte le frame ?
  ymax <- as.numeric(st_bbox(x)[4]) # prendre en compte le frame ?
  
  # Centering
  cx <- as.numeric((st_bbox(x)[3] + st_bbox(x)[1]) / 2)
  cy <- as.numeric((st_bbox(x)[4] + st_bbox(x)[2]) / 2)
  geom <- st_geometry(x)
  geom <- geom - c(cx, cy)
  st_geometry(x) <- geom
  
  
  # Rotate
  
  x <- rotate(x, rotate)
  
  # translate nodes
  
  for (i in 1:nrow(x)){
  
 # i = 1 # enlever  
  poly <- st_geometry(x)[[i]]
  pts <- st_coordinates(poly)
  idx <- unique(pts[, colnames(pts) %in% c("L1", "L2", "L3")])
 
        # For each polygons in multipolygons
        for(k in 1:nrow(idx)) {
         
# k = 1 # enlever
          
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

x <- deform(states)
f <- deform(getframe(states))
plot(st_geometry(f))
plot(st_geometry(x), add=T)
# View(x)
#x <- x[x$id %in% c("CA", "AZ"),]

# EXTRUDE

x <- deform(states)
var = "pop2019"
k = 0.5
col = "red"
add = FALSE
extrude <- function(x, var, k = 0.5, col = "red", add = FALSE) {
  
  # multi polygons -> polygons
  x <- st_cast(x, "POLYGON")
  x$id <- row.names(x)

  # colors
  if(col == "white"){
  fill <- c("white","white","white") 
  border <- c("black","black","black")
  } else {
  pal <- colorRampPalette(c("white",col,"black"))(11)
  fill <- c(col, pal[3],pal[7]) 
  border <- c(pal[9], pal[4],pal[8])
  }
  
  # sort polygons
  x$order <- st_coordinates(st_centroid(st_geometry(x)))[,2]
  x <- x[order(x$order, decreasing = TRUE),]
  
  # plot(st_geometry(x), col="#CCCCCC", border="white",add = add)
  
  for (i in 1:nrow(x)){

    height <-  data.frame(x[i,var])[1]
    poly <- st_geometry(x)[[i]]
    poly2 <- poly + c(0, as.numeric(x[i,var])[1] * k)
    top <- st_sf(x[i,], geometry = st_sfc(poly2))
    pts <- st_coordinates(poly)
    pts2 <- st_coordinates(poly2)
    nb <- dim(pts)[1] - 1
    idx <- unique(pts[, colnames(pts) %in% c("L1", "L2", "L3")])
    nb <- dim(pts)[1] - 1
    
      for(z in 1:nb){
      dot1 <- c(pts[z,"X"], pts[z,"Y"])
      dot1["X"]
      dot2 <- c(pts[z+1,"X"], pts[z+1,"Y"])
      dot3 <- c(pts2[z+1,"X"], pts2[z+1,"Y"])
      dot4 <- c(pts2[z,"X"], pts2[z,"Y"])
      pos <- as.numeric((dot1["Y"] +  dot2["Y"]) / 2)
      ang <- atan((dot2["Y"] - dot1["Y"]) / ( dot2["X"] - dot1["X"]))*180/pi
      if (z == 1 & i == 1){
        faces <- st_sf(id = x$id[i], pos = pos, ang = ang, fill = fill[2], border = border[2], height = height, geometry=st_sfc(st_polygon(list(rbind(dot1, dot2, dot3, dot4, dot1)))))

      } else {
        face <- st_sf(id = x$id[i], pos = pos, ang = ang, fill = fill[2], border = border[2], height = height, geometry=st_sfc(st_polygon(list(rbind(dot1, dot2, dot3, dot4, dot1)))))
        faces <- rbind(faces, face)

      }
      }
    
    if (i == 1){
      tops <- top
    } else {
      tops <- rbind(tops,top)
    }
    
  }
    
  faces <- faces[order(faces$pos, decreasing = TRUE),]
  faces$fill <- as.character(faces$fill)
  faces$border <- as.character(faces$border)
  faces[faces$ang > 0,"fill"] <- fill[3]   
  faces[faces$ang > 0,"border"] <- border[3]   
  #plot(st_geometry(faces), col=faces$fill, border = faces$border, lwd=0.4, add=TRUE)
  # plot(st_geometry(poly2), col=fill[1], border = border[1], add= TRUE)
  
  return(list(faces,tops))
}


View(x)
test <- extrude(x[x$id %in% c("CA","NV","AZ"),], "pop2019", k = 0.01, col ="#f0a800",add=T)
# test <- extrude(x, "pop2019", k = 0.01, col ="#f0a800",add=T)
faces <- test[[1]]
tops <- test[[2]]

faces$dbl <- duplicated(faces$pos)
pos <- faces[faces$dbl == TRUE, "pos"]
st_geometry(pos) <- NULL

# Faces interieures

for (i in 1:length(pos[,1])){
  p = pos[i,1]
  vars <- faces[faces$pos == p,var]
  st_geometry(vars) <- NULL
  x1 <- faces[faces$pos == p & faces$pop2019 == max(vars),]
  x2 <- faces[faces$pos == p & faces$pop2019 == min(vars),]
  f <- st_difference(x1,x2)
  if(i == 1){int <- f} else {int <- rbind(int,f)}
}

# Faces exterieurs

for (i in 1:nrow(tops)){
  id = tops[i,"id"]
  st_geometry(id) <- NULL
  ok <- faces[faces$id == id$id,]
  ok <- st_difference(ok,tops[i,])
  if (i == 1) {
    ext <- ok
  } else {
    ext <- rbind(ext,ok)
  }
}


ids <-tops[order(tops$pop2019, decreasing = FALSE),"id"]
st_geometry(ids) <- NULL
ids <- ids$id

plot(st_geometry(x[x$id %in% c("CA","NV","AZ"),]))

i = 1

p1_a <- ext[ext$id == ids[i], ]
p1_b <- int[int$id == ids[i], ]
p1_c <- tops[tops$id == ids[i],]
plot(st_geometry(p1_b), col = "green", add=T)
plot(st_geometry(p1_c), col = "yellow", add=T)
plot(st_geometry(p1_a), col = "blue", add= T)
i = 2

p1_a <- ext[ext$id == ids[i], ]
p1_b <- int[int$id == ids[i], ]
p1_c <- tops[tops$id == ids[i],]
plot(st_geometry(p1_b), col = "green", add=T)
plot(st_geometry(p1_c), col = "yellow", add=T)
plot(st_geometry(p1_a), col = "blue", add= T)

i = 3

p1_a <- ext[ext$id == ids[i], ]
p1_b <- int[int$id == ids[i], ]
p1_c <- tops[tops$id == ids[i],]
plot(st_geometry(p1_b), col = "green", add=T)
plot(st_geometry(p1_c), col = "yellow", add=T)
plot(st_geometry(p1_a), col = "blue", add= T)
