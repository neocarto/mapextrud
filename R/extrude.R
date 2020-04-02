#' Extruded map (beta version)
#'
#' @param x an sf object (polygon or multipolygons).
#' @param var name of the numeric field in df to plot.
#' @param k numeric value ti adjust the height of the extruded polygons
#' @param border color border
#' @param lwd line thickness
#' @param col One color or the field name containg colors for each polygons
#' @param regular use TRUE with regular grids for faster calculation
#' @param add whether to add the layer to an existing plot (TRUE) or not (FALSE).
#'
#' @return
#' @export
#'
#' @examples
#' library(sf)
#' us <- st_read(system.file("us.gpkg", package="mapextrud"))
#' basemap <- deform(us)
#' extrude(basemap, var = "pop2019" , k = 1, col = "white")
extrude <- function(x, var, k = 1, lwd=1, col = "white", border = "black", regular = FALSE, add = FALSE) {
#

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

  # g <- unique(as.character(sf::st_geometry_type(x)))

  single <- x[st_geometry_type(x) == "POLYGON",]
  multi <- x[st_geometry_type(x) == "MULTIPOLYGON",]
  exploded <- st_cast(multi,"POLYGON", warn = FALSE)

  x <- rbind(single, exploded)

  # x <- st_cast(x, "POLYGON", warn = FALSE)


  n2 <- dim(x)[1]
  if(n2 > n1){
    message("Splitting multi-part polygon into single polygons. The same value is assigned to each splitted polygon.")
    x$id <- row.names(x)
  }

  nodes <- st_cast(x,"MULTIPOINT", warn = FALSE)
  nodes <- st_cast(nodes, "POINT", warn=FALSE)


  if(dim(nodes)[1]> 2000 & regular == FALSE){
    if (interactive()){
      cat(paste0("Sorry, the script is not optimized...\nThe basemap is not enough generalized (",dim(nodes)[1]," nodes) and the computation will take time.\nDo you still want to continue ? [y/n]"))
      z <- readLines(con = stdin(), n = 1)
      while (!z %in% c("n","y")){
        cat ("Enter y or n")
        z <- readLines(con = stdin(), n = 1)
      }
      if (z == "y"){
        cat("Okay!\nGo get a coffee and come back later to see the result (or press esc to abort)\n")
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


  if(col %in% names(x)){
    fill <- c("white","#d1c9b2","#b8b1a0")
    faces$fill <- fill[2]
    faces[faces$ang > 0,"fill"] <- fill[3]

    }  else{
    if(col == "white"){
      fill <- c("white","white","white")
    } else {
      pal <- colorRampPalette(c("white",col,"black"))(11)
      fill <- c(col, pal[3],pal[7])
    }
    faces$fill <- fill[2]
    faces[faces$ang > 0,"fill"] <- fill[3]
  }

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

    plot(st_geometry(xraw),col=fill[1], lwd = lwd, add=add)
    # plot(st_geometry(xraw),col="white", lwd = lwd, add=add)

    for(i in tops$id)
    {
      plot(st_geometry(faces[faces$id == i,]), col=faces$fill[faces$id ==i], lwd = lwd, border = border, add = TRUE)

      if(col %in% names(x)) {
        plot(st_geometry(tops[tops$id == i,]), col=tops$color[tops$id == i], lwd = lwd, border = border, add = TRUE)
      } else {
        plot(st_geometry(tops[tops$id == i,]), col=fill[1], lwd = lwd, border = border, add = TRUE)
      }
    }
    message("")
    message("Done")

  } else {

    # f <- faces

    # Sort

    message("Extrusion et ordering")


    faces <- faces[,c("id","pos","fill")]
    for (i in tops$id){
      tops[tops$id == i,"pos"] <- max(faces[faces$id == i,"pos"] %>% st_drop_geometry()) - 1000
    }

    if(col %in% names(x)){
    tops$fill <- (tops[,col] %>% st_drop_geometry())[,1]
    } else {
     tops$fill <- fill[1]
    }

    tops <- tops[,c("id","pos","fill")]

    geom <- rbind(faces, tops)
    geom <- geom[order(geom$pos, decreasing = TRUE),]

    # Display

    plot(st_geometry(xraw), lwd = lwd, border = border, add=add)
    plot(st_geometry(geom), col=geom$fill, lwd = lwd, border = border, add = TRUE)

  }
}
