#' Deform basemap.
#' @param x an sf object (polygon or multipolygons).
#' @param flatten numerical value indicating the scaling in Y (1 = no flattening).
#' @param rescale a vector of two numeric values indicating the deformation of the top and bottom of the figure (c(1,1) = no deformation).
#' @return an sf object (polygon or multipolygons).
#' @export
#
#' @examples
#' library(sf)
#' us <- st_read(system.file("us.gpkg", package="mapextrud"))
#' f <- getframe(us)
#' us2 <- deform(us)
#' # Example 1 (default values)
#' f2 <- deform(f)
#' plot(st_geometry(f2))
#' plot(st_geometry(us2), add = TRUE)
#' # Exemple 2 (user values)
#' us3 <- deform(us, flatten=0.5, rescale = c(0.5, 3))
#' f3 <- deform(f, flatten=0.5, rescale = c(0.5, 3))
#' plot(st_geometry(f3))
#' plot(st_geometry(us3), add = TRUE)
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

      for (n in 1:nrow(newpts)){

        Y <- newpts[n,"Y"]
        X <- newpts[n,"X"]

        # transformation en Y

        # Y <- Y + 100000
        Y <- Y * flatten
        newpts[n,"Y"] <- Y

        # transformation en X
        v <- c(ymin, Y, ymax)
        Y2 <- scales::rescale(v, c(rescale[2], rescale[1]))[2]
        X <- X * Y2
        newpts[n,"X"] <- X
      }

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
