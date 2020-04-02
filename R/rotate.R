#' @title Rotate basemap.
#' @description Rotate sf basemap.
#' @name rotate
#' @param x an sf object.
#' @param deg degree angle.
#' @return an sf object.
#' @export
#'
#' @examples
#' library(sf)
#' us <- st_read(system.file("us.gpkg", package="mapextrud"))
#' us2 <-rotate(us, 180)
#' plot(st_geometry(us2))
rotate <- function(x, deg){
  geom <- st_geometry(x)
  rot = function(a) matrix(c(cos(a), sin(a), -sin(a), cos(a)), 2, 2)
  ctr <- st_centroid(st_as_sfc(st_bbox(geom)))
  geom <- (geom - ctr) * rot(deg * pi/180) + ctr
  st_geometry(x) <- geom
  return(x)
}
