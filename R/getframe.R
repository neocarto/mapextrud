#' @title Get Frame.
#' @description Make a frame around the map.
#' @name getframe
#' @param x an sf object.
#' @return an sf object.
#' @export
#'
#' @examples
#' library(sf)
#' us <- st_read(system.file("us.gpkg", package="mapextrud"))
#' f <- getframe(us)
#' plot(st_geometry(f))
#' plot(st_geometry(us), add = TRUE)
getframe <- function(x){
  bb <- st_bbox(x)
  d <- (bb[3] - bb[1]) / 50
  bb <- bb + c(-d, -d, d, d)
  frame <- st_as_sfc(bb, crs = st_crs(x))
  frame <- st_sf(geometry = frame)
  return(frame)
}
