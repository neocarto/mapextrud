#' Legend
#' @param x an sf object (polygon or multipolygons).
#' @param var name of the numeric field in df to plot.
#' @param k numeric value to adjust the height of the extruded polygons
#' @param title.txt Title of the legend
#' @param unit unit of the variable
#' @param title.cex title cex
#' @param values.cex velues cex
#' @param pos c(x,y) position on the legend
#' @param title.font title font
#' @return
#' @export
#' @examples
#' library(sf)
#' us <- st_read(system.file("us.gpkg", package="mapextrud"))
#' basemap <- deform(us)
#' extrude(basemap, var = "pop2019" , k = 1, col = "white")
#' legendextrud(x = basemap, var = "pop2019", k = 1, title.txt = "Population in 2019", unit = "inh.")

legendextrud <- function(x, var,k, title.txt = "Title of the legend", unit = "",
                         title.cex = 0.8, values.cex = 0.6,
                         pos = NULL, title.font = 2){

h <- st_bbox(x)[4]-st_bbox(x)[2]
m <- max(x[,var] %>% st_drop_geometry())
k <- as.numeric(k * 0.1 * h / m)

valmin <- min(x[,var] %>% st_drop_geometry(), na.rm=TRUE)
valmax <- max(x[,var] %>% st_drop_geometry(), na.rm=TRUE)
heightmin <- valmin * k
heightmax <- valmax * k
height <- heightmax - heightmin

delta <- (par()$usr[2] - par()$usr[1]) / 50
if (is.null(pos)){
  pos <- c(par()$usr[1] + delta ,par()$usr[4] - delta)
}


text(x = pos[1] ,y = pos[2], title.txt, adj = c(0,0), cex = title.cex, font = title.font)
segments(x0 = pos[1], y0 = pos[2] - delta, x1 = pos[1] + delta, y1 = pos[2] - delta, col= 'black')
text(x = pos[1] + delta * 1.5, y = pos[2] - delta, paste0(valmax, " ",  unit), cex = values.cex, adj = c(0,0.4))
segments(x0 = pos[1], y0 = pos[2] - delta, x1 = pos[1], y1 = pos[2] - delta - height, col= 'black')
segments(x0 = pos[1], y0 = pos[2] - delta - height, x1 = pos[1] + delta, y1 = pos[2] - delta - height, col= 'black')
text(x = pos[1] + delta * 1.5, y = pos[2] - delta - height, valmin, cex = values.cex, adj = c(0,0.4))
}
