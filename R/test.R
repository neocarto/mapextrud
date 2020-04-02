library(sf)
library(mapextrud)


library(raster)
library(rnaturalearth)


# Map Projection

crs <- "+proj=eqc +lat_ts=0 +lat_0=0 +lon_0=0 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=m +no_defs"

# Countries

countries <- ne_countries(scale = 110, type = "countries", returnclass = "sf")
countries <- st_transform(countries, crs)
countries <- countries[countries$adm0_a3 != "ATA",]
countries <- st_sf(st_buffer(st_union(countries),1))

# World population

r <- raster(system.file("worldpop.tif", package="mapextrud"))
dots <- as(r, 'SpatialPointsDataFrame')
dots <- st_as_sf(dots)
dots <- st_transform(dots, crs)
colnames(dots) <- c("pop2020","geometry")
grid <- st_make_grid(countries, cellsize = 200000, square = FALSE)
grid <- aggregate(dots["pop2020"], grid, sum)

# Deform

basemap <- deform(grid, flatten = 1.2)
frame <- deform(getframe(countries), flatten = 1.2)
world <- deform(countries, flatten = 1.2)

# Selection

total <- sum(basemap$pop2020, na.rm = TRUE)
basemap <- basemap[basemap$pop2020 > 5000000 & !is.na(basemap$pop2020),]
pct <- round((sum(basemap$pop2020) / total) * 100,0)

# Cartography

plot(st_geometry(frame), col = "#c5dbed", lwd = 0.2)
plot(st_geometry(world), col = "#e3dbca", border = "#7f8aad", lwd = 0.3, add = TRUE)
extrude(x = basemap, var = "pop2020", k = 5, col = "#e65c5c", lwd = 0.2, regular = TRUE, add = TRUE)

# Layout
text(x=  -28000000, y =11500000, "2020: A Urban World", cex = 0.9, col = "#5e698a", pos = 4)
text(x=  42000000, y = -11500000, "Made with the R package mapextrud", cex = 0.6, col = "#414245", pos = 2)
