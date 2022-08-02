# Create raster at resolution of night lights grid summarizing height, area,
# volume of NYC buildings

# Buildings dataset from:
# https://github.com/CityOfNewYork/nyc-planimetrics/blob/master/Capture_Rules.md
# https://data.cityofnewyork.us/Housing-Development/Shapefiles-and-base-map/

library(geojsonsf)
library(sf)
library(dplyr)
library(terra)
library(here)

# system.time(buildings <- st_read(here("nightlight/buildings.geojson")))
system.time(buildings <- geojson_sf(here("nightlight/buildings.geojson")))

sf_use_s2(FALSE)
buildings <- buildings %>%
  dplyr::select(doitt_id, groundelev, heightroof) %>%
  mutate(heightroof_1 = as.numeric(heightroof) / 3.281)

nld <- rast(here("nightlight/s.tif"))
r <- nld[[1]]
r[] <- 1:ncell(r)
r <- mask(r, nld[[1]])
p <- as.polygons(r) %>% st_as_sf()

## Create building volume grid
# Convert building height to meters, calculate area, then calculate volume
buildings <- st_buffer(st_make_valid(buildings), dist = 0)
build_int <- st_intersection(buildings, st_transform(p, st_crs(buildings)))
build_int <- build_int %>%
  mutate(area_seg = as.numeric(units::set_units(st_area(.), m^2))) %>%
  mutate(volume_seg = area_seg / area * volume)

# Find total building volume in each cell
build_dims <- st_drop_geometry(build_int) %>%
  group_by(s_1) %>%
  summarise(area = sum(area_seg, na.rm = TRUE),
            vol = sum(volume_seg, na.rm = TRUE) / 1e06, # to cubic hectometers
            hgt = mean(heightroof_1, na.rm = TRUE)) %>%
  left_join(p, .)

# Create gridded building volume raster
buildr <- lapply(c("area", "vol", "hgt"), function(x) {
  rasterize(vect(build_dims), r, x)
}) %>% do.call(c, .)
# plot(buildr)

if(!dir.exists(here("inst/exdata/"))) {
  dir.create(here("inst/exdata/"), recursive = TRUE)
}
writeRaster(buildr, filename = "inst/exdata/nyc_building_dims.tif")


