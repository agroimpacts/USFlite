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

system.time(buildings <- st_read(here("external/data/buildings.geojson")))
# system.time(buildings <- geojson_sf(here("external/data/buildings.geojson")))

sf_use_s2(FALSE)
buildings <- buildings %>%
  dplyr::select(doitt_id, groundelev, heightroof) %>%
  mutate(heightroof_1 = as.numeric(heightroof) / 3.281) %>%
  mutate(area = as.numeric(units::set_units(st_area(.), "m^2"))) %>%
  mutate(volume = area * heightroof_1)

nld <- rast(system.file("extdata/nightlights_mean.tif", package = "USFlite"))
nyc <- readRDS(system.file("extdata/nyc.rds", package = "USFlite"))

r <- nld[[1]]
r[] <- 1:ncell(r)
r <- mask(r, vect(nyc))
p <- as.polygons(r) %>% st_as_sf()
# plot(p)
pgeo <- st_buffer(st_transform(p, st_crs(buildings)), dist = 0)

## Create building volume grid
# Convert building height to meters, calculate area, then calculate volume
buildings <- st_buffer(st_make_valid(buildings), dist = 0)
build_int <- st_intersection(buildings, pgeo)
build_int <- build_int %>%
  mutate(area_seg = as.numeric(units::set_units(st_area(.), m^2))) %>%
  mutate(volume_seg = area_seg / area * volume)

# Find total building volume in each cell
build_dims <- st_drop_geometry(build_int) %>%
  rename(cid = X2012.2021) %>%
  group_by(cid) %>%
  summarise(area = sum(area_seg, na.rm = TRUE),
            vol = sum(volume_seg, na.rm = TRUE) / 1e06, # to cubic hectometers
            hgt = mean(heightroof_1, na.rm = TRUE)) %>%
  left_join(p %>% rename(cid = "2012-2021"), .) %>%
  na.omit(.)

# Create gridded building volume raster
buildr <- lapply(c("area", "vol", "hgt"), function(x) {
  rasterize(vect(build_dims), r, x)
}) %>% do.call(c, .)
# plot(buildr)

writeRaster(buildr, filename = "inst/extdata/nyc_building_dims.tif",
            overwrite = TRUE)


