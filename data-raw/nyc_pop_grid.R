# create gridded population raster from NYC census data, as well as grid of
# percentages of public housing

library(sf)
library(dplyr)
library(terra)


# NYC population raster
nytract <- readRDS(system.file("extdata/nytract.RDS", package = "USFlite"))
ny_pop <- readRDS(system.file("extdata/ny_pop.RDS", package = "USFlite"))
nycha <- readRDS(system.file("extdata/nycha.RDS", package = "USFlite"))

# Add area to census tracts then join population data
nytract_pop <- nytract %>%
  mutate(area = as.numeric(st_area(.) / 10^6)) %>%
  mutate(BoroCT2020 = as.numeric(BoroCT2020)) %>%
  inner_join(., ny_pop, by = "BoroCT2020")

# Get nightlights grid
nld <- rast(system.file("extdata/nightlights_mean.tif", package = "USFlite"))
nyc <- readRDS(system.file("extdata/nyc.rds", package = "USFlite"))
r <- nld[[1]]
r[] <- 1:ncell(r)
r <- mask(r, vect(nyc))
p <- as.polygons(r) %>% st_as_sf()
pgeo <- st_buffer(st_transform(p, st_crs(nytract_pop)), dist = 0) %>%
  rename(cid = "2012-2021")
# plot(p)

# Intersect tracts with polygon grid and calculate area of segments
# then calculate population within each 'cutup' census tract piece
tracts_cut <- st_intersection(x = nytract_pop, y = pgeo) %>%
  mutate(area_seg = as.numeric(st_area(.) / 10^6)) %>%
  mutate(pop_seg = area_seg / area * Pop_20)

# Calculate sum of population of tract segments within each cell
# The 'layer' column indicates which cell index the tract segment belongs to
r_pop_vals <- tracts_cut %>% group_by(cid) %>% st_drop_geometry() %>%
  summarise(., pop = sum(pop_seg, na.rm = TRUE))
# tracts_cut %>% slice(1:100) %>% select(cid) %>% plot()
# tracts_cut %>% filter(cid == 216) %>% select(area) %>% plot()

cids <- values(r)[!is.na(values(r))]
# plot(p$geometry)
# plot(p %>% filter(`2012-2021` == cids[!cids %in% r_pop_vals$cid]), add = TRUE,
#      col = "red")

plot(nytract_pop["Pop_20"])
popr <- subst(r, from = r_pop_vals$cid, to = r_pop_vals$pop)
popr[popr == cids[!cids %in% r_pop_vals$cid]] <- 0
plot(popr)

# Convert population to integer
popr[] <- as.integer(popr[])

writeRaster(popr, filename = "inst/extdata/nyc_pop.tif",
            overwrite = TRUE)
# popr <- rast("inst/extdata/nyc_pop.tif")

# Create raster with proportion of pixel covered by public housing
city_background <- popr
city_background[city_background >= 0] <- 0

ph_fraction <- exactextractr::coverage_fraction(
  raster::raster(r), st_combine(nycha)
)[[1]]
ph_fraction[is.na(ph_fraction)] <- 0
ph_fraction <- mask(rast(ph_fraction), city_background)
# plot(ph_fraction)

writeRaster(ph_fraction, filename = "inst/extdata/nyc_ph_coverage.tif",
            overwrite = TRUE)

