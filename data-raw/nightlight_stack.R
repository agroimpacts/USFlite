# Prepare monthly nightlights data downloaded from
# https://ladsweb.modaps.eosdis.nasa.gov
# and first converted to geotiff using translate_hdf5.ipynb

library(dplyr)
library(sf)
library(terra)


nytract <- readRDS(system.file("extdata/nytract.RDS", package = "USFlite")) %>%
  st_transform(4326)
imgs <- dir("~/Dropbox/data/imagery/nightlights/", full.names = TRUE,
            pattern = "tif")

nlr <- lapply(imgs, function(x) {  # x <- imgs[1]
  print(basename(x))
  r <- rast(x)
  yd <- gsub("A", "", strsplit(basename(x), split = "\\.")[[1]][2])
  yd <- as.Date(yd, "%Y%j")

  cr <- crop(r, vect(nytract))
  cr[cr == 65535] <- NA
  names(cr) <- yd
  cr
})

nlrs <- do.call(c, nlr)
# plot(nlrs[[1]])
# plot(vect(nytract), add = TRUE)

# Monthly means
nldates <- as.Date(names(nlrs), "%Y-%m-%d")
mos <- lubridate::month(nldates)

monthly_means <- lapply(unique(mos), function(x) {
  app(nlrs[[which(mos == x)]], mean, na.rm = TRUE)
}) %>% do.call(c, .)
names(monthly_means) <- unique(mos)

longterm <- app(nlrs, mean, na.rm = TRUE)
names(longterm) <- "2012-2021"

# write out results
writeRaster(nlrs, filename = "inst/extdata/nightlights.tif", overwrite = TRUE)
writeRaster(monthly_means, filename = "inst/extdata/nightlights_mean_month.tif",
            overwrite = TRUE)
writeRaster(longterm, filename = "inst/extdata/nightlights_mean.tif",
            overwrite = TRUE)
saveRDS(nldates, file = "inst/extdata/nightlight_dates.rds")

