
library(sf)
library(here)

nytract <- readRDS(here("inst/extdata/nytract.RDS"))

sf_use_s2(FALSE)
nyoutline <- st_union(nytract) %>% st_buffer(dist = 0.0001) %>%
  rmapshaper::ms_simplify(.)
nyc <- nyoutline %>% st_transform(4326)

saveRDS(nyc, file = "inst/extdata/nyc.rds")
