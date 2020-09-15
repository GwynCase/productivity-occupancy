setwd('C:/Users/gwync/sfuvault/productivity-occupancy/notebooks')
library(tidyverse)
library(sf)
library(raster)

#################
# Rasterize HSI
#################

# The first step is to bring in the HSI shapefile and prep the raster.

## First import habitat HSI shapefile.
f.hsi <- st_read('../data/external/foraging_sc.shp')

## Set raster extent based on HSI shapefile.
ext <- extent(f.hsi)

## Make an empty raster to populate with values.
## (100m is the resolution of the original data.)
r <- raster(ext, res=c(100, 100))


# For the default raster, just use the grid code already present in the data.

## Populate HSI polygon data onto empty raster grid.
r.f.hsi <- rasterize(f.hsi, r, 'gridcode')

## Save the raster image.
writeRaster(r.f.hsi, '../data/processed/foraging_sc.tif', format='GTiff')


# For measuring suitable habitat, 2s and 3s need to be rolled together into a single class.
# Everything else needs to be rolled into a single class, too.

## Classify 2s and 3s as 4s ("suitable") and everything else as 0s.
f.hsi <- mutate(f.hsi, gridcode=case_when(
  gridcode == 2 ~ 4,
  gridcode == 3 ~ 4,
  TRUE ~ 0
))

## Populate HSI polygon data onto empty raster grid.
r.f.hsi <- rasterize(f.hsi, r, 'gridcode')

## And save it.
writeRaster(r.f.hsi, '../data/processed/foraging_sc_suitable.tif', format='GTiff', overwrite=TRUE)









