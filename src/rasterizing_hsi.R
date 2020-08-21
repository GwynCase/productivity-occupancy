setwd('C:/Users/gwync/sfuvault/productivity-occupancy/notebooks')
# library(tidyverse)
# library(lubridate)

#################
# Rasterize HSI
#################

# The first step is to bring in the HSI shapefile and prep the raster.

## First import habitat HSI shapefile.
f.hsi <- st_read('../data/external/foraging_sc.shp')

## Set raster extent based on HSI shapefile.
ext <- extent(f.hsi)

## Make an empty raster to populate with values.
r <- raster(ext, res=c(100, 100))


# For the default raster, just use the grid code already present in the data.

## Populate BEC polygon data onto empty raster grid.
r.f.hsi <- rasterize(f.hsi, r, 'gridcode')

## Save the raster image.
writeRaster(r.f.hsi, '../data/processed/foraging_sc.tif', format='GTiff')


# For measuring suitable habitat, 2s and 3s need to be rolled together into a single class.
