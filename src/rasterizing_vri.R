setwd('C:/Users/gwync/sfuvault/productivity-occupancy/notebooks')
library(tidyverse)
library(sf)
library(raster)

#################
# Rasterize HSI
#################

# The first step is to bring the VRI shapefile and prepare the raster.
shp.vri <- st_read('../data/external/VRI_sc_all/VRI_chilliwack.shp')
# shp.vri <- st_read('../data/external/VRI_sc_all/VRI_harrison.shp')
# shp.vri <- st_read('../data/external/VRI_sc_all/VRI_lower-mainland.shp')
# shp.vri <- st_read('../data/external/VRI_sc_all/VRI_pemberton.shp')
# shp.vri <- st_read('../data/external/VRI_sc_all/VRI_sunshine-coast.shp')

# Set raster extent based on VRI shapefile.
ext <- extent(shp.vri)

# Make an empty raster to populate with values.
r <- raster(ext, res=c(15, 15))


## For the landcover raster, assign a numeric code based on cover type and age.

# Define class levels.
landcover.levels <- data.frame(ID=0:11, class.name=
                                     c('undefined', 'rock', 'ocean', 'freshwater',
                                       'alpine', 'wetland',
                                       'shrub', 'deciduous', 'regen',
                                       'young', 'mature', 'old'))

# Add to VRI.
shp.vri <- shp.vri %>% mutate(land.cover=case_when(
  # Bare ground/rock
  BCLCS_LV_1 == 'N' & BCLCS_LV_2 != 'W' ~ 1,
  # Ocean
  BCLCS_LV_2 == 'W' & BCLCS_LV_5 == 'OC' ~ 2 ,
  # Freshwater (rivers, lakes, & ponds)
  BCLCS_LV_2 == 'W' & BCLCS_LV_5 != 'OC' ~ 3,
  # Shrub
  BCLCS_LV_1 == 'V' & BCLCS_LV_2 == 'N' ~ 6,
  # Burns 
  BCLCS_LV_1 == 'N' & BCLCS_LV_5 == 'BU' ~ 6,
  # Mixed or deciduous trees
  BCLCS_LV_1 == 'V' & BCLCS_LV_4 %in% c('TB', 'TM') ~ 7,
  # Coniferous trees
  BCLCS_LV_4 == 'TC' & PROJ_AGE_1 < 20 ~ 8,
  BCLCS_LV_4 == 'TC' & PROJ_AGE_1 >= 20 & PROJ_AGE_1 < 70 ~ 9,
  BCLCS_LV_4 == 'TC' & PROJ_AGE_1 >= 70 & PROJ_AGE_1 < 250 ~ 10,
  BCLCS_LV_4 == 'TC' & PROJ_AGE_1 >= 250 ~ 11,
  # Alpine
  BCLCS_LV_3 == 'A' ~ 4,
  # Wetland
  BCLCS_LV_3 == 'W'~ 5,
  TRUE ~ 0
))

# Populate VRI polygon data onto empty raster grid.
r.vri <- rasterize(shp.vri, r, 'land.cover')

# Add levels to raster.
levels(r.vri) <- landcover.levels

# Assign CRS.
crs(r.vri) <- CRS('+proj=utm +zone=10 +datum=NAD83 +units=m +no_defs')

# And save.
writeRaster(r.vri, '../data/processed/vri_lower-mainland.tif', format='GTiff')


## For gaps and openings, assign shrubs, burns, and very new clearcuts to one class. 

# Define class levels.
gap.levels <- data.frame(ID=0:1, class.name=
                                 c('undefined', 'gap'))

# Add to VRI.
shp.vri <- shp.vri %>% mutate(gap.cover=case_when(
  # Shrub
  BCLCS_LV_1 == 'V' & BCLCS_LV_2 == 'N' ~ 1,
  # Burns 
  BCLCS_LV_1 == 'N' & BCLCS_LV_5 == 'BU' ~ 1,
  # Very young forest
  BCLCS_LV_4 == 'TC' & PROJ_AGE_1 < 5 ~ 1,
  TRUE ~ 0
))

# Populate VRI polygon data onto empty raster grid.
r.vri <- rasterize(shp.vri, r, 'gap.cover')

# Add levels to raster.
levels(r.vri) <- gap.levels

# Assign CRS.
crs(r.vri) <- CRS('+proj=utm +zone=10 +datum=NAD83 +units=m +no_defs')

# And save.
writeRaster(r.vri, '../data/processed/gaps_chilliwack.tif', format='GTiff')




