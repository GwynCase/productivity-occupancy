# Landscape rasters.

# This code loads in rasters used for landscape analysis, then adds levels and a CRS.

# Set working directory and load libraries.
setwd('C:/Users/gwync/sfuvault/productivity-occupancy/notebooks')
library(raster)

# Load all the rasters.
r.bec <- raster('../data/external/bec_raster_full.tif')
r.landcover <- raster('../data/processed/vri_sc_all-sites.tif')
r.older <- raster('../data/processed/older_sc_100.tif')
r.gaps <- raster('../data/processed/gaps_sc_100.tif')
r.canopy <- raster('../data/processed/canopy_sc_100.tif')
r.hsi <- raster('../data/processed/foraging_sc.tif')
r.suitable <- raster('../data/processed/foraging_sc_suitable.tif')

# Define levels for each raster.
bec.levels <- levels(r.bec) %>% data.frame()
landcover.levels <- data.frame(ID=0:11, class.name=
                                 c('undefined', 'rock', 'ocean', 'freshwater',
                                   'alpine', 'wetland',
                                   'shrub', 'deciduous', 'regen',
                                   'young', 'mature', 'old'))
older.levels <- data.frame(ID=0:1, class.name=
                             c('undefined', 'older'))
gap.levels <- data.frame(ID=0:1, class.name=
                           c('undefined', 'gap'))
canopy.levels <- data.frame(ID=c(-1, 0, 50, 75), 
                            class.name=c('undefined', 'none', 'moderate', 'high'))
hsi.levels <- data.frame(ID=c(-10, -2, -1, 0, 1, 2, 3), 
                         class.name=c('ocean', 'freshwater', 'river', 
                                      'nil', 'low', 'moderate', 'high'))
suitable.levels <- data.frame(ID=c(0, 4), 
                              class.name=c('unsuitable', 'suitable'))

# Add levels to each raster.
levels(r.landcover) <- landcover.levels
levels(r.older) <- older.levels
levels(r.gaps) <- gap.levels
levels(r.canopy) <- canopy.levels
levels(r.hsi) <- hsi.levels
levels(r.suitable) <- suitable.levels

# Finally, assign a CRS.
lapply(c(r.bec, r.landcover, r.older, r.gaps, r.canopy, r.hsi, r.suitable), 
       function(x) {crs(x) <- CRS('+proj=utm +zone=10 +datum=NAD83 +units=m +no_defs')})

# Doesn't work for BEC???
crs(r.bec) <- CRS('+proj=utm +zone=10 +datum=NAD83 +units=m +no_defs')
crs(r.hsi) <- CRS('+proj=utm +zone=10 +datum=NAD83 +units=m +no_defs')
crs(r.suitable) <- CRS('+proj=utm +zone=10 +datum=NAD83 +units=m +no_defs')
