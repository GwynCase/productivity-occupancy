ggplot(ml.tz, aes(x=amt.tz, y=ratio, label=site)) +
  geom_point() +
  geom_text(hjust='inward', nudge_y = 0.5) +
  theme_classic()

library(sf)
library(raster)
library(rgdal)

# Import habitat HSI shapefile
n.hsi <- st_read('C:/Users/gwync/sfuvault/map/external/habitat_model/nesting_sc.shp')

# Set raster extent based on HSI shapefile.
ext <- extent(n.hsi)

# Make an empty raster to populate with values.
r <- raster(ext, res=c(100, 100))

n.hsi.levels <- data.frame(ID=c(-10, -2, -1, 0, 1, 2, 3), 
                           class.name=c('ocean', 'freshwater', 'river', 'nil', 'low', 'moderate', 'high'))

# Populate BEC polygon data onto empty raster grid.
r.n.hsi <- rasterize(n.hsi, r, 'gridcode')

# Add the levels to the raster.
levels(r.n.hsi) <- n.hsi.levels

# Save the raster image.
writeRaster(r.n.hsi, 'C:/Users/gwync/sfuvault/productivity-occupancy/data/processed/nesting_sc.tif', format='GTiff')
