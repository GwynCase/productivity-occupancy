# Import the VRI raster.
 r.vri <- raster('../data/interim/vri_camera-sites_2019.tif')

# Calculate area per class per site.
vri.class.amount <- sample_lsm(r.vri, y=cam.sites.sf, size=r.hr.m, what='lsm_c_ca', 
                               shape='circle') %>% 
  # Amend class number with habitat class name.
  left_join(data.frame(levels(r.vri)), by=c('class'='ID')) %>% 
  inner_join(cam.sites)

saveRDS(vri.class.amount, '../data/interim/vri_class_amount.rds')
