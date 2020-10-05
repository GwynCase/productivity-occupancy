# Calculate metrics.

# This calculates landscape metrics from a series of rasters.
# You will need to have the rasters loaded.
# You will also need to define a set of scales and have a list of sites.

# Set working directory and load libraries.
setwd('C:/Users/gwync/sfuvault/productivity-occupancy/notebooks')
library(raster)
library(landscapemetrics)

# Make a list of BEC metrics to calculate.
bec.metrics <- c('lsm_l_sidi')

# Make a function to do the calculations and formatting.
calc.bec.metrics <- function(x) {
  sample_lsm(r.bec, y=sites.sf, size=x, plot_id=site.names, shape='circle', what=bec.metrics) %>% 
    left_join(bec.levels, by=c('class'='ID')) %>% 
    group_by(plot_id, metric) %>% 
    top_n(1, value) %>% ungroup() %>%  
    fill(category) %>% 
    filter(level == 'landscape') %>% 
    pivot_wider(names_from=metric, values_from=value) %>% 
    mutate(radius=x)
}

# Run the function for each sample size.
bec.landscape.metrics <- map_df(landscape$radius, calc.bec.metrics)

# Tidy things up.
bec.landscape.metrics <- bec.landscape.metrics %>% 
  select(nest=plot_id, bec.diversity=sidi, radius)

bec.landscape.metrics <- select(landscape, radius, size) %>% right_join(bec.landscape.metrics, by='radius')

#####

# Make a list of landcover metrics to calculate.
landcover.metrics <- c('lsm_c_pland', 'lsm_l_ed', 'lsm_l_contag', 'lsm_l_sidi', 'lsm_l_siei', 'lsm_l_prd')

# Make a function to do the calculations and formatting.
calc.landcover.metrics <- function(x) {
  sample_lsm(r.landcover, y=sites.sf, size=x, plot_id=site.names, shape='circle', 
             what=landcover.metrics) %>% 
    left_join(landcover.levels, by=c('class'='ID')) %>% 
    mutate(class.name=ifelse(is.na(class.name), metric, class.name)) %>% 
    select(-class, -metric, -level) %>%  
    pivot_wider(names_from=class.name, values_from=value) %>% 
    mutate(radius=x)
}

# Run the function for each sample size.
landcover.landscape.metrics <- map_df(landscape$radius, calc.landcover.metrics)

# Do some cleanup: fill NAs with zeros and rename columns, calculate mature forest.
landcover.landscape.metrics <- landcover.landscape.metrics %>% 
  replace_na(list(old=0, mature=0)) %>% 
  mutate(proportion.cover.mature=mature + old) %>% 
  select(radius, nest=plot_id, proportion.cover.mature,
         cover.edge.density=ed, cover.contagion=contag,
         cover.diversity=sidi, cover.evenness=siei, cover.richness=prd) %>% 
  filter(nest != 'TCR2019')

landcover.landscape.metrics <- select(landscape, radius, size) %>% right_join(landcover.landscape.metrics)

#####

# Make a list of gap metrics to calculate.
gap.metrics <- c('lsm_l_ed')

# Make a function to do the calculations and formatting.
calc.gap.metrics <- function(x) {
  sample_lsm(r.gaps, y=sites.sf, size=x, plot_id=site.names, shape='circle', 
             what=gap.metrics) %>% 
    left_join(landcover.levels, by=c('class'='ID')) %>% 
    mutate(class.name=ifelse(is.na(class.name), metric, class.name)) %>% 
    select(-class, -metric, -level) %>%  
    pivot_wider(names_from=class.name, values_from=value) %>% 
    mutate(radius=x)
}

# Run the function for each sample size.
gap.landscape.metrics <- map_df(landscape$radius, calc.gap.metrics)

# Tidy things up.
gap.landscape.metrics <- gap.landscape.metrics %>% 
  select(nest=plot_id, gap.edge.density=ed, radius)

gap.landscape.metrics <- select(landscape, radius, size) %>% right_join(gap.landscape.metrics)

#####

# Make a list of canopy metrics to calculate.
canopy.metrics <- c('lsm_c_pland')

# Make a function to do the calculations and formatting.
calc.canopy.metrics <- function(x) {
  sample_lsm(r.canopy, y=sites.sf, size=x, plot_id=site.names, shape='circle', 
             what=canopy.metrics) %>% 
    left_join(canopy.levels, by=c('class'='ID')) %>% 
    mutate(class.name=ifelse(is.na(class.name), metric, class.name)) %>% 
    select(-class, -metric, -level) %>%  
    pivot_wider(names_from=class.name, values_from=value) %>% 
    mutate(radius=x)
}

# Run the function for each sample size.
canopy.landscape.metrics <- map_df(landscape$radius, calc.canopy.metrics)

# Do some cleanup: fill NAs with zeros and rename columns, calculate mature forest.
canopy.landscape.metrics <- canopy.landscape.metrics %>% 
  mutate_all(~replace(., is.na(.), 0)) %>%  
  select(radius, nest=plot_id, canopy.none=none,
         canopy.moderate=moderate,
         canopy.high=high) %>% 
  filter(nest != 'TCR2019')

canopy.landscape.metrics <- select(landscape, radius, size) %>% right_join(canopy.landscape.metrics)

#####

# Make a list of metrics to calculate.
hsi.metrics <- c('lsm_l_ed', 'lsm_l_contag')

# Make a function to do the calculations and formatting.
calc.hsi.metrics <- function(x) {
  sample_lsm(r.hsi, y=sites.sf, size=x, plot_id=site.names, shape='circle', 
             what=hsi.metrics) %>% 
    left_join(hsi.levels, by=c('class'='ID')) %>% 
    mutate(class.name=ifelse(is.na(class.name), metric, class.name)) %>% 
    select(-class, -metric, -level) %>%  
    pivot_wider(names_from=class.name, values_from=value) %>% 
    mutate(radius=x)
}

# Run the function for each sample size.
hsi.landscape.metrics <- map_df(landscape$radius, calc.hsi.metrics)

# Do some cleanup
hsi.landscape.metrics <- hsi.landscape.metrics %>% #replace(is.na(.), 0) %>% 
  select(radius, nest=plot_id, hsi.edge.density=ed, hsi.contagion=contag) %>% 
  filter(nest != 'TCR2019')

hsi.landscape.metrics <- select(landscape, radius, size) %>% right_join(hsi.landscape.metrics)

#####

# Make a list of metrics to calculate.
suitable.metrics <- c('lsm_c_pland', 'lsm_l_ed')

# Make a function to do the calculations and formatting.
calc.suitable.metrics <- function(x) {
  sample_lsm(r.suitable, y=sites.sf, size=x, plot_id=site.names, shape='circle', 
             what=suitable.metrics) %>% 
    left_join(hsi.levels, by=c('class'='ID')) %>% 
    mutate(class.name=ifelse(is.na(class.name), metric, class.name)) %>% 
    select(-class, -metric, -level) %>%  
    pivot_wider(names_from=class.name, values_from=value) %>% 
    mutate(radius=x)
}

# Run the function for each sample size.
suitable.landscape.metrics <- map_df(landscape$radius, calc.suitable.metrics)

# Do some cleanup
suitable.landscape.metrics <- suitable.landscape.metrics %>% #replace(is.na(.), 0) %>% 
  select(radius, nest=plot_id, suitable.edge.density=ed, proportion.suitable=pland) %>% 
  filter(nest != 'TCR2019')

hsi.landscape.metrics <- select(landscape, radius, size) %>% right_join(hsi.landscape.metrics)
