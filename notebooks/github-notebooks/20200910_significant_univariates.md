Significant univariates
================

# 

## Setup

I’m making a table of significant univariate models.

``` r
# Import conflict settings.
source('../src/conflicted.R')

#Load some libraries.
library(tidyverse)
library(vegan)
library(raster)
library(sf)
library(landscapemetrics)
library(broom)
library(knitr)
library(kableExtra)
```

Start by defining the four nested scales.

``` r
# Area is in hectares.
landscape <- data.frame(
  size=c('PFA', 'breeding area', 'home range', 'maximum range'),
  area=c(60, 200, 3800, 32902.97)
)

# Convert area in hectares to radii in meters.
landscape <- landscape %>% mutate(radius=sqrt(area*10000/pi))

landscape
```

    ##            size     area     radius
    ## 1           PFA    60.00   437.0194
    ## 2 breeding area   200.00   797.8846
    ## 3    home range  3800.00  3477.8982
    ## 4 maximum range 32902.97 10233.9341

Now bring in diet data.

``` r
# Load in diet data from cameras.
source('../src/prey_attributes_revised.R')

# Take only items identified to genus and twist to a wide format.
camera.diet.wide <- diet.items %>% filter(binomial != 'Unidentified item' & method == 'camera') %>% 
  group_by(nest, genus, species) %>% 
  mutate(count=n()) %>% ungroup() %>% 
  dplyr::select(nest, binomial, count) %>%
  distinct() %>% 
  pivot_wider(names_from=binomial, values_from=count,
              values_fill=list(count = 0))

remains.diet.wide <- diet.items %>% filter(binomial != 'Unidentified item' & method == 'remains') %>% 
  group_by(nest, genus, species) %>% 
  mutate(count=n()) %>% ungroup() %>% 
  dplyr::select(nest, binomial, count) %>%
  distinct() %>% 
  pivot_wider(names_from=binomial, values_from=count,
              values_fill=list(count = 0))
```

Calculate diet variables–these are the response variables.

``` r
# Calculate diet diversity.
camera.diet.diversity <- plyr::ddply(camera.diet.wide, ~nest, function(x) {
           data.frame(diet.diversity=diversity(x[-1], index='simpson'))
   })

# Proportion of diet made up of squirrel, by biomass.
camera.proportion.squirrel <- diet.items %>% 
  filter(method == 'camera') %>% 
  mutate(mass=as.numeric(mass)) %>% 
  group_by(nest) %>% 
  mutate(total=sum(mass)) %>% 
  filter(genus == 'Tamiasciurus') %>% 
  mutate(amount.sq=sum(mass), proportion.squirrel=amount.sq/total) %>% 
  select(nest, proportion.squirrel) %>% distinct()

# Proportion of diet made up of mammal, by biomass.
camera.proportion.mammal <- diet.items %>% 
  filter(method == 'camera') %>% 
  mutate(mass=as.numeric(mass)) %>% 
  group_by(nest) %>% 
  mutate(total=sum(mass)) %>% 
  filter(class == 'Mammalia') %>% 
  mutate(amount.mm=sum(mass), proportion.mammal=amount.mm/total) %>% 
  select(nest, proportion.mammal) %>% distinct()

# Join them together.
camera.diet.variables <- full_join(camera.diet.diversity, camera.proportion.mammal, by=c('nest')) %>% 
  full_join(camera.proportion.squirrel, by=c('nest'))

camera.diet.variables
```

    ##      nest diet.diversity proportion.mammal proportion.squirrel
    ## 1 MTC2019      0.4813841         0.7750943           0.7172089
    ## 2 MTF2019      0.4805010         0.8449601           0.7167476
    ## 3 RLK2019      0.0768000         0.8350906           0.7755876
    ## 4 TCR2019      0.3495111         0.8474688           0.8112324
    ## 5 TMC2019      0.5840995         0.7567553           0.4872530
    ## 6 UTZ2019      0.6620643         0.9123422           0.7271133

Then calculate landscape variables–the predictor variables. First bring
in the landscape data and get it organized.

``` r
# Import raster data for BEC zones.
r.bec <- raster('../data/external/bec_raster_full.tif')

# Get BEC levels.
bec.levels <- levels(r.bec) %>% data.frame()

# Assign CRS.
crs(r.bec) <- CRS('+proj=utm +zone=10 +datum=NAD83 +units=m +no_defs')

# Bring in land cover data.
r.landcover <- raster('../data/processed/vri_sc_all-sites.tif')

# Define land cover levels.
landcover.levels <- data.frame(ID=0:11, class.name=
                                     c('undefined', 'rock', 'ocean', 'freshwater',
                                       'alpine', 'wetland',
                                       'shrub', 'deciduous', 'regen',
                                       'young', 'mature', 'old'))

# Add levels to raster.
levels(r.landcover) <- landcover.levels

# Assign CRS.
crs(r.landcover) <- CRS('+proj=utm +zone=10 +datum=NAD83 +units=m +no_defs')

# Bring in HSI data.
r.hsi <- raster('../data/processed/foraging_sc.tif')

# Define levels for HSI raster.
hsi.levels <- data.frame(ID=c(-10, -2, -1, 0, 1, 2, 3), 
                           class.name=c('ocean', 'freshwater', 'river', 'nil', 'low', 'moderate', 'high'))

# Add to raster.
levels(r.hsi) <- hsi.levels

# Assign CRS.
crs(r.hsi) <- CRS('+proj=utm +zone=10 +datum=NAD83 +units=m +no_defs')

# Bring in suitable habitat data.
r.suitable <- raster('../data/processed/foraging_sc_suitable.tif')

# Define levels for HSI raster.
suitable.levels <- data.frame(ID=c(0, 4), 
                           class.name=c('unsuitable', 'suitable'))

# Add to raster.
levels(r.suitable) <- suitable.levels

# Assign CRS.
crs(r.suitable) <- CRS('+proj=utm +zone=10 +datum=NAD83 +units=m +no_defs')

# Bring in canopy cover data.
# Bring in mature forest data.
```

I’ll need a list of sites at which I’m calculating.

``` r
# Load in camera sites from 2019.
camera.sites.2019 <- read_csv('../data/raw/camera_nests_2019.csv')

# For now, also make a spatial object of camera sites only.
camera.sites.sf <-camera.sites.2019 %>% st_as_sf(coords=c('lon', 'lat')) %>%
  st_set_crs('+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs') %>%
  st_transform("+proj=utm +zone=10 +datum=WGS84 +units=m +no_defs")

# Make a vector of the nest names.
camera.nests <- camera.sites.sf$nest
```

Now make a list of the metrics to calculate. They are:

  - Landscape composition
      - proportion older forest
      - proportion suitable habitat
      - amount canopy cover
  - Configuration
      - edge density of landcover
      - edge density of HSI
      - edge density of shrub/open space
      - edge density of suitable habitat
  - Aggregation
      - contagion of landcover
      - contagion of HSI
  - Diversity
      - Simpson’s diversity index (forest type)
      - Simpson’s diversity index (landcover)
      - Simpson’s evenness index (landcover)
      - patch richness density (landcover)

<!-- Also zone -->

To rearrange by data source:

  - BEC
      - Simpson’s diversity index
  - Landcover
      - proportion older forest
      - edge density
      - contagion
      - Simpson’s diversity index
      - Simpson’s evenness index
      - patch richness density
  - Open space
      - edge density
  - Canopy cover
      - amount cover
  - HSI
      - edge density
      - contagion
  - Amount suitable
      - proportion suitable
      - edge density

Now calculate the metrics.

``` r
# Make a list of BEC metrics to calculate.
bec.metrics <- c('lsm_l_sidi')

# Make a function to do the calculations and formatting.
calc.bec.metrics <- function(x) {
  sample_lsm(r.bec, y=camera.sites.sf, size=x, plot_id=camera.nests, shape='circle', what=bec.metrics) %>% 
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
```

``` r
# Make a list of landcover metrics to calculate.
landcover.metrics <- c('lsm_c_pland', 'lsm_l_ed', 'lsm_l_contag', 'lsm_l_sidi', 'lsm_l_siei', 'lsm_l_prd')

# Make a function to do the calculations and formatting.
calc.landcover.metrics <- function(x) {
  sample_lsm(r.landcover, y=camera.sites.sf, size=x, plot_id=camera.nests, shape='circle', 
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
```

``` r
# Make a list of metrics to calculate.
hsi.metrics <- c('lsm_l_ed', 'lsm_l_contag')

# Make a function to do the calculations and formatting.
calc.hsi.metrics <- function(x) {
  sample_lsm(r.hsi, y=camera.sites.sf, size=x, plot_id=camera.nests, shape='circle', 
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
```

``` r
# Make a list of metrics to calculate.
suitable.metrics <- c('lsm_c_pland', 'lsm_l_ed')

# Make a function to do the calculations and formatting.
calc.suitable.metrics <- function(x) {
  sample_lsm(r.suitable, y=camera.sites.sf, size=x, plot_id=camera.nests, shape='circle', 
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
```

To do: canopy cover, amount suitable, open space.

## Diet diversity \~ landscape

Finally, combine everything together and make the univariate models.

``` r
# Join the data together.
data <- full_join(camera.diet.variables, bec.landscape.metrics, by=c('nest')) %>% 
  full_join(suitable.landscape.metrics, by=c('nest', 'radius')) %>% 
  full_join(landcover.landscape.metrics, by=c('nest', 'radius')) %>% 
  full_join(hsi.landscape.metrics, by=c('nest', 'radius'))

# Forest type diversity.
diet.diversity.by.bec.diversity <- data %>% 
  drop_na(diet.diversity) %>% 
  split(.$size) %>% 
  map(~lm(diet.diversity ~ bec.diversity, data=.x)) %>% 
  map(summary) %>% 
  map(glance) %>% 
  bind_rows(.id='size') %>% 
  mutate(variable='forest type diversity')

# Proportion older forest.
diet.diversity.by.proportion.cover.mature <- data %>% 
  drop_na(diet.diversity) %>% 
  filter(nest != 'TCR2019') %>% 
  split(.$size) %>% 
  map(~lm(diet.diversity ~ proportion.cover.mature, data=.x)) %>% 
  map(summary) %>% 
  map(glance) %>% 
  bind_rows(.id='size') %>% 
  mutate(variable='proportion older forest')

# Landcover edge density.
diet.diversity.by.cover.edge.density <- data %>% 
  drop_na(diet.diversity) %>% 
  filter(nest != 'TCR2019') %>% 
  split(.$size) %>% 
  map(~lm(diet.diversity ~ cover.edge.density, data=.x)) %>% 
  map(summary) %>% 
  map(glance) %>% 
  bind_rows(.id='size') %>% 
  mutate(variable='landcover edge density')

# Landcover contagion.
diet.diversity.by.cover.contagion <- data %>% 
  drop_na(diet.diversity) %>% 
  filter(nest != 'TCR2019') %>% 
  split(.$size) %>% 
  map(~lm(diet.diversity ~ cover.contagion, data=.x)) %>% 
  map(summary) %>% 
  map(glance) %>% 
  bind_rows(.id='size') %>% 
  mutate(variable='landcover contagion')

# Landcover diversity.
diet.diversity.by.cover.diversity <- data %>% 
  drop_na(diet.diversity) %>% 
  filter(nest != 'TCR2019') %>% 
  split(.$size) %>% 
  map(~lm(diet.diversity ~ cover.diversity, data=.x)) %>% 
  map(summary) %>% 
  map(glance) %>% 
  bind_rows(.id='size') %>% 
  mutate(variable='landcover diversity')

# Landcover evenness.
diet.diversity.by.cover.evenness <- data %>% 
  drop_na(diet.diversity) %>% 
  filter(nest != 'TCR2019') %>% 
  split(.$size) %>% 
  map(~lm(diet.diversity ~ cover.evenness, data=.x)) %>% 
  map(summary) %>% 
  map(glance) %>% 
  bind_rows(.id='size') %>% 
  mutate(variable='landcover evenness')

# Landcover richness.
diet.diversity.by.cover.richness <- data %>% 
  drop_na(diet.diversity) %>% 
  filter(nest != 'TCR2019') %>% 
  split(.$size) %>% 
  map(~lm(diet.diversity ~ cover.richness, data=.x)) %>% 
  map(summary) %>% 
  map(glance) %>% 
  bind_rows(.id='size') %>% 
  mutate(variable='landcover richness')

# HSI edge density.
diet.diversity.by.hsi.edge.density <- data %>% 
  drop_na(diet.diversity) %>% 
  filter(nest != 'TCR2019') %>% 
  split(.$size) %>% 
  map(~lm(diet.diversity ~ hsi.edge.density, data=.x)) %>% 
  map(summary) %>% 
  map(glance) %>% 
  bind_rows(.id='size') %>% 
  mutate(variable='hsi edge density')

# HSI contagion.
diet.diversity.by.hsi.contagion <- data %>% 
  drop_na(diet.diversity) %>% 
  filter(nest != 'TCR2019') %>% 
  split(.$size) %>% 
  map(~lm(diet.diversity ~ hsi.contagion, data=.x)) %>% 
  map(summary) %>% 
  map(glance) %>% 
  bind_rows(.id='size') %>% 
  mutate(variable='hsi contagion')

# Proportion suitable.
diet.diversity.by.proportion.suitable <- data %>% 
  drop_na(diet.diversity) %>% 
  filter(nest != 'TCR2019') %>% 
  split(.$size) %>% 
  map(~lm(diet.diversity ~ proportion.suitable, data=.x)) %>% 
  map(summary) %>% 
  map(glance) %>% 
  bind_rows(.id='size') %>% 
  mutate(variable='proportion suitable')

# Suitable habitat edge density.
diet.diversity.by.suitable.edge.density <- data %>% 
  drop_na(diet.diversity) %>% 
  filter(nest != 'TCR2019') %>% 
  split(.$size) %>% 
  map(~lm(diet.diversity ~ suitable.edge.density, data=.x)) %>% 
  map(summary) %>% 
  map(glance) %>% 
  bind_rows(.id='size') %>% 
  mutate(variable='suitable habitat edge density')

# Bind it all together.
diet.diversity.statistics <- bind_rows(diet.diversity.by.bec.diversity, 
          diet.diversity.by.proportion.cover.mature,
          diet.diversity.by.cover.edge.density,
          diet.diversity.by.cover.contagion,
          diet.diversity.by.cover.diversity,
          diet.diversity.by.cover.evenness,
          diet.diversity.by.cover.richness,
          diet.diversity.by.hsi.edge.density,
          diet.diversity.by.hsi.contagion,
          diet.diversity.by.proportion.suitable,
          diet.diversity.by.suitable.edge.density)
```

And let’s put it in a beautiful table. Currently showing significance at
the 0.10 level just so I can test how to add asterisks to significant
things.

``` r
diet.diversity.statistics <- diet.diversity.statistics %>% mutate(sig=round(p.value, digits=2)) %>% 
  mutate(sig=case_when(
  p.value < 0.1 ~ paste(as.character(sig), '*'),
  TRUE ~  paste(as.character(sig))
))

diet.diversity.statistics %>% select(size, sig, variable) %>% 
  pivot_wider(names_from=size, values_from=sig) %>% 
  select(variable, PFA, everything()) %>% 
  kable() %>% kable_styling(bootstrap_options=c('striped'))
```

<table class="table table-striped" style="margin-left: auto; margin-right: auto;">

<thead>

<tr>

<th style="text-align:left;">

variable

</th>

<th style="text-align:left;">

PFA

</th>

<th style="text-align:left;">

breeding area

</th>

<th style="text-align:left;">

home range

</th>

<th style="text-align:left;">

maximum range

</th>

</tr>

</thead>

<tbody>

<tr>

<td style="text-align:left;">

forest type diversity

</td>

<td style="text-align:left;">

0.38

</td>

<td style="text-align:left;">

0.39

</td>

<td style="text-align:left;">

0.36

</td>

<td style="text-align:left;">

0.1 \*

</td>

</tr>

<tr>

<td style="text-align:left;">

proportion older forest

</td>

<td style="text-align:left;">

0.92

</td>

<td style="text-align:left;">

0.86

</td>

<td style="text-align:left;">

0.39

</td>

<td style="text-align:left;">

0.07 \*

</td>

</tr>

<tr>

<td style="text-align:left;">

landcover edge density

</td>

<td style="text-align:left;">

0.96

</td>

<td style="text-align:left;">

0.92

</td>

<td style="text-align:left;">

0.8

</td>

<td style="text-align:left;">

0.43

</td>

</tr>

<tr>

<td style="text-align:left;">

landcover contagion

</td>

<td style="text-align:left;">

0.23

</td>

<td style="text-align:left;">

0.85

</td>

<td style="text-align:left;">

0.51

</td>

<td style="text-align:left;">

0.45

</td>

</tr>

<tr>

<td style="text-align:left;">

landcover diversity

</td>

<td style="text-align:left;">

0.93

</td>

<td style="text-align:left;">

0.81

</td>

<td style="text-align:left;">

0.53

</td>

<td style="text-align:left;">

0.29

</td>

</tr>

<tr>

<td style="text-align:left;">

landcover evenness

</td>

<td style="text-align:left;">

0.24

</td>

<td style="text-align:left;">

0.85

</td>

<td style="text-align:left;">

0.51

</td>

<td style="text-align:left;">

0.31

</td>

</tr>

<tr>

<td style="text-align:left;">

landcover richness

</td>

<td style="text-align:left;">

0.62

</td>

<td style="text-align:left;">

0.46

</td>

<td style="text-align:left;">

0.84

</td>

<td style="text-align:left;">

0.92

</td>

</tr>

<tr>

<td style="text-align:left;">

hsi edge density

</td>

<td style="text-align:left;">

0.98

</td>

<td style="text-align:left;">

0.97

</td>

<td style="text-align:left;">

0.91

</td>

<td style="text-align:left;">

0.33

</td>

</tr>

<tr>

<td style="text-align:left;">

hsi contagion

</td>

<td style="text-align:left;">

0.44

</td>

<td style="text-align:left;">

0.91

</td>

<td style="text-align:left;">

0.45

</td>

<td style="text-align:left;">

0.72

</td>

</tr>

<tr>

<td style="text-align:left;">

proportion suitable

</td>

<td style="text-align:left;">

0.91

</td>

<td style="text-align:left;">

0.61

</td>

<td style="text-align:left;">

0.28

</td>

<td style="text-align:left;">

0.24

</td>

</tr>

<tr>

<td style="text-align:left;">

suitable habitat edge density

</td>

<td style="text-align:left;">

0.94

</td>

<td style="text-align:left;">

0.73

</td>

<td style="text-align:left;">

0.67

</td>

<td style="text-align:left;">

0.61

</td>

</tr>

</tbody>

</table>

## Proportion squirrel \~ landscape

And now do it all over again with proportion squirrel.

``` r
# Forest type diversity.
proportion.squirrel.by.bec.diversity <- data %>% 
  drop_na(proportion.squirrel) %>% 
  split(.$size) %>% 
  map(~lm(proportion.squirrel ~ bec.diversity, data=.x)) %>% 
  map(summary) %>% 
  map(glance) %>% 
  bind_rows(.id='size') %>% 
  mutate(variable='forest type diversity')

# Proportion older forest.
proportion.squirrel.by.proportion.cover.mature <- data %>% 
  drop_na(proportion.squirrel) %>% 
  filter(nest != 'TCR2019') %>% 
  split(.$size) %>% 
  map(~lm(proportion.squirrel ~ proportion.cover.mature, data=.x)) %>% 
  map(summary) %>% 
  map(glance) %>% 
  bind_rows(.id='size') %>% 
  mutate(variable='proportion older forest')

# Landcover edge density.
proportion.squirrel.by.cover.edge.density <- data %>% 
  drop_na(proportion.squirrel) %>% 
  filter(nest != 'TCR2019') %>% 
  split(.$size) %>% 
  map(~lm(proportion.squirrel ~ cover.edge.density, data=.x)) %>% 
  map(summary) %>% 
  map(glance) %>% 
  bind_rows(.id='size') %>% 
  mutate(variable='landcover edge density')

# Landcover contagion.
proportion.squirrel.by.cover.contagion <- data %>% 
  drop_na(proportion.squirrel) %>% 
  filter(nest != 'TCR2019') %>% 
  split(.$size) %>% 
  map(~lm(proportion.squirrel ~ cover.contagion, data=.x)) %>% 
  map(summary) %>% 
  map(glance) %>% 
  bind_rows(.id='size') %>% 
  mutate(variable='landcover contagion')

# Landcover diversity.
proportion.squirrel.by.cover.diversity <- data %>% 
  drop_na(proportion.squirrel) %>% 
  filter(nest != 'TCR2019') %>% 
  split(.$size) %>% 
  map(~lm(proportion.squirrel ~ cover.diversity, data=.x)) %>% 
  map(summary) %>% 
  map(glance) %>% 
  bind_rows(.id='size') %>% 
  mutate(variable='landcover diversity')

# Landcover evenness.
proportion.squirrel.by.cover.evenness <- data %>% 
  drop_na(proportion.squirrel) %>% 
  filter(nest != 'TCR2019') %>% 
  split(.$size) %>% 
  map(~lm(proportion.squirrel ~ cover.evenness, data=.x)) %>% 
  map(summary) %>% 
  map(glance) %>% 
  bind_rows(.id='size') %>% 
  mutate(variable='landcover evenness')

# Landcover richness.
proportion.squirrel.by.cover.richness <- data %>% 
  drop_na(proportion.squirrel) %>% 
  filter(nest != 'TCR2019') %>% 
  split(.$size) %>% 
  map(~lm(proportion.squirrel ~ cover.richness, data=.x)) %>% 
  map(summary) %>% 
  map(glance) %>% 
  bind_rows(.id='size') %>% 
  mutate(variable='landcover richness')

# HSI edge density.
proportion.squirrel.by.hsi.edge.density <- data %>% 
  drop_na(proportion.squirrel) %>% 
  filter(nest != 'TCR2019') %>% 
  split(.$size) %>% 
  map(~lm(proportion.squirrel ~ hsi.edge.density, data=.x)) %>% 
  map(summary) %>% 
  map(glance) %>% 
  bind_rows(.id='size') %>% 
  mutate(variable='hsi edge density')

# HSI contagion.
proportion.squirrel.by.hsi.contagion <- data %>% 
  drop_na(proportion.squirrel) %>% 
  filter(nest != 'TCR2019') %>% 
  split(.$size) %>% 
  map(~lm(proportion.squirrel ~ hsi.contagion, data=.x)) %>% 
  map(summary) %>% 
  map(glance) %>% 
  bind_rows(.id='size') %>% 
  mutate(variable='hsi contagion')

# Proportion suitable.
proportion.squirrel.by.proportion.suitable <- data %>% 
  drop_na(proportion.squirrel) %>% 
  filter(nest != 'TCR2019') %>% 
  split(.$size) %>% 
  map(~lm(proportion.squirrel ~ proportion.suitable, data=.x)) %>% 
  map(summary) %>% 
  map(glance) %>% 
  bind_rows(.id='size') %>% 
  mutate(variable='proportion suitable')

# Suitable habitat edge density.
proportion.squirrel.by.suitable.edge.density <- data %>% 
  drop_na(proportion.squirrel) %>% 
  filter(nest != 'TCR2019') %>% 
  split(.$size) %>% 
  map(~lm(proportion.squirrel ~ suitable.edge.density, data=.x)) %>% 
  map(summary) %>% 
  map(glance) %>% 
  bind_rows(.id='size') %>% 
  mutate(variable='suitable habitat edge density')

# Bind it all together.
proportion.squirrel.statistics <- bind_rows(proportion.squirrel.by.bec.diversity, 
          proportion.squirrel.by.proportion.cover.mature,
          proportion.squirrel.by.cover.edge.density,
          proportion.squirrel.by.cover.contagion,
          proportion.squirrel.by.cover.diversity,
          proportion.squirrel.by.cover.evenness,
          proportion.squirrel.by.cover.richness,
          proportion.squirrel.by.hsi.edge.density,
          proportion.squirrel.by.hsi.contagion,
          proportion.squirrel.by.proportion.suitable,
          proportion.squirrel.by.suitable.edge.density)
```

And into a beautiful table.

``` r
proportion.squirrel.statistics <- proportion.squirrel.statistics %>% 
  mutate(sig=round(p.value, digits=2)) %>% 
  mutate(sig=case_when(
  p.value < 0.1 ~ paste(as.character(sig), '*'),
  TRUE ~  paste(as.character(sig))
))

proportion.squirrel.statistics %>% select(size, sig, variable) %>% 
  pivot_wider(names_from=size, values_from=sig) %>% 
  select(variable, PFA, everything()) %>% 
  kable() %>% kable_styling(bootstrap_options=c('striped'))
```

<table class="table table-striped" style="margin-left: auto; margin-right: auto;">

<thead>

<tr>

<th style="text-align:left;">

variable

</th>

<th style="text-align:left;">

PFA

</th>

<th style="text-align:left;">

breeding area

</th>

<th style="text-align:left;">

home range

</th>

<th style="text-align:left;">

maximum range

</th>

</tr>

</thead>

<tbody>

<tr>

<td style="text-align:left;">

forest type diversity

</td>

<td style="text-align:left;">

0.62

</td>

<td style="text-align:left;">

0.67

</td>

<td style="text-align:left;">

0.55

</td>

<td style="text-align:left;">

0.63

</td>

</tr>

<tr>

<td style="text-align:left;">

proportion older forest

</td>

<td style="text-align:left;">

0.61

</td>

<td style="text-align:left;">

0.44

</td>

<td style="text-align:left;">

0.06 \*

</td>

<td style="text-align:left;">

0.12

</td>

</tr>

<tr>

<td style="text-align:left;">

landcover edge density

</td>

<td style="text-align:left;">

0.27

</td>

<td style="text-align:left;">

0.23

</td>

<td style="text-align:left;">

0.52

</td>

<td style="text-align:left;">

0.72

</td>

</tr>

<tr>

<td style="text-align:left;">

landcover contagion

</td>

<td style="text-align:left;">

0.6

</td>

<td style="text-align:left;">

0.12

</td>

<td style="text-align:left;">

0.03 \*

</td>

<td style="text-align:left;">

0.18

</td>

</tr>

<tr>

<td style="text-align:left;">

landcover diversity

</td>

<td style="text-align:left;">

0.17

</td>

<td style="text-align:left;">

0.15

</td>

<td style="text-align:left;">

0.01 \*

</td>

<td style="text-align:left;">

0.14

</td>

</tr>

<tr>

<td style="text-align:left;">

landcover evenness

</td>

<td style="text-align:left;">

0.56

</td>

<td style="text-align:left;">

0.15

</td>

<td style="text-align:left;">

0.01 \*

</td>

<td style="text-align:left;">

0.15

</td>

</tr>

<tr>

<td style="text-align:left;">

landcover richness

</td>

<td style="text-align:left;">

0.26

</td>

<td style="text-align:left;">

0.09 \*

</td>

<td style="text-align:left;">

0.14

</td>

<td style="text-align:left;">

0.8

</td>

</tr>

<tr>

<td style="text-align:left;">

hsi edge density

</td>

<td style="text-align:left;">

0.7

</td>

<td style="text-align:left;">

0.58

</td>

<td style="text-align:left;">

0.65

</td>

<td style="text-align:left;">

0.68

</td>

</tr>

<tr>

<td style="text-align:left;">

hsi contagion

</td>

<td style="text-align:left;">

0.45

</td>

<td style="text-align:left;">

0.78

</td>

<td style="text-align:left;">

0.71

</td>

<td style="text-align:left;">

0.99

</td>

</tr>

<tr>

<td style="text-align:left;">

proportion suitable

</td>

<td style="text-align:left;">

0.61

</td>

<td style="text-align:left;">

0.33

</td>

<td style="text-align:left;">

0.06 \*

</td>

<td style="text-align:left;">

0.27

</td>

</tr>

<tr>

<td style="text-align:left;">

suitable habitat edge density

</td>

<td style="text-align:left;">

0.66

</td>

<td style="text-align:left;">

0.4

</td>

<td style="text-align:left;">

0.25

</td>

<td style="text-align:left;">

0.67

</td>

</tr>

</tbody>

</table>

## Productivity \~ diet

Then let’s twist it around and look at productivity \~ diet.

``` r
# Add productivity to dataset.
data <- camera.sites.2019 %>% select(nest, n_fledge) %>% full_join(data, by=c('nest'))

# Make a bunch of models.

# Diet diversity.
productivity.by.diet.diversity <- data %>% 
  split(.$size) %>% 
  map(~lm(n_fledge ~ diet.diversity, data=.x)) %>% 
  map(summary) %>% 
  map(glance) %>% 
  bind_rows(.id='size') %>% 
  mutate(variable='diet diversity')

# Proportion squirrel.
productivity.by.proportion.squirrel <- data %>% 
  split(.$size) %>% 
  map(~lm(n_fledge ~ proportion.squirrel, data=.x)) %>% 
  map(summary) %>% 
  map(glance) %>% 
  bind_rows(.id='size') %>% 
  mutate(variable='proportion squirrel')

# Bind it all together.
productivity.diet.statistics <- bind_rows(productivity.by.diet.diversity, 
          productivity.by.proportion.squirrel)
```

Another beautiful table. A bit ridiculous because the data are the same
for all scales, but…

``` r
productivity.diet.statistics <- productivity.diet.statistics %>% 
  mutate(sig=round(p.value, digits=2)) %>% 
  mutate(sig=case_when(
  p.value < 0.1 ~ paste(as.character(sig), '*'),
  TRUE ~  paste(as.character(sig))
))

productivity.diet.statistics %>% select(size, sig, variable) %>% 
  pivot_wider(names_from=size, values_from=sig) %>% 
  select(variable, PFA, everything()) %>% 
  kable() %>% kable_styling(bootstrap_options=c('striped'))
```

<table class="table table-striped" style="margin-left: auto; margin-right: auto;">

<thead>

<tr>

<th style="text-align:left;">

variable

</th>

<th style="text-align:left;">

PFA

</th>

<th style="text-align:left;">

breeding area

</th>

<th style="text-align:left;">

home range

</th>

<th style="text-align:left;">

maximum range

</th>

</tr>

</thead>

<tbody>

<tr>

<td style="text-align:left;">

diet diversity

</td>

<td style="text-align:left;">

0.24

</td>

<td style="text-align:left;">

0.24

</td>

<td style="text-align:left;">

0.24

</td>

<td style="text-align:left;">

0.24

</td>

</tr>

<tr>

<td style="text-align:left;">

proportion squirrel

</td>

<td style="text-align:left;">

0.05 \*

</td>

<td style="text-align:left;">

0.05 \*

</td>

<td style="text-align:left;">

0.05 \*

</td>

<td style="text-align:left;">

0.05 \*

</td>

</tr>

</tbody>

</table>

## Productivity \~ landscape

``` r
# Forest type diversity.
productivity.by.bec.diversity <- data %>% 
  drop_na(n_fledge) %>% 
  split(.$size) %>% 
  map(~lm(n_fledge ~ bec.diversity, data=.x)) %>% 
  map(summary) %>% 
  map(glance) %>% 
  bind_rows(.id='size') %>% 
  mutate(variable='forest type diversity')

# Proportion older forest.
productivity.by.proportion.cover.mature <- data %>% 
  drop_na(n_fledge) %>% 
  filter(nest != 'TCR2019') %>% 
  split(.$size) %>% 
  map(~lm(n_fledge ~ proportion.cover.mature, data=.x)) %>% 
  map(summary) %>% 
  map(glance) %>% 
  bind_rows(.id='size') %>% 
  mutate(variable='proportion older forest')

# Landcover edge density.
productivity.by.cover.edge.density <- data %>% 
  drop_na(n_fledge) %>% 
  filter(nest != 'TCR2019') %>% 
  split(.$size) %>% 
  map(~lm(n_fledge ~ cover.edge.density, data=.x)) %>% 
  map(summary) %>% 
  map(glance) %>% 
  bind_rows(.id='size') %>% 
  mutate(variable='landcover edge density')

# Landcover contagion.
productivity.by.cover.contagion <- data %>% 
  drop_na(n_fledge) %>% 
  filter(nest != 'TCR2019') %>% 
  split(.$size) %>% 
  map(~lm(n_fledge ~ cover.contagion, data=.x)) %>% 
  map(summary) %>% 
  map(glance) %>% 
  bind_rows(.id='size') %>% 
  mutate(variable='landcover contagion')

# Landcover diversity.
productivity.by.cover.diversity <- data %>% 
  drop_na(n_fledge) %>% 
  filter(nest != 'TCR2019') %>% 
  split(.$size) %>% 
  map(~lm(n_fledge ~ cover.diversity, data=.x)) %>% 
  map(summary) %>% 
  map(glance) %>% 
  bind_rows(.id='size') %>% 
  mutate(variable='landcover diversity')

# Landcover evenness.
productivity.by.cover.evenness <- data %>% 
  drop_na(n_fledge) %>% 
  filter(nest != 'TCR2019') %>% 
  split(.$size) %>% 
  map(~lm(n_fledge ~ cover.evenness, data=.x)) %>% 
  map(summary) %>% 
  map(glance) %>% 
  bind_rows(.id='size') %>% 
  mutate(variable='landcover evenness')

# Landcover richness.
productivity.by.cover.richness <- data %>% 
  drop_na(n_fledge) %>% 
  filter(nest != 'TCR2019') %>% 
  split(.$size) %>% 
  map(~lm(n_fledge ~ cover.richness, data=.x)) %>% 
  map(summary) %>% 
  map(glance) %>% 
  bind_rows(.id='size') %>% 
  mutate(variable='landcover richness')

# HSI edge density.
productivity.by.hsi.edge.density <- data %>% 
  drop_na(n_fledge) %>% 
  filter(nest != 'TCR2019') %>% 
  split(.$size) %>% 
  map(~lm(n_fledge ~ hsi.edge.density, data=.x)) %>% 
  map(summary) %>% 
  map(glance) %>% 
  bind_rows(.id='size') %>% 
  mutate(variable='hsi edge density')

# HSI contagion.
productivity.by.hsi.contagion <- data %>% 
  drop_na(n_fledge) %>% 
  filter(nest != 'TCR2019') %>% 
  split(.$size) %>% 
  map(~lm(n_fledge ~ hsi.contagion, data=.x)) %>% 
  map(summary) %>% 
  map(glance) %>% 
  bind_rows(.id='size') %>% 
  mutate(variable='hsi contagion')

# Proportion suitable.
productivity.by.proportion.suitable <- data %>% 
  drop_na(n_fledge) %>% 
  filter(nest != 'TCR2019') %>% 
  split(.$size) %>% 
  map(~lm(n_fledge ~ proportion.suitable, data=.x)) %>% 
  map(summary) %>% 
  map(glance) %>% 
  bind_rows(.id='size') %>% 
  mutate(variable='proportion suitable')

# Suitable habitat edge density.
productivity.by.suitable.edge.density <- data %>% 
  drop_na(n_fledge) %>% 
  filter(nest != 'TCR2019') %>% 
  split(.$size) %>% 
  map(~lm(n_fledge ~ suitable.edge.density, data=.x)) %>% 
  map(summary) %>% 
  map(glance) %>% 
  bind_rows(.id='size') %>% 
  mutate(variable='suitable habitat edge density')

# Bind it all together.
productivity.landscape.statistics <- bind_rows(productivity.by.bec.diversity, 
          productivity.by.proportion.cover.mature,
          productivity.by.cover.edge.density,
          productivity.by.cover.contagion,
          productivity.by.cover.diversity,
          productivity.by.cover.evenness,
          productivity.by.cover.richness,
          productivity.by.hsi.edge.density,
          productivity.by.hsi.contagion,
          productivity.by.proportion.suitable,
          productivity.by.suitable.edge.density)
```

And one last table.

``` r
productivity.landscape.statistics <- productivity.landscape.statistics %>% 
  mutate(sig=round(p.value, digits=2)) %>% 
  mutate(sig=case_when(
  p.value < 0.1 ~ paste(as.character(sig), '*'),
  TRUE ~  paste(as.character(sig))
))

productivity.landscape.statistics %>% select(size, sig, variable) %>% 
  pivot_wider(names_from=size, values_from=sig) %>% 
  select(variable, PFA, everything()) %>% 
  kable() %>% kable_styling(bootstrap_options=c('striped'))
```

<table class="table table-striped" style="margin-left: auto; margin-right: auto;">

<thead>

<tr>

<th style="text-align:left;">

variable

</th>

<th style="text-align:left;">

PFA

</th>

<th style="text-align:left;">

breeding area

</th>

<th style="text-align:left;">

home range

</th>

<th style="text-align:left;">

maximum range

</th>

</tr>

</thead>

<tbody>

<tr>

<td style="text-align:left;">

forest type diversity

</td>

<td style="text-align:left;">

0.61

</td>

<td style="text-align:left;">

0.64

</td>

<td style="text-align:left;">

0.89

</td>

<td style="text-align:left;">

0.26

</td>

</tr>

<tr>

<td style="text-align:left;">

proportion older forest

</td>

<td style="text-align:left;">

0.86

</td>

<td style="text-align:left;">

0.99

</td>

<td style="text-align:left;">

0.3

</td>

<td style="text-align:left;">

0.18

</td>

</tr>

<tr>

<td style="text-align:left;">

landcover edge density

</td>

<td style="text-align:left;">

0.79

</td>

<td style="text-align:left;">

0.68

</td>

<td style="text-align:left;">

0.9

</td>

<td style="text-align:left;">

0.81

</td>

</tr>

<tr>

<td style="text-align:left;">

landcover contagion

</td>

<td style="text-align:left;">

0.61

</td>

<td style="text-align:left;">

0.41

</td>

<td style="text-align:left;">

0.25

</td>

<td style="text-align:left;">

0.5

</td>

</tr>

<tr>

<td style="text-align:left;">

landcover diversity

</td>

<td style="text-align:left;">

0.59

</td>

<td style="text-align:left;">

0.45

</td>

<td style="text-align:left;">

0.18

</td>

<td style="text-align:left;">

0.33

</td>

</tr>

<tr>

<td style="text-align:left;">

landcover evenness

</td>

<td style="text-align:left;">

0.48

</td>

<td style="text-align:left;">

0.42

</td>

<td style="text-align:left;">

0.19

</td>

<td style="text-align:left;">

0.36

</td>

</tr>

<tr>

<td style="text-align:left;">

landcover richness

</td>

<td style="text-align:left;">

0.7

</td>

<td style="text-align:left;">

0.39

</td>

<td style="text-align:left;">

0.26

</td>

<td style="text-align:left;">

0.63

</td>

</tr>

<tr>

<td style="text-align:left;">

hsi edge density

</td>

<td style="text-align:left;">

0.73

</td>

<td style="text-align:left;">

0.85

</td>

<td style="text-align:left;">

0.76

</td>

<td style="text-align:left;">

0.8

</td>

</tr>

<tr>

<td style="text-align:left;">

hsi contagion

</td>

<td style="text-align:left;">

0.11

</td>

<td style="text-align:left;">

0.69

</td>

<td style="text-align:left;">

0.91

</td>

<td style="text-align:left;">

0.59

</td>

</tr>

<tr>

<td style="text-align:left;">

proportion suitable

</td>

<td style="text-align:left;">

0.85

</td>

<td style="text-align:left;">

0.77

</td>

<td style="text-align:left;">

0.22

</td>

<td style="text-align:left;">

0.51

</td>

</tr>

<tr>

<td style="text-align:left;">

suitable habitat edge density

</td>

<td style="text-align:left;">

0.77

</td>

<td style="text-align:left;">

0.91

</td>

<td style="text-align:left;">

0.7

</td>

<td style="text-align:left;">

0.55

</td>

</tr>

</tbody>

</table>
