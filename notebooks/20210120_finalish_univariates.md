Finalish univariates
================

This is just a repeat of an earlier notebook, but now I have my final
territory quality dataset so it’s no longer rough draft.

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
library(GGally)
library(extrafont)
library(QuantPsyc)
```

Start by defining the four nested scales.

``` r
# Area is in hectares.
landscape <- data.frame(
  size=c('PFA', 'breeding area', 'home range', 'maximum range'),
  area=c(60, 200, 3800, 15600)
)

# Convert area in hectares to radii in meters.
landscape <- landscape %>% mutate(radius=sqrt(area*10000/pi))

landscape
```

    ##            size  area    radius
    ## 1           PFA    60  437.0194
    ## 2 breeding area   200  797.8846
    ## 3    home range  3800 3477.8982
    ## 4 maximum range 15600 7046.7256

Then load in occupancy data. This now includes data from 2020. I also
filter out all sites with a single year of survey data, since occupied
sites are more likely to be detected so there is a bias towards
one-year-occupieds. Finally, I also drop any sites where NOGO have
*never* been detected. This probably skews my data set a bit, but it
avoids the isssue of non-NOGO nests being misidentified.

In this data set, 0 = no surveys conducted, 1 = surveyed but no NOGO
detected, 2 = NOGO detected but no breeding, and 3 = breeding.

``` r
# Read in the data.
occupancy <- read_csv('../data/processed/occupancy_sc.csv')

# Calculate number of years NOGO detected out of number of years surveyed.
territory.quality <- occupancy %>% pivot_longer(-c(site, name), names_to='year', values_to='status') %>%
  filter(status > 0) %>% 
  group_by(site, status) %>% 
  add_tally() %>% 
  distinct(site, status, .keep_all=TRUE) %>% 
  select(-year) %>% 
  pivot_wider(names_from=status, values_from=n, values_fill=0) %>% 
  ungroup() %>% rowwise(site, name) %>% 
  mutate(years.surveyed=sum(c(`3`, `2`, `1`)),
         years.detect=sum(c(`3`, `2`)),
         years.no.detect=years.surveyed-years.detect,
         quality.index=years.detect/years.surveyed) %>% 
  select(site, name, years.surveyed, years.detect, years.no.detect, quality.index) %>% 
  filter(years.surveyed > 1) %>% 
  filter(years.detect >= 1)

# Look at it.
territory.quality
```

    ## # A tibble: 44 x 6
    ## # Rowwise:  site, name
    ##    site  name          years.surveyed years.detect years.no.detect quality.index
    ##    <chr> <chr>                  <int>        <int>           <int>         <dbl>
    ##  1 BWC   Brew Creek                 5            1               4         0.2  
    ##  2 BRO   Brohm                      4            1               3         0.25 
    ##  3 CLW   Clowhom                    4            3               1         0.75 
    ##  4 DWC   Dewdney Creek              2            1               1         0.5  
    ##  5 DCR   Dipper Creek               6            3               3         0.5  
    ##  6 FMT   Fire Mountain              2            2               0         1    
    ##  7 FRE   Freil                      3            2               1         0.667
    ##  8 GIO   Giovanno                   7            5               2         0.714
    ##  9 GCR   Glacial Creek              2            1               1         0.5  
    ## 10 GLK   Glacier Lake               2            1               1         0.5  
    ## # ... with 34 more rows

To summarize:

``` r
# How many sites have occupancy data?
nrow(territory.quality)
```

    ## [1] 44

``` r
# How many years have sites been surveyed?
mean(territory.quality$years.surveyed)
```

    ## [1] 3.522727

``` r
range(territory.quality$years.surveyed)
```

    ## [1] 2 7

``` r
# What kind of quality do we see?
mean(territory.quality$quality.index)
```

    ## [1] 0.6453463

``` r
sd(territory.quality$quality.index)
```

    ## [1] 0.2456643

Those are some interestingly low numbers. Generally, sites are only
surveyed about three years, and are occupied for only a little more than
half of them.

Now I need to generate centroids for all of these. Bring in the nest
list for the sites–this has every known nest for every site.

``` r
# Read in the data.
nests <- read_csv('../data/processed/sc_nests.csv')

# Calculate a centroid for each site, and keep only ones with a quality index.
centroids <- nests %>% group_by(site) %>% 
  mutate(mean.x=mean(xcoord), mean.y=mean(ycoord)) %>% 
  distinct(site, name, mean.x, mean.y)

sites.sf <- left_join(territory.quality, centroids, by=c('site', 'name')) %>% 
  rename(xcoord=mean.x, ycoord=mean.y)

# Make it a spatial object for later.
sites.sf <- sites.sf %>% 
  st_as_sf(coords=c('xcoord', 'ycoord')) %>% 
  st_set_crs('+proj=utm +zone=10 +datum=WGS84 +units=m +no_defs')

# Also make a list of site names for later.
site.names <- sites.sf$site
```

Next step is to bring in the landscape rasters and calculate all the
metrics.

``` r
# Load all the rasters.
source('../src/load_rasters.R')

# Calculate metrics for each site.
source('../src/calc_land_metrics.R')

# Join the data together.
data <- full_join(territory.quality, bec.landscape.metrics, by=c('site' = 'nest')) %>%
  full_join(suitable.landscape.metrics, by=c('site' = 'nest', 'radius', 'size')) %>% 
  full_join(landcover.landscape.metrics, by=c('site' = 'nest', 'radius', 'size')) %>% 
  full_join(hsi.landscape.metrics, by=c('site' = 'nest', 'radius', 'size')) %>% 
  full_join(gap.landscape.metrics, by=c('site' = 'nest', 'radius', 'size')) %>% 
  full_join(canopy.landscape.metrics, by=c('site' = 'nest', 'radius', 'size'))
```

And I will save all these metrics now, since they won’t change in the
future unless I change my rasters.

``` r
write_csv(data, '../data/processed/landscape_metrics.csv')
```
