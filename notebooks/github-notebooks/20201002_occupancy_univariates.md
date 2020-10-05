Occupancy univariates
================

Running the same landscape variables I used for diet and productivity,
but this time for occupancy.

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
library(sf)
library(raster)
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

Then load in occupancy data.

``` r
occupancy <- read_csv('../data/processed/occupancy_sc.csv')

occupied <- occupancy %>% pivot_longer(-c(site, name), names_to='year', values_to='status') %>%
  filter(status > 0) %>% 
  group_by(site, status) %>% 
  add_tally() %>% 
  distinct(site, status, .keep_all=TRUE) %>% 
  select(-year) %>% 
  pivot_wider(names_from=status, values_from=n, values_fill=0) %>% 
  ungroup() %>% rowwise(site, name) %>% 
  mutate(years.surveyed=sum(c(`3`, `2`, `1`)),
         years.occupied=sum(c(`3`, `2`)),
         proportion.occupied=years.occupied/years.surveyed) %>% 
  select(site, name, years.surveyed, years.occupied, proportion.occupied) %>% 
  arrange(desc(years.surveyed, years.occupied))

occupied
```

    ## # A tibble: 70 x 5
    ## # Rowwise:  site, name
    ##    site  name           years.surveyed years.occupied proportion.occupied
    ##    <chr> <chr>                   <int>          <int>               <dbl>
    ##  1 GIO   Giovanno                    7              5               0.714
    ##  2 DCR   Dipper Creek                6              3               0.5  
    ##  3 HAS   Haslam                      6              1               0.167
    ##  4 PWD   Powell Daniels              6              5               0.833
    ##  5 RLK   Ruby Lake                   6              6               1    
    ##  6 TCR   Turbid Creek                6              5               0.833
    ##  7 WCR   Wedge Creek                 6              5               0.833
    ##  8 BWC   Brew Creek                  5              1               0.2  
    ##  9 JVC   Jarvis                      5              0               0    
    ## 10 NAN   Nanton                      5              1               0.2  
    ## # ... with 60 more rows

To summarize:

``` r
# How many sites have occupancy data?
nrow(occupied)
```

    ## [1] 70

``` r
# How many years have sites been surveyed?
mean(occupied$years.surveyed)
```

    ## [1] 2.671429

``` r
range(occupied$years.surveyed)
```

    ## [1] 1 7

Now bring in the centroids for all of these sites. The centroids are
just calculated from all the known nests for a given site and provide a
stable, central “capital” for the territory. Which is BS but whatever.

``` r
# Bring in centroid data.
centroids <- read_csv('../data/interim/zone_centroid_sites.csv')

# Join to occupancy data. Not all sites have occupancy data, so we'll keep just the ones that do.
occupied <- left_join(occupied, centroids, by=c('site')) %>% 
  rename(xcoord=MEAN_X, ycoord=MEAN_Y) %>% 
  drop_na()

# Make it a spatial object for later.
sites.sf <- occupied %>% 
  st_as_sf(coords=c('xcoord', 'ycoord')) %>% 
  st_set_crs('+proj=utm +zone=10 +datum=WGS84 +units=m +no_defs')

# Also make a list of site names for later.
site.names <- occupied$site
```

The `drop_na()` is to drop Gravell, a new site in 2020 for which I don’t
yet have a nest location. I’ll have to add it back once I know where it
is.

Next step is to bring in the landscape rasters and calculate all the
metrics.

``` r
# Load all the rasters.
source('../src/load_rasters.R')

# Calculate metrics for each site.
source('../src/calc_land_metrics.R')

# Join the data together.
data <- full_join(occupied, bec.landscape.metrics, by=c('site' = 'nest')) %>%
  full_join(suitable.landscape.metrics, by=c('site' = 'nest', 'radius')) %>% 
  full_join(landcover.landscape.metrics, by=c('site' = 'nest', 'radius', 'size')) %>% 
  full_join(hsi.landscape.metrics, by=c('site' = 'nest', 'radius', 'size')) %>% 
  full_join(gap.landscape.metrics, by=c('site' = 'nest', 'radius', 'size')) %>% 
  full_join(canopy.landscape.metrics, by=c('site' = 'nest', 'radius', 'size'))
```

``` r
# Bring in previously calculated data.
data <- readRDS('../data/interim/occupancy_landscape_metrics.rds')
```

Note that a few of these sites have less than 90% of their area with
data coverage for some things (I think about four sites, as some scale
or another) and I haven’t tracked them down and removed them yet.

``` r
# Save for now, since it takes a while to calculate.
saveRDS(data, '../data/interim/occupancy_landscape_metrics.rds')
```

Now… finally make the models?

Here’s what one looks like, just to show the simple code:

``` r
glm(proportion.occupied ~ proportion.suitable, data=data, family=binomial, weights=years.surveyed)
```

    ## 
    ## Call:  glm(formula = proportion.occupied ~ proportion.suitable, family = binomial, 
    ##     data = data, weights = years.surveyed)
    ## 
    ## Coefficients:
    ##         (Intercept)  proportion.suitable  
    ##           -0.045835             0.004071  
    ## 
    ## Degrees of Freedom: 274 Total (i.e. Null);  273 Residual
    ##   (1 observation deleted due to missingness)
    ## Null Deviance:       530.8 
    ## Residual Deviance: 529.2     AIC: 716.2

And here’s the whole slew of them:

``` r
# Proportion suitable.
occupancy.by.bec.diversity <- data %>% 
  split(.$size) %>% 
  map(~glm(proportion.occupied ~ bec.diversity, data=.x, 
           family=binomial, weights=years.surveyed)) %>% 
  map(tidy) %>% 
  bind_rows(.id='size') %>% 
  mutate(variable='forest type diversity')

occupancy.by.proportion.cover.mature <- data %>% 
  split(.$size) %>% 
  map(~glm(proportion.occupied ~ proportion.cover.mature, data=.x, 
           family=binomial, weights=years.surveyed)) %>% 
  map(tidy) %>% 
  bind_rows(.id='size') %>% 
  mutate(variable='proportion older forest')

occupancy.by.cover.edge.density <- data %>% 
  split(.$size) %>% 
  map(~glm(proportion.occupied ~ cover.edge.density, data=.x, 
           family=binomial, weights=years.surveyed)) %>% 
  map(tidy) %>% 
  bind_rows(.id='size') %>% 
  mutate(variable='landcover edge density')

occupancy.by.cover.contagion <- data %>% 
  split(.$size) %>% 
  map(~glm(proportion.occupied ~ cover.contagion, data=.x, 
           family=binomial, weights=years.surveyed)) %>% 
  map(tidy) %>% 
  bind_rows(.id='size') %>% 
  mutate(variable='landcover contagion')

occupancy.by.cover.diversity <- data %>% 
  split(.$size) %>% 
  map(~glm(proportion.occupied ~ cover.diversity, data=.x, 
           family=binomial, weights=years.surveyed)) %>% 
  map(tidy) %>% 
  bind_rows(.id='size') %>% 
  mutate(variable='landcover diversity')

occupancy.by.cover.richness <- data %>% 
  split(.$size) %>% 
  map(~glm(proportion.occupied ~ cover.richness, data=.x, 
           family=binomial, weights=years.surveyed)) %>% 
  map(tidy) %>% 
  bind_rows(.id='size') %>% 
  mutate(variable='landcover richness')

occupancy.by.cover.evenness <- data %>% 
  split(.$size) %>% 
  map(~glm(proportion.occupied ~ cover.evenness, data=.x, 
           family=binomial, weights=years.surveyed)) %>% 
  map(tidy) %>% 
  bind_rows(.id='size') %>% 
  mutate(variable='landcover evenness')

occupancy.by.gap.edge.density <- data %>% 
  split(.$size) %>% 
  map(~glm(proportion.occupied ~ gap.edge.density, data=.x, 
           family=binomial, weights=years.surveyed)) %>% 
  map(tidy) %>% 
  bind_rows(.id='size') %>% 
  mutate(variable='gap edge density')

occupancy.by.canopy.cover <- data %>% 
  split(.$size) %>% 
  map(~glm(proportion.occupied ~ canopy.high, data=.x, 
           family=binomial, weights=years.surveyed)) %>% 
  map(tidy) %>% 
  bind_rows(.id='size') %>% 
  mutate(variable='high canopy cover')

occupancy.by.hsi.edge.density <- data %>% 
  split(.$size) %>% 
  map(~glm(proportion.occupied ~ hsi.edge.density, data=.x, 
           family=binomial, weights=years.surveyed)) %>% 
  map(tidy) %>% 
  bind_rows(.id='size') %>% 
  mutate(variable='hsi edge density')

occupancy.by.hsi.contagion <- data %>% 
  split(.$size) %>% 
  map(~glm(proportion.occupied ~ hsi.contagion, data=.x, 
           family=binomial, weights=years.surveyed)) %>% 
  map(tidy) %>% 
  bind_rows(.id='size') %>% 
  mutate(variable='hsi contagion')

occupancy.by.proportion.suitable <- data %>% 
  split(.$size) %>% 
  map(~glm(proportion.occupied ~ proportion.suitable, data=.x, 
           family=binomial, weights=years.surveyed)) %>% 
  map(tidy) %>% 
  bind_rows(.id='size') %>% 
  mutate(variable='proportion suitable')

occupancy.by.suitable.edge.density <- data %>% 
  split(.$size) %>% 
  map(~glm(proportion.occupied ~ suitable.edge.density, data=.x, 
           family=binomial, weights=years.surveyed)) %>% 
  map(tidy) %>% 
  bind_rows(.id='size') %>% 
  mutate(variable='suitable habitat edge density')

# Bind it all together.
occupancy.landscape.statistics <- bind_rows(occupancy.by.bec.diversity, 
          occupancy.by.proportion.cover.mature,
          occupancy.by.cover.edge.density,
          occupancy.by.cover.contagion,
          occupancy.by.cover.diversity,
          occupancy.by.cover.evenness,
          occupancy.by.cover.richness,
          occupancy.by.gap.edge.density,
          occupancy.by.canopy.cover,
          occupancy.by.hsi.edge.density,
          occupancy.by.hsi.contagion,
          occupancy.by.proportion.suitable,
          occupancy.by.suitable.edge.density)
```

Stick it all in a beautiful table of p-values.

``` r
occupancy.landscape.statistics <- occupancy.landscape.statistics %>% 
  mutate(sig=round(p.value, digits=2)) %>% 
  mutate(sig=case_when(
  p.value < 0.05 ~ paste(as.character(sig), '*'),
  TRUE ~  paste(as.character(sig))
))

occupancy.landscape.statistics %>% 
  filter(term != '(Intercept)') %>% 
  select(size, sig, variable) %>% 
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

0.57

</td>

<td style="text-align:left;">

0.27

</td>

<td style="text-align:left;">

0.36

</td>

<td style="text-align:left;">

0.27

</td>

</tr>

<tr>

<td style="text-align:left;">

proportion older forest

</td>

<td style="text-align:left;">

0.15

</td>

<td style="text-align:left;">

0.33

</td>

<td style="text-align:left;">

0.27

</td>

<td style="text-align:left;">

0.13

</td>

</tr>

<tr>

<td style="text-align:left;">

landcover edge density

</td>

<td style="text-align:left;">

0.08

</td>

<td style="text-align:left;">

0.05

</td>

<td style="text-align:left;">

0.96

</td>

<td style="text-align:left;">

0.67

</td>

</tr>

<tr>

<td style="text-align:left;">

landcover contagion

</td>

<td style="text-align:left;">

0.08

</td>

<td style="text-align:left;">

0.14

</td>

<td style="text-align:left;">

0.24

</td>

<td style="text-align:left;">

0.19

</td>

</tr>

<tr>

<td style="text-align:left;">

landcover diversity

</td>

<td style="text-align:left;">

0.05 \*

</td>

<td style="text-align:left;">

0.2

</td>

<td style="text-align:left;">

0.05 \*

</td>

<td style="text-align:left;">

0.08

</td>

</tr>

<tr>

<td style="text-align:left;">

landcover evenness

</td>

<td style="text-align:left;">

0.08

</td>

<td style="text-align:left;">

0.21

</td>

<td style="text-align:left;">

0.05

</td>

<td style="text-align:left;">

0.07

</td>

</tr>

<tr>

<td style="text-align:left;">

landcover richness

</td>

<td style="text-align:left;">

0.52

</td>

<td style="text-align:left;">

0.92

</td>

<td style="text-align:left;">

0.04 \*

</td>

<td style="text-align:left;">

0.77

</td>

</tr>

<tr>

<td style="text-align:left;">

gap edge density

</td>

<td style="text-align:left;">

0 \*

</td>

<td style="text-align:left;">

0 \*

</td>

<td style="text-align:left;">

0.07

</td>

<td style="text-align:left;">

0.27

</td>

</tr>

<tr>

<td style="text-align:left;">

high canopy cover

</td>

<td style="text-align:left;">

0.67

</td>

<td style="text-align:left;">

0.3

</td>

<td style="text-align:left;">

0.45

</td>

<td style="text-align:left;">

0.98

</td>

</tr>

<tr>

<td style="text-align:left;">

hsi edge density

</td>

<td style="text-align:left;">

0.14

</td>

<td style="text-align:left;">

0.44

</td>

<td style="text-align:left;">

0.96

</td>

<td style="text-align:left;">

0.21

</td>

</tr>

<tr>

<td style="text-align:left;">

hsi contagion

</td>

<td style="text-align:left;">

0.15

</td>

<td style="text-align:left;">

0.76

</td>

<td style="text-align:left;">

0.95

</td>

<td style="text-align:left;">

0.41

</td>

</tr>

<tr>

<td style="text-align:left;">

proportion suitable

</td>

<td style="text-align:left;">

0 \*

</td>

<td style="text-align:left;">

0.17

</td>

<td style="text-align:left;">

0.18

</td>

<td style="text-align:left;">

0.47

</td>

</tr>

<tr>

<td style="text-align:left;">

suitable habitat edge density

</td>

<td style="text-align:left;">

0 \*

</td>

<td style="text-align:left;">

0.02 \*

</td>

<td style="text-align:left;">

0.61

</td>

<td style="text-align:left;">

0.03 \*

</td>

</tr>

</tbody>

</table>

So this is interesting because the only things that are significant are
at the smaller scales, while for the diet analysis the only things
significant were at the larger scales. This seems to be showing that
lots of contiguous, high-quality habitat close to the nest is what
determines occupancy. Which is not surprising at all.

To look a little closer…

``` r
occupancy.landscape.statistics %>% 
  filter(term != '(Intercept)' & p.value < 0.05) %>% 
  select(size, estimate, variable) %>% 
  pivot_wider(names_from=size, values_from=estimate) %>% 
  select(variable, PFA, `breeding area`, everything()) %>% 
  kable() %>% kable_styling(bootstrap_options=c('striped'))
```

<table class="table table-striped" style="margin-left: auto; margin-right: auto;">

<thead>

<tr>

<th style="text-align:left;">

variable

</th>

<th style="text-align:right;">

PFA

</th>

<th style="text-align:right;">

breeding area

</th>

<th style="text-align:right;">

home range

</th>

<th style="text-align:right;">

maximum range

</th>

</tr>

</thead>

<tbody>

<tr>

<td style="text-align:left;">

landcover diversity

</td>

<td style="text-align:right;">

\-1.6084409

</td>

<td style="text-align:right;">

NA

</td>

<td style="text-align:right;">

2.68198

</td>

<td style="text-align:right;">

NA

</td>

</tr>

<tr>

<td style="text-align:left;">

landcover richness

</td>

<td style="text-align:right;">

NA

</td>

<td style="text-align:right;">

NA

</td>

<td style="text-align:right;">

15.63961

</td>

<td style="text-align:right;">

NA

</td>

</tr>

<tr>

<td style="text-align:left;">

gap edge density

</td>

<td style="text-align:right;">

\-0.0355846

</td>

<td style="text-align:right;">

\-0.0419487

</td>

<td style="text-align:right;">

NA

</td>

<td style="text-align:right;">

NA

</td>

</tr>

<tr>

<td style="text-align:left;">

proportion suitable

</td>

<td style="text-align:right;">

0.0282701

</td>

<td style="text-align:right;">

NA

</td>

<td style="text-align:right;">

NA

</td>

<td style="text-align:right;">

NA

</td>

</tr>

<tr>

<td style="text-align:left;">

suitable habitat edge density

</td>

<td style="text-align:right;">

\-0.0266530

</td>

<td style="text-align:right;">

\-0.0248540

</td>

<td style="text-align:right;">

NA

</td>

<td style="text-align:right;">

\-0.0619947

</td>

</tr>

</tbody>

</table>

This is showing the slope estimates for variables/scales that are
significant, and yes, sure enough, the edge/fragmentation metrics have a
negative effect, while proportion suitable has a positive effect.
Interesting that the richness/diversity are only beneficial at larger
scales.
