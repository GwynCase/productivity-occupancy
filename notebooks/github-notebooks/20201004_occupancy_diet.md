Occupancy and diet
================

Apparently I skipped straight to occupancy \~ landscape in my last
notebook and completely forgot to do occupancy \~ diet. Or rather, I did
it, but sloppily in the storytime notebook which is not a place I should
be doing original work. So.

``` r
# Import conflict settings.
source('../src/conflicted.R')

#Load some libraries.
library(tidyverse)
library(vegan)
library(landscapemetrics)
library(broom)
library(knitr)
library(kableExtra)
library(AICcmodavg)
```

Start by bringing in diet and occupancy data. Since there’s no
difference between cs & tz in terms of occupancy, and no difference
between zones in terms of squirrel biomass (at least based on cameras),
I’m not sure there’s any point in trying to use the pooled physical
remains data, since that would only be useful for a cs vs tz comparison.
So I’ll stick to the camera sites, even though it’s a very small sample
size.

``` r
# Bring in occupancy data.
occupancy <- read_csv('../data/processed/occupancy_sc.csv')

# Clean it up.
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

# Bring in diet data.
source('../src/prey_attributes_revised.R')

# Take only items identified to genus and twist to a wide format.
camera.diet.wide <- diet.items %>% filter(binomial != 'Unidentified item' & method == 'camera') %>% 
  group_by(nest, genus, species) %>% 
  mutate(count=n()) %>% ungroup() %>% 
  dplyr::select(nest, binomial, count) %>%
  distinct() %>% 
  pivot_wider(names_from=binomial, values_from=count,
              values_fill=list(count = 0))

# Calculate diet diversity.
camera.diet.diversity <- plyr::ddply(camera.diet.wide, ~nest, function(x) {
           data.frame(diet.diversity=diversity(x[-1], index='simpson'))
   })

# Calculate proportion of squirrel by biomass and count for camera data.
proportion.squirrel.camera <- diet.items %>%
  filter(method == 'camera') %>%
  group_by(nest) %>% 
  mutate(total.mass=sum(mass), total.count=n()) %>% 
  filter(genus == 'Tamiasciurus') %>% 
  mutate(mass.sq=sum(mass), count.sq=n(), 
         prop.sq.mass=mass.sq/total.mass, prop.sq.count=count.sq/total.count) %>% 
  select(site, nest, prop.sq.mass, prop.sq.count) %>% distinct() 

# Bind together into one data frame.
camera.diet.variables <- full_join(proportion.squirrel.camera, camera.diet.diversity, by=c('nest'))

# Add occupancy data.
camera.diet.variables <- left_join(camera.diet.variables, occupied, by=c('site'))
```

Now that that’s in order, let’s make some models.

``` r
occupied.by.diet.diversity <- glm(proportion.occupied ~ diet.diversity, 
                                  data=camera.diet.variables, family=binomial, weights=years.surveyed)

occupied.by.proportion.squirrel <- glm(proportion.occupied ~ prop.sq.mass, 
                                       data=camera.diet.variables, family=binomial, weights=years.surveyed)

occupied.by.diet.diversity %>% tidy()
```

    ## # A tibble: 2 x 5
    ##   term           estimate std.error statistic p.value
    ##   <chr>             <dbl>     <dbl>     <dbl>   <dbl>
    ## 1 (Intercept)        4.27      2.27      1.88  0.0601
    ## 2 diet.diversity    -5.89      4.57     -1.29  0.197

``` r
occupied.by.proportion.squirrel %>% tidy()
```

    ## # A tibble: 2 x 5
    ##   term         estimate std.error statistic p.value
    ##   <chr>           <dbl>     <dbl>     <dbl>   <dbl>
    ## 1 (Intercept)     1.98       6.82    0.290    0.772
    ## 2 prop.sq.mass   -0.251      9.04   -0.0278   0.978

So nothing.
