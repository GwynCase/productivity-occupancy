R Notebook
================

So apparently I never actually made a full notebook for my chi-squared
tests.

``` r
# Import conflict settings.
source('../src/conflicted.R')

#Load some libraries.
library(tidyverse)

# Bring in diet data.
source('../src/prey_attributes_revised.R')

# Add grouping variable.
diet.items <- diet.items %>% mutate(group=case_when(
  class == 'Aves' & family == 'Phasianidae' ~ 'grouse',
  class == 'Aves' & family == 'Corvidae' ~ 'corvid',
  class == 'Aves' & family == 'Turdidae' ~ 'thrush',
  class == 'Aves' ~ 'bird',
  class == 'Mammalia' & genus == 'Tamiasciurus' ~ 'squirrel',
  class == 'Mammalia' & genus == 'Lepus' ~ 'hare',
  class == 'Mammalia' ~ 'mammal',
  TRUE ~ 'unknown'
))

# Bring in zone data.
centroids <- read_csv('../data/interim/zone_centroid_sites.csv')

# Make a frequency table of groups for camera data.
freq.camera <- centroids %>% 
  select(site, zone) %>% right_join(diet.items, by=c('site')) %>% 
  filter(method == 'camera') %>%
  group_by(zone, group) %>%
  mutate(count=n()) %>% 
  select(zone, group, count) %>% 
  distinct() %>% ungroup() %>% 
  pivot_wider(names_from='group', values_from='count', values_fill=0) %>% 
  column_to_rownames(var='zone')

freq.camera
```

    ##    corvid grouse thrush mammal squirrel unknown bird hare
    ## tz     12      6     36     50      248      36   21    2
    ## cs      0      2     24      7       84      20    6    0

So far so good. Now run a chi-squared test.

``` r
chi.camera <- chisq.test(freq.camera, correct=FALSE, simulate.p.value=TRUE)

chi.camera
```

    ## 
    ##  Pearson's Chi-squared test with simulated p-value (based on 2000
    ##  replicates)
    ## 
    ## data:  freq.camera
    ## X-squared = 19.725, df = NA, p-value = 0.006497

Oh good lord, why is that a different number than I got last time?? I
used the exact same code…

Ah well, still significant so…

Ok, so the distribution of prey items is different between the two
zones. But *how* is it different?

``` r
# Raw residuals.
chi.camera$residuals
```

    ##       corvid      grouse    thrush    mammal   squirrel    unknown       bird
    ## tz  1.038128  0.02667360 -1.275917  1.186096  0.1081143 -0.8603029  0.2165791
    ## cs -1.759964 -0.04522042  2.163093 -2.010817 -0.1832888  1.4584928 -0.3671720
    ##          hare
    ## tz  0.4238139
    ## cs -0.7185023

``` r
# Standardized residuals.
chi.camera$stdres
```

    ##       corvid      grouse    thrush    mammal   squirrel   unknown       bird
    ## tz  2.065822  0.05288435 -2.659504  2.464809  0.3361617 -1.785988  0.4370721
    ## cs -2.065822 -0.05288435  2.659504 -2.464809 -0.3361617  1.785988 -0.4370721
    ##          hare
    ## tz  0.8356943
    ## cs -0.8356943

So same results
