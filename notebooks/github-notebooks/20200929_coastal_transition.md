Coastal and transition
================

Here’s checking whether there’s any difference at all between coastal
and transition diet.

``` r
# Import conflict settings.
source('../src/conflicted.R')

# Load some libraries.
options(tidyverse.quiet = TRUE)
options(dplyr.summarise.inform = FALSE)
library(tidyverse)
library(lubridate)
library(vegan)
library(ggplot2)
library(knitr)
library(kableExtra)

# Bring in data.
source('../src/prey_attributes_revised.R')
```

First bring in data on which sites are coastal and which sites are
transition.

``` r
# Zone data.
centroids <- read_csv('../data/interim/zone_centroid_sites.csv')

# Add to diet data.
diet.items <- centroids %>% select(site, zone) %>% right_join(diet.items)
```

Using just the camera data, how many sites do I have for each zone, and
how many items?

``` r
# How many sites?
diet.items %>% filter(method == 'camera') %>% 
  distinct(site, zone) %>% group_by(zone) %>% 
  summarize(count=n())
```

    ## # A tibble: 2 x 2
    ##   zone  count
    ##   <chr> <int>
    ## 1 cs        2
    ## 2 tz        4

``` r
# How many items?
diet.items %>% filter(method == 'camera') %>% 
  group_by(zone) %>% summarize(count=n())
```

    ## # A tibble: 2 x 2
    ##   zone  count
    ##   <chr> <int>
    ## 1 cs      143
    ## 2 tz      411

What’s the mammal and squirrel breakdown?

``` r
camera.proportions <- diet.items %>%
  filter(method == 'camera') %>%
  group_by(site, zone) %>% 
  mutate(total.mass=sum(mass), total.count=n()) %>% 
  filter(class == 'Mammalia') %>% 
  mutate(mass.mm=sum(mass), count.mm=n()) %>% 
  filter(genus == 'Tamiasciurus') %>% 
  mutate(mass.sq=sum(mass), count.sq=n(), 
         prop.mm.mass=mass.mm/total.mass, prop.mm.count=count.mm/total.count,
         prop.sq.mass=mass.sq/total.mass, prop.sq.count=count.sq/total.count) %>% 
  select(site, zone, prop.mm.mass, prop.mm.count, prop.sq.mass, prop.sq.count) %>% distinct()

camera.proportions
```

    ## # A tibble: 6 x 6
    ## # Groups:   site, zone [6]
    ##   site  zone  prop.mm.mass prop.mm.count prop.sq.mass prop.sq.count
    ##   <chr> <chr>        <dbl>         <dbl>        <dbl>         <dbl>
    ## 1 TCR   tz           0.847         0.677        0.811         0.645
    ## 2 MTC   cs           0.775         0.623        0.717         0.566
    ## 3 UTZ   tz           0.912         0.87         0.727         0.76 
    ## 4 TMC   tz           0.757         0.48         0.487         0.32 
    ## 5 MTF   tz           0.845         0.797        0.717         0.615
    ## 6 RLK   cs           0.835         0.676        0.776         0.649

Just eyeballing them, they don’t look particularly different. But I can
do something formal to check.

``` r
# Split into different dataframes.
cp.tz <- camera.proportions %>% filter(zone == 'tz')
cp.cs <- camera.proportions %>% filter(zone == 'cs')

# t-test for proportion mammal.
t.test(cp.cs$prop.mm.mass, cp.tz$prop.mm.mass)
```

    ## 
    ##  Welch Two Sample t-test
    ## 
    ## data:  cp.cs$prop.mm.mass and cp.tz$prop.mm.mass
    ## t = -0.80532, df = 3.1873, p-value = 0.4764
    ## alternative hypothesis: true difference in means is not equal to 0
    ## 95 percent confidence interval:
    ##  -0.17021902  0.09964166
    ## sample estimates:
    ## mean of x mean of y 
    ## 0.8050924 0.8403811

Look at that massive p-value\! So mammal biomass is not significantly
different between coastal and transition zones. How about squirrels?

``` r
# t-test for proportion squirrel
t.test(cp.cs$prop.sq.mass, cp.tz$prop.sq.mass)
```

    ## 
    ##  Welch Two Sample t-test
    ## 
    ## data:  cp.cs$prop.sq.mass and cp.tz$prop.sq.mass
    ## t = 0.80757, df = 3.7985, p-value = 0.4669
    ## alternative hypothesis: true difference in means is not equal to 0
    ## 95 percent confidence interval:
    ##  -0.1527039  0.2743248
    ## sample estimates:
    ## mean of x mean of y 
    ## 0.7463982 0.6855878

Same. Let’s try that all again with pellet data.

``` r
# How many sites?
diet.items %>% filter(source == 'P') %>% 
  distinct(site, zone) %>% group_by(zone) %>% 
  summarize(count=n())
```

    ## # A tibble: 2 x 2
    ##   zone  count
    ##   <chr> <int>
    ## 1 cs        4
    ## 2 tz        8

``` r
# How many items?
diet.items %>% filter(source == 'P') %>% 
  group_by(zone) %>% summarize(count=n())
```

    ## # A tibble: 2 x 2
    ##   zone  count
    ##   <chr> <int>
    ## 1 cs       35
    ## 2 tz       36

What’s the mammal and squirrel breakdown?

``` r
pellet.proportions <- diet.items %>%
  filter(source == 'P') %>%
  group_by(site, zone) %>% 
  mutate(total.mass=sum(mass), total.count=n()) %>% 
  filter(class == 'Mammalia') %>% 
  mutate(mass.mm=sum(mass), count.mm=n()) %>% 
  filter(genus == 'Tamiasciurus') %>% 
  mutate(mass.sq=sum(mass), count.sq=n(), 
         prop.mm.mass=mass.mm/total.mass, prop.mm.count=count.mm/total.count,
         prop.sq.mass=mass.sq/total.mass, prop.sq.count=count.sq/total.count) %>% 
  select(site, zone, prop.mm.mass, prop.mm.count, prop.sq.mass, prop.sq.count) %>% distinct()

pellet.proportions
```

    ## # A tibble: 7 x 6
    ## # Groups:   site, zone [7]
    ##   site  zone  prop.mm.mass prop.mm.count prop.sq.mass prop.sq.count
    ##   <chr> <chr>        <dbl>         <dbl>        <dbl>         <dbl>
    ## 1 UTZ   tz           0.952         0.909        0.905        0.818 
    ## 2 MTF   tz           0.904         0.833        0.401        0.333 
    ## 3 RLK   cs           0.729         0.667        0.668        0.556 
    ## 4 PCR   tz           0.217         0.1          0.217        0.1   
    ## 5 BKH   tz           1             1            0.661        0.667 
    ## 6 MPT   cs           0.875         0.6          0.875        0.6   
    ## 7 PTL   cs           0.593         0.533        0.124        0.0667

The problem is that this doesn’t really make sense, since the sample
sizes are so low for most of the sites. Well, here goes, anyway…

``` r
# Split into different dataframes.
pp.tz <- pellet.proportions %>% filter(zone == 'tz')
pp.cs <- pellet.proportions %>% filter(zone == 'cs')

# t-test for proportion mammal.
t.test(pp.tz$prop.mm.mass, pp.cs$prop.mm.mass)
```

    ## 
    ##  Welch Two Sample t-test
    ## 
    ## data:  pp.tz$prop.mm.mass and pp.cs$prop.mm.mass
    ## t = 0.1766, df = 4.0447, p-value = 0.8683
    ## alternative hypothesis: true difference in means is not equal to 0
    ## 95 percent confidence interval:
    ##  -0.5225104  0.5938280
    ## sample estimates:
    ## mean of x mean of y 
    ## 0.7679427 0.7322839

``` r
# t-test for proportion squirrel.
t.test(pp.tz$prop.sq.mass, pp.cs$prop.sq.mass)
```

    ## 
    ##  Welch Two Sample t-test
    ## 
    ## data:  pp.tz$prop.sq.mass and pp.cs$prop.sq.mass
    ## t = -0.034854, df = 3.7125, p-value = 0.974
    ## alternative hypothesis: true difference in means is not equal to 0
    ## 95 percent confidence interval:
    ##  -0.7818224  0.7630180
    ## sample estimates:
    ## mean of x mean of y 
    ## 0.5459934 0.5553956

Oh yeah.

Well, that’s enough depressing results for one day.
