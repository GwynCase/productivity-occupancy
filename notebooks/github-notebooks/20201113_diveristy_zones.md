Diversity & similarity by zone
================

I did quite a bit of diversity and similarity stuff, but it was a while
ago and also not in reference to differences between the two zones. So
here’s that.

``` r
# Import conflict settings.
source('../src/conflicted.R')

#Load some libraries.
library(tidyverse)
library(vegan)
library(ggplot2)

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

# Add zone to data and pull out just camera items.
camera.data <- centroids %>% 
  select(site, zone) %>% right_join(diet.items, by=c('site')) %>% 
  filter(method == 'camera')
```

So that’s my data wrangled. Now let’s twist it wide and calculate an
overall diversity score for the whole study area, using items identified
to genus or better.

``` r
# Twist the data.
camera.data.flat <- camera.data %>% filter(binomial != 'Unidentified item') %>% 
  group_by(binomial) %>% 
  mutate(count=n()) %>% 
  select(binomial, count) %>% 
  distinct() %>% 
  pivot_wider(names_from=binomial, values_from=count, values_fill=list(count = 0))

# Calculate diversity.
diversity(camera.data.flat[-1], index='simpson')
```

    ## [1] 0.5384894

Not very high. Not very low. Hard to say anything interesting about that
number, really.

Let’s calculate it for each site, next.

``` r
# Twist the data.
camera.data.site <- camera.data %>% filter(binomial != 'Unidentified item') %>% 
  group_by(site, binomial) %>% 
  mutate(count=n()) %>% 
  select(site, binomial, count) %>% 
  distinct() %>% 
  pivot_wider(names_from=binomial, values_from=count, values_fill=list(count = 0))

# Calculate diversity.
plyr::ddply(camera.data.site, ~site, function(x) {
  data.frame(diet.diversity=diversity(x[-1], index='simpson'))
})
```

    ##   site diet.diversity
    ## 1  MTC      0.4813841
    ## 2  MTF      0.4805010
    ## 3  RLK      0.0768000
    ## 4  TCR      0.3495111
    ## 5  TMC      0.5840995
    ## 6  UTZ      0.6620643

Okay, that looks familiar. So far so good.

Now to do something new and calculate by zone.

``` r
# Twist the data.
camera.data.zone <- camera.data %>% filter(binomial != 'Unidentified item') %>% 
  group_by(zone, binomial) %>% 
  mutate(count=n()) %>% 
  select(zone, binomial, count) %>% 
  distinct() %>% 
  pivot_wider(names_from=binomial, values_from=count, values_fill=list(count = 0))

# Calculate diversity.
plyr::ddply(camera.data.zone, ~zone, function(x) {
  data.frame(diet.diversity=diversity(x[-1], index='simpson'))
})
```

    ##   zone diet.diversity
    ## 1   cs      0.4046281
    ## 2   tz      0.5871686

Aha\! So that is interesting, because a higher diversity is exactly what
was expected for the transition zone. Specifically, a higher diversity
of mammals.

``` r
# Twist just mammal prey wide.
camera.data.mm <- camera.data %>% filter(binomial != 'Unidentified item') %>% 
  filter(class == 'Mammalia') %>% 
  group_by(zone, binomial) %>% 
  mutate(count=n()) %>% 
  select(zone, binomial, count) %>% 
  distinct() %>% 
  pivot_wider(names_from=binomial, values_from=count, values_fill=list(count = 0))

# Look at it.
camera.data.mm
```

    ## # A tibble: 2 x 9
    ## # Groups:   zone [2]
    ##   zone  `Rattus sp` `Tamiasciurus d~ `Neotamias sp` `Glaucomys sabr~ `Myotis sp`
    ##   <chr>       <int>            <int>          <int>            <int>       <int>
    ## 1 tz             10              212             24                8           1
    ## 2 cs              1               84              0                4           0
    ## # ... with 3 more variables: `Lepus americanus` <int>, `Tamiasciurus
    ## #   hudsonicus` <int>, `Neotoma cinerea` <int>

Well, just to look at it sure seems like tz birds are eating more kinds
of mammals. Let’s see what the statistics say.

``` r
# Calculate diversity.
plyr::ddply(camera.data.mm, ~zone, function(x) {
  data.frame(diet.diversity=diversity(x[-1], index='simpson'))
})
```

    ##   zone diet.diversity
    ## 1   cs      0.1266667
    ## 2   tz      0.4527018

Oh, yeah. Is the inverse true? Do cs birds eat a wider variety of birds?

``` r
# Twist just mammal prey wide.
camera.data.av <- camera.data %>% filter(binomial != 'Unidentified item') %>% 
  filter(class == 'Aves') %>% 
  group_by(zone, binomial) %>% 
  mutate(count=n()) %>% 
  select(zone, binomial, count) %>% 
  distinct() %>% 
  pivot_wider(names_from=binomial, values_from=count, values_fill=list(count = 0))

# Calculate diversity.
plyr::ddply(camera.data.av, ~zone, function(x) {
  data.frame(diet.diversity=diversity(x[-1], index='simpson'))
})
```

    ##   zone diet.diversity
    ## 1   cs      0.6750000
    ## 2   tz      0.7835539

Nope, tz still higher. And at any rate, these numbers are so close it
doesn’t seem to matter.

Let’s move on to overlap, starting with overlap between individual
nests.

``` r
# Calculate overlap between nests.
camera.data.site %>% column_to_rownames(var='site') %>% 
  vegdist(., method='morisita')
```

    ##             MTF         MTC         TCR         TMC         UTZ
    ## MTC 0.046419986                                                
    ## TCR 0.043463016 0.007635603                                    
    ## TMC 0.139968517 0.113006700 0.118088319                        
    ## UTZ 0.301571514 0.273124094 0.302822798 0.351793365            
    ## RLK 0.073339067 0.054150213 0.019010357 0.189384801 0.346809772

Yes, that looks familiar. And now zone.

``` r
camera.data.zone %>% column_to_rownames(var='zone') %>% 
  vegdist(., method='morisita')
```

    ##            tz
    ## cs 0.03831484

Oh, quite low. How interesting. I suppose the thing I really want to
know is whether the overlap *within* zones is higher than the overlap
*between* zones. Which could be tricky to calculate. I guess really I
ought to do more clustering analysis or something, but I’ll try this
instead…

First coastal:

``` r
# Overlap within coastal zone.
camera.data %>% filter(binomial != 'Unidentified item') %>% 
  filter(zone == 'cs') %>% 
  group_by(site, binomial) %>% 
  mutate(count=n()) %>% 
  select(site, binomial, count) %>% 
  distinct() %>% 
  pivot_wider(names_from=binomial, values_from=count, values_fill=list(count = 0)) %>% 
  column_to_rownames(var='site') %>% 
  vegdist(., method='morisita')
```

    ##            MTC
    ## RLK 0.05415021

Then transition:

``` r
# Overlap within transition zone.
camera.data %>% filter(binomial != 'Unidentified item') %>% 
  filter(zone == 'tz') %>% 
  group_by(site, binomial) %>% 
  mutate(count=n()) %>% 
  select(site, binomial, count) %>% 
  distinct() %>% 
  pivot_wider(names_from=binomial, values_from=count, values_fill=list(count = 0)) %>% 
  column_to_rownames(var='site') %>% 
  vegdist(., method='morisita')
```

    ##            MTF        TCR        TMC
    ## TCR 0.04346302                      
    ## TMC 0.13996852 0.11808832           
    ## UTZ 0.30157151 0.30282280 0.35179337

But how to collapse this to a single number? Maybe calculate mean
overlap??

``` r
camera.data %>% filter(binomial != 'Unidentified item') %>% 
  filter(zone == 'tz') %>% 
  group_by(site, binomial) %>% 
  mutate(count=n()) %>% 
  select(site, binomial, count) %>% 
  distinct() %>% 
  pivot_wider(names_from=binomial, values_from=count, values_fill=list(count = 0)) %>% 
  column_to_rownames(var='site') %>% 
  vegdist(., method='morisita') %>% 
  mean()
```

    ## [1] 0.2096179

So overlap within the coastal zone is **0.05** and overlap within the
transition zone is **0.21** and overlap between the zones is **0.03.**
Which does make nests within the zones slightly (very slightly) more
similar to each other than to nests in another zone. This happens to be
a very different results than I got earlier from a cluster dendrogram so
who knows if this actually means anything.

Finally, lets do some of the summary things I’ve been doing this whole
time for each zone individually.

Start with coastal zone.

``` r
# How many different species identified from the coastal zone?
camera.data %>% filter(binomial != 'Unidentified item' & zone == 'cs') %>% 
  distinct(binomial, common) %>% summarize(n())
```

    ## # A tibble: 1 x 1
    ##   `n()`
    ##   <int>
    ## 1     8

``` r
# And which ones?
camera.data %>% filter(binomial != 'Unidentified item' & zone == 'cs') %>% 
  distinct(binomial, common)
```

    ## # A tibble: 8 x 2
    ##   binomial               common              
    ##   <chr>                  <chr>               
    ## 1 Dendragapus fulignosus sooty grouse        
    ## 2 Catharus ustulatus     swainson's thrush   
    ## 3 Ixoreus naevius        varied thrush       
    ## 4 Turdus migratorius     american robin      
    ## 5 Rattus sp              rat                 
    ## 6 Tamiasciurus douglasii douglas squirrel    
    ## 7 Glaucomys sabrinus     flying squirrel     
    ## 8 Neotoma cinerea        bushy-tailed woodrat

``` r
# Proportions mammal/avian/unknown?
camera.data %>% filter(zone == 'cs') %>% 
  mutate(total.mass=sum(mass), total.count=n()) %>% 
  group_by(class) %>% 
  mutate(mass.cl=sum(mass), count.cl=n(),
         prop.mass=mass.cl/total.mass, prop.count=count.cl/total.count,
         data='camera') %>% 
  select(class, prop.mass, prop.count) %>% distinct()
```

    ## # A tibble: 3 x 3
    ## # Groups:   class [3]
    ##   class    prop.mass prop.count
    ##   <chr>        <dbl>      <dbl>
    ## 1 Aves        0.163       0.224
    ## 2 Mammalia    0.791       0.636
    ## 3 Unknown     0.0460      0.140

``` r
# And proportion squirrel.
camera.data %>% filter(zone == 'cs') %>% 
  mutate(total.mass=sum(mass), total.count=n()) %>% 
  filter(genus == 'Tamiasciurus') %>% 
  mutate(mass.sq=sum(mass), count.sq=n(), 
         prop.sq.mass=mass.sq/total.mass, prop.sq.count=count.sq/total.count,
         data='camera') %>% 
  select(prop.sq.mass, prop.sq.count) %>% distinct()
```

    ## # A tibble: 1 x 2
    ##   prop.sq.mass prop.sq.count
    ##          <dbl>         <dbl>
    ## 1        0.733         0.587

And do it all again for transition zone.

``` r
# How many different species identified from the transition zone?
camera.data %>% filter(binomial != 'Unidentified item' & zone == 'tz') %>% 
  distinct(binomial, common) %>% summarize(n())
```

    ## # A tibble: 1 x 1
    ##   `n()`
    ##   <int>
    ## 1    15

``` r
# And which ones?
camera.data %>% filter(binomial != 'Unidentified item' & zone == 'tz') %>% 
  distinct(binomial, common)
```

    ## # A tibble: 15 x 2
    ##    binomial                common            
    ##    <chr>                   <chr>             
    ##  1 Cyanocitta stelleri     steller's jay     
    ##  2 Perisoreus canadensis   gray jay          
    ##  3 Dendragapus fulignosus  sooty grouse      
    ##  4 Ixoreus naevius         varied thrush     
    ##  5 Rattus sp               rat               
    ##  6 Tamiasciurus douglasii  douglas squirrel  
    ##  7 Neotamias sp            chipmunk          
    ##  8 Patagoienas fasciata    band-tailed pigeon
    ##  9 Catharus ustulatus      swainson's thrush 
    ## 10 Myotis sp               bat               
    ## 11 Bonasa umbellus         ruffed grouse     
    ## 12 Turdus migratorius      american robin    
    ## 13 Lepus americanus        snowshoe hare     
    ## 14 Tamiasciurus hudsonicus red squirrel      
    ## 15 Glaucomys sabrinus      flying squirrel

``` r
# Proportions mammal/avian/unknown?
camera.data %>% filter(zone == 'tz') %>% 
  mutate(total.mass=sum(mass), total.count=n()) %>% 
  group_by(class) %>% 
  mutate(mass.cl=sum(mass), count.cl=n(),
         prop.mass=mass.cl/total.mass, prop.count=count.cl/total.count,
         data='camera') %>% 
  select(class, prop.mass, prop.count) %>% distinct()
```

    ## # A tibble: 3 x 3
    ## # Groups:   class [3]
    ##   class    prop.mass prop.count
    ##   <chr>        <dbl>      <dbl>
    ## 1 Aves        0.125      0.182 
    ## 2 Mammalia    0.854      0.730 
    ## 3 Unknown     0.0205     0.0876

``` r
# And proportion squirrel.
camera.data %>% filter(zone == 'tz') %>% 
  mutate(total.mass=sum(mass), total.count=n()) %>% 
  filter(genus == 'Tamiasciurus') %>% 
  mutate(mass.sq=sum(mass), count.sq=n(), 
         prop.sq.mass=mass.sq/total.mass, prop.sq.count=count.sq/total.count,
         data='camera') %>% 
  select(prop.sq.mass, prop.sq.count) %>% distinct()
```

    ## # A tibble: 1 x 2
    ##   prop.sq.mass prop.sq.count
    ##          <dbl>         <dbl>
    ## 1        0.708         0.603

Again, that same pattern–more mammalian biomass in the transition zone
but proportion squirrel is about the same.
