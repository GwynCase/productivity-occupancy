The sum of chi squares
================

I ran into a problem when I tried to break down my chi-sqared tz-vs-cs
results. This me trying to run around the problem.

``` r
# Import conflict settings.
source('../src/conflicted.R')

# Load some libraries.
library(tidyverse)
library(lubridate)
library(broom)
library(chisq.posthoc.test)
library(sf)
library(rmapshaper)
```

Start with some preliminary stuff.

``` r
# Read in nest data.
nests <- read_csv('../data/processed/sc_nests.csv')

# Calculate a centroid for each site.
centroids <- nests %>% group_by(site) %>% 
  mutate(xcoord=mean(xcoord), ycoord=mean(ycoord)) %>% 
  distinct(site, name, xcoord, ycoord) %>% ungroup()

# Make it a spatial object for later.
centroids.sf <- centroids %>% 
  st_as_sf(coords=c('xcoord', 'ycoord')) %>% 
  st_set_crs('+proj=utm +zone=10 +datum=WGS84 +units=m +no_defs') %>% 
  st_as_sf()

# Bring in study area shapefile.
tz.region <- read_sf(dsn='../data/processed/new_transition_zone.shp', 
                     layer='new_transition_zone') %>% 
  ms_simplify()

# Add zone information to centroids.
centroids.sf <- centroids.sf %>% mutate(
  zone=as.integer(st_intersects(geometry, tz.region)),
  zone=case_when(
    is.na(zone) ~ 'cs',
    TRUE ~ 'tz'
  )
)

# Bring in specimen data.
remains.data <- read_csv('../data/processed/specimens_20210318.csv', guess_max=7000) %>% 
  ## filter only records with at least size assigned...
  filter(size != 'U')

# Bring in camera data.
photos <- read_csv('../data/interim/cameras_20210315.csv', guess_max=7000)
camera.data <- photos %>% 
  ## filter only records with at least size assigned...
  filter(size != 'U')

# Bring in a list of site abbreviations and site names.
nest.list <- read_csv('../data/processed/site_abbreviations.csv')

# Standardize the specimen data.
remains.data <- left_join(remains.data, nest.list, by=c('name', 'site')) %>% 
  select(site, date, class, order, family, genus, species, common, size, age, source)

# Standardize the camera data.
camera.data <- camera.data %>% 
  mutate(date=date(datetime), source='C') %>% 
  select(site, date, class, order, family, genus, species, common, size, age, source)

# Join them together.
diet.data <- bind_rows(remains.data, camera.data)

# Add a unique site/year identifier.
diet.data <- diet.data %>% 
  mutate(year=year(date), nest=paste(site, year, sep=''))

# Do some cleanup.
diet.data <- diet.data %>% 
  mutate(size=case_when(
    size %in% c('S', 'Small') ~ 'small',
    size %in% c('M', 'Medium') ~ 'medium',
    size %in% c('L', 'Large') ~ 'large'
  ))

# Bring in a list of all known prey.
prey.list <- read_csv('../data/interim/prey_attributes.csv')

# Join the biomass data to the list of diet items.
diet.items <- prey.list %>% select(genus, species, binomial, common, category, mass) %>% 
  right_join(diet.data, by=c('genus', 'species', 'common'))

# For unidentified items, classify them by size and class.
diet.items <- diet.items %>% mutate(category=case_when(
  is.na(category) & class == 'Mammalia' & size == 'small' ~ 'small mammal',
  is.na(category) & class == 'Mammalia' & size == 'medium' ~ 'medium mammal',
  is.na(category) & class == 'Mammalia' & size == 'large' ~ 'large mammal',
  is.na(category) & class == 'Aves' & size == 'small' ~ 'small bird',
  is.na(category) & class == 'Aves' & size == 'medium' ~ 'medium bird',
  is.na(category) & class == 'Aves' & size == 'large' ~ 'large bird',
  is.na(category) & class == 'Unknown' ~ paste(size, 'item'),
  TRUE ~ category))

# For unidentified items, fill in the binomial column.
diet.items <- diet.items %>% replace_na(list(binomial = 'Unidentified item'))

# Calculate average masses for unidentified items, based of known species.
mean.mass <- diet.items %>% drop_na(mass) %>% 
  distinct(binomial, mass, category) %>% 
  group_by(category) %>% 
  ## averaging the mass for each size & class category...
  summarize(average=mean(mass)) %>% 
  pivot_wider(names_from=category, values_from=average) %>% 
  ## calculating the average mass for complete unknowns...
  mutate(`large item` = mean(c(`large bird`, `large mammal`)),
         `medium item` = mean(c(`medium bird`, `medium mammal`)),
         `small item` = mean(c(`small bird`, `small mammal`))) %>% 
  ## and reassembling it in a tidy format.
  pivot_longer(everything(), names_to='category', values_to='average')

# Join average mass to diet items...
diet.items <- left_join(diet.items, mean.mass, by='category') %>% 
  ## and fill in missing mass with average values
  mutate(mass=coalesce(mass, average)) %>% 
  ## then drop no longer needed average column and rearrange.
  select(site, year, nest, class, order, family, genus, species, binomial, common, 
         category, size, mass, age, source)

# Change mass for juvenile items.
diet.items <- diet.items %>% mutate(mass=case_when(
  age == 'J' ~ 0.5*mass,
  TRUE ~ mass
))

# Add grouping categories.
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

# Calculate counts per group per zone per source.
zone.counts <- left_join(diet.items, centroids.sf, by=c('site')) %>% 
  group_by(source) %>% nest() %>% 
  mutate(count=map(data, function(data) {
    data %>% group_by(zone, group) %>% 
      mutate(count=n()) %>% 
      select(zone, group, count) %>% 
      distinct()
  })) %>% 
  unnest(count) %>% select(!data)

zone.counts
```

    ## # A tibble: 38 x 4
    ## # Groups:   source [3]
    ##    source zone  group  count
    ##    <chr>  <chr> <chr>  <int>
    ##  1 R      cs    bird      23
    ##  2 R      tz    corvid     6
    ##  3 R      cs    corvid     3
    ##  4 R      tz    bird      18
    ##  5 R      tz    grouse    22
    ##  6 R      cs    grouse    19
    ##  7 R      tz    thrush     6
    ##  8 R      cs    thrush     2
    ##  9 R      tz    hare      11
    ## 10 R      cs    hare       1
    ## # ... with 28 more rows

So that’s all of the prey items, grouped into categories, divided by
zone and by data source. Fine. Now run some chi-squared tests.

``` r
# Run a chi-square test for each data source.
zone.chi <-zone.counts %>% pivot_wider(names_from=source, values_from=count) %>% 
  mutate(RP=R + P) %>% 
  pivot_longer(cols=c('R', 'C', 'P', 'RP'), names_to='source', values_to='count') %>% 
  filter(!is.na(count)) %>% 
  group_by(source) %>% nest() %>% 
  mutate(ch=map(data, function(data) {
    data %>% pivot_wider(names_from=group, values_from=count, values_fill=0) %>% 
      column_to_rownames(var='zone') %>% 
      chisq.test(., correct=FALSE, simulate.p.value=TRUE)
  }),
        gl=map(ch, glance))

zone.chi
```

    ## # A tibble: 4 x 4
    ## # Groups:   source [4]
    ##   source data              ch      gl              
    ##   <chr>  <list>            <list>  <list>          
    ## 1 R      <tibble [13 x 3]> <htest> <tibble [1 x 4]>
    ## 2 C      <tibble [15 x 3]> <htest> <tibble [1 x 4]>
    ## 3 P      <tibble [10 x 3]> <htest> <tibble [1 x 4]>
    ## 4 RP     <tibble [9 x 3]>  <htest> <tibble [1 x 4]>

First one is for remains only, I’m not interested in that. Next is
camera data.

``` r
# Output for camera data chi-squared test.
filter(zone.chi, source == 'C') %>% unnest(gl)
```

    ## # A tibble: 1 x 7
    ## # Groups:   source [1]
    ##   source data      ch     statistic p.value parameter method                    
    ##   <chr>  <list>    <list>     <dbl>   <dbl> <lgl>     <chr>                     
    ## 1 C      <tibble ~ <htes~      23.8 0.00250 NA        "Pearson's Chi-squared te~

Then pellets only.

``` r
# Output for chi-squared test run with pellet data only.
filter(zone.chi, source == 'P') %>% unnest(gl)
```

    ## # A tibble: 1 x 7
    ## # Groups:   source [1]
    ##   source data      ch     statistic p.value parameter method                    
    ##   <chr>  <list>    <list>     <dbl>   <dbl> <lgl>     <chr>                     
    ## 1 P      <tibble ~ <htes~      7.53   0.102 NA        "Pearson's Chi-squared te~

And finally for the pooled pellets-and-remains.

``` r
# Output for chi-squared test of pooled pellets-and-remains.
filter(zone.chi, source == 'RP') %>% unnest(gl)
```

    ## # A tibble: 1 x 7
    ## # Groups:   source [1]
    ##   source data      ch     statistic p.value parameter method                    
    ##   <chr>  <list>    <list>     <dbl>   <dbl> <lgl>     <chr>                     
    ## 1 RP     <tibble ~ <htes~      41.8 5.00e-4 NA        "Pearson's Chi-squared te~

So camera data is significantly different between zones, pellet data is
not significantly different between zones, and pooled
pellets-and-remains data is very significantly different between zones.
Which is exactly what I expected to find.

Of course, this only tells me that they *are* different, it doesn’t tell
me *how* they’re different.

I can pull out the standardized residuals like this:

``` r
# Standardized residuals for camera data.
zone.chi[[3]][[2]]$stdres
```

    ##         bird     corvid     grouse     thrush      hare    mammal  squirrel
    ## cs -1.894393 -0.2583241 -0.2809134  0.9974016 -1.170969 -1.828344 -1.424807
    ## tz  1.894393  0.2583241  0.2809134 -0.9974016  1.170969  1.828344  1.424807
    ##      unknown
    ## cs  4.262165
    ## tz -4.262165

The rule of thumb is that standardized residuals greater than |2| are
significant. So by that standard, only unknown items are significantly
different between zones. But this is just a rule of thumb, an actual
test of significance would be nice. Which I can do with this handy test.

``` r
# Run post-hoc analysis with bonferroni adjustment for camera data.
post.hoc.camera <- zone.chi[[2]][[2]] %>% 
  pivot_wider(names_from=zone, values_from=count, values_fill=0) %>% 
  column_to_rownames(var='group') %>% 
  chisq.posthoc.test(., method='bonferroni', simulate.p.value=TRUE, correct=FALSE)

post.hoc.camera
```

    ##    Dimension     Value         cs         tz
    ## 1       bird Residuals -1.8943933  1.8943933
    ## 2       bird  p values  0.9307650  0.9307650
    ## 3     corvid Residuals -0.2583241  0.2583241
    ## 4     corvid  p values  1.0000000  1.0000000
    ## 5     grouse Residuals -0.2809134  0.2809134
    ## 6     grouse  p values  1.0000000  1.0000000
    ## 7     thrush Residuals  0.9974016 -0.9974016
    ## 8     thrush  p values  1.0000000  1.0000000
    ## 9       hare Residuals -1.1709686  1.1709686
    ## 10      hare  p values  1.0000000  1.0000000
    ## 11    mammal Residuals -1.8283438  1.8283438
    ## 12    mammal  p values  1.0000000  1.0000000
    ## 13  squirrel Residuals -1.4248074  1.4248074
    ## 14  squirrel  p values  1.0000000  1.0000000
    ## 15   unknown Residuals  4.2621654 -4.2621654
    ## 16   unknown  p values  0.0003240  0.0003240

That’s a lot to deal with. Let’s start by making sure the residuals are
the same via both methods.

``` r
# Pull out and reorganize residuals for camera data for comparison.
filter(post.hoc.camera, Value == 'Residuals') %>% select(!Value) %>% 
  pivot_longer(-Dimension, names_to='zone', values_to='resids') %>% 
  pivot_wider(names_from=Dimension, values_from=resids)
```

    ## # A tibble: 2 x 9
    ##   zone   bird corvid grouse thrush  hare mammal squirrel unknown
    ##   <chr> <dbl>  <dbl>  <dbl>  <dbl> <dbl>  <dbl>    <dbl>   <dbl>
    ## 1 cs    -1.89 -0.258 -0.281  0.997 -1.17  -1.83    -1.42    4.26
    ## 2 tz     1.89  0.258  0.281 -0.997  1.17   1.83     1.42   -4.26

Yes, exactly the same. So the test appears to be running the chi-squared
part just fine. Now let’s look at p-values.

``` r
# Pull out and reorganize p-values for camera data for comparison.
filter(post.hoc.camera, Value == 'p values') %>% select(!Value) %>% 
  pivot_longer(-Dimension, names_to='zone', values_to='p.values') %>% 
  pivot_wider(names_from=Dimension, values_from=p.values)
```

    ## # A tibble: 2 x 9
    ##   zone   bird corvid grouse thrush  hare mammal squirrel  unknown
    ##   <chr> <dbl>  <dbl>  <dbl>  <dbl> <dbl>  <dbl>    <dbl>    <dbl>
    ## 1 cs    0.931      1      1      1     1      1        1 0.000324
    ## 2 tz    0.931      1      1      1     1      1        1 0.000324

So far so good. This looks exactly like what I expected to see.

We can run this exact same set of analyses for pellet-only data, but
nothing should turn up.

``` r
# Run post-hoc analysis with bonferroni adjustment for pellet data.
post.hoc.pellets <- zone.chi[[2]][[3]] %>% 
  pivot_wider(names_from=zone, values_from=count, values_fill=0) %>% 
  column_to_rownames(var='group') %>% 
  chisq.posthoc.test(., method='bonferroni', simulate.p.value=TRUE, correct=FALSE)

# Pull out and reorganize residuals for camera data for comparison.
filter(post.hoc.pellets, Value == 'Residuals') %>% select(!Value) %>% 
  pivot_longer(-Dimension, names_to='zone', values_to='resids') %>% 
  pivot_wider(names_from=Dimension, values_from=resids)
```

    ## # A tibble: 2 x 6
    ##   zone   bird corvid thrush mammal squirrel
    ##   <chr> <dbl>  <dbl>  <dbl>  <dbl>    <dbl>
    ## 1 cs     2.49  -1.06 -0.623  -1.47   -0.740
    ## 2 tz    -2.49   1.06  0.623   1.47    0.740

``` r
# Pull out and reorganize p-values for camera data for comparison.
filter(post.hoc.pellets, Value == 'p values') %>% select(!Value) %>% 
  pivot_longer(-Dimension, names_to='zone', values_to='p.values') %>% 
  pivot_wider(names_from=Dimension, values_from=p.values)
```

    ## # A tibble: 2 x 6
    ##   zone   bird corvid thrush mammal squirrel
    ##   <chr> <dbl>  <dbl>  <dbl>  <dbl>    <dbl>
    ## 1 cs    0.128      1      1      1        1
    ## 2 tz    0.128      1      1      1        1

So rule-of-thumb says birds are just barely significantly different
between zones. But this does not show up in the p-values. But the
residuals are only *just* over 2, and that’s just a rule of thum so this
isn’t weird yet.

But it will get weird.

Finally, for the pooled pellets-and-remains.

``` r
# Run post-hoc analysis with bonferroni adjustment for pellet data.
post.hoc.pooled <- zone.chi[[2]][[4]] %>% 
  pivot_wider(names_from=zone, values_from=count, values_fill=0) %>% 
  column_to_rownames(var='group') %>% 
  chisq.posthoc.test(., method='bonferroni', simulate.p.value=TRUE, correct=FALSE)

# Pull out and reorganize residuals for camera data for comparison.
filter(post.hoc.pooled, Value == 'Residuals') %>% select(!Value) %>% 
  pivot_longer(-Dimension, names_to='zone', values_to='resids') %>% 
  pivot_wider(names_from=Dimension, values_from=resids)
```

    ## # A tibble: 2 x 6
    ##   zone   bird corvid thrush mammal squirrel
    ##   <chr> <dbl>  <dbl>  <dbl>  <dbl>    <dbl>
    ## 1 cs     5.63 -0.756 -0.935 -0.418    -5.57
    ## 2 tz    -5.63  0.756  0.935  0.418     5.57

``` r
# Pull out and reorganize p-values for camera data for comparison.
filter(post.hoc.pooled, Value == 'p values') %>% select(!Value) %>% 
  pivot_longer(-Dimension, names_to='zone', values_to='p.values') %>% 
  pivot_wider(names_from=Dimension, values_from=p.values)
```

    ## # A tibble: 2 x 6
    ##   zone   bird corvid thrush mammal squirrel
    ##   <chr> <dbl>  <dbl>  <dbl>  <dbl>    <dbl>
    ## 1 cs        0      1      1      1        0
    ## 2 tz        0      1      1      1        0

The chi-squared test for the pooled pellets-and-remains was very
significant, and the residuals do indeed show that birds and squirrels
are very, very different between the zones with this data set, with a
lot more squirrels than expected in the transition zone and a lot more
birds than expected in the coastal zone. Oh\! *And* that shows up in the
p-values.

That was not what I was getting yesterday, so obvs I was screwing
something up in my rush.

So, in summary, a chi-squared test of pellet data showed no difference
in diet composition between the two zones. However, a chi-squared test
of the pooled pellets-and-remains data and the camera did did show a
significant difference between the zones. The post-hoc comparison showed
that within the camera data, the difference was driven by the number of
unknown items, which was much higher among coastal zone nests. On the
other hand, the post-hoc comparison of the pooled pellets-and-remains
data showed the difference to be the result of far more other birds and
far fewer squirrels among coastal zone nests.
