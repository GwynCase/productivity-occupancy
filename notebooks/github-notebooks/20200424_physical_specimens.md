Physical specimens
================

Although all of my samples are stuck in the lab freezer, I have a few identified things I can use for making some preliminary code.

Physical remains were collected using two different methodologies. Opportunistic collections were gathered by inventory technicians during regular goshawk surveys. Prey remains and regurgiated pellets were collected from beneath pluck posts, perches, and active and inactive nests when discovered by surveyors. Items from each pluck post, perch, or nest were pooled into a single sample. Systematic collections were gathered during thorough searches of the ground within a 50-m radius of an active nest. All physical remains from a single nest visit we pooled into a single sample.

We reconstructed physical remains following a modification of Lewis et al (2006). Within each sample, prey remains were identified to the lower possible taxonomic category and the minimum number of individuals counted (ie 1.5 vole mandibles = 2 voles \[family: Cricetidae\]). Intact or broken but reassembled pellets were analyzed individually, while fragmented pellets were combined within each sample. Pellets were dissected and feathers, fur, and hard parts (bones, teeth, claws) were identified to the lowest taxonomic level. We counted the minimum number of individuals represented within the pellet or pellet collection (ie, Douglas squirrel fur and 3 squirrel claws = 1 *Tamiascuirus douglasii*). Items were additionally categorized to size and assigned mass as per camera data.

``` r
# Load up some libraries
library('tidyverse')
library('lubridate')
library('ggplot2')

# Read in the data.
df <- read.csv('../data/raw/20200424_specimens.csv', stringsAsFactors=FALSE)

# How many sites & samples?
df %>% group_by(site) %>% 
  summarize(n())
```

    ## # A tibble: 4 x 2
    ##   site          `n()`
    ##   <chr>         <int>
    ## 1 Comsock           1
    ## 2 Douglas Creek     4
    ## 3 Middle Point      1
    ## 4 Potlatch         22

Oh well. I don't want to deal with biomass at this stage, but I can look at counts.

``` r
df %>% mutate(total=n()) %>% 
  group_by(class) %>% 
  mutate(t.class=n(), prop=t.class/total) %>% 
  dplyr::select(class, prop) %>% 
  distinct()
```

    ## # A tibble: 2 x 2
    ## # Groups:   class [2]
    ##   class     prop
    ##   <chr>    <dbl>
    ## 1 Mammalia 0.357
    ## 2 Aves     0.643

A lot more birds at this points, probably due to all of those "ducks". How many were identified?

``` r
# What percent was IDed to family and genus/species?
df %>% mutate(total=n()) %>% 
  filter(family != 'U') %>% 
  mutate(t.family=n(), p.family=t.family/total*100) %>% 
  filter(genus != 'U') %>% 
  mutate(t.genus=n(), p.genus=t.genus/total*100) %>% 
  dplyr::select(p.family, p.genus) %>% 
  distinct()
```

    ##   p.family p.genus
    ## 1 42.85714      25

The lower proportion of items identified to genus/species is due to more small mammals, which are really tricky to id, and also, oddly, large birds. These were generally easier to ID on camera but their skeletons are trickier. Probably ducks, but I think I need to look at specimens to be sure.

What are the things that have been IDed?

``` r
df %>% group_by(class, family, genus, species) %>% 
  summarize(n())
```

    ## # A tibble: 11 x 5
    ## # Groups:   class, family, genus [11]
    ##    class    family        genus        species   `n()`
    ##    <chr>    <chr>         <chr>        <chr>     <int>
    ##  1 Aves     Columbidae    Patagioenas  fasciata      1
    ##  2 Aves     Parulidae     U            U             1
    ##  3 Aves     Passerellidae Pipilo       maculatus     1
    ##  4 Aves     Passerellidae U            U             1
    ##  5 Aves     Picidae       Colaptes     auratus       3
    ##  6 Aves     U             U            U            11
    ##  7 Mammalia Cricetidae    U            sp            2
    ##  8 Mammalia Rodentidae    U            U             1
    ##  9 Mammalia Sciuridae     Tamiasciurus douglasii     1
    ## 10 Mammalia Scuiridae     Tamiasciurus douglasii     1
    ## 11 Mammalia U             U            U             5

There's a typo there with the voles I need to get to at some point.
