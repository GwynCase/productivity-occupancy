Bloxton and Thraikill
================

Bloxton 2002 gives a brief account of goshawk diet in the Olympic
peninsula but provides his results as proportions of counts, rather than
proportions of biomass, which makes comparisons difficult. Thraikill et
al. 2000 does the same. Bloxton pools pellets, remains, and nest camera
data, while Thraikill used prey remains only.

``` r
# Import conflict settings.
source('../src/conflicted.R')

#Load some libraries.
library(tidyverse)
```

The first step is to reproduce the results from Bloxton and Thraikill.

``` r
blx <- tribble(
  ~class, ~family, ~category, ~common, ~prop.count,
  'Aves', 'Phasianidae', 'large bird', 'grouse', 31,
  'Aves', 'Columbidae', 'large bird', 'band-tailed pigeon', 22,
  'Aves', 'Corvidae', 'medium bird', "steller\'s jay", 14,
  'Aves', 'Picidae', 'medium bird', 'woodpecker', 4,
  'Aves', 'Turdidae', 'small bird', 'thrush', 4,
  'Aves', 'Unknown', 'small bird', 'unknown', 2,
  'Mammalia', 'Leporidae', 'large mammal', 'snowshoe hare', 10,
  'Mammalia', 'Sciuridae', 'medium mammal', 'douglas squirrel', 6,
  'Mammalia', 'Sciuridae', 'small mammal', 'flying squirrel', 4,
  'Mammalia', 'Rodentia', 'small mammal', 'unknown', 3
)

blx
```

    ## # A tibble: 10 x 5
    ##    class    family      category      common             prop.count
    ##    <chr>    <chr>       <chr>         <chr>                   <dbl>
    ##  1 Aves     Phasianidae large bird    grouse                     31
    ##  2 Aves     Columbidae  large bird    band-tailed pigeon         22
    ##  3 Aves     Corvidae    medium bird   steller's jay              14
    ##  4 Aves     Picidae     medium bird   woodpecker                  4
    ##  5 Aves     Turdidae    small bird    thrush                      4
    ##  6 Aves     Unknown     small bird    unknown                     2
    ##  7 Mammalia Leporidae   large mammal  snowshoe hare              10
    ##  8 Mammalia Sciuridae   medium mammal douglas squirrel            6
    ##  9 Mammalia Sciuridae   small mammal  flying squirrel             4
    ## 10 Mammalia Rodentia    small mammal  unknown                     3

I made some judgment calls here: Bloxton lists “other rodents” and
“other birds”, and I decided these were “small” items. Based on my own
data, there must be *some* small things present, and everything he
explicitly identifies is either medium or large, so I think any small
items are most likely to be captured in these categories. He also
combines woodpeckers and thrushes as 8% of the sample, and I split these
into 4% each.

``` r
thr <- tribble(
  ~class, ~family, ~category, ~common, ~prop.count,
  'Aves', 'Phasianidae', 'large bird', 'ruffed grouse', 45,
  'Aves', 'Corvidae', 'medium bird', "steller\'s jay", 13,
  'Aves', 'Turdidae', 'medium bird', 'american robin', 13,
  'Aves', 'Phasianidae', 'large bird', 'ring-necked pheasant', 8,
  'Aves', 'Odontophoridae', 'large bird', 'mountain quail', 5,
  'Mammalia', 'Sciuridae', 'medium mammal', 'douglas squirrel', 13,
  'Mammalia', 'Aplodontiidae', 'large mammal', 'mountain beaver', 3
)

thr
```

    ## # A tibble: 7 x 5
    ##   class    family         category      common               prop.count
    ##   <chr>    <chr>          <chr>         <chr>                     <dbl>
    ## 1 Aves     Phasianidae    large bird    ruffed grouse                45
    ## 2 Aves     Corvidae       medium bird   steller's jay                13
    ## 3 Aves     Turdidae       medium bird   american robin               13
    ## 4 Aves     Phasianidae    large bird    ring-necked pheasant          8
    ## 5 Aves     Odontophoridae large bird    mountain quail                5
    ## 6 Mammalia Sciuridae      medium mammal douglas squirrel             13
    ## 7 Mammalia Aplodontiidae  large mammal  mountain beaver               3

It’s a bit suspicious that Thraikill has no unidentified items.

Now add the mass information.

``` r
# Add author information to each data frame.
blx <- blx %>% mutate(author='bloxton')
thr <- thr %>% mutate(author='thraikill')

# Join the two frames together.
rv <- bind_rows(blx, thr)

# Import prey information.
prey.list <- read_csv('../data/interim/prey_attributes.csv')

# Create a table of average masses.
average.sizes <- prey.list %>% 
  group_by(category) %>% 
  ## averaging the mass for each size & class category...
  summarize(average=mean(mass)) %>% 
  pivot_wider(names_from=category, values_from=average) %>% 
  ## calculating the average mass for complete unknowns...
  mutate(`large item` = mean(`large bird`, `large mammal`),
         `medium item` = mean(`medium bird`, `medium mammal`),
         `small item` = mean(`small bird`, `small mammal`)) %>% 
  ## and reassembling it in a tidy format.
  pivot_longer(everything(), names_to='category', values_to='average')

# Add mass.
rv <- left_join(rv, prey.list) %>% mutate_if(is.character, replace_na, 'Unknown') %>% 
  left_join(average.sizes) %>%
  mutate(mass=coalesce(mass, average)) %>%
  select(author, class, family, order, genus, species, binomial, common, category, prop.count, mass)

# Calculate biomass.
rv <- rv %>% mutate(biomass=prop.count*mass)
```

Now I can bring in my own numbers for comparison, plus Watson et
al. 1998. My numbers are pooled pellets and remains, and so are
Watson’s. Watson gives numbers as both counts and biomass; here I’m
using their biomass numbers.

``` r
# Add in new data.
mn <- tribble(
  ~author, ~class, ~prop.mass,
  'case', 'Aves', 62.7,
  'case', 'Mammalia', 37.9,
  'watson', 'Aves', 52.6,
  'watson', 'Mammalia', 47.1
)

# Join with Bloxton & Thraikill.
rv %>% group_by(author) %>% 
  mutate(total.mass=sum(biomass)) %>% 
  group_by(author, class) %>% 
  mutate(class.mass=sum(biomass), prop.mass=class.mass/total.mass*100) %>%
  distinct(author, class, prop.mass) %>% 
  bind_rows(mn) %>% 
  pivot_wider(names_from=class, values_from=prop.mass) %>% 
  arrange(Aves)
```

    ## # A tibble: 4 x 3
    ## # Groups:   author [4]
    ##   author     Aves Mammalia
    ##   <chr>     <dbl>    <dbl>
    ## 1 watson     52.6     47.1
    ## 2 case       62.7     37.9
    ## 3 bloxton    69.4     30.6
    ## 4 thraikill  86.4     13.6

So this is showing that I have a problem: when using physical specimens,
BC does not look any different that the rest of the PNW. So any
difference I’m seeing between my hawks’ diet and diet elsewhere is
probably due to methodology rather than a real difference in diet. Which
throws my cute story right out the window.

I can check this with squirrels, too.

``` r
# Add in my and Watson's data.
sq <- tribble(
  ~author, ~genus, ~prop.mass,
  'case', 'Tamiasciurus', 14.1,
  'watson', 'Tamiasciurus', 9.6,
)

# Join with Bloxton & Thraikill.
rv %>% group_by(author) %>% 
  mutate(total.mass=sum(biomass)) %>% 
  group_by(author, genus) %>% 
  mutate(genus.mass=sum(biomass), prop.mass=genus.mass/total.mass*100) %>% 
  filter(genus == 'Tamiasciurus') %>% 
  distinct(author, genus, prop.mass) %>% 
  bind_rows(sq) %>% 
  arrange(prop.mass)
```

    ## # A tibble: 4 x 3
    ## # Groups:   author, genus [4]
    ##   author    genus        prop.mass
    ##   <chr>     <chr>            <dbl>
    ## 1 bloxton   Tamiasciurus      2.42
    ## 2 thraikill Tamiasciurus      6.33
    ## 3 watson    Tamiasciurus      9.6 
    ## 4 case      Tamiasciurus     14.1

Well, at least that pattern still holds: my sample has a truly absurd
number of squirrels.
