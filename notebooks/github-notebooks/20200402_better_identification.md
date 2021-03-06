Better prey identification
================

> We quantified the diet of breeding goshawks using digital trail cameras placed at 6 nests during 2019. Nest were selected for camera installation based on timing of discovery, ease of access, and the presence of suitable trees. Each camera was mounted in a tree adjacent to the nest tree, roughly five meters away and slightly above the nest itself. Cameras (Reconyx, UltraFire and HyperFire models) were programmed to take three photos when triggered by motion, and an additional single photo ever 30 minutes. Installation took place during the nestling period (early May) and cameras were left in place until after fledging (September).

> Chicks were aged using a pictoral guide (Boal 1994) from photos taken shortly after cameras were installed. Each nest was assigned a single hatch date based on median chick age. Due to the camera's limited field of view, fledge date was defined as the first day all chicks voluntarily left the nest. Productivity was defined as the number of chicks to successfully fledge.

> We identified each prey item delivered to the nest following Lewis et al. (2004). Items were identifed to species when possible, using a combination of study skins, field guides, and photographs. When identification to species was not possible, items were identified to the lowest possible taxonomic level. Items that could not be identifed even to family were assigned to a size category (small, medium, or large). As very few items could be successfully aged, we did not include prey age in our analysis.

> Goshawks are known to cache prey items for re-delivery to the nest at a later time. Due to the discrete nature of our data, we were not able to identify previously cached items. However, we feel there was a low risk of double-counting items because most intact prey remained visible in the nest until mostly or entirely consumed. Partial items could rarely be identified beyond class and so were not included in analyises of counts.

``` r
# Load up some libraries.
library('tidyverse')
library('lubridate')
library('ggplot2')
library('knitr')
library('kableExtra')

# Import the data.
df <- read.csv('../data/interim/camera_data.csv', stringsAsFactors=FALSE)

# Do the datetime thing.
df <- df %>% mutate(datetime=parse_date_time(datetime, 
                       orders=c('%Y-%m-%d %H:%M:%S', '%Y/%m/%d %H:%M:%S')))

# Looks like there's a typo I missed...
df <- df %>% replace_na(list(order='unknown'))

# And another...
df <- df %>% mutate(genus=case_when(
  species == 'fulignosus' ~ 'Dendragapus',
  TRUE ~ genus),
  family=case_when(
    species == 'fulignosus' ~ 'Phasianidae',
    TRUE ~ family
  )
)

# And another. This was a juvenile...
df <- df %>% mutate(size=case_when(
  family == 'Turdidae' & size == 'Unknown' ~ 'Small',
  TRUE ~ size))

# Fill in a bit of missing data.
df <- df %>% mutate(species=case_when(
  genus == 'Tamiasciurus' & species == 'unknown' ~ 'sp',
  genus == 'Myotis' ~ 'sp',
  TRUE ~ species
))

df <- df %>% mutate(common=case_when(
  genus == 'Myotis' ~ 'bat',
  genus == 'Tamiasciurus' & species == 'sp' ~ 'tree squirrel',
  TRUE ~ common
))

# Get all unique instances.
items <- df %>% filter(class != '') %>%
  dplyr::select(class, family, genus, species, common, size) %>% 
  distinct()

items
```

    ##       class           family        genus     species               common
    ## 1      Aves         Turdidae       Turdus migratorius       American robin
    ## 2   Unknown          Unknown      Unknown     unknown              unknown
    ## 3   Unknown          Unknown      Unknown     unknown              unknown
    ## 4   Unknown          Unknown      Unknown     unknown              unknown
    ## 5      Aves         Turdidae     Catharus   ustulatus    Swainson's thrush
    ## 6  Mammalia        Sciuridae Tamiasciurus   douglasii     Douglas squirrel
    ## 7      Aves          Unknown      Unknown     unknown              unknown
    ## 8      Aves      Phasianidae  Dendragapus  fulignosus         sooty grouse
    ## 9  Mammalia          Unknown      Unknown     unknown              unknown
    ## 10     Aves         Turdidae      Unknown     unknown              unknown
    ## 11 Mammalia        Sciuridae    Glaucomys    sabrinus      flying squirrel
    ## 12 Mammalia          Muridae       Rattus          sp                  rat
    ## 13     Aves          Unknown      Unknown     unknown              unknown
    ## 14     Aves         Turdidae      Ixoreus     naevius        varied thrush
    ## 15 Mammalia        Sciuridae    Neotamias          sp             chipmunk
    ## 16 Mammalia          Unknown      Unknown     unknown              unknown
    ## 17     Aves         Corvidae   Cyanocitta    stelleri        Steller's jay
    ## 18     Aves         Corvidae   Perisoreus  canadensis             gray jay
    ## 19     Aves         Turdidae      Unknown     unknown              unknown
    ## 20 Mammalia          Neotoma   Cricetidae     cinerea bushy-tailed woodrat
    ## 21     Aves       Columbidae  Patagoienas    fasciata   band-tailed pigeon
    ## 22     Aves      Phasianidae      Unknown     unknown              unknown
    ## 23 Mammalia Vespertilionidae       Myotis          sp                  bat
    ## 24 Mammalia          Unknown      Unknown     unknown              unknown
    ## 25     Aves          Picidae      Unknown     unknown              unknown
    ## 26 Mammalia          Unknown      Unknown     unknown              unknown
    ## 27     Aves          Unknown      Unknown     unknown              unknown
    ## 28 Mammalia        Sciuridae Tamiasciurus  hudsonicus         red squirrel
    ## 29     Aves      Phasianidae       Bonasa    umbellus        ruffed grouse
    ## 30 Mammalia        Sciuridae Tamiasciurus          sp        tree squirrel
    ## 31 Mammalia        Leporidae        Lepus  americanus        snowshoe hare
    ##       size
    ## 1   Medium
    ## 2    Small
    ## 3   Medium
    ## 4  Unknown
    ## 5    Small
    ## 6   Medium
    ## 7    Small
    ## 8    Large
    ## 9    Small
    ## 10   Small
    ## 11   Small
    ## 12  Medium
    ## 13  Medium
    ## 14  Medium
    ## 15  Medium
    ## 16   Large
    ## 17  Medium
    ## 18  Medium
    ## 19  Medium
    ## 20   Small
    ## 21  Medium
    ## 22   Small
    ## 23   Small
    ## 24  Medium
    ## 25  Medium
    ## 26 Unknown
    ## 27 Unknown
    ## 28  Medium
    ## 29   Large
    ## 30  Medium
    ## 31   Large

It's hard to figure out what to do with the items that are Us all the way across. These are primarily items that aren't even seen on camera--you can see the adult arrive and feed the chicks, but the birds' bodies completely block all view of the item.

Rogers et al. (2006) doesn't address this at all--apparently all of their items could at least be identified down to size. Lucky them. They do note that they "excluded all questionable prey items delivered to the nest," questionable items being those that were presumed to be cached. But they seem to have at least been able to identify which items were pieces of things that had been delivered previously.

Miller et al. (2014) seems to have done something fancy involving handling time, where an item requiring a long time to be consumed was given a large mass and a short time to be consumed a small mass. Which is alright for some, but I don't have continuous video so that doesn't work for me.

Lewis et al. (2004 & 2006) make no mention of this problem, either. Apparently it's just me. I don't see any option except to drop them... I can use them for delivery rate and count, maybe, but I simply can't use them in any calculations regarding biomass. I don't see why this should bias my data, both large and small items could easily be obscured in this way.

As for the ones that are U for size but have other information, I can double-check them.

``` r
# I double-checked the photos on these unknowns.
df <- df %>% mutate(size=case_when(
  class=='Aves' & size == 'Unknown' ~ 'Small',
  class=='Mammalia' & size == 'Unknown' ~ 'Medium',
  TRUE ~ size
))

# Get unique instances, discarding complete unknowns.
items <- df %>% filter(class != '') %>%
  filter(size != 'Unknown') %>%
  dplyr::select(class, family, genus, species, common, size) %>% 
  arrange(class, family, genus, species, size) %>%
  distinct()
```

That looks good. Now I need to fill in sizes and groups. My original plan was to group things by size, because it makes more sense that you would estimate how large something is on a camera than how much it weighs. But this actually produces some nonsensical categories. Like rats being "large" because of their long tails. So I guess weight it is! This doesn't really affect unidentified items because those were classed relatively (ie, squirrel-sized or vole-sized) rather than by lenth to begin with.

``` r
# Avian size groups.
items <- items %>% mutate(group=case_when(
    species == 'fasciata' ~ 'Large bird',
    species == 'umbellus' ~ 'Large bird',
    species == 'fulignosus' ~ 'Large bird',
    species == 'stelleri' ~ 'Medium bird',
    species == 'canadensis' ~ 'Medium bird',
    species == 'ustulatus' ~ 'Small bird',
    species == 'naevius' ~ 'Medium bird',
    species == 'migratorius' ~ 'Medium bird',
    class == 'Aves' & species == 'unknown' & size == 'Small' ~ 'Small bird',
    class == 'Aves' & species == 'unknown' & size == 'Medium' ~ 'Medium bird',
    class == 'Aves' & species == 'unknown' & size == 'Large' ~ 'Large bird',
    TRUE ~ 'group'))

# Group the unknown bird species by size.
items <- items %>% mutate(common=case_when(
  class == 'Aves' & species == 'unknown' & size == 'Small' ~ 'average small bird',
  class == 'Aves' & species == 'unknown' & size == 'Medium' ~ 'average medium bird',
   class == 'Aves' & species == 'unknown' & size == 'Large' ~ 'average large bird',
  TRUE ~ common))

# Fil in mass for known bird species.
items <- items %>% mutate(mass=case_when(
    species == 'fasciata' ~ 392,
    species == 'umbellus' ~ 600,
    species == 'fulignosus' ~ 600,
    species == 'stelleri' ~ 120,
    species == 'canadensis' ~ 67.5,
    species == 'ustulatus' ~ 34,
    species == 'naevius' ~ 82.5,
    species == 'migratorius' ~ 77,
    TRUE ~ 0))

# Assign each mammal to a size group.
items <- items %>% mutate(group=case_when(
    species == 'americanus' ~ 'Large mammal',
    genus == 'Rattus' ~ 'Medium mammal',
    species == 'cinerea' ~ 'Medium mammal',
    species == 'sabrinus' ~ 'Small mammal',
    genus == 'Neotamias' ~ 'Small mammal',
    genus == 'Tamiasciurus' ~ 'Medium mammal',
    genus == 'Myotis' ~ 'Small mammal',
    class == 'Mammalia' & species == 'unknown' & size == 'Small' ~ 'Small mammal',
    class == 'Mammalia' & species == 'unknown' & size == 'Medium' ~ 'Medium mammal',
    class == 'Mammalia' & species == 'unknown' & size == 'Large' ~ 'Large mammal',
    TRUE ~ group))

# Group the unknown mammal species by size.
items <- items %>% mutate(common=case_when(
  class == 'Mammalia' & species == 'unknown' & size == 'Small' ~ 'average small mammal',
  class == 'Mammalia' & species == 'unknown' & size == 'Medium' ~ 'average medium mammal',
   class == 'Mammalia' & species == 'unknown' & size == 'Large' ~ 'average large mammal',
  TRUE ~ common))

# Fil in mass for known bird species.
items <- items %>% mutate(mass=case_when(
    species == 'americanus' ~ 1340,
    genus == 'Rattus' ~ 269.8,
    species == 'cinerea' ~ 374.7,
    species == 'sabrinus' ~ 155.5,
    genus == 'Neotamias' ~ 66.4,
    species == 'hudsonicus' ~ 224.5,
    species == 'douglasii' ~ 203.5,
    genus == 'Tamiasciurus' & species == 'sp' ~ 214,
    genus == 'Myotis' ~ 5.8,
    TRUE ~ mass))

# A last few unknowns.
items <- items %>% mutate(group=case_when(
  class == 'Unknown' & size == 'Medium' ~ 'Medium item',
  class == 'Unknown' & size == 'Small' ~ 'Small item',
  TRUE ~ group))

items <- items %>% mutate(common=case_when(
  class == 'Unknown' & size == 'Medium' ~ 'average medium item',
  class == 'Unknown' & size == 'Small' ~ 'average small item',
  TRUE ~ common))

# Make it pretty.
items <- items %>% unite(name, 3:4, sep=' ', remove=FALSE) %>%
  mutate(name=case_when(
    name == 'Unknown unknown' ~ ' ',
    TRUE ~ name
  )) %>%
  arrange(class, group, genus) 
  

prey.table <- items %>% dplyr::select(common, name, mass) %>%
  distinct() %>%
  kable(col.names=c('Prey category', '', 'Mass (g)')) %>%
  kable_styling(full_width=TRUE) %>%
  pack_rows('Large birds (> 150 g)', 1, 3) %>%
  pack_rows('Medium birds (60-150 g)', 4, 8) %>%
  pack_rows('Small birds (< 40 g)', 9, 10) %>% 
  pack_rows('Large mammals (> 600 g)', 11, 12) %>%
  pack_rows('Medium mammals (200-600 g)', 13, 18) %>%
  pack_rows('Small mammals (< 200 g)', 19, 22) %>%
  pack_rows('Unidentified items', 23, 24)
```

Some notes on these mass numbers: the bird weights are total BS, they're just taken from the internet because I don't have accesss right now to the resources I need for real numbers (thanks, coronavirus!). The mammal weights are taken from Nagorsen's *An Identification Manual to the Small Mammals of British Columbia*, and are averages for males and females combined. The mass for *Tamiasciurus* sp. is an average of *T. hudsonicus* and *T. douglasii*. And the mass for *Myotis* sp. is an average for the 6 possible species given the location it was found (TCR). The mass for the chipmunk is also an average of the 2 possible species where it was recorded.

And this looks great so far, except that I don't have any averages. And some of these don't have enough numbers to make averages. But I suppose that since my bird numbers are BS anyway it doesn't matter if my averages are BS, too, since this is only preliminary.

Ok, so let's do some BS math.

``` r
# Calculate averages per group.
m.mass <- items %>% filter(mass != 0) %>%
  group_by(group) %>%
  summarise(mean=mean(mass))

m.mass
```

    ## # A tibble: 6 x 2
    ##   group           mean
    ##   <chr>          <dbl>
    ## 1 Large bird     531. 
    ## 2 Large mammal  1340  
    ## 3 Medium bird     86.8
    ## 4 Medium mammal  257. 
    ## 5 Small bird      34  
    ## 6 Small mammal    75.9

Easy so far. Now plug it in to the table.

``` r
items <- left_join(items, m.mass, by='group') %>%
  mutate(mass = ifelse(mass > 1, mass, mean)) %>%
  dplyr::select(-mean)
```

That looks great, but what about unknown small and unknown medium items? None of the other authors address this issue. Rogers et al. have separate lines for unknown mammal, unknown bird, *and* unknown prey items, so obviously they had things they couldn't identify as bird or mammal. But they don't address how they dealt with assigning a mass to those items. Lewis et al. don't even have that much.

I suppose the most logical thing is to just average the size classes for bird and mammals.

``` r
# Calculate average small item.
sm <- items %>% filter(group %in% c('Small mammal', 'Small bird')) %>%
  distinct(mass) %>%
  summarize(mean(mass))

# Calculate average medium item.
md <- items %>% filter(group %in% c('Medium mammal', 'Medium bird')) %>%
  distinct(mass) %>%
  summarize(mean(mass))

# Plug it in.
items <- items %>% mutate(mass=replace(mass, group=='Small item', sm), 
                                    mass=replace(mass, group=='Medium item', md))
```

Now let's put it all together:

``` r
items %>% dplyr::select(common, name, mass) %>%
  mutate(mass=as.numeric(mass)) %>% 
  distinct() %>% 
  kable(col.names=c('Prey category', '', 'Mass (g)'), digits=2) %>%
  kable_styling(full_width=TRUE) %>%
  pack_rows('Large birds (> 150 g)', 1, 3) %>%
  pack_rows('Medium birds (60-150 g)', 4, 8) %>%
  pack_rows('Small birds (< 40 g)', 9, 10) %>% 
  pack_rows('Large mammals (> 600 g)', 11, 12) %>%
  pack_rows('Medium mammals (200-600 g)', 13, 18) %>%
  pack_rows('Small mammals (< 200 g)', 19, 22) %>%
  pack_rows('Unidentified items', 23, 24)
```

<table class="table" style="margin-left: auto; margin-right: auto;">
<thead>
<tr>
<th style="text-align:left;">
Prey category
</th>
<th style="text-align:left;">
</th>
<th style="text-align:right;">
Mass (g)
</th>
</tr>
</thead>
<tbody>
<tr grouplength="3">
<td colspan="3" style="border-bottom: 1px solid;">
<strong>Large birds (&gt; 150 g)</strong>
</td>
</tr>
<tr>
<td style="text-align:left; padding-left: 2em;" indentlevel="1">
ruffed grouse
</td>
<td style="text-align:left;">
Bonasa umbellus
</td>
<td style="text-align:right;">
600.00
</td>
</tr>
<tr>
<td style="text-align:left; padding-left: 2em;" indentlevel="1">
sooty grouse
</td>
<td style="text-align:left;">
Dendragapus fulignosus
</td>
<td style="text-align:right;">
600.00
</td>
</tr>
<tr>
<td style="text-align:left; padding-left: 2em;" indentlevel="1">
band-tailed pigeon
</td>
<td style="text-align:left;">
Patagoienas fasciata
</td>
<td style="text-align:right;">
392.00
</td>
</tr>
<tr grouplength="5">
<td colspan="3" style="border-bottom: 1px solid;">
<strong>Medium birds (60-150 g)</strong>
</td>
</tr>
<tr>
<td style="text-align:left; padding-left: 2em;" indentlevel="1">
Steller's jay
</td>
<td style="text-align:left;">
Cyanocitta stelleri
</td>
<td style="text-align:right;">
120.00
</td>
</tr>
<tr>
<td style="text-align:left; padding-left: 2em;" indentlevel="1">
varied thrush
</td>
<td style="text-align:left;">
Ixoreus naevius
</td>
<td style="text-align:right;">
82.50
</td>
</tr>
<tr>
<td style="text-align:left; padding-left: 2em;" indentlevel="1">
gray jay
</td>
<td style="text-align:left;">
Perisoreus canadensis
</td>
<td style="text-align:right;">
67.50
</td>
</tr>
<tr>
<td style="text-align:left; padding-left: 2em;" indentlevel="1">
American robin
</td>
<td style="text-align:left;">
Turdus migratorius
</td>
<td style="text-align:right;">
77.00
</td>
</tr>
<tr>
<td style="text-align:left; padding-left: 2em;" indentlevel="1">
average medium bird
</td>
<td style="text-align:left;">
</td>
<td style="text-align:right;">
86.75
</td>
</tr>
<tr grouplength="2">
<td colspan="3" style="border-bottom: 1px solid;">
<strong>Small birds (&lt; 40 g)</strong>
</td>
</tr>
<tr>
<td style="text-align:left; padding-left: 2em;" indentlevel="1">
Swainson's thrush
</td>
<td style="text-align:left;">
Catharus ustulatus
</td>
<td style="text-align:right;">
34.00
</td>
</tr>
<tr>
<td style="text-align:left; padding-left: 2em;" indentlevel="1">
average small bird
</td>
<td style="text-align:left;">
</td>
<td style="text-align:right;">
34.00
</td>
</tr>
<tr grouplength="2">
<td colspan="3" style="border-bottom: 1px solid;">
<strong>Large mammals (&gt; 600 g)</strong>
</td>
</tr>
<tr>
<td style="text-align:left; padding-left: 2em;" indentlevel="1">
snowshoe hare
</td>
<td style="text-align:left;">
Lepus americanus
</td>
<td style="text-align:right;">
1340.00
</td>
</tr>
<tr>
<td style="text-align:left; padding-left: 2em;" indentlevel="1">
average large mammal
</td>
<td style="text-align:left;">
</td>
<td style="text-align:right;">
1340.00
</td>
</tr>
<tr grouplength="6">
<td colspan="3" style="border-bottom: 1px solid;">
<strong>Medium mammals (200-600 g)</strong>
</td>
</tr>
<tr>
<td style="text-align:left; padding-left: 2em;" indentlevel="1">
bushy-tailed woodrat
</td>
<td style="text-align:left;">
Cricetidae cinerea
</td>
<td style="text-align:right;">
374.70
</td>
</tr>
<tr>
<td style="text-align:left; padding-left: 2em;" indentlevel="1">
rat
</td>
<td style="text-align:left;">
Rattus sp
</td>
<td style="text-align:right;">
269.80
</td>
</tr>
<tr>
<td style="text-align:left; padding-left: 2em;" indentlevel="1">
Douglas squirrel
</td>
<td style="text-align:left;">
Tamiasciurus douglasii
</td>
<td style="text-align:right;">
203.50
</td>
</tr>
<tr>
<td style="text-align:left; padding-left: 2em;" indentlevel="1">
red squirrel
</td>
<td style="text-align:left;">
Tamiasciurus hudsonicus
</td>
<td style="text-align:right;">
224.50
</td>
</tr>
<tr>
<td style="text-align:left; padding-left: 2em;" indentlevel="1">
tree squirrel
</td>
<td style="text-align:left;">
Tamiasciurus sp
</td>
<td style="text-align:right;">
214.00
</td>
</tr>
<tr>
<td style="text-align:left; padding-left: 2em;" indentlevel="1">
average medium mammal
</td>
<td style="text-align:left;">
</td>
<td style="text-align:right;">
257.30
</td>
</tr>
<tr grouplength="4">
<td colspan="3" style="border-bottom: 1px solid;">
<strong>Small mammals (&lt; 200 g)</strong>
</td>
</tr>
<tr>
<td style="text-align:left; padding-left: 2em;" indentlevel="1">
flying squirrel
</td>
<td style="text-align:left;">
Glaucomys sabrinus
</td>
<td style="text-align:right;">
155.50
</td>
</tr>
<tr>
<td style="text-align:left; padding-left: 2em;" indentlevel="1">
bat
</td>
<td style="text-align:left;">
Myotis sp
</td>
<td style="text-align:right;">
5.80
</td>
</tr>
<tr>
<td style="text-align:left; padding-left: 2em;" indentlevel="1">
chipmunk
</td>
<td style="text-align:left;">
Neotamias sp
</td>
<td style="text-align:right;">
66.40
</td>
</tr>
<tr>
<td style="text-align:left; padding-left: 2em;" indentlevel="1">
average small mammal
</td>
<td style="text-align:left;">
</td>
<td style="text-align:right;">
75.90
</td>
</tr>
<tr grouplength="2">
<td colspan="3" style="border-bottom: 1px solid;">
<strong>Unidentified items</strong>
</td>
</tr>
<tr>
<td style="text-align:left; padding-left: 2em;" indentlevel="1">
average medium item
</td>
<td style="text-align:left;">
</td>
<td style="text-align:right;">
179.78
</td>
</tr>
<tr>
<td style="text-align:left; padding-left: 2em;" indentlevel="1">
average small item
</td>
<td style="text-align:left;">
</td>
<td style="text-align:right;">
67.52
</td>
</tr>
</tbody>
</table>
And since I put so much effort into tracking down those typoes, I should save them as a new, modified data set.

``` r
write.csv(df, '../data/interim/camera_corrected.csv', row.names=FALSE)
```
