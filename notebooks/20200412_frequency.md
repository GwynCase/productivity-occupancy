Prey frequency
================

I want to quantify the percent of prey items that were identified to genus, species, and class to each nest, by count and by biomass.

``` r
# Load up some libraries.
library('tidyverse')
library('lubridate')
library('questionr')
library('ggplot2')
library('knitr')
library('kableExtra')

# Import the data.
df <- read.csv('../data/interim/camera_corrected.csv', stringsAsFactors=FALSE)

# Do the datetime thing.
df <- df %>% mutate(datetime=parse_date_time(datetime, 
                       orders=c('%Y-%m-%d %H:%M:%S', '%Y/%m/%d %H:%M:%S')))
source('../src/prey_attributes.R')
```

Start by winnowing down to items IDed to certain levels.

``` r
to.genus <- items %>% filter(genus != 'Unknown')
to.family <- items %>% filter(family != 'Unknown')
to.class <- items %>% filter(class != 'Unknown')
```

Look at proportion of counts.

``` r
count.by.genus <- to.genus %>% mutate(mass=as.numeric(mass)) %>% 
  group_by(site) %>% 
  mutate(total=n()) %>% 
  group_by(site, genus, species) %>% 
  mutate(count=n()) %>% 
  distinct() %>% 
  ungroup() %>% 
  mutate(prop=count/total) %>% 
  dplyr::select(site, class, family, genus, species, count, total, prop) %>% 
  arrange(site, class)

count.by.genus %>% dplyr::select(-site, -total) %>% 
  kable(digits=2) %>% 
  kable_styling(full_width=TRUE) %>% 
  pack_rows('Mount Currie', 1, 7) %>% 
  pack_rows('Mount Ford', 8, 14) %>% 
  pack_rows('Ruby Lake', 15, 16) %>% 
  pack_rows('Turbid Creek', 17, 22) %>% 
  pack_rows('Twenty-Mile Creek', 23, 25) %>% 
  pack_rows('Utziletz', 26, 33)
```

<table class="table" style="margin-left: auto; margin-right: auto;">
<thead>
<tr>
<th style="text-align:left;">
class
</th>
<th style="text-align:left;">
family
</th>
<th style="text-align:left;">
genus
</th>
<th style="text-align:left;">
species
</th>
<th style="text-align:right;">
count
</th>
<th style="text-align:right;">
prop
</th>
</tr>
</thead>
<tbody>
<tr grouplength="7">
<td colspan="6" style="border-bottom: 1px solid;">
<strong>Mount Currie</strong>
</td>
</tr>
<tr>
<td style="text-align:left; padding-left: 2em;" indentlevel="1">
Aves
</td>
<td style="text-align:left;">
Phasianidae
</td>
<td style="text-align:left;">
Dendragapus
</td>
<td style="text-align:left;">
fulignosus
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
0.04
</td>
</tr>
<tr>
<td style="text-align:left; padding-left: 2em;" indentlevel="1">
Aves
</td>
<td style="text-align:left;">
Turdidae
</td>
<td style="text-align:left;">
Catharus
</td>
<td style="text-align:left;">
ustulatus
</td>
<td style="text-align:right;">
3
</td>
<td style="text-align:right;">
0.12
</td>
</tr>
<tr>
<td style="text-align:left; padding-left: 2em;" indentlevel="1">
Aves
</td>
<td style="text-align:left;">
Turdidae
</td>
<td style="text-align:left;">
Ixoreus
</td>
<td style="text-align:left;">
naevius
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
0.04
</td>
</tr>
<tr>
<td style="text-align:left; padding-left: 2em;" indentlevel="1">
Aves
</td>
<td style="text-align:left;">
Turdidae
</td>
<td style="text-align:left;">
Turdus
</td>
<td style="text-align:left;">
migratorius
</td>
<td style="text-align:right;">
2
</td>
<td style="text-align:right;">
0.08
</td>
</tr>
<tr>
<td style="text-align:left; padding-left: 2em;" indentlevel="1">
Mammalia
</td>
<td style="text-align:left;">
Muridae
</td>
<td style="text-align:left;">
Rattus
</td>
<td style="text-align:left;">
sp
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
0.04
</td>
</tr>
<tr>
<td style="text-align:left; padding-left: 2em;" indentlevel="1">
Mammalia
</td>
<td style="text-align:left;">
Sciuridae
</td>
<td style="text-align:left;">
Glaucomys
</td>
<td style="text-align:left;">
sabrinus
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
0.04
</td>
</tr>
<tr>
<td style="text-align:left; padding-left: 2em;" indentlevel="1">
Mammalia
</td>
<td style="text-align:left;">
Sciuridae
</td>
<td style="text-align:left;">
Tamiasciurus
</td>
<td style="text-align:left;">
douglasii
</td>
<td style="text-align:right;">
15
</td>
<td style="text-align:right;">
0.62
</td>
</tr>
<tr grouplength="7">
<td colspan="6" style="border-bottom: 1px solid;">
<strong>Mount Ford</strong>
</td>
</tr>
<tr>
<td style="text-align:left; padding-left: 2em;" indentlevel="1">
Aves
</td>
<td style="text-align:left;">
Corvidae
</td>
<td style="text-align:left;">
Cyanocitta
</td>
<td style="text-align:left;">
stelleri
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
0.03
</td>
</tr>
<tr>
<td style="text-align:left; padding-left: 2em;" indentlevel="1">
Aves
</td>
<td style="text-align:left;">
Corvidae
</td>
<td style="text-align:left;">
Perisoreus
</td>
<td style="text-align:left;">
canadensis
</td>
<td style="text-align:right;">
2
</td>
<td style="text-align:right;">
0.06
</td>
</tr>
<tr>
<td style="text-align:left; padding-left: 2em;" indentlevel="1">
Aves
</td>
<td style="text-align:left;">
Phasianidae
</td>
<td style="text-align:left;">
Dendragapus
</td>
<td style="text-align:left;">
fulignosus
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
0.03
</td>
</tr>
<tr>
<td style="text-align:left; padding-left: 2em;" indentlevel="1">
Aves
</td>
<td style="text-align:left;">
Turdidae
</td>
<td style="text-align:left;">
Ixoreus
</td>
<td style="text-align:left;">
naevius
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
0.03
</td>
</tr>
<tr>
<td style="text-align:left; padding-left: 2em;" indentlevel="1">
Mammalia
</td>
<td style="text-align:left;">
Muridae
</td>
<td style="text-align:left;">
Rattus
</td>
<td style="text-align:left;">
sp
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
0.03
</td>
</tr>
<tr>
<td style="text-align:left; padding-left: 2em;" indentlevel="1">
Mammalia
</td>
<td style="text-align:left;">
Sciuridae
</td>
<td style="text-align:left;">
Neotamias
</td>
<td style="text-align:left;">
sp
</td>
<td style="text-align:right;">
6
</td>
<td style="text-align:right;">
0.18
</td>
</tr>
<tr>
<td style="text-align:left; padding-left: 2em;" indentlevel="1">
Mammalia
</td>
<td style="text-align:left;">
Sciuridae
</td>
<td style="text-align:left;">
Tamiasciurus
</td>
<td style="text-align:left;">
douglasii
</td>
<td style="text-align:right;">
22
</td>
<td style="text-align:right;">
0.65
</td>
</tr>
<tr grouplength="2">
<td colspan="6" style="border-bottom: 1px solid;">
<strong>Ruby Lake</strong>
</td>
</tr>
<tr>
<td style="text-align:left; padding-left: 2em;" indentlevel="1">
Mammalia
</td>
<td style="text-align:left;">
Neotoma
</td>
<td style="text-align:left;">
Cricetidae
</td>
<td style="text-align:left;">
cinerea
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
0.14
</td>
</tr>
<tr>
<td style="text-align:left; padding-left: 2em;" indentlevel="1">
Mammalia
</td>
<td style="text-align:left;">
Sciuridae
</td>
<td style="text-align:left;">
Tamiasciurus
</td>
<td style="text-align:left;">
douglasii
</td>
<td style="text-align:right;">
6
</td>
<td style="text-align:right;">
0.86
</td>
</tr>
<tr grouplength="6">
<td colspan="6" style="border-bottom: 1px solid;">
<strong>Turbid Creek</strong>
</td>
</tr>
<tr>
<td style="text-align:left; padding-left: 2em;" indentlevel="1">
Aves
</td>
<td style="text-align:left;">
Columbidae
</td>
<td style="text-align:left;">
Patagoienas
</td>
<td style="text-align:left;">
fasciata
</td>
<td style="text-align:right;">
3
</td>
<td style="text-align:right;">
0.12
</td>
</tr>
<tr>
<td style="text-align:left; padding-left: 2em;" indentlevel="1">
Aves
</td>
<td style="text-align:left;">
Turdidae
</td>
<td style="text-align:left;">
Catharus
</td>
<td style="text-align:left;">
ustulatus
</td>
<td style="text-align:right;">
2
</td>
<td style="text-align:right;">
0.08
</td>
</tr>
<tr>
<td style="text-align:left; padding-left: 2em;" indentlevel="1">
Aves
</td>
<td style="text-align:left;">
Turdidae
</td>
<td style="text-align:left;">
Ixoreus
</td>
<td style="text-align:left;">
naevius
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
0.04
</td>
</tr>
<tr>
<td style="text-align:left; padding-left: 2em;" indentlevel="1">
Mammalia
</td>
<td style="text-align:left;">
Muridae
</td>
<td style="text-align:left;">
Rattus
</td>
<td style="text-align:left;">
sp
</td>
<td style="text-align:right;">
2
</td>
<td style="text-align:right;">
0.08
</td>
</tr>
<tr>
<td style="text-align:left; padding-left: 2em;" indentlevel="1">
Mammalia
</td>
<td style="text-align:left;">
Sciuridae
</td>
<td style="text-align:left;">
Tamiasciurus
</td>
<td style="text-align:left;">
douglasii
</td>
<td style="text-align:right;">
15
</td>
<td style="text-align:right;">
0.62
</td>
</tr>
<tr>
<td style="text-align:left; padding-left: 2em;" indentlevel="1">
Mammalia
</td>
<td style="text-align:left;">
Vespertilionidae
</td>
<td style="text-align:left;">
Myotis
</td>
<td style="text-align:left;">
sp
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
0.04
</td>
</tr>
<tr grouplength="3">
<td colspan="6" style="border-bottom: 1px solid;">
<strong>Twenty-Mile Creek</strong>
</td>
</tr>
<tr>
<td style="text-align:left; padding-left: 2em;" indentlevel="1">
Aves
</td>
<td style="text-align:left;">
Turdidae
</td>
<td style="text-align:left;">
Ixoreus
</td>
<td style="text-align:left;">
naevius
</td>
<td style="text-align:right;">
4
</td>
<td style="text-align:right;">
0.24
</td>
</tr>
<tr>
<td style="text-align:left; padding-left: 2em;" indentlevel="1">
Mammalia
</td>
<td style="text-align:left;">
Muridae
</td>
<td style="text-align:left;">
Rattus
</td>
<td style="text-align:left;">
sp
</td>
<td style="text-align:right;">
7
</td>
<td style="text-align:right;">
0.41
</td>
</tr>
<tr>
<td style="text-align:left; padding-left: 2em;" indentlevel="1">
Mammalia
</td>
<td style="text-align:left;">
Sciuridae
</td>
<td style="text-align:left;">
Tamiasciurus
</td>
<td style="text-align:left;">
douglasii
</td>
<td style="text-align:right;">
6
</td>
<td style="text-align:right;">
0.35
</td>
</tr>
<tr grouplength="8">
<td colspan="6" style="border-bottom: 1px solid;">
<strong>Utziletz</strong>
</td>
</tr>
<tr>
<td style="text-align:left; padding-left: 2em;" indentlevel="1">
Aves
</td>
<td style="text-align:left;">
Corvidae
</td>
<td style="text-align:left;">
Cyanocitta
</td>
<td style="text-align:left;">
stelleri
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
0.04
</td>
</tr>
<tr>
<td style="text-align:left; padding-left: 2em;" indentlevel="1">
Aves
</td>
<td style="text-align:left;">
Phasianidae
</td>
<td style="text-align:left;">
Bonasa
</td>
<td style="text-align:left;">
umbellus
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
0.04
</td>
</tr>
<tr>
<td style="text-align:left; padding-left: 2em;" indentlevel="1">
Aves
</td>
<td style="text-align:left;">
Turdidae
</td>
<td style="text-align:left;">
Turdus
</td>
<td style="text-align:left;">
migratorius
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
0.04
</td>
</tr>
<tr>
<td style="text-align:left; padding-left: 2em;" indentlevel="1">
Mammalia
</td>
<td style="text-align:left;">
Leporidae
</td>
<td style="text-align:left;">
Lepus
</td>
<td style="text-align:left;">
americanus
</td>
<td style="text-align:right;">
2
</td>
<td style="text-align:right;">
0.08
</td>
</tr>
<tr>
<td style="text-align:left; padding-left: 2em;" indentlevel="1">
Mammalia
</td>
<td style="text-align:left;">
Sciuridae
</td>
<td style="text-align:left;">
Glaucomys
</td>
<td style="text-align:left;">
sabrinus
</td>
<td style="text-align:right;">
2
</td>
<td style="text-align:right;">
0.08
</td>
</tr>
<tr>
<td style="text-align:left; padding-left: 2em;" indentlevel="1">
Mammalia
</td>
<td style="text-align:left;">
Sciuridae
</td>
<td style="text-align:left;">
Tamiasciurus
</td>
<td style="text-align:left;">
douglasii
</td>
<td style="text-align:right;">
9
</td>
<td style="text-align:right;">
0.35
</td>
</tr>
<tr>
<td style="text-align:left; padding-left: 2em;" indentlevel="1">
Mammalia
</td>
<td style="text-align:left;">
Sciuridae
</td>
<td style="text-align:left;">
Tamiasciurus
</td>
<td style="text-align:left;">
hudsonicus
</td>
<td style="text-align:right;">
9
</td>
<td style="text-align:right;">
0.35
</td>
</tr>
<tr>
<td style="text-align:left; padding-left: 2em;" indentlevel="1">
Mammalia
</td>
<td style="text-align:left;">
Sciuridae
</td>
<td style="text-align:left;">
Tamiasciurus
</td>
<td style="text-align:left;">
sp
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
0.04
</td>
</tr>
</tbody>
</table>
``` r
count.by.family <- to.family %>% mutate(mass=as.numeric(mass)) %>% 
  group_by(site) %>% 
  mutate(total=n()) %>% 
  group_by(site, family) %>% 
  mutate(count=n()) %>% 
  ungroup() %>% 
  mutate(prop=count/total) %>% 
  dplyr::select(site, class, family, count, total, prop) %>% 
  arrange(site, class) %>% 
  distinct()

count.by.family %>% dplyr::select(-site, -total) %>% 
  kable(digits=2) %>% 
  kable_styling(full_width=TRUE) %>% 
  pack_rows('Mount Currie', 1, 4) %>% 
  pack_rows('Mount Ford', 5, 9) %>% 
  pack_rows('Ruby Lake', 10, 12) %>% 
  pack_rows('Turbid Creek', 13, 18) %>% 
  pack_rows('Twenty-Mile Creek', 19, 22) %>% 
  pack_rows('Utziletz', 23, 27)
```

<table class="table" style="margin-left: auto; margin-right: auto;">
<thead>
<tr>
<th style="text-align:left;">
class
</th>
<th style="text-align:left;">
family
</th>
<th style="text-align:right;">
count
</th>
<th style="text-align:right;">
prop
</th>
</tr>
</thead>
<tbody>
<tr grouplength="4">
<td colspan="4" style="border-bottom: 1px solid;">
<strong>Mount Currie</strong>
</td>
</tr>
<tr>
<td style="text-align:left; padding-left: 2em;" indentlevel="1">
Aves
</td>
<td style="text-align:left;">
Phasianidae
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
0.04
</td>
</tr>
<tr>
<td style="text-align:left; padding-left: 2em;" indentlevel="1">
Aves
</td>
<td style="text-align:left;">
Turdidae
</td>
<td style="text-align:right;">
7
</td>
<td style="text-align:right;">
0.28
</td>
</tr>
<tr>
<td style="text-align:left; padding-left: 2em;" indentlevel="1">
Mammalia
</td>
<td style="text-align:left;">
Muridae
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
0.04
</td>
</tr>
<tr>
<td style="text-align:left; padding-left: 2em;" indentlevel="1">
Mammalia
</td>
<td style="text-align:left;">
Sciuridae
</td>
<td style="text-align:right;">
16
</td>
<td style="text-align:right;">
0.64
</td>
</tr>
<tr grouplength="5">
<td colspan="4" style="border-bottom: 1px solid;">
<strong>Mount Ford</strong>
</td>
</tr>
<tr>
<td style="text-align:left; padding-left: 2em;" indentlevel="1">
Aves
</td>
<td style="text-align:left;">
Corvidae
</td>
<td style="text-align:right;">
3
</td>
<td style="text-align:right;">
0.09
</td>
</tr>
<tr>
<td style="text-align:left; padding-left: 2em;" indentlevel="1">
Aves
</td>
<td style="text-align:left;">
Phasianidae
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
0.03
</td>
</tr>
<tr>
<td style="text-align:left; padding-left: 2em;" indentlevel="1">
Aves
</td>
<td style="text-align:left;">
Turdidae
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
0.03
</td>
</tr>
<tr>
<td style="text-align:left; padding-left: 2em;" indentlevel="1">
Mammalia
</td>
<td style="text-align:left;">
Muridae
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
0.03
</td>
</tr>
<tr>
<td style="text-align:left; padding-left: 2em;" indentlevel="1">
Mammalia
</td>
<td style="text-align:left;">
Sciuridae
</td>
<td style="text-align:right;">
28
</td>
<td style="text-align:right;">
0.82
</td>
</tr>
<tr grouplength="3">
<td colspan="4" style="border-bottom: 1px solid;">
<strong>Ruby Lake</strong>
</td>
</tr>
<tr>
<td style="text-align:left; padding-left: 2em;" indentlevel="1">
Aves
</td>
<td style="text-align:left;">
Turdidae
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
0.12
</td>
</tr>
<tr>
<td style="text-align:left; padding-left: 2em;" indentlevel="1">
Mammalia
</td>
<td style="text-align:left;">
Neotoma
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
0.12
</td>
</tr>
<tr>
<td style="text-align:left; padding-left: 2em;" indentlevel="1">
Mammalia
</td>
<td style="text-align:left;">
Sciuridae
</td>
<td style="text-align:right;">
6
</td>
<td style="text-align:right;">
0.75
</td>
</tr>
<tr grouplength="6">
<td colspan="4" style="border-bottom: 1px solid;">
<strong>Turbid Creek</strong>
</td>
</tr>
<tr>
<td style="text-align:left; padding-left: 2em;" indentlevel="1">
Aves
</td>
<td style="text-align:left;">
Columbidae
</td>
<td style="text-align:right;">
3
</td>
<td style="text-align:right;">
0.12
</td>
</tr>
<tr>
<td style="text-align:left; padding-left: 2em;" indentlevel="1">
Aves
</td>
<td style="text-align:left;">
Phasianidae
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
0.04
</td>
</tr>
<tr>
<td style="text-align:left; padding-left: 2em;" indentlevel="1">
Aves
</td>
<td style="text-align:left;">
Turdidae
</td>
<td style="text-align:right;">
3
</td>
<td style="text-align:right;">
0.12
</td>
</tr>
<tr>
<td style="text-align:left; padding-left: 2em;" indentlevel="1">
Mammalia
</td>
<td style="text-align:left;">
Muridae
</td>
<td style="text-align:right;">
2
</td>
<td style="text-align:right;">
0.08
</td>
</tr>
<tr>
<td style="text-align:left; padding-left: 2em;" indentlevel="1">
Mammalia
</td>
<td style="text-align:left;">
Sciuridae
</td>
<td style="text-align:right;">
15
</td>
<td style="text-align:right;">
0.60
</td>
</tr>
<tr>
<td style="text-align:left; padding-left: 2em;" indentlevel="1">
Mammalia
</td>
<td style="text-align:left;">
Vespertilionidae
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
0.04
</td>
</tr>
<tr grouplength="4">
<td colspan="4" style="border-bottom: 1px solid;">
<strong>Twenty-Mile Creek</strong>
</td>
</tr>
<tr>
<td style="text-align:left; padding-left: 2em;" indentlevel="1">
Aves
</td>
<td style="text-align:left;">
Picidae
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
0.05
</td>
</tr>
<tr>
<td style="text-align:left; padding-left: 2em;" indentlevel="1">
Aves
</td>
<td style="text-align:left;">
Turdidae
</td>
<td style="text-align:right;">
7
</td>
<td style="text-align:right;">
0.33
</td>
</tr>
<tr>
<td style="text-align:left; padding-left: 2em;" indentlevel="1">
Mammalia
</td>
<td style="text-align:left;">
Muridae
</td>
<td style="text-align:right;">
7
</td>
<td style="text-align:right;">
0.33
</td>
</tr>
<tr>
<td style="text-align:left; padding-left: 2em;" indentlevel="1">
Mammalia
</td>
<td style="text-align:left;">
Sciuridae
</td>
<td style="text-align:right;">
6
</td>
<td style="text-align:right;">
0.29
</td>
</tr>
<tr grouplength="5">
<td colspan="4" style="border-bottom: 1px solid;">
<strong>Utziletz</strong>
</td>
</tr>
<tr>
<td style="text-align:left; padding-left: 2em;" indentlevel="1">
Aves
</td>
<td style="text-align:left;">
Corvidae
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
0.04
</td>
</tr>
<tr>
<td style="text-align:left; padding-left: 2em;" indentlevel="1">
Aves
</td>
<td style="text-align:left;">
Phasianidae
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
0.04
</td>
</tr>
<tr>
<td style="text-align:left; padding-left: 2em;" indentlevel="1">
Aves
</td>
<td style="text-align:left;">
Turdidae
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
0.04
</td>
</tr>
<tr>
<td style="text-align:left; padding-left: 2em;" indentlevel="1">
Mammalia
</td>
<td style="text-align:left;">
Leporidae
</td>
<td style="text-align:right;">
2
</td>
<td style="text-align:right;">
0.08
</td>
</tr>
<tr>
<td style="text-align:left; padding-left: 2em;" indentlevel="1">
Mammalia
</td>
<td style="text-align:left;">
Sciuridae
</td>
<td style="text-align:right;">
21
</td>
<td style="text-align:right;">
0.81
</td>
</tr>
</tbody>
</table>
``` r
count.by.class <- to.class %>%
  group_by(site) %>% 
  mutate(total=n()) %>% 
  group_by(site, class) %>% 
  mutate(count=n()) %>% 
  ungroup() %>% 
  mutate(prop=count/total) %>% 
  dplyr::select(site, class, count, total, prop) %>% 
  arrange(site, class) %>% 
  distinct()

count.by.class %>% dplyr::select(-site, -total) %>% 
  kable(digits=2) %>% 
  kable_styling(full_width=TRUE) %>% 
  pack_rows('Mount Currie', 1, 2) %>% 
  pack_rows('Mount Ford', 3, 4) %>% 
  pack_rows('Ruby Lake', 5, 6) %>% 
  pack_rows('Turbid Creek', 7, 8) %>% 
  pack_rows('Twenty-Mile Creek', 9, 10) %>% 
  pack_rows('Utziletz', 11, 12)
```

<table class="table" style="margin-left: auto; margin-right: auto;">
<thead>
<tr>
<th style="text-align:left;">
class
</th>
<th style="text-align:right;">
count
</th>
<th style="text-align:right;">
prop
</th>
</tr>
</thead>
<tbody>
<tr grouplength="2">
<td colspan="3" style="border-bottom: 1px solid;">
<strong>Mount Currie</strong>
</td>
</tr>
<tr>
<td style="text-align:left; padding-left: 2em;" indentlevel="1">
Aves
</td>
<td style="text-align:right;">
13
</td>
<td style="text-align:right;">
0.42
</td>
</tr>
<tr>
<td style="text-align:left; padding-left: 2em;" indentlevel="1">
Mammalia
</td>
<td style="text-align:right;">
18
</td>
<td style="text-align:right;">
0.58
</td>
</tr>
<tr grouplength="2">
<td colspan="3" style="border-bottom: 1px solid;">
<strong>Mount Ford</strong>
</td>
</tr>
<tr>
<td style="text-align:left; padding-left: 2em;" indentlevel="1">
Aves
</td>
<td style="text-align:right;">
6
</td>
<td style="text-align:right;">
0.17
</td>
</tr>
<tr>
<td style="text-align:left; padding-left: 2em;" indentlevel="1">
Mammalia
</td>
<td style="text-align:right;">
30
</td>
<td style="text-align:right;">
0.83
</td>
</tr>
<tr grouplength="2">
<td colspan="3" style="border-bottom: 1px solid;">
<strong>Ruby Lake</strong>
</td>
</tr>
<tr>
<td style="text-align:left; padding-left: 2em;" indentlevel="1">
Aves
</td>
<td style="text-align:right;">
2
</td>
<td style="text-align:right;">
0.22
</td>
</tr>
<tr>
<td style="text-align:left; padding-left: 2em;" indentlevel="1">
Mammalia
</td>
<td style="text-align:right;">
7
</td>
<td style="text-align:right;">
0.78
</td>
</tr>
<tr grouplength="2">
<td colspan="3" style="border-bottom: 1px solid;">
<strong>Turbid Creek</strong>
</td>
</tr>
<tr>
<td style="text-align:left; padding-left: 2em;" indentlevel="1">
Aves
</td>
<td style="text-align:right;">
13
</td>
<td style="text-align:right;">
0.42
</td>
</tr>
<tr>
<td style="text-align:left; padding-left: 2em;" indentlevel="1">
Mammalia
</td>
<td style="text-align:right;">
18
</td>
<td style="text-align:right;">
0.58
</td>
</tr>
<tr grouplength="2">
<td colspan="3" style="border-bottom: 1px solid;">
<strong>Twenty-Mile Creek</strong>
</td>
</tr>
<tr>
<td style="text-align:left; padding-left: 2em;" indentlevel="1">
Aves
</td>
<td style="text-align:right;">
15
</td>
<td style="text-align:right;">
0.45
</td>
</tr>
<tr>
<td style="text-align:left; padding-left: 2em;" indentlevel="1">
Mammalia
</td>
<td style="text-align:right;">
18
</td>
<td style="text-align:right;">
0.55
</td>
</tr>
<tr grouplength="2">
<td colspan="3" style="border-bottom: 1px solid;">
<strong>Utziletz</strong>
</td>
</tr>
<tr>
<td style="text-align:left; padding-left: 2em;" indentlevel="1">
Aves
</td>
<td style="text-align:right;">
5
</td>
<td style="text-align:right;">
0.17
</td>
</tr>
<tr>
<td style="text-align:left; padding-left: 2em;" indentlevel="1">
Mammalia
</td>
<td style="text-align:right;">
24
</td>
<td style="text-align:right;">
0.83
</td>
</tr>
</tbody>
</table>
And then do it all again for biomass. First for items identfied to genus/species.

``` r
mass.by.genus <- to.genus %>% mutate(mass=as.numeric(mass)) %>% 
  group_by(site) %>% 
  mutate(total=sum(mass)) %>% 
  group_by(site, genus, species) %>% 
  mutate(amount=sum(mass)) %>% 
  ungroup() %>% 
  mutate(prop=amount/total) %>% 
  dplyr::select(site, class, family, genus, species, amount, total, prop) %>% 
  arrange(site, class) %>% 
  distinct()

mass.by.genus %>% dplyr::select(-site, -total) %>% 
  kable(digits=2) %>% 
  kable_styling(full_width=TRUE) %>% 
  pack_rows('Mount Currie', 1, 7) %>% 
  pack_rows('Mount Ford', 8, 14) %>% 
  pack_rows('Ruby Lake', 15, 16) %>% 
  pack_rows('Turbid Creek', 17, 22) %>% 
  pack_rows('Twenty-Mile Creek', 23, 25) %>% 
  pack_rows('Utziletz', 26, 33)
```

<table class="table" style="margin-left: auto; margin-right: auto;">
<thead>
<tr>
<th style="text-align:left;">
class
</th>
<th style="text-align:left;">
family
</th>
<th style="text-align:left;">
genus
</th>
<th style="text-align:left;">
species
</th>
<th style="text-align:right;">
amount
</th>
<th style="text-align:right;">
prop
</th>
</tr>
</thead>
<tbody>
<tr grouplength="7">
<td colspan="6" style="border-bottom: 1px solid;">
<strong>Mount Currie</strong>
</td>
</tr>
<tr>
<td style="text-align:left; padding-left: 2em;" indentlevel="1">
Aves
</td>
<td style="text-align:left;">
Phasianidae
</td>
<td style="text-align:left;">
Dendragapus
</td>
<td style="text-align:left;">
fulignosus
</td>
<td style="text-align:right;">
600.0
</td>
<td style="text-align:right;">
0.14
</td>
</tr>
<tr>
<td style="text-align:left; padding-left: 2em;" indentlevel="1">
Aves
</td>
<td style="text-align:left;">
Turdidae
</td>
<td style="text-align:left;">
Catharus
</td>
<td style="text-align:left;">
ustulatus
</td>
<td style="text-align:right;">
102.0
</td>
<td style="text-align:right;">
0.02
</td>
</tr>
<tr>
<td style="text-align:left; padding-left: 2em;" indentlevel="1">
Aves
</td>
<td style="text-align:left;">
Turdidae
</td>
<td style="text-align:left;">
Ixoreus
</td>
<td style="text-align:left;">
naevius
</td>
<td style="text-align:right;">
82.5
</td>
<td style="text-align:right;">
0.02
</td>
</tr>
<tr>
<td style="text-align:left; padding-left: 2em;" indentlevel="1">
Aves
</td>
<td style="text-align:left;">
Turdidae
</td>
<td style="text-align:left;">
Turdus
</td>
<td style="text-align:left;">
migratorius
</td>
<td style="text-align:right;">
154.0
</td>
<td style="text-align:right;">
0.03
</td>
</tr>
<tr>
<td style="text-align:left; padding-left: 2em;" indentlevel="1">
Mammalia
</td>
<td style="text-align:left;">
Muridae
</td>
<td style="text-align:left;">
Rattus
</td>
<td style="text-align:left;">
sp
</td>
<td style="text-align:right;">
269.8
</td>
<td style="text-align:right;">
0.06
</td>
</tr>
<tr>
<td style="text-align:left; padding-left: 2em;" indentlevel="1">
Mammalia
</td>
<td style="text-align:left;">
Sciuridae
</td>
<td style="text-align:left;">
Glaucomys
</td>
<td style="text-align:left;">
sabrinus
</td>
<td style="text-align:right;">
155.5
</td>
<td style="text-align:right;">
0.04
</td>
</tr>
<tr>
<td style="text-align:left; padding-left: 2em;" indentlevel="1">
Mammalia
</td>
<td style="text-align:left;">
Sciuridae
</td>
<td style="text-align:left;">
Tamiasciurus
</td>
<td style="text-align:left;">
douglasii
</td>
<td style="text-align:right;">
3052.5
</td>
<td style="text-align:right;">
0.69
</td>
</tr>
<tr grouplength="7">
<td colspan="6" style="border-bottom: 1px solid;">
<strong>Mount Ford</strong>
</td>
</tr>
<tr>
<td style="text-align:left; padding-left: 2em;" indentlevel="1">
Aves
</td>
<td style="text-align:left;">
Corvidae
</td>
<td style="text-align:left;">
Cyanocitta
</td>
<td style="text-align:left;">
stelleri
</td>
<td style="text-align:right;">
120.0
</td>
<td style="text-align:right;">
0.02
</td>
</tr>
<tr>
<td style="text-align:left; padding-left: 2em;" indentlevel="1">
Aves
</td>
<td style="text-align:left;">
Corvidae
</td>
<td style="text-align:left;">
Perisoreus
</td>
<td style="text-align:left;">
canadensis
</td>
<td style="text-align:right;">
135.0
</td>
<td style="text-align:right;">
0.02
</td>
</tr>
<tr>
<td style="text-align:left; padding-left: 2em;" indentlevel="1">
Aves
</td>
<td style="text-align:left;">
Phasianidae
</td>
<td style="text-align:left;">
Dendragapus
</td>
<td style="text-align:left;">
fulignosus
</td>
<td style="text-align:right;">
600.0
</td>
<td style="text-align:right;">
0.10
</td>
</tr>
<tr>
<td style="text-align:left; padding-left: 2em;" indentlevel="1">
Aves
</td>
<td style="text-align:left;">
Turdidae
</td>
<td style="text-align:left;">
Ixoreus
</td>
<td style="text-align:left;">
naevius
</td>
<td style="text-align:right;">
82.5
</td>
<td style="text-align:right;">
0.01
</td>
</tr>
<tr>
<td style="text-align:left; padding-left: 2em;" indentlevel="1">
Mammalia
</td>
<td style="text-align:left;">
Muridae
</td>
<td style="text-align:left;">
Rattus
</td>
<td style="text-align:left;">
sp
</td>
<td style="text-align:right;">
269.8
</td>
<td style="text-align:right;">
0.04
</td>
</tr>
<tr>
<td style="text-align:left; padding-left: 2em;" indentlevel="1">
Mammalia
</td>
<td style="text-align:left;">
Sciuridae
</td>
<td style="text-align:left;">
Neotamias
</td>
<td style="text-align:left;">
sp
</td>
<td style="text-align:right;">
398.4
</td>
<td style="text-align:right;">
0.07
</td>
</tr>
<tr>
<td style="text-align:left; padding-left: 2em;" indentlevel="1">
Mammalia
</td>
<td style="text-align:left;">
Sciuridae
</td>
<td style="text-align:left;">
Tamiasciurus
</td>
<td style="text-align:left;">
douglasii
</td>
<td style="text-align:right;">
4477.0
</td>
<td style="text-align:right;">
0.74
</td>
</tr>
<tr grouplength="2">
<td colspan="6" style="border-bottom: 1px solid;">
<strong>Ruby Lake</strong>
</td>
</tr>
<tr>
<td style="text-align:left; padding-left: 2em;" indentlevel="1">
Mammalia
</td>
<td style="text-align:left;">
Neotoma
</td>
<td style="text-align:left;">
Cricetidae
</td>
<td style="text-align:left;">
cinerea
</td>
<td style="text-align:right;">
374.7
</td>
<td style="text-align:right;">
0.23
</td>
</tr>
<tr>
<td style="text-align:left; padding-left: 2em;" indentlevel="1">
Mammalia
</td>
<td style="text-align:left;">
Sciuridae
</td>
<td style="text-align:left;">
Tamiasciurus
</td>
<td style="text-align:left;">
douglasii
</td>
<td style="text-align:right;">
1221.0
</td>
<td style="text-align:right;">
0.77
</td>
</tr>
<tr grouplength="6">
<td colspan="6" style="border-bottom: 1px solid;">
<strong>Turbid Creek</strong>
</td>
</tr>
<tr>
<td style="text-align:left; padding-left: 2em;" indentlevel="1">
Aves
</td>
<td style="text-align:left;">
Columbidae
</td>
<td style="text-align:left;">
Patagoienas
</td>
<td style="text-align:left;">
fasciata
</td>
<td style="text-align:right;">
1176.0
</td>
<td style="text-align:right;">
0.24
</td>
</tr>
<tr>
<td style="text-align:left; padding-left: 2em;" indentlevel="1">
Aves
</td>
<td style="text-align:left;">
Turdidae
</td>
<td style="text-align:left;">
Catharus
</td>
<td style="text-align:left;">
ustulatus
</td>
<td style="text-align:right;">
68.0
</td>
<td style="text-align:right;">
0.01
</td>
</tr>
<tr>
<td style="text-align:left; padding-left: 2em;" indentlevel="1">
Aves
</td>
<td style="text-align:left;">
Turdidae
</td>
<td style="text-align:left;">
Ixoreus
</td>
<td style="text-align:left;">
naevius
</td>
<td style="text-align:right;">
82.5
</td>
<td style="text-align:right;">
0.02
</td>
</tr>
<tr>
<td style="text-align:left; padding-left: 2em;" indentlevel="1">
Mammalia
</td>
<td style="text-align:left;">
Muridae
</td>
<td style="text-align:left;">
Rattus
</td>
<td style="text-align:left;">
sp
</td>
<td style="text-align:right;">
539.6
</td>
<td style="text-align:right;">
0.11
</td>
</tr>
<tr>
<td style="text-align:left; padding-left: 2em;" indentlevel="1">
Mammalia
</td>
<td style="text-align:left;">
Sciuridae
</td>
<td style="text-align:left;">
Tamiasciurus
</td>
<td style="text-align:left;">
douglasii
</td>
<td style="text-align:right;">
3052.5
</td>
<td style="text-align:right;">
0.62
</td>
</tr>
<tr>
<td style="text-align:left; padding-left: 2em;" indentlevel="1">
Mammalia
</td>
<td style="text-align:left;">
Vespertilionidae
</td>
<td style="text-align:left;">
Myotis
</td>
<td style="text-align:left;">
sp
</td>
<td style="text-align:right;">
5.8
</td>
<td style="text-align:right;">
0.00
</td>
</tr>
<tr grouplength="3">
<td colspan="6" style="border-bottom: 1px solid;">
<strong>Twenty-Mile Creek</strong>
</td>
</tr>
<tr>
<td style="text-align:left; padding-left: 2em;" indentlevel="1">
Aves
</td>
<td style="text-align:left;">
Turdidae
</td>
<td style="text-align:left;">
Ixoreus
</td>
<td style="text-align:left;">
naevius
</td>
<td style="text-align:right;">
330.0
</td>
<td style="text-align:right;">
0.10
</td>
</tr>
<tr>
<td style="text-align:left; padding-left: 2em;" indentlevel="1">
Mammalia
</td>
<td style="text-align:left;">
Muridae
</td>
<td style="text-align:left;">
Rattus
</td>
<td style="text-align:left;">
sp
</td>
<td style="text-align:right;">
1888.6
</td>
<td style="text-align:right;">
0.55
</td>
</tr>
<tr>
<td style="text-align:left; padding-left: 2em;" indentlevel="1">
Mammalia
</td>
<td style="text-align:left;">
Sciuridae
</td>
<td style="text-align:left;">
Tamiasciurus
</td>
<td style="text-align:left;">
douglasii
</td>
<td style="text-align:right;">
1221.0
</td>
<td style="text-align:right;">
0.35
</td>
</tr>
<tr grouplength="8">
<td colspan="6" style="border-bottom: 1px solid;">
<strong>Utziletz</strong>
</td>
</tr>
<tr>
<td style="text-align:left; padding-left: 2em;" indentlevel="1">
Aves
</td>
<td style="text-align:left;">
Corvidae
</td>
<td style="text-align:left;">
Cyanocitta
</td>
<td style="text-align:left;">
stelleri
</td>
<td style="text-align:right;">
120.0
</td>
<td style="text-align:right;">
0.02
</td>
</tr>
<tr>
<td style="text-align:left; padding-left: 2em;" indentlevel="1">
Aves
</td>
<td style="text-align:left;">
Phasianidae
</td>
<td style="text-align:left;">
Bonasa
</td>
<td style="text-align:left;">
umbellus
</td>
<td style="text-align:right;">
600.0
</td>
<td style="text-align:right;">
0.08
</td>
</tr>
<tr>
<td style="text-align:left; padding-left: 2em;" indentlevel="1">
Aves
</td>
<td style="text-align:left;">
Turdidae
</td>
<td style="text-align:left;">
Turdus
</td>
<td style="text-align:left;">
migratorius
</td>
<td style="text-align:right;">
77.0
</td>
<td style="text-align:right;">
0.01
</td>
</tr>
<tr>
<td style="text-align:left; padding-left: 2em;" indentlevel="1">
Mammalia
</td>
<td style="text-align:left;">
Leporidae
</td>
<td style="text-align:left;">
Lepus
</td>
<td style="text-align:left;">
americanus
</td>
<td style="text-align:right;">
2680.0
</td>
<td style="text-align:right;">
0.34
</td>
</tr>
<tr>
<td style="text-align:left; padding-left: 2em;" indentlevel="1">
Mammalia
</td>
<td style="text-align:left;">
Sciuridae
</td>
<td style="text-align:left;">
Glaucomys
</td>
<td style="text-align:left;">
sabrinus
</td>
<td style="text-align:right;">
311.0
</td>
<td style="text-align:right;">
0.04
</td>
</tr>
<tr>
<td style="text-align:left; padding-left: 2em;" indentlevel="1">
Mammalia
</td>
<td style="text-align:left;">
Sciuridae
</td>
<td style="text-align:left;">
Tamiasciurus
</td>
<td style="text-align:left;">
douglasii
</td>
<td style="text-align:right;">
1831.5
</td>
<td style="text-align:right;">
0.23
</td>
</tr>
<tr>
<td style="text-align:left; padding-left: 2em;" indentlevel="1">
Mammalia
</td>
<td style="text-align:left;">
Sciuridae
</td>
<td style="text-align:left;">
Tamiasciurus
</td>
<td style="text-align:left;">
hudsonicus
</td>
<td style="text-align:right;">
2020.5
</td>
<td style="text-align:right;">
0.26
</td>
</tr>
<tr>
<td style="text-align:left; padding-left: 2em;" indentlevel="1">
Mammalia
</td>
<td style="text-align:left;">
Sciuridae
</td>
<td style="text-align:left;">
Tamiasciurus
</td>
<td style="text-align:left;">
sp
</td>
<td style="text-align:right;">
214.0
</td>
<td style="text-align:right;">
0.03
</td>
</tr>
</tbody>
</table>
Then items identified to family.

``` r
mass.by.family <- to.family %>% mutate(mass=as.numeric(mass)) %>% 
  group_by(site) %>% 
  mutate(total=sum(mass)) %>% 
  group_by(site, family) %>% 
  mutate(amount=sum(mass)) %>% 
  ungroup() %>% 
  mutate(prop=amount/total) %>% 
  dplyr::select(site, class, family, amount, total, prop) %>% 
  arrange(site, class) %>% 
  distinct()

mass.by.family %>% dplyr::select(-site, -total) %>% 
  kable(digits=2) %>% 
  kable_styling(full_width=TRUE) %>% 
  pack_rows('Mount Currie', 1, 4) %>% 
  pack_rows('Mount Ford', 5, 9) %>% 
  pack_rows('Ruby Lake', 10, 12) %>% 
  pack_rows('Turbid Creek', 13, 18) %>% 
  pack_rows('Twenty-Mile Creek', 19, 22) %>% 
  pack_rows('Utziletz', 23, 27)
```

<table class="table" style="margin-left: auto; margin-right: auto;">
<thead>
<tr>
<th style="text-align:left;">
class
</th>
<th style="text-align:left;">
family
</th>
<th style="text-align:right;">
amount
</th>
<th style="text-align:right;">
prop
</th>
</tr>
</thead>
<tbody>
<tr grouplength="4">
<td colspan="4" style="border-bottom: 1px solid;">
<strong>Mount Currie</strong>
</td>
</tr>
<tr>
<td style="text-align:left; padding-left: 2em;" indentlevel="1">
Aves
</td>
<td style="text-align:left;">
Phasianidae
</td>
<td style="text-align:right;">
600.00
</td>
<td style="text-align:right;">
0.13
</td>
</tr>
<tr>
<td style="text-align:left; padding-left: 2em;" indentlevel="1">
Aves
</td>
<td style="text-align:left;">
Turdidae
</td>
<td style="text-align:right;">
372.50
</td>
<td style="text-align:right;">
0.08
</td>
</tr>
<tr>
<td style="text-align:left; padding-left: 2em;" indentlevel="1">
Mammalia
</td>
<td style="text-align:left;">
Muridae
</td>
<td style="text-align:right;">
269.80
</td>
<td style="text-align:right;">
0.06
</td>
</tr>
<tr>
<td style="text-align:left; padding-left: 2em;" indentlevel="1">
Mammalia
</td>
<td style="text-align:left;">
Sciuridae
</td>
<td style="text-align:right;">
3208.00
</td>
<td style="text-align:right;">
0.72
</td>
</tr>
<tr grouplength="5">
<td colspan="4" style="border-bottom: 1px solid;">
<strong>Mount Ford</strong>
</td>
</tr>
<tr>
<td style="text-align:left; padding-left: 2em;" indentlevel="1">
Aves
</td>
<td style="text-align:left;">
Corvidae
</td>
<td style="text-align:right;">
255.00
</td>
<td style="text-align:right;">
0.04
</td>
</tr>
<tr>
<td style="text-align:left; padding-left: 2em;" indentlevel="1">
Aves
</td>
<td style="text-align:left;">
Phasianidae
</td>
<td style="text-align:right;">
600.00
</td>
<td style="text-align:right;">
0.10
</td>
</tr>
<tr>
<td style="text-align:left; padding-left: 2em;" indentlevel="1">
Aves
</td>
<td style="text-align:left;">
Turdidae
</td>
<td style="text-align:right;">
82.50
</td>
<td style="text-align:right;">
0.01
</td>
</tr>
<tr>
<td style="text-align:left; padding-left: 2em;" indentlevel="1">
Mammalia
</td>
<td style="text-align:left;">
Muridae
</td>
<td style="text-align:right;">
269.80
</td>
<td style="text-align:right;">
0.04
</td>
</tr>
<tr>
<td style="text-align:left; padding-left: 2em;" indentlevel="1">
Mammalia
</td>
<td style="text-align:left;">
Sciuridae
</td>
<td style="text-align:right;">
4875.40
</td>
<td style="text-align:right;">
0.80
</td>
</tr>
<tr grouplength="3">
<td colspan="4" style="border-bottom: 1px solid;">
<strong>Ruby Lake</strong>
</td>
</tr>
<tr>
<td style="text-align:left; padding-left: 2em;" indentlevel="1">
Aves
</td>
<td style="text-align:left;">
Turdidae
</td>
<td style="text-align:right;">
84.54
</td>
<td style="text-align:right;">
0.05
</td>
</tr>
<tr>
<td style="text-align:left; padding-left: 2em;" indentlevel="1">
Mammalia
</td>
<td style="text-align:left;">
Neotoma
</td>
<td style="text-align:right;">
374.70
</td>
<td style="text-align:right;">
0.22
</td>
</tr>
<tr>
<td style="text-align:left; padding-left: 2em;" indentlevel="1">
Mammalia
</td>
<td style="text-align:left;">
Sciuridae
</td>
<td style="text-align:right;">
1221.00
</td>
<td style="text-align:right;">
0.73
</td>
</tr>
<tr grouplength="6">
<td colspan="4" style="border-bottom: 1px solid;">
<strong>Turbid Creek</strong>
</td>
</tr>
<tr>
<td style="text-align:left; padding-left: 2em;" indentlevel="1">
Aves
</td>
<td style="text-align:left;">
Columbidae
</td>
<td style="text-align:right;">
1176.00
</td>
<td style="text-align:right;">
0.24
</td>
</tr>
<tr>
<td style="text-align:left; padding-left: 2em;" indentlevel="1">
Aves
</td>
<td style="text-align:left;">
Phasianidae
</td>
<td style="text-align:right;">
34.00
</td>
<td style="text-align:right;">
0.01
</td>
</tr>
<tr>
<td style="text-align:left; padding-left: 2em;" indentlevel="1">
Aves
</td>
<td style="text-align:left;">
Turdidae
</td>
<td style="text-align:right;">
150.50
</td>
<td style="text-align:right;">
0.03
</td>
</tr>
<tr>
<td style="text-align:left; padding-left: 2em;" indentlevel="1">
Mammalia
</td>
<td style="text-align:left;">
Muridae
</td>
<td style="text-align:right;">
539.60
</td>
<td style="text-align:right;">
0.11
</td>
</tr>
<tr>
<td style="text-align:left; padding-left: 2em;" indentlevel="1">
Mammalia
</td>
<td style="text-align:left;">
Sciuridae
</td>
<td style="text-align:right;">
3052.50
</td>
<td style="text-align:right;">
0.62
</td>
</tr>
<tr>
<td style="text-align:left; padding-left: 2em;" indentlevel="1">
Mammalia
</td>
<td style="text-align:left;">
Vespertilionidae
</td>
<td style="text-align:right;">
5.80
</td>
<td style="text-align:right;">
0.00
</td>
</tr>
<tr grouplength="4">
<td colspan="4" style="border-bottom: 1px solid;">
<strong>Twenty-Mile Creek</strong>
</td>
</tr>
<tr>
<td style="text-align:left; padding-left: 2em;" indentlevel="1">
Aves
</td>
<td style="text-align:left;">
Picidae
</td>
<td style="text-align:right;">
84.54
</td>
<td style="text-align:right;">
0.02
</td>
</tr>
<tr>
<td style="text-align:left; padding-left: 2em;" indentlevel="1">
Aves
</td>
<td style="text-align:left;">
Turdidae
</td>
<td style="text-align:right;">
533.07
</td>
<td style="text-align:right;">
0.14
</td>
</tr>
<tr>
<td style="text-align:left; padding-left: 2em;" indentlevel="1">
Mammalia
</td>
<td style="text-align:left;">
Muridae
</td>
<td style="text-align:right;">
1888.60
</td>
<td style="text-align:right;">
0.51
</td>
</tr>
<tr>
<td style="text-align:left; padding-left: 2em;" indentlevel="1">
Mammalia
</td>
<td style="text-align:left;">
Sciuridae
</td>
<td style="text-align:right;">
1221.00
</td>
<td style="text-align:right;">
0.33
</td>
</tr>
<tr grouplength="5">
<td colspan="4" style="border-bottom: 1px solid;">
<strong>Utziletz</strong>
</td>
</tr>
<tr>
<td style="text-align:left; padding-left: 2em;" indentlevel="1">
Aves
</td>
<td style="text-align:left;">
Corvidae
</td>
<td style="text-align:right;">
120.00
</td>
<td style="text-align:right;">
0.02
</td>
</tr>
<tr>
<td style="text-align:left; padding-left: 2em;" indentlevel="1">
Aves
</td>
<td style="text-align:left;">
Phasianidae
</td>
<td style="text-align:right;">
600.00
</td>
<td style="text-align:right;">
0.08
</td>
</tr>
<tr>
<td style="text-align:left; padding-left: 2em;" indentlevel="1">
Aves
</td>
<td style="text-align:left;">
Turdidae
</td>
<td style="text-align:right;">
77.00
</td>
<td style="text-align:right;">
0.01
</td>
</tr>
<tr>
<td style="text-align:left; padding-left: 2em;" indentlevel="1">
Mammalia
</td>
<td style="text-align:left;">
Leporidae
</td>
<td style="text-align:right;">
2680.00
</td>
<td style="text-align:right;">
0.34
</td>
</tr>
<tr>
<td style="text-align:left; padding-left: 2em;" indentlevel="1">
Mammalia
</td>
<td style="text-align:left;">
Sciuridae
</td>
<td style="text-align:right;">
4377.00
</td>
<td style="text-align:right;">
0.56
</td>
</tr>
</tbody>
</table>
And finally to class only.

``` r
mass.by.class <- to.class %>% mutate(mass=as.numeric(mass)) %>% 
  group_by(site) %>% 
  mutate(total=sum(mass)) %>% 
  group_by(site, class) %>% 
  mutate(amount=sum(mass)) %>% 
  ungroup() %>% 
  mutate(prop=amount/total) %>% 
  dplyr::select(site, class, amount, total, prop) %>% 
  arrange(site, class) %>% 
  distinct()

mass.by.class %>% dplyr::select(-site, -total) %>% 
  kable(digits=2) %>% 
  kable_styling(full_width=TRUE) %>% 
  pack_rows('Mount Currie', 1, 2) %>% 
  pack_rows('Mount Ford', 3, 4) %>% 
  pack_rows('Ruby Lake', 5, 6) %>% 
  pack_rows('Turbid Creek', 7, 8) %>% 
  pack_rows('Twenty-Mile Creek', 9, 10) %>% 
  pack_rows('Utziletz', 11, 12)
```

<table class="table" style="margin-left: auto; margin-right: auto;">
<thead>
<tr>
<th style="text-align:left;">
class
</th>
<th style="text-align:right;">
amount
</th>
<th style="text-align:right;">
prop
</th>
</tr>
</thead>
<tbody>
<tr grouplength="2">
<td colspan="3" style="border-bottom: 1px solid;">
<strong>Mount Currie</strong>
</td>
</tr>
<tr>
<td style="text-align:left; padding-left: 2em;" indentlevel="1">
Aves
</td>
<td style="text-align:right;">
1193.04
</td>
<td style="text-align:right;">
0.25
</td>
</tr>
<tr>
<td style="text-align:left; padding-left: 2em;" indentlevel="1">
Mammalia
</td>
<td style="text-align:right;">
3564.87
</td>
<td style="text-align:right;">
0.75
</td>
</tr>
<tr grouplength="2">
<td colspan="3" style="border-bottom: 1px solid;">
<strong>Mount Ford</strong>
</td>
</tr>
<tr>
<td style="text-align:left; padding-left: 2em;" indentlevel="1">
Aves
</td>
<td style="text-align:right;">
1022.04
</td>
<td style="text-align:right;">
0.14
</td>
</tr>
<tr>
<td style="text-align:left; padding-left: 2em;" indentlevel="1">
Mammalia
</td>
<td style="text-align:right;">
6485.20
</td>
<td style="text-align:right;">
0.86
</td>
</tr>
<tr grouplength="2">
<td colspan="3" style="border-bottom: 1px solid;">
<strong>Ruby Lake</strong>
</td>
</tr>
<tr>
<td style="text-align:left; padding-left: 2em;" indentlevel="1">
Aves
</td>
<td style="text-align:right;">
169.07
</td>
<td style="text-align:right;">
0.10
</td>
</tr>
<tr>
<td style="text-align:left; padding-left: 2em;" indentlevel="1">
Mammalia
</td>
<td style="text-align:right;">
1595.70
</td>
<td style="text-align:right;">
0.90
</td>
</tr>
<tr grouplength="2">
<td colspan="3" style="border-bottom: 1px solid;">
<strong>Turbid Creek</strong>
</td>
</tr>
<tr>
<td style="text-align:left; padding-left: 2em;" indentlevel="1">
Aves
</td>
<td style="text-align:right;">
1615.04
</td>
<td style="text-align:right;">
0.31
</td>
</tr>
<tr>
<td style="text-align:left; padding-left: 2em;" indentlevel="1">
Mammalia
</td>
<td style="text-align:right;">
3597.90
</td>
<td style="text-align:right;">
0.69
</td>
</tr>
<tr grouplength="2">
<td colspan="3" style="border-bottom: 1px solid;">
<strong>Twenty-Mile Creek</strong>
</td>
</tr>
<tr>
<td style="text-align:left; padding-left: 2em;" indentlevel="1">
Aves
</td>
<td style="text-align:right;">
906.14
</td>
<td style="text-align:right;">
0.19
</td>
</tr>
<tr>
<td style="text-align:left; padding-left: 2em;" indentlevel="1">
Mammalia
</td>
<td style="text-align:right;">
3928.98
</td>
<td style="text-align:right;">
0.81
</td>
</tr>
<tr grouplength="2">
<td colspan="3" style="border-bottom: 1px solid;">
<strong>Utziletz</strong>
</td>
</tr>
<tr>
<td style="text-align:left; padding-left: 2em;" indentlevel="1">
Aves
</td>
<td style="text-align:right;">
966.07
</td>
<td style="text-align:right;">
0.12
</td>
</tr>
<tr>
<td style="text-align:left; padding-left: 2em;" indentlevel="1">
Mammalia
</td>
<td style="text-align:right;">
7272.08
</td>
<td style="text-align:right;">
0.88
</td>
</tr>
</tbody>
</table>
