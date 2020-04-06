Data cleaning
================

There's only a few weeks-worth of photos that haven't been analyzed yet, so it's time to set up a workflow for processing the data sheets.

``` r
# Load some libraries.
library('tidyverse')

# Find all the files.
files <- list.files('../data/raw', pattern='photos*', full.names=TRUE)

# Check how they should be imported.
files %>% map(~spec_csv(.))
```

    ## [[1]]
    ## cols(
    ##   filename = col_character(),
    ##   datetime = col_character(),
    ##   serial = col_character(),
    ##   site = col_character(),
    ##   interest = col_character(),
    ##   live.chicks = col_double(),
    ##   class = col_character(),
    ##   order = col_character(),
    ##   family = col_character(),
    ##   genus = col_character(),
    ##   species = col_character(),
    ##   common = col_character(),
    ##   size = col_character(),
    ##   comments = col_character()
    ## )
    ## 
    ## [[2]]
    ## cols(
    ##   filename = col_character(),
    ##   datetime = col_character(),
    ##   serial = col_character(),
    ##   site = col_character(),
    ##   interest = col_character(),
    ##   live.chicks = col_logical(),
    ##   class = col_character(),
    ##   order = col_character(),
    ##   family = col_character(),
    ##   genus = col_character(),
    ##   species = col_character(),
    ##   common = col_character(),
    ##   size = col_character(),
    ##   comments = col_character()
    ## )
    ## 
    ## [[3]]
    ## cols(
    ##   filename = col_character(),
    ##   datetime = col_character(),
    ##   serial = col_character(),
    ##   site = col_character(),
    ##   interest = col_character(),
    ##   live.chicks = col_logical(),
    ##   class = col_character(),
    ##   order = col_character(),
    ##   family = col_character(),
    ##   genus = col_character(),
    ##   species = col_character(),
    ##   common = col_character(),
    ##   size = col_character(),
    ##   comments = col_character()
    ## )
    ## 
    ## [[4]]
    ## cols(
    ##   filename = col_character(),
    ##   datetime = col_character(),
    ##   serial = col_character(),
    ##   site = col_character(),
    ##   interest = col_character(),
    ##   live.chicks = col_double(),
    ##   class = col_character(),
    ##   order = col_character(),
    ##   family = col_character(),
    ##   genus = col_character(),
    ##   species = col_character(),
    ##   common = col_character(),
    ##   size = col_character(),
    ##   comments = col_character()
    ## )
    ## 
    ## [[5]]
    ## cols(
    ##   filename = col_character(),
    ##   datetime = col_character(),
    ##   serial = col_character(),
    ##   site = col_character(),
    ##   interest = col_character(),
    ##   live.chicks = col_logical(),
    ##   class = col_character(),
    ##   order = col_character(),
    ##   family = col_character(),
    ##   genus = col_character(),
    ##   species = col_character(),
    ##   common = col_character(),
    ##   size = col_character(),
    ##   comments = col_logical()
    ## )
    ## 
    ## [[6]]
    ## cols(
    ##   filename = col_character(),
    ##   datetime = col_character(),
    ##   serial = col_character(),
    ##   site = col_character(),
    ##   interest = col_character(),
    ##   live.chicks = col_logical(),
    ##   class = col_character(),
    ##   order = col_character(),
    ##   family = col_character(),
    ##   genus = col_character(),
    ##   species = col_character(),
    ##   common = col_character(),
    ##   size = col_character(),
    ##   comments = col_logical()
    ## )
    ## 
    ## [[7]]
    ## cols(
    ##   filename = col_character(),
    ##   datetime = col_character(),
    ##   serial = col_character(),
    ##   site = col_character(),
    ##   interest = col_character(),
    ##   live.chicks = col_double(),
    ##   class = col_character(),
    ##   order = col_character(),
    ##   family = col_character(),
    ##   genus = col_character(),
    ##   species = col_character(),
    ##   common = col_character(),
    ##   size = col_character(),
    ##   comments = col_character()
    ## )
    ## 
    ## [[8]]
    ## cols(
    ##   filename = col_character(),
    ##   datetime = col_character(),
    ##   serial = col_character(),
    ##   site = col_character(),
    ##   interest = col_character(),
    ##   live.chicks = col_logical(),
    ##   class = col_character(),
    ##   order = col_character(),
    ##   family = col_character(),
    ##   genus = col_character(),
    ##   species = col_character(),
    ##   common = col_character(),
    ##   size = col_character(),
    ##   comments = col_character()
    ## )
    ## 
    ## [[9]]
    ## cols(
    ##   filename = col_character(),
    ##   datetime = col_character(),
    ##   serial = col_character(),
    ##   site = col_character(),
    ##   interest = col_character(),
    ##   live.chicks = col_logical(),
    ##   class = col_character(),
    ##   order = col_character(),
    ##   family = col_character(),
    ##   genus = col_character(),
    ##   species = col_character(),
    ##   common = col_character(),
    ##   size = col_character(),
    ##   comments = col_logical()
    ## )
    ## 
    ## [[10]]
    ## cols(
    ##   filename = col_character(),
    ##   datetime = col_character(),
    ##   serial = col_character(),
    ##   site = col_character(),
    ##   interest = col_character(),
    ##   live.chicks = col_double(),
    ##   class = col_character(),
    ##   order = col_character(),
    ##   family = col_character(),
    ##   genus = col_character(),
    ##   species = col_character(),
    ##   common = col_character(),
    ##   size = col_character(),
    ##   comments = col_character()
    ## )
    ## 
    ## [[11]]
    ## cols(
    ##   filename = col_character(),
    ##   datetime = col_character(),
    ##   serial = col_character(),
    ##   site = col_character(),
    ##   interest = col_character(),
    ##   live.chicks = col_double(),
    ##   class = col_character(),
    ##   order = col_character(),
    ##   family = col_character(),
    ##   genus = col_character(),
    ##   species = col_character(),
    ##   common = col_character(),
    ##   size = col_character(),
    ##   comments = col_character()
    ## )
    ## 
    ## [[12]]
    ## cols(
    ##   filename = col_character(),
    ##   datetime = col_character(),
    ##   serial = col_character(),
    ##   site = col_character(),
    ##   interest = col_character(),
    ##   live.chicks = col_double(),
    ##   class = col_character(),
    ##   order = col_character(),
    ##   family = col_character(),
    ##   genus = col_character(),
    ##   species = col_character(),
    ##   common = col_character(),
    ##   size = col_character(),
    ##   comments = col_logical(),
    ##   sex = col_character()
    ## )
    ## 
    ## [[13]]
    ## cols(
    ##   filename = col_character(),
    ##   datetime = col_character(),
    ##   serial = col_character(),
    ##   site = col_character(),
    ##   interest = col_character(),
    ##   live.chicks = col_double(),
    ##   class = col_character(),
    ##   order = col_character(),
    ##   family = col_character(),
    ##   genus = col_character(),
    ##   species = col_character(),
    ##   common = col_character(),
    ##   size = col_character(),
    ##   comments = col_logical(),
    ##   sex = col_character()
    ## )
    ## 
    ## [[14]]
    ## cols(
    ##   filename = col_character(),
    ##   datetime = col_character(),
    ##   serial = col_character(),
    ##   site = col_character(),
    ##   interest = col_character(),
    ##   live.chicks = col_double(),
    ##   class = col_character(),
    ##   order = col_character(),
    ##   family = col_character(),
    ##   genus = col_character(),
    ##   species = col_character(),
    ##   common = col_character(),
    ##   size = col_character(),
    ##   comments = col_character(),
    ##   sex = col_character()
    ## )
    ## 
    ## [[15]]
    ## cols(
    ##   filename = col_character(),
    ##   datetime = col_character(),
    ##   serial = col_character(),
    ##   site = col_character(),
    ##   interest = col_character(),
    ##   live.chicks = col_double(),
    ##   class = col_character(),
    ##   order = col_character(),
    ##   family = col_character(),
    ##   genus = col_character(),
    ##   species = col_character(),
    ##   common = col_character(),
    ##   size = col_character(),
    ##   comments = col_logical(),
    ##   sex = col_logical()
    ## )
    ## 
    ## [[16]]
    ## cols(
    ##   filename = col_character(),
    ##   datetime = col_character(),
    ##   serial = col_character(),
    ##   site = col_character(),
    ##   interest = col_logical(),
    ##   live.chicks = col_double(),
    ##   class = col_logical(),
    ##   order = col_logical(),
    ##   family = col_logical(),
    ##   genus = col_logical(),
    ##   species = col_logical(),
    ##   common = col_logical(),
    ##   size = col_logical(),
    ##   comments = col_logical(),
    ##   sex = col_logical()
    ## )
    ## 
    ## [[17]]
    ## cols(
    ##   filename = col_character(),
    ##   datetime = col_character(),
    ##   serial = col_character(),
    ##   site = col_character(),
    ##   interest = col_logical(),
    ##   live.chicks = col_double(),
    ##   class = col_logical(),
    ##   order = col_logical(),
    ##   family = col_logical(),
    ##   genus = col_logical(),
    ##   species = col_logical(),
    ##   common = col_logical(),
    ##   size = col_logical(),
    ##   comments = col_logical()
    ## )
    ## 
    ## [[18]]
    ## cols(
    ##   filename = col_character(),
    ##   datetime = col_datetime(format = ""),
    ##   serial = col_character(),
    ##   site = col_character(),
    ##   interest = col_character(),
    ##   live.chicks = col_double(),
    ##   class = col_character(),
    ##   order = col_character(),
    ##   family = col_character(),
    ##   genus = col_character(),
    ##   species = col_character(),
    ##   common = col_character(),
    ##   size = col_character(),
    ##   comments = col_logical(),
    ##   sex = col_character()
    ## )
    ## 
    ## [[19]]
    ## cols(
    ##   filename = col_character(),
    ##   datetime = col_character(),
    ##   serial = col_character(),
    ##   site = col_character(),
    ##   interest = col_character(),
    ##   live.chicks = col_logical(),
    ##   class = col_character(),
    ##   order = col_character(),
    ##   family = col_character(),
    ##   genus = col_character(),
    ##   species = col_character(),
    ##   common = col_character(),
    ##   size = col_character(),
    ##   comments = col_character(),
    ##   sex = col_character()
    ## )
    ## 
    ## [[20]]
    ## cols(
    ##   filename = col_character(),
    ##   datetime = col_character(),
    ##   serial = col_character(),
    ##   site = col_character(),
    ##   interest = col_character(),
    ##   live.chicks = col_logical(),
    ##   class = col_character(),
    ##   order = col_character(),
    ##   family = col_character(),
    ##   genus = col_character(),
    ##   species = col_character(),
    ##   common = col_character(),
    ##   size = col_character(),
    ##   comments = col_character(),
    ##   sex = col_character()
    ## )
    ## 
    ## [[21]]
    ## cols(
    ##   filename = col_character(),
    ##   datetime = col_character(),
    ##   serial = col_character(),
    ##   site = col_character(),
    ##   interest = col_character(),
    ##   live.chicks = col_logical(),
    ##   class = col_character(),
    ##   order = col_character(),
    ##   family = col_character(),
    ##   genus = col_character(),
    ##   species = col_character(),
    ##   common = col_character(),
    ##   size = col_character(),
    ##   comments = col_logical(),
    ##   sex = col_character()
    ## )
    ## 
    ## [[22]]
    ## cols(
    ##   filename = col_character(),
    ##   datetime = col_character(),
    ##   serial = col_character(),
    ##   site = col_character(),
    ##   interest = col_character(),
    ##   live.chicks = col_logical(),
    ##   class = col_character(),
    ##   order = col_character(),
    ##   family = col_character(),
    ##   genus = col_character(),
    ##   species = col_character(),
    ##   common = col_character(),
    ##   size = col_character(),
    ##   comments = col_logical(),
    ##   sex = col_character()
    ## )
    ## 
    ## [[23]]
    ## cols(
    ##   filename = col_character(),
    ##   datetime = col_datetime(format = ""),
    ##   serial = col_character(),
    ##   site = col_character(),
    ##   interest = col_character(),
    ##   live.chicks = col_double(),
    ##   class = col_character(),
    ##   order = col_character(),
    ##   family = col_character(),
    ##   genus = col_character(),
    ##   species = col_character(),
    ##   common = col_character(),
    ##   size = col_character(),
    ##   comments = col_logical()
    ## )
    ## 
    ## [[24]]
    ## cols(
    ##   filename = col_character(),
    ##   datetime = col_character(),
    ##   serial = col_character(),
    ##   site = col_character(),
    ##   interest = col_character(),
    ##   live.chicks = col_logical(),
    ##   class = col_character(),
    ##   order = col_character(),
    ##   family = col_character(),
    ##   genus = col_character(),
    ##   species = col_character(),
    ##   common = col_character(),
    ##   size = col_character(),
    ##   comments = col_character()
    ## )
    ## 
    ## [[25]]
    ## cols(
    ##   filename = col_character(),
    ##   datetime = col_character(),
    ##   serial = col_character(),
    ##   site = col_character(),
    ##   interest = col_character(),
    ##   live.chicks = col_double(),
    ##   class = col_character(),
    ##   order = col_character(),
    ##   family = col_character(),
    ##   genus = col_character(),
    ##   species = col_character(),
    ##   common = col_character(),
    ##   size = col_character(),
    ##   comments = col_character()
    ## )
    ## 
    ## [[26]]
    ## cols(
    ##   filename = col_character(),
    ##   datetime = col_character(),
    ##   serial = col_character(),
    ##   site = col_character(),
    ##   interest = col_character(),
    ##   live.chicks = col_logical(),
    ##   class = col_character(),
    ##   order = col_character(),
    ##   family = col_character(),
    ##   genus = col_character(),
    ##   species = col_character(),
    ##   common = col_character(),
    ##   size = col_character(),
    ##   comments = col_character()
    ## )
    ## 
    ## [[27]]
    ## cols(
    ##   filename = col_character(),
    ##   datetime = col_character(),
    ##   serial = col_character(),
    ##   site = col_character(),
    ##   interest = col_character(),
    ##   live.chicks = col_logical(),
    ##   class = col_logical(),
    ##   order = col_logical(),
    ##   family = col_logical(),
    ##   genus = col_logical(),
    ##   species = col_logical(),
    ##   common = col_logical(),
    ##   size = col_logical(),
    ##   comments = col_logical()
    ## )
    ## 
    ## [[28]]
    ## cols(
    ##   filename = col_character(),
    ##   datetime = col_character(),
    ##   serial = col_character(),
    ##   site = col_character(),
    ##   interest = col_character(),
    ##   live.chicks = col_double(),
    ##   class = col_character(),
    ##   order = col_character(),
    ##   family = col_character(),
    ##   genus = col_character(),
    ##   species = col_character(),
    ##   common = col_character(),
    ##   size = col_character(),
    ##   comments = col_logical()
    ## )
    ## 
    ## [[29]]
    ## cols(
    ##   filename = col_character(),
    ##   datetime = col_character(),
    ##   serial = col_character(),
    ##   site = col_character(),
    ##   interest = col_character(),
    ##   live.chicks = col_double(),
    ##   class = col_character(),
    ##   order = col_character(),
    ##   family = col_character(),
    ##   genus = col_character(),
    ##   species = col_character(),
    ##   common = col_character(),
    ##   size = col_character(),
    ##   comments = col_logical(),
    ##   sex = col_character()
    ## )
    ## 
    ## [[30]]
    ## cols(
    ##   filename = col_character(),
    ##   datetime = col_character(),
    ##   serial = col_character(),
    ##   site = col_character(),
    ##   interest = col_character(),
    ##   live.chicks = col_double(),
    ##   class = col_logical(),
    ##   order = col_logical(),
    ##   family = col_logical(),
    ##   genus = col_logical(),
    ##   species = col_logical(),
    ##   common = col_logical(),
    ##   size = col_logical(),
    ##   comments = col_logical(),
    ##   sex = col_character()
    ## )

Well, that explains why I'm having so much trouble importing them! What a mess!

``` r
df <- list.files('../data/raw', pattern='photos*', full.names=TRUE) %>%
  map_df(~read_csv(., col_types=cols(
  filename = col_character(),
  datetime = col_character(),
  serial = col_character(),
  site = col_character(),
  interest = col_character(),
  live.chicks = col_integer(),
  class = col_character(),
  order = col_character(),
  family = col_character(),
  genus = col_character(),
  species = col_character(),
  common = col_character(),
  size = col_character(),
  comments = col_character(),
  sex = col_character()
)))

str(df)
```

    ## Classes 'spec_tbl_df', 'tbl_df', 'tbl' and 'data.frame': 26577 obs. of  15 variables:
    ##  $ filename   : chr  "RCNX0001.JPG" "RCNX0002.JPG" "RCNX0003.JPG" "RCNX0004.JPG" ...
    ##  $ datetime   : chr  "2019-06-11 12:19:15" "2019-06-11 12:19:16" "2019-06-11 12:19:17" "2019-06-11 12:25:53" ...
    ##  $ serial     : chr  "UXP9BM02019752" "UXP9BM02019752" "UXP9BM02019752" "UXP9BM02019752" ...
    ##  $ site       : chr  "MTC" "MTC" "MTC" "MTC" ...
    ##  $ interest   : chr  NA NA NA NA ...
    ##  $ live.chicks: int  NA NA NA NA NA NA 1 NA NA 2 ...
    ##  $ class      : chr  NA NA NA NA ...
    ##  $ order      : chr  NA NA NA NA ...
    ##  $ family     : chr  NA NA NA NA ...
    ##  $ genus      : chr  NA NA NA NA ...
    ##  $ species    : chr  NA NA NA NA ...
    ##  $ common     : chr  NA NA NA NA ...
    ##  $ size       : chr  NA NA NA NA ...
    ##  $ comments   : chr  NA NA NA NA ...
    ##  $ sex        : chr  NA NA NA NA ...

The "interest" column is to designate photos thaat have something interesting going on, specifically deliveries and new prey.

``` r
distinct(df, interest)
```

    ## # A tibble: 7 x 1
    ##   interest
    ##   <chr>   
    ## 1 <NA>    
    ## 2 visit   
    ## 3 newprey 
    ## 4 delivery
    ## 5 cool    
    ## 6 prey    
    ## 7 fledge

Looks like there aren't any typos in that column to clean up. Let's check for typos in the other columns.

``` r
distinct(df, class)
```

    ## # A tibble: 4 x 1
    ##   class   
    ##   <chr>   
    ## 1 <NA>    
    ## 2 Aves    
    ## 3 U       
    ## 4 Mammalia

``` r
distinct(df, order)
```

    ## # A tibble: 9 x 1
    ##   order        
    ##   <chr>        
    ## 1 <NA>         
    ## 2 Passeriformes
    ## 3 U            
    ## 4 Rodentia     
    ## 5 Galliformes  
    ## 6 Columbiformes
    ## 7 Chiroptera   
    ## 8 Piciformes   
    ## 9 Lagomorpha

``` r
distinct(df, family)
```

    ## # A tibble: 14 x 1
    ##    family          
    ##    <chr>           
    ##  1 <NA>            
    ##  2 Turdidae        
    ##  3 U               
    ##  4 Sciuridae       
    ##  5 Phasianidae     
    ##  6 Muridae         
    ##  7 Corvidae        
    ##  8 Dendragapus     
    ##  9 Neotoma         
    ## 10 Columbidae      
    ## 11 Vespertilionidae
    ## 12 Picidae         
    ## 13 Sciruidae       
    ## 14 Leporidae

``` r
distinct(df, genus)
```

    ## # A tibble: 19 x 1
    ##    genus       
    ##    <chr>       
    ##  1 <NA>        
    ##  2 Turdus      
    ##  3 U           
    ##  4 Catharus    
    ##  5 Tamasciurus 
    ##  6 Dendragapus 
    ##  7 Glaucomys   
    ##  8 Rattus      
    ##  9 Ixoreus     
    ## 10 Neotamias   
    ## 11 Cyanocitta  
    ## 12 Perisoreus  
    ## 13 Phasianidae 
    ## 14 Cricetidae  
    ## 15 Patagoienas 
    ## 16 Myotis      
    ## 17 Tamiasciurus
    ## 18 Bonasa      
    ## 19 Lepus

``` r
distinct(df, species)
```

    ## # A tibble: 18 x 1
    ##    species    
    ##    <chr>      
    ##  1 <NA>       
    ##  2 migratorius
    ##  3 U          
    ##  4 ustulatus  
    ##  5 douglasii  
    ##  6 fulignosus 
    ##  7 sabrinus   
    ##  8 sp         
    ##  9 naevius    
    ## 10 unknown    
    ## 11 stelleri   
    ## 12 canadensis 
    ## 13 cinerea    
    ## 14 fasciata   
    ## 15 spp        
    ## 16 hudsonicus 
    ## 17 umbellus   
    ## 18 americanus

``` r
distinct(df, common)
```

    ## # A tibble: 25 x 1
    ##    common                  
    ##    <chr>                   
    ##  1 <NA>                    
    ##  2 American.robin          
    ##  3 unknown                 
    ##  4 Swainsons.thrush        
    ##  5 U                       
    ##  6 Douglas.squirrel        
    ##  7 sooty.grouse            
    ##  8 douglas.squirrel        
    ##  9 northern.flying.squirrel
    ## 10 Rat                     
    ## # … with 15 more rows

``` r
distinct(df, size)
```

    ## # A tibble: 5 x 1
    ##   size 
    ##   <chr>
    ## 1 <NA> 
    ## 2 M    
    ## 3 S    
    ## 4 U    
    ## 5 L

So Tamiasciurus is mispelled Tamasciurus, Sciuridae is mispelled Sciruidae, and I'd like to clean up the species and common names.

``` r
# Fix genus.
df <- df %>% 
  mutate(genus = replace(genus, genus=='Tamasciurus', 'Tamiasciurus')) %>%
  mutate(genus = replace(genus, genus=='U', 'Unknown'))

# Fix family.
df <- df %>% 
  mutate(family = replace(family, family=='Sciruidae', 'Sciuridae')) %>%
  mutate(family = replace(family, family=='U', 'Unknown'))

# Let's do order, too.
df <- df %>% 
  mutate(order = replace(order, order=='U', 'Unknown'))

# Also class.
df <- df %>% 
  mutate(class = replace(class, class=='U', 'Unknown'))

# Make sizes prettier.
df <- df %>% mutate(size=
  case_when(size == 'S' ~ 'Small',
            size == 'M' ~ 'Medium',
            size == 'L' ~ 'Large',
            size == 'U' ~ 'Unknown',
            TRUE ~ NA_character_))

# Species names.
df <- df %>% mutate(species=
  case_when(species == 'spp' ~ 'sp',
            species == 'U' ~ 'unknown',
            TRUE ~ species))

# And finally common names.
df <- df %>% mutate(common=
  case_when(common == 'American.robin' ~ 'American robin',
            common == 'american.robin' ~ 'American robin',
            common == 'Swainsons.thrush' ~ 'Swainson\'s thrush',
            common == 'swainsons.thrush' ~ 'Swainson\'s thrush',
            common == 'varied.thrush' ~ 'varied thrush',
            common == 'Varied.thrush' ~ 'varied thrush',
            common == 'stellers.jay' ~ 'Steller\'s jay',
            common == 'gray.jay' ~ 'gray jay',
            common == 'band-tailed.pigeon' ~ 'band-tailed pigeon',
            common == 'bad-tailed.pigeon' ~ 'band-tailed pigeon',
            common == 'U' ~ 'unknown',
            common == 'Unknown' ~ 'unknown',
            common == 'Douglas.squirrel' ~ 'Douglas squirrel',
            common == 'douglas.squirrel' ~ 'Douglas squirrel',
            common == 'bushy-tailed.woodrat' ~ 'bushy-tailed woodrat',
            common == 'chimpunk' ~ 'chipmunk',
            common == 'sooty.grouse' ~ 'sooty grouse',
            common == 'ruffed.grouse' ~ 'ruffed grouse',
            common == 'northern.flying.squirrel' ~ 'flying squirrel',
            common == 'Rat' ~ 'rat',
            common == 'red.squirrel' ~ 'red squirrel',
            common == 'snowshoe.hare' ~ 'snowshoe hare',
            TRUE ~ common))
```

Ok, so now that it's pretty, I'll check for missing data. In particular, I suspect some items may be missing size.

``` r
# Filter out only observations of prey.
prey <- df %>% filter(!is.na(class))

# Check if any are missing size values.
prey %>% filter(is.na(size))
```

    ## # A tibble: 1 x 15
    ##   filename datetime serial site  interest live.chicks class order family genus
    ##   <chr>    <chr>    <chr>  <chr> <chr>          <int> <chr> <chr> <chr>  <chr>
    ## 1 RCNX532… 2019-07… UXP9B… UTZ   delivery           1 Mamm… Lago… Lepor… Lepus
    ## # … with 5 more variables: species <chr>, common <chr>, size <chr>,
    ## #   comments <chr>, sex <chr>

Only one! And it's a snowshoe hare, so it's super easy to fix.

``` r
# Make the correction.
df$size[df$genus=='Lepus'] <- 'Large'

# And check.
df %>% filter(!is.na(class)) %>%
  filter(is.na(size))
```

    ## # A tibble: 0 x 15
    ## # … with 15 variables: filename <chr>, datetime <chr>, serial <chr>,
    ## #   site <chr>, interest <chr>, live.chicks <int>, class <chr>, order <chr>,
    ## #   family <chr>, genus <chr>, species <chr>, common <chr>, size <chr>,
    ## #   comments <chr>, sex <chr>

Yes! It worked! The last thing I need to check is whether there are any deliveries that have no data at all.

``` r
# Number of prey deliveried to nest.
df %>% filter(interest %in% c('newprey', 'delivery')) %>%
  summarize(n())
```

    ## # A tibble: 1 x 1
    ##   `n()`
    ##   <int>
    ## 1   268

``` r
# Number of prey identified.
df %>% filter(!is.na(class)) %>%
  summarize(n())
```

    ## # A tibble: 1 x 1
    ##   `n()`
    ##   <int>
    ## 1   260

That's a difference of 8... I'm not sure I can be bothered with that, given how much trouble it would be to track them down... and I can almost guarantee that they'll be "unknown" all the way across. So I think I'll just leave it at that.

Then my last step is to write this out as a csv.

``` r
write.csv(df, '../data/interim/camera_data.csv')
```
