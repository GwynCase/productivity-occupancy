Occupancy
================

I have survey data in a couple of different forms from the South Coast and Vancouver Island, so I want to dig into them a little to see which ones have been surveyed in the past couple of years, and hopefully find a set that has been surveyed both years.

``` r
# Load up some libraries.
library('tidyverse')
library('lubridate')

# Vancouver Island 2018 survey data.
vi.2018 <- read_csv('../data/external/VI_surveydata_2018.csv')

# Vancouver Island 2019 survey data.
vi.2019 <- read_csv('../data/external/VI_surveydata_2019.csv')

# South Coast nest database (2013-2019).
sc.nests <- read_csv('../data/external/SC_nest_database.csv')
```

The VI data have some idiosynchrasies I dealt with in a previous notebook.

``` r
# Sites that were checked on Vancouver Island in 2018.
vi.checked.2018 <- vi.2018 %>%
  filter(NestAssess == 'Yes')

# Sites that were checked on Vancouver Island in 2019.
vi.checked.2019 <- vi.2019 %>% filter(`Nest  Assess` == 'Yes') %>% 
  mutate(`Site Name`=case_when(
    `Site Name` == 'Lukwa S' ~ 'Lukwa South',
    `Site Name` == 'Seymour River W' ~ 'Seymour River West',
    `Site Name` == 'Tsitika W' ~ 'Tsitika West',
    TRUE ~ `Site Name`
  ))

# Sites that were checked on the South Coast in 2018.
sc.checked.2018 <- sc.nests %>% filter(Occup2018 %in% c('Breeding', 'Not Occupied', 'Resident')) %>% 
  mutate(TerrName=case_when(
    TerrName == 'St.Vincent' ~ 'St. Vincent',
    TRUE ~ TerrName
  ))

# Sites that were checked on the South Coast in 2019.
sc.checked.2019 <- sc.nests %>% filter(Occup2019 %in% c('Breeding', 'Not Occupied', 'Resident'))

# Vancouver Island sites checked both years.
intersect(vi.checked.2018$SiteName, vi.checked.2019$`Site Name`)
```

    ## [1] "Goose Creek"  "Rona Loop"    "Lukwa South"  "Tsitika West" "Cook Creek"  
    ## [6] "Mahatta"      "China Beach"  "Taylor River" "Keta"

``` r
# South Coast sites checked both years.
intersect(sc.checked.2018$TerrName, sc.checked.2019$TerrName)
```

    ##  [1] "DewdneyCreek"   "Ford Mountain"  "MtHolden"       "Silver"        
    ##  [5] "Clowhom"        "Duck Lake"      "Freil"          "Giovanno"      
    ##  [9] "Granite Mtn"    "Haslam"         "Maurell Island" "McNair"        
    ## [13] "MtPearkes"      "Nanton"         "Osgood"         "Phantom"       
    ## [17] "Potlatch"       "Powell Daniels" "Ruby Lake"      "Skaiakos"      
    ## [21] "St. Vincent"    "Brohm"          "Dipper Creek"   "Jarvis"        
    ## [25] "Lillooette"     "Millars Pond"   "Turbid"         "Wedge Creek"

Great! That's not a bad set. Of course, I know most of them were unoccupied, so that sucks, but let's put them together in a nice table anyway.

Start with South Coast, because the data is a little tidier to start with.

``` r
# Take the intersection, make pretty site names.
sc.sites <- intersect(sc.checked.2018$TerrName, sc.checked.2019$TerrName) %>% 
  data.frame() %>% rename(old.name=1) %>% 
  mutate(old.name=as.character(old.name), site=case_when(
    old.name == 'DewdneyCreek' ~ 'Dewdney Creek',
    old.name == 'MtHolden' ~ 'Mt Holden',
    old.name == 'Granite Mtn' ~ 'Granite Mt',
    old.name == 'MtPearkes' ~ 'Mt Pearkes',
    TRUE ~ old.name
  ))

# Add occupancy info from 2018.
sc.sites <- sc.checked.2018 %>% dplyr::select(TerrName, Occup2018) %>% 
  right_join(sc.sites, by=c('TerrName'='old.name')) %>% 
  distinct() %>% 
  rename(old.name=TerrName, st.18=Occup2018)

# Add occupancy info from 2019.
sc.sites <- sc.checked.2019 %>% dplyr::select(TerrName, Occup2019) %>% 
  right_join(sc.sites, by=c('TerrName'='old.name')) %>% 
  distinct() %>% 
  rename(old.name=TerrName, st.19=Occup2019)

# Fix occupancy.
sc.sites <- sc.sites %>% 
  mutate_at(.vars=vars(st.19, st.18),
            .funs=funs(.=case_when(
              . == 'Not Occupied' ~ 1,
              . == 'Breeding' ~ 3,
              . == 'Resident' ~ 2,
              TRUE ~ NA_real_
            ))) %>% 
  dplyr::select(site, status.2018=st.18_., status.2019=st.19_., old.name)

sc.sites %>% dplyr::select(-old.name) %>% print()
```

    ## # A tibble: 28 x 3
    ##    site          status.2018 status.2019
    ##    <chr>               <dbl>       <dbl>
    ##  1 Dewdney Creek           3           1
    ##  2 Ford Mountain           1           3
    ##  3 Mt Holden               1           1
    ##  4 Silver                  2           1
    ##  5 Clowhom                 3           3
    ##  6 Duck Lake               1           1
    ##  7 Freil                   2           3
    ##  8 Giovanno                2           1
    ##  9 Granite Mt              1           3
    ## 10 Haslam                  1           1
    ## # … with 18 more rows

Looking good! Then on to Vancouver Island, which could be a bit trickier.

``` r
# Find the interestion of the two years.
vi.sites <- intersect(vi.checked.2018$SiteName, vi.checked.2019$`Site Name`) %>%   data.frame() %>% rename(site=1)

# Add nest status from 2018.
vi.sites <- vi.checked.2018 %>% dplyr::select(SiteName, NestStatus) %>% 
  right_join(vi.sites, by=c('SiteName'='site')) %>% 
  distinct() %>% rename(st.18=NestStatus) %>% 
  filter(st.18 != 'Not Found')

# Add nest status from 2019.
vi.sites <- vi.checked.2019 %>% dplyr::select(`Site Name`, `Nest Status`) %>% 
  right_join(vi.sites, by=c(`Site Name`='SiteName')) %>% 
  distinct() %>% rename(st.19=`Nest Status`) %>% 
  filter(st.19 != 'Not Found')

# Fix occupancy.
vi.sites <- vi.sites %>% 
  mutate_at(.vars=vars(st.19, st.18),
            .funs=funs(.=case_when(
              . == 'Inactive' ~ 1,
              . == 'Active' ~ 3,
              TRUE ~ NA_real_
            ))) %>% 
  dplyr::select(site=`Site Name`, status.2018=st.18_., status.2019=st.19_.)

vi.sites
```

    ## # A tibble: 9 x 3
    ##   site         status.2018 status.2019
    ##   <chr>              <dbl>       <dbl>
    ## 1 Goose Creek            1           1
    ## 2 Rona Loop              1           1
    ## 3 Lukwa South            1           1
    ## 4 Tsitika West           1           1
    ## 5 Cook Creek             1           1
    ## 6 Mahatta                1           1
    ## 7 China Beach            1           1
    ## 8 Taylor River           1           1
    ## 9 Keta                   3           1

So at this stage the VI data looks suspiciously useless, since none save one of the sites that were checked in both years were active. I know there are more sites that were active in one year or the other, but none of them were checked both years so I don't think they help me much.

There's one last thing to check, which is that some of these sites may actually have been inactive but had resident birds.

``` r
# In 2018?
vi.2018 %>% right_join(vi.sites, by=c('SiteName'='site')) %>% 
  dplyr::select(SiteName, TotalNumberOfGoshawks) %>% 
  drop_na() %>% distinct()
```

    ## # A tibble: 5 x 2
    ##   SiteName     TotalNumberOfGoshawks   
    ##   <chr>        <chr>                   
    ## 1 Tsitika West Not detected            
    ## 2 Mahatta      Not detected            
    ## 3 China Beach  Not detected            
    ## 4 China Beach  1 Adult Unclassified Sex
    ## 5 Keta         1 Fledged Juvenile

``` r
# And in 2019?
vi.2019 %>% right_join(vi.sites, by=c(`Site Name`='site')) %>% 
  dplyr::select(`Site Name`, `Total Number Of Goshawks`) %>% 
  drop_na() %>% distinct()
```

    ## # A tibble: 8 x 2
    ##   `Site Name`  `Total Number Of Goshawks`
    ##   <chr>        <chr>                     
    ## 1 Goose Creek  Not detected              
    ## 2 Rona Loop    Not detected              
    ## 3 Cook Creek   1 Adult                   
    ## 4 Cook Creek   Not detected              
    ## 5 Mahatta      Not detected              
    ## 6 China Beach  Not detected              
    ## 7 Taylor River Not detected              
    ## 8 Keta         Not detected

The active site we already had, but it turns out there are two sites with resident nonbreeders.

``` r
# Fix missed occupancy status.
vi.sites <- vi.sites %>% mutate(status.2018=case_when(
  site == 'China Beach' ~ 2,
  TRUE ~ status.2018
  ),
  status.2019=case_when(
    site == 'Cook Creek' ~ 2,
    TRUE ~ status.2019
  )
)
```

And I think that's a wrap. I'll just save these as csvs for later.

``` r
write_csv(vi.sites, '../data/processed/vi_occupancy_2018-2019.csv')
write_csv(sc.sites, '../data/processed/sc_occupancy_2018-2019.csv')
```
