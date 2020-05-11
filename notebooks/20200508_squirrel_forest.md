R Notebook
================

I'm going to try a really small, basic analysis to see if I have the hang of how this works: does forest type at the estimate Vancouver Island home range of 3700 ha, as mapped by BEC zone, predict proportion of squirrel biomass in the diet?

``` r
# Load some libraries.
library('raster')
library('tidyverse')
library('sf')
library('landscapemetrics')

# Bring in diet data.
df <- read_csv('../data/interim/camera_corrected.csv')
source('../src/prey_attributes.R')

# Calculate proportion of squirrel biomass per site.
squirrel.mass <- items %>% mutate(mass=as.numeric(mass)) %>% 
  group_by(site) %>% 
  mutate(total=sum(mass)) %>% 
  filter(genus == 'Tamiasciurus') %>% 
  mutate(amount.sq=sum(mass), prop.sq=amount.sq/total) %>% 
  select(site, prop.sq) %>% distinct()

squirrel.mass
```

    ## # A tibble: 6 x 2
    ## # Groups:   site [6]
    ##   site  prop.sq
    ##   <chr>   <dbl>
    ## 1 MTC     0.499
    ## 2 MTF     0.483
    ## 3 RLK     0.412
    ## 4 TCR     0.505
    ## 5 TMC     0.221
    ## 6 UTZ     0.485

Now I need to calculate proportion of each forest type for each site.

``` r
# Import BEC raster created previously.
bec.raster <- raster('../data/interim/bec_raster_SC.tif')

# Double-check that it's ok.
check_landscape(bec.raster)
```

    ##   layer       crs units   class n_classes       OK
    ## 1     1 projected     m integer        16 <U+2713>

Looks fine. Now I need to define the sample areas.

``` r
# Import data and get centroids for camera nests.
sites <- read_csv('../data/processed/the_big_list_of_nests.csv') %>% 
  group_by(name) %>% 
  mutate_at(c('lat', 'lon'), mean) %>% 
  mutate_at(vars(starts_with('status')), max) %>% 
  mutate_at(c('telemetry', 'cameras', 'remains'), max) %>% 
  select(-nest, -NOTES) %>% 
  distinct() %>% 
  filter(cameras > 0)

# Make site table a spatial object and make it UTMs.
sites.sf <- st_as_sf(sites, coords=c('lon', 'lat')) %>%
  st_set_crs('+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs') %>%
  st_transform("+proj=utm +zone=10 +datum=WGS84 +units=m +no_defs")

# Convert area in ha to radius in m
a.hr.ha <- 3700
r.hr.m <- sqrt(a.hr.ha*10000/pi)
```

Now I have to actually pick what metrics I'm going to use to represent "forest type". Dominant forest type and diversity seem like promising starting points for a quick test. Or I could do proportion of each forest type.

``` r
# Calculate area per class per site.
class.area <- sample_lsm(bec.raster, y=sites.sf, size=r.hr.m, what='lsm_c_ca', 
           shape='circle') %>% 
# Amend class number with BEC name.
  left_join(data.frame(levels(bec.raster)), by=c('class'='ID')) %>% 
# Reorganize by site.  
  pivot_wider(id_cols=plot_id, names_from=category, values_from=value, 
              values_fill=list(value=0))

# Amend with site names.
class.area <- select(sites, site) %>% rownames_to_column() %>% 
  mutate(rowname=as.integer(rowname)) %>% 
  right_join(class.area, by=c('rowname'='plot_id'))

class.area
```

    ## # A tibble: 6 x 12
    ## # Groups:   name [6]
    ##   rowname name  site  CWHds1 CWHms1 MHmm2 IDFww CWHdm CWHxm1 CMAunp ESSFmw1
    ##     <int> <chr> <chr>  <dbl>  <dbl> <dbl> <dbl> <dbl>  <dbl>  <dbl>   <dbl>
    ## 1       1 Moun… MTF     1372   1902   423     0     0      0      0       0
    ## 2       2 Mt C… MTC      565     98     0  3029     0      0      0       0
    ## 3       3 Ruby… RLK        0      0     0     0  2585   1106      0       0
    ## 4       4 Turb… TCR     2099   1355   176     0     0      0     70       0
    ## 5       5 Twen… TMC        0   2223   428     0     0      0    348     616
    ## 6       6 Utzi… UTZ     1196   1523     0   929     0      0      0      44
    ## # … with 1 more variable: IMAunp <dbl>

Umm yes, ok, very pretty data, but too many variables. Let's try summing the tranzition zone types.

``` r
class.area <- class.area %>% mutate(amt.tz=CWHds1+CWHms1)
```

That's a bit more reasonable. Let's try add the squirrel and try a model...

``` r
# Add proportion of biomass that is squirrel.
class.area <- left_join(class.area, squirrel.mass)

# Make a model.
sq.x.tz <- lm(prop.sq ~ amt.tz, data=class.area)

# Look at it.
summary(sq.x.tz)
```

    ## 
    ## Call:
    ## lm(formula = prop.sq ~ amt.tz, data = class.area)
    ## 
    ## Residuals:
    ##          1          2          3          4          5          6 
    ##  0.0354773  0.0802855  0.0006749  0.0551471 -0.2150785  0.0434937 
    ## 
    ## Coefficients:
    ##              Estimate Std. Error t value Pr(>|t|)  
    ## (Intercept) 4.112e-01  9.290e-02   4.427   0.0115 *
    ## amt.tz      1.120e-05  3.823e-05   0.293   0.7842  
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 0.1213 on 4 degrees of freedom
    ## Multiple R-squared:  0.021,  Adjusted R-squared:  -0.2238 
    ## F-statistic: 0.08578 on 1 and 4 DF,  p-value: 0.7842

Absolutely useless, which seems about right. What might be slightly more reasonable it proportion mammal ~ amount transition.

``` r
# Calculate proportion of mammalian biomass.
mammal.mass <- items %>% mutate(mass=as.numeric(mass)) %>% 
  group_by(site) %>% 
  mutate(total=sum(mass)) %>% 
  filter(class == 'Mammalia') %>% 
  mutate(amount.sq=sum(mass), prop.ml=amount.sq/total) %>% 
  select(site, prop.ml) %>% distinct()

# Add to the main data set.
class.area <- left_join(class.area, mammal.mass)

# Try another model.
ml.x.tz <- lm(prop.ml ~ amt.tz, data=class.area)

# Look at it.
summary(ml.x.tz)
```

    ## 
    ## Call:
    ## lm(formula = prop.ml ~ amt.tz, data = class.area)
    ## 
    ## Residuals:
    ##        1        2        3        4        5        6 
    ## -0.02170 -0.01927 -0.03319 -0.13487  0.03769  0.17134 
    ## 
    ## Coefficients:
    ##              Estimate Std. Error t value Pr(>|t|)   
    ## (Intercept) 5.715e-01  8.637e-02   6.617   0.0027 **
    ## amt.tz      4.593e-05  3.555e-05   1.292   0.2659   
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 0.1128 on 4 degrees of freedom
    ## Multiple R-squared:  0.2945, Adjusted R-squared:  0.1181 
    ## F-statistic:  1.67 on 1 and 4 DF,  p-value: 0.2659

Lol nope.

K so that's two negatives but it ruled some things out and was a nice test of how to use the package.
