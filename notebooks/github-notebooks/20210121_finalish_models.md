Finalish models, univariate edition
================

Again, basically redoing part of a previous notebook.

``` r
# Import conflict settings.
source('../src/conflicted.R')

#Load some libraries.
library(tidyverse)
library(vegan)
library(raster)
library(sf)
library(landscapemetrics)
library(broom)
library(knitr)
library(kableExtra)
library(GGally)
library(extrafont)
library(QuantPsyc)

# Load in the processed data from last time.
data <- read_csv('../data/processed/landscape_metrics.csv')
```

Now make all those models.

``` r
occupancy.by.bec.diversity <- data %>% 
  split(.$size) %>% 
  map(~glm(cbind(years.detect, years.no.detect) ~ bec.diversity, data=.x, 
           family=binomial(logit)))

occupancy.by.proportion.cover.mature <- data %>% 
  split(.$size) %>% 
  map(~glm(cbind(years.detect, years.no.detect) ~ proportion.cover.mature, data=.x, 
           family=binomial(logit)))

occupancy.by.cover.edge.density <- data %>% 
  split(.$size) %>% 
  map(~glm(cbind(years.detect, years.no.detect) ~ cover.edge.density, data=.x, 
           family=binomial(logit)))

occupancy.by.cover.contagion <- data %>% 
  split(.$size) %>% 
  map(~glm(cbind(years.detect, years.no.detect) ~ cover.contagion, data=.x, 
           family=binomial(logit)))

occupancy.by.cover.diversity <- data %>% 
  split(.$size) %>% 
  map(~glm(cbind(years.detect, years.no.detect) ~ cover.diversity, data=.x, 
           family=binomial(logit)))

occupancy.by.cover.richness <- data %>% 
  split(.$size) %>% 
  map(~glm(cbind(years.detect, years.no.detect) ~ cover.richness, data=.x, 
           family=binomial(logit)))

occupancy.by.cover.evenness <- data %>% 
  split(.$size) %>% 
  map(~glm(cbind(years.detect, years.no.detect) ~ cover.evenness, data=.x, 
           family=binomial(logit)))

occupancy.by.gap.edge.density <- data %>% 
  split(.$size) %>% 
  map(~glm(cbind(years.detect, years.no.detect) ~ gap.edge.density, data=.x, 
           family=binomial(logit)))

occupancy.by.canopy.cover <- data %>% 
  split(.$size) %>% 
  map(~glm(cbind(years.detect, years.no.detect) ~ canopy.high, data=.x, 
           family=binomial(logit)))

occupancy.by.hsi.edge.density <- data %>% 
  split(.$size) %>% 
  map(~glm(cbind(years.detect, years.no.detect) ~ hsi.edge.density, data=.x, 
           family=binomial(logit)))

occupancy.by.hsi.contagion <- data %>% 
  split(.$size) %>% 
  map(~glm(cbind(years.detect, years.no.detect) ~ hsi.contagion, data=.x, 
           family=binomial(logit)))

occupancy.by.proportion.suitable <- data %>% 
  split(.$size) %>% 
  map(~glm(cbind(years.detect, years.no.detect) ~ proportion.suitable, data=.x, 
           family=binomial(logit))) 

occupancy.by.suitable.edge.density <- data %>% 
  split(.$size) %>% 
  map(~glm(cbind(years.detect, years.no.detect) ~ suitable.edge.density, data=.x, 
           family=binomial(logit)))

# Bind it all together.
occupancy.models <- c(occupancy.by.bec.diversity, 
          occupancy.by.proportion.cover.mature,
          occupancy.by.cover.edge.density,
          occupancy.by.cover.contagion,
          occupancy.by.cover.diversity,
          occupancy.by.cover.evenness,
          occupancy.by.cover.richness,
          occupancy.by.gap.edge.density,
          occupancy.by.canopy.cover,
          occupancy.by.hsi.edge.density,
          occupancy.by.hsi.contagion,
          occupancy.by.proportion.suitable,
          occupancy.by.suitable.edge.density)
```

Make a beautiful table of p-values.

``` r
occupancy.models %>% map(tidy) %>% 
  bind_rows(.id='size') %>% 
  mutate(sig=round(p.value, digits=2)) %>% 
  mutate(sig=case_when(
    p.value < 0.05 ~ paste(as.character(sig), '*'),
    TRUE ~  paste(as.character(sig))
  )) %>% 
  filter(term != '(Intercept)') %>% 
  select(size, sig, term) %>% 
  pivot_wider(names_from=size, values_from=sig) %>% 
  select(term, PFA, everything()) %>% 
  kable() %>% kable_styling(bootstrap_options=c('striped'))
```

<table class="table table-striped" style="margin-left: auto; margin-right: auto;">

<thead>

<tr>

<th style="text-align:left;">

term

</th>

<th style="text-align:left;">

PFA

</th>

<th style="text-align:left;">

breeding area

</th>

<th style="text-align:left;">

home range

</th>

<th style="text-align:left;">

maximum range

</th>

</tr>

</thead>

<tbody>

<tr>

<td style="text-align:left;">

bec.diversity

</td>

<td style="text-align:left;">

0.21

</td>

<td style="text-align:left;">

0.62

</td>

<td style="text-align:left;">

0.49

</td>

<td style="text-align:left;">

0.26

</td>

</tr>

<tr>

<td style="text-align:left;">

proportion.cover.mature

</td>

<td style="text-align:left;">

0.87

</td>

<td style="text-align:left;">

0.9

</td>

<td style="text-align:left;">

0.83

</td>

<td style="text-align:left;">

0.84

</td>

</tr>

<tr>

<td style="text-align:left;">

cover.edge.density

</td>

<td style="text-align:left;">

0.29

</td>

<td style="text-align:left;">

0.17

</td>

<td style="text-align:left;">

0.4

</td>

<td style="text-align:left;">

0.47

</td>

</tr>

<tr>

<td style="text-align:left;">

cover.contagion

</td>

<td style="text-align:left;">

0.42

</td>

<td style="text-align:left;">

0.5

</td>

<td style="text-align:left;">

0.07

</td>

<td style="text-align:left;">

0.29

</td>

</tr>

<tr>

<td style="text-align:left;">

cover.diversity

</td>

<td style="text-align:left;">

0.72

</td>

<td style="text-align:left;">

0.75

</td>

<td style="text-align:left;">

0.66

</td>

<td style="text-align:left;">

0.79

</td>

</tr>

<tr>

<td style="text-align:left;">

cover.evenness

</td>

<td style="text-align:left;">

0.57

</td>

<td style="text-align:left;">

0.7

</td>

<td style="text-align:left;">

0.17

</td>

<td style="text-align:left;">

0.34

</td>

</tr>

<tr>

<td style="text-align:left;">

cover.richness

</td>

<td style="text-align:left;">

0.5

</td>

<td style="text-align:left;">

0.87

</td>

<td style="text-align:left;">

0.38

</td>

<td style="text-align:left;">

0.82

</td>

</tr>

<tr>

<td style="text-align:left;">

gap.edge.density

</td>

<td style="text-align:left;">

0.03 \*

</td>

<td style="text-align:left;">

0.03 \*

</td>

<td style="text-align:left;">

0.54

</td>

<td style="text-align:left;">

0.43

</td>

</tr>

<tr>

<td style="text-align:left;">

canopy.high

</td>

<td style="text-align:left;">

0.64

</td>

<td style="text-align:left;">

0.99

</td>

<td style="text-align:left;">

0.58

</td>

<td style="text-align:left;">

0.38

</td>

</tr>

<tr>

<td style="text-align:left;">

hsi.edge.density

</td>

<td style="text-align:left;">

0.81

</td>

<td style="text-align:left;">

0.95

</td>

<td style="text-align:left;">

0.61

</td>

<td style="text-align:left;">

0.26

</td>

</tr>

<tr>

<td style="text-align:left;">

hsi.contagion

</td>

<td style="text-align:left;">

0.85

</td>

<td style="text-align:left;">

0.75

</td>

<td style="text-align:left;">

0.27

</td>

<td style="text-align:left;">

0.35

</td>

</tr>

<tr>

<td style="text-align:left;">

proportion.suitable

</td>

<td style="text-align:left;">

0.83

</td>

<td style="text-align:left;">

0.89

</td>

<td style="text-align:left;">

0.68

</td>

<td style="text-align:left;">

0.6

</td>

</tr>

<tr>

<td style="text-align:left;">

suitable.edge.density

</td>

<td style="text-align:left;">

0.38

</td>

<td style="text-align:left;">

0.31

</td>

<td style="text-align:left;">

0.28

</td>

<td style="text-align:left;">

0.09

</td>

</tr>

</tbody>

</table>

And look closer at standardized effect sizes.

``` r
# Set some knitr options.
options(knitr.kable.NA = '-')

# Round up the significant terms at each scale.
sig.terms <- occupancy.models %>% map(tidy) %>% 
  bind_rows(.id='size') %>% 
  filter(p.value >= 0.05) %>% 
  filter(term != '(Intercept)')

# Standardize coefficients of significant terms.
occupancy.models %>% 
  map(lm.beta) %>% 
  bind_rows(.id='size') %>% 
  pivot_longer(!size, names_to='term', values_to='coef') %>%
  drop_na() %>% 
  anti_join(sig.terms, by=c('size', 'term')) %>% 
  pivot_wider(names_from=size, values_from=coef) %>% 
  select(term, PFA, `breeding area`, everything()) %>% 
  kable() %>% kable_styling(bootstrap_options=c('striped'))
```

<table class="table table-striped" style="margin-left: auto; margin-right: auto;">

<thead>

<tr>

<th style="text-align:left;">

term

</th>

<th style="text-align:right;">

PFA

</th>

<th style="text-align:right;">

breeding area

</th>

</tr>

</thead>

<tbody>

<tr>

<td style="text-align:left;">

gap.edge.density

</td>

<td style="text-align:right;">

\-0.2429436

</td>

<td style="text-align:right;">

\-0.2508171

</td>

</tr>

</tbody>

</table>
