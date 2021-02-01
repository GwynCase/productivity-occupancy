Multivariate desperation
================

Playing with multivariate models that probs won’t work but oh well.

``` r
# Import conflict settings.
source('../src/conflicted.R')

# Load some libraries.
library(tidyverse)
library(ggplot2)
library(raster)
library(sf)
library(landscapemetrics)
library(knitr)
library(kableExtra)
library(GGally)
library(broom)
library(QuantPsyc)

# Load in the processed data from last time.
data <- read_csv('../data/processed/landscape_metrics_index.csv')

# Remove problematic TCR.
df <- data %>% filter(site != 'TCR')
```

What are some candidate models?

  - Proportion suitable
  - Proportion suitable + suitable edge density
  - Proportion mature
  - Proportion mature + landcover diversity
  - Proportion mature + gap edge density

The problem is that with four spatial scales, these escalate quickly.
Each bivariate models has… what? 16 possible combinations? With three
bivariate models and two univariates that’s 56 models. Which is insane.

Let’s stick to one-size-at-a-time for now.

# The models

``` r
# Proportion suitable
proportion.suitable.model <- function(df) {
  glm(cbind(years.detect, years.no.detect) ~ proportion.suitable, data=df, family=binomial(logit))
}

# Proportion suitable + suitable edge density
suitable.edge.density.model <- function(df) {
  glm(cbind(years.detect, years.no.detect) ~ proportion.suitable + suitable.edge.density, data=df, family=binomial(logit))
}

# Proportion mature forest
proportion.mature.model <- function(df) {
  glm(cbind(years.detect, years.no.detect) ~ proportion.cover.mature, data=df, family=binomial(logit))
}

# Proportion mature + landcover diversity
mature.diversity.model <- function(df) {
  glm(cbind(years.detect, years.no.detect) ~ proportion.cover.mature + cover.diversity, data=df, family=binomial(logit))
}

# Proportion mature + gap edge density
mature.edge.density.model <- function(df) {
  glm(cbind(years.detect, years.no.detect) ~ proportion.cover.mature + gap.edge.density, data=df, family=binomial(logit))
}
```

Then nest the data frame for ease of coding and apply each function to
generate a disturbingly large number of models really fast.

``` r
# Nest the data frame.
nf <- df %>% group_by(size) %>% nest()

# Apply the functions.
nf <- nf %>% 
  mutate(
    m.proportion.suitable=map(data, proportion.suitable.model),
    m.suitable.edge.density=map(data, suitable.edge.density.model),
    m.proportion.mature=map(data, proportion.mature.model),
    m.mature.diversity=map(data, mature.diversity.model),
    m.mature.edge.density=map(data, mature.edge.density.model)
         )
```

Now let’s assess.

``` r
nf.long <- nf %>% 
  pivot_longer(-c(size, data), names_to='modname', values_to='model') %>% 
  mutate(glance=map(model, glance))

# Take a look.
nf.long %>%
  unnest(glance) %>% 
  select(-model, -data) %>% 
  arrange(AIC) %>% 
  kable() %>% kable_styling(bootstrap_options=c('striped'))
```

<table class="table table-striped" style="margin-left: auto; margin-right: auto;">

<thead>

<tr>

<th style="text-align:left;">

size

</th>

<th style="text-align:left;">

modname

</th>

<th style="text-align:right;">

null.deviance

</th>

<th style="text-align:right;">

df.null

</th>

<th style="text-align:right;">

logLik

</th>

<th style="text-align:right;">

AIC

</th>

<th style="text-align:right;">

BIC

</th>

<th style="text-align:right;">

deviance

</th>

<th style="text-align:right;">

df.residual

</th>

</tr>

</thead>

<tbody>

<tr>

<td style="text-align:left;">

breeding area

</td>

<td style="text-align:left;">

m.mature.edge.density

</td>

<td style="text-align:right;">

48.83562

</td>

<td style="text-align:right;">

42

</td>

<td style="text-align:right;">

\-50.24146

</td>

<td style="text-align:right;">

106.4829

</td>

<td style="text-align:right;">

111.7665

</td>

<td style="text-align:right;">

43.53585

</td>

<td style="text-align:right;">

40

</td>

</tr>

<tr>

<td style="text-align:left;">

PFA

</td>

<td style="text-align:left;">

m.mature.edge.density

</td>

<td style="text-align:right;">

48.83562

</td>

<td style="text-align:right;">

42

</td>

<td style="text-align:right;">

\-50.56552

</td>

<td style="text-align:right;">

107.1310

</td>

<td style="text-align:right;">

112.4146

</td>

<td style="text-align:right;">

44.18399

</td>

<td style="text-align:right;">

40

</td>

</tr>

<tr>

<td style="text-align:left;">

breeding area

</td>

<td style="text-align:left;">

m.proportion.suitable

</td>

<td style="text-align:right;">

48.83562

</td>

<td style="text-align:right;">

42

</td>

<td style="text-align:right;">

\-52.57163

</td>

<td style="text-align:right;">

109.1433

</td>

<td style="text-align:right;">

112.6657

</td>

<td style="text-align:right;">

48.19620

</td>

<td style="text-align:right;">

41

</td>

</tr>

<tr>

<td style="text-align:left;">

maximum range

</td>

<td style="text-align:left;">

m.proportion.mature

</td>

<td style="text-align:right;">

48.83562

</td>

<td style="text-align:right;">

42

</td>

<td style="text-align:right;">

\-52.67848

</td>

<td style="text-align:right;">

109.3570

</td>

<td style="text-align:right;">

112.8794

</td>

<td style="text-align:right;">

48.40990

</td>

<td style="text-align:right;">

41

</td>

</tr>

<tr>

<td style="text-align:left;">

home range

</td>

<td style="text-align:left;">

m.proportion.mature

</td>

<td style="text-align:right;">

48.83562

</td>

<td style="text-align:right;">

42

</td>

<td style="text-align:right;">

\-52.68594

</td>

<td style="text-align:right;">

109.3719

</td>

<td style="text-align:right;">

112.8943

</td>

<td style="text-align:right;">

48.42483

</td>

<td style="text-align:right;">

41

</td>

</tr>

<tr>

<td style="text-align:left;">

breeding area

</td>

<td style="text-align:left;">

m.proportion.mature

</td>

<td style="text-align:right;">

48.83562

</td>

<td style="text-align:right;">

42

</td>

<td style="text-align:right;">

\-52.88043

</td>

<td style="text-align:right;">

109.7609

</td>

<td style="text-align:right;">

113.2833

</td>

<td style="text-align:right;">

48.81381

</td>

<td style="text-align:right;">

41

</td>

</tr>

<tr>

<td style="text-align:left;">

maximum range

</td>

<td style="text-align:left;">

m.proportion.suitable

</td>

<td style="text-align:right;">

48.83562

</td>

<td style="text-align:right;">

42

</td>

<td style="text-align:right;">

\-52.88308

</td>

<td style="text-align:right;">

109.7662

</td>

<td style="text-align:right;">

113.2886

</td>

<td style="text-align:right;">

48.81911

</td>

<td style="text-align:right;">

41

</td>

</tr>

<tr>

<td style="text-align:left;">

PFA

</td>

<td style="text-align:left;">

m.proportion.mature

</td>

<td style="text-align:right;">

48.83562

</td>

<td style="text-align:right;">

42

</td>

<td style="text-align:right;">

\-52.88489

</td>

<td style="text-align:right;">

109.7698

</td>

<td style="text-align:right;">

113.2922

</td>

<td style="text-align:right;">

48.82273

</td>

<td style="text-align:right;">

41

</td>

</tr>

<tr>

<td style="text-align:left;">

PFA

</td>

<td style="text-align:left;">

m.proportion.suitable

</td>

<td style="text-align:right;">

48.83562

</td>

<td style="text-align:right;">

42

</td>

<td style="text-align:right;">

\-52.88554

</td>

<td style="text-align:right;">

109.7711

</td>

<td style="text-align:right;">

113.2935

</td>

<td style="text-align:right;">

48.82403

</td>

<td style="text-align:right;">

41

</td>

</tr>

<tr>

<td style="text-align:left;">

home range

</td>

<td style="text-align:left;">

m.proportion.suitable

</td>

<td style="text-align:right;">

48.83562

</td>

<td style="text-align:right;">

42

</td>

<td style="text-align:right;">

\-52.89131

</td>

<td style="text-align:right;">

109.7826

</td>

<td style="text-align:right;">

113.3050

</td>

<td style="text-align:right;">

48.83556

</td>

<td style="text-align:right;">

41

</td>

</tr>

<tr>

<td style="text-align:left;">

maximum range

</td>

<td style="text-align:left;">

m.suitable.edge.density

</td>

<td style="text-align:right;">

48.83562

</td>

<td style="text-align:right;">

42

</td>

<td style="text-align:right;">

\-51.89750

</td>

<td style="text-align:right;">

109.7950

</td>

<td style="text-align:right;">

115.0786

</td>

<td style="text-align:right;">

46.84795

</td>

<td style="text-align:right;">

40

</td>

</tr>

<tr>

<td style="text-align:left;">

maximum range

</td>

<td style="text-align:left;">

m.mature.edge.density

</td>

<td style="text-align:right;">

48.83562

</td>

<td style="text-align:right;">

42

</td>

<td style="text-align:right;">

\-52.22933

</td>

<td style="text-align:right;">

110.4587

</td>

<td style="text-align:right;">

115.7423

</td>

<td style="text-align:right;">

47.51160

</td>

<td style="text-align:right;">

40

</td>

</tr>

<tr>

<td style="text-align:left;">

PFA

</td>

<td style="text-align:left;">

m.suitable.edge.density

</td>

<td style="text-align:right;">

48.83562

</td>

<td style="text-align:right;">

42

</td>

<td style="text-align:right;">

\-52.39183

</td>

<td style="text-align:right;">

110.7837

</td>

<td style="text-align:right;">

116.0673

</td>

<td style="text-align:right;">

47.83660

</td>

<td style="text-align:right;">

40

</td>

</tr>

<tr>

<td style="text-align:left;">

home range

</td>

<td style="text-align:left;">

m.mature.diversity

</td>

<td style="text-align:right;">

48.83562

</td>

<td style="text-align:right;">

42

</td>

<td style="text-align:right;">

\-52.47012

</td>

<td style="text-align:right;">

110.9402

</td>

<td style="text-align:right;">

116.2238

</td>

<td style="text-align:right;">

47.99319

</td>

<td style="text-align:right;">

40

</td>

</tr>

<tr>

<td style="text-align:left;">

home range

</td>

<td style="text-align:left;">

m.mature.edge.density

</td>

<td style="text-align:right;">

48.83562

</td>

<td style="text-align:right;">

42

</td>

<td style="text-align:right;">

\-52.52230

</td>

<td style="text-align:right;">

111.0446

</td>

<td style="text-align:right;">

116.3282

</td>

<td style="text-align:right;">

48.09755

</td>

<td style="text-align:right;">

40

</td>

</tr>

<tr>

<td style="text-align:left;">

breeding area

</td>

<td style="text-align:left;">

m.suitable.edge.density

</td>

<td style="text-align:right;">

48.83562

</td>

<td style="text-align:right;">

42

</td>

<td style="text-align:right;">

\-52.57091

</td>

<td style="text-align:right;">

111.1418

</td>

<td style="text-align:right;">

116.4254

</td>

<td style="text-align:right;">

48.19477

</td>

<td style="text-align:right;">

40

</td>

</tr>

<tr>

<td style="text-align:left;">

home range

</td>

<td style="text-align:left;">

m.suitable.edge.density

</td>

<td style="text-align:right;">

48.83562

</td>

<td style="text-align:right;">

42

</td>

<td style="text-align:right;">

\-52.57886

</td>

<td style="text-align:right;">

111.1577

</td>

<td style="text-align:right;">

116.4413

</td>

<td style="text-align:right;">

48.21067

</td>

<td style="text-align:right;">

40

</td>

</tr>

<tr>

<td style="text-align:left;">

maximum range

</td>

<td style="text-align:left;">

m.mature.diversity

</td>

<td style="text-align:right;">

48.83562

</td>

<td style="text-align:right;">

42

</td>

<td style="text-align:right;">

\-52.60110

</td>

<td style="text-align:right;">

111.2022

</td>

<td style="text-align:right;">

116.4858

</td>

<td style="text-align:right;">

48.25514

</td>

<td style="text-align:right;">

40

</td>

</tr>

<tr>

<td style="text-align:left;">

breeding area

</td>

<td style="text-align:left;">

m.mature.diversity

</td>

<td style="text-align:right;">

48.83562

</td>

<td style="text-align:right;">

42

</td>

<td style="text-align:right;">

\-52.88043

</td>

<td style="text-align:right;">

111.7609

</td>

<td style="text-align:right;">

117.0445

</td>

<td style="text-align:right;">

48.81380

</td>

<td style="text-align:right;">

40

</td>

</tr>

<tr>

<td style="text-align:left;">

PFA

</td>

<td style="text-align:left;">

m.mature.diversity

</td>

<td style="text-align:right;">

48.83562

</td>

<td style="text-align:right;">

42

</td>

<td style="text-align:right;">

\-52.88450

</td>

<td style="text-align:right;">

111.7690

</td>

<td style="text-align:right;">

117.0526

</td>

<td style="text-align:right;">

48.82194

</td>

<td style="text-align:right;">

40

</td>

</tr>

</tbody>

</table>
