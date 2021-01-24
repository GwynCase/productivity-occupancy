Return to reproductive success
================

With a larger sample size, how do environmental variables do with
reproductive success?

This isn’t a very sensible question. It makes more sense to ask whether
environmental variables affect diet, but I don’t have my full diet
dataset yet. Or to ask wether diet affects reproductive success, but I
don’t have my full diet dataset yet. Plus it looks like my environmental
variables are about to become useless if I can’t find something helpful
to do with them. So.

``` r
# Import conflict settings.
source('../src/conflicted.R')

# Load some libraries.
library(tidyverse)
library(lme4)
library(broom)
library(modelr)
library(MuMIn)
library(knitr)
library(kableExtra)

# Load in the habitat variables prepped earlier.
ls <- read_csv('../data/processed/landscape_metrics_full.csv')

# And load in the productivity data.
pd <- read_csv('../data/raw/productivity.csv')

# Join them together, keeping just sites with productivity data.
df <- left_join(pd, ls)
```

<!-- The first interesting this is that there is, in fact, high overlap between the productivity data and the occupancy data. This is because Gravell doesn't actually have productivity data, so only TMC is missing occupancy. So that's a very, very fast analysis I can do. -->

Create a bunch of functions for creating a bunch of models.

``` r
# Cover diversity
landcover.model <- function(df) {
  lmer(n.fledge ~ cover.diversity + (1|site), data=df)
}

# High canopy cover
canopy.model <- function(df) {
  lmer(n.fledge ~ canopy.high + (1|site), data=df)
}

# BEC diversity
bec.model <- function(df) {
  lmer(n.fledge ~ bec.diversity + (1|site), data=df)
}

# Proportion suitable
suitable.model <- function(df) {
  lmer(n.fledge ~ proportion.suitable + (1|site), data=df)
}

# Gap edge density
gap.model <- function(df) {
  lmer(n.fledge ~ gap.edge.density + (1|site), data=df)
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
    landcover=map(data, landcover.model),
    canopy=map(data, canopy.model),
    bec=map(data, bec.model),
    suitable=map(data, suitable.model),
    gap=map(data, gap.model)
         )
```

Aaaaaaand that generates a ton of “singular” error messages. God knows.

I’ll make it long and add some model quality assessors.

``` r
# Twist and assess.
nf.long <- nf %>% 
  pivot_longer(-c(size, data), names_to='modname', values_to='model') %>% 
  mutate(glance=map(model, glance)) %>% 
  mutate(rsq=map(model, r.squaredGLMM)) %>% 
  mutate(rsq=map(rsq, as.data.frame))

# Take a look.
nf.long %>% unnest(glance) %>% 
  unnest(rsq) %>% 
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

sigma

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

<th style="text-align:right;">

R2m

</th>

<th style="text-align:right;">

R2c

</th>

</tr>

</thead>

<tbody>

<tr>

<td style="text-align:left;">

home range

</td>

<td style="text-align:left;">

landcover

</td>

<td style="text-align:right;">

0.7970903

</td>

<td style="text-align:right;">

\-10.98782

</td>

<td style="text-align:right;">

29.97563

</td>

<td style="text-align:right;">

31.56722

</td>

<td style="text-align:right;">

24.01995

</td>

<td style="text-align:right;">

7

</td>

<td style="text-align:right;">

0.1152069

</td>

<td style="text-align:right;">

0.1152069

</td>

</tr>

<tr>

<td style="text-align:left;">

maximum range

</td>

<td style="text-align:left;">

landcover

</td>

<td style="text-align:right;">

0.8445074

</td>

<td style="text-align:right;">

\-11.25856

</td>

<td style="text-align:right;">

30.51713

</td>

<td style="text-align:right;">

32.10871

</td>

<td style="text-align:right;">

25.29123

</td>

<td style="text-align:right;">

7

</td>

<td style="text-align:right;">

0.0174579

</td>

<td style="text-align:right;">

0.0174579

</td>

</tr>

<tr>

<td style="text-align:left;">

maximum range

</td>

<td style="text-align:left;">

bec

</td>

<td style="text-align:right;">

0.8406860

</td>

<td style="text-align:right;">

\-11.67012

</td>

<td style="text-align:right;">

31.34023

</td>

<td style="text-align:right;">

32.93181

</td>

<td style="text-align:right;">

25.19145

</td>

<td style="text-align:right;">

7

</td>

<td style="text-align:right;">

0.0254652

</td>

<td style="text-align:right;">

0.0254652

</td>

</tr>

<tr>

<td style="text-align:left;">

home range

</td>

<td style="text-align:left;">

bec

</td>

<td style="text-align:right;">

0.8521226

</td>

<td style="text-align:right;">

\-11.91073

</td>

<td style="text-align:right;">

31.82146

</td>

<td style="text-align:right;">

33.41304

</td>

<td style="text-align:right;">

25.48872

</td>

<td style="text-align:right;">

7

</td>

<td style="text-align:right;">

0.0014356

</td>

<td style="text-align:right;">

0.0014356

</td>

</tr>

<tr>

<td style="text-align:left;">

PFA

</td>

<td style="text-align:left;">

bec

</td>

<td style="text-align:right;">

0.8515407

</td>

<td style="text-align:right;">

\-12.05342

</td>

<td style="text-align:right;">

32.10685

</td>

<td style="text-align:right;">

33.69843

</td>

<td style="text-align:right;">

25.47369

</td>

<td style="text-align:right;">

7

</td>

<td style="text-align:right;">

0.0026629

</td>

<td style="text-align:right;">

0.0026629

</td>

</tr>

<tr>

<td style="text-align:left;">

PFA

</td>

<td style="text-align:left;">

landcover

</td>

<td style="text-align:right;">

0.8475304

</td>

<td style="text-align:right;">

\-12.16213

</td>

<td style="text-align:right;">

32.32426

</td>

<td style="text-align:right;">

33.91584

</td>

<td style="text-align:right;">

25.36984

</td>

<td style="text-align:right;">

7

</td>

<td style="text-align:right;">

0.0111078

</td>

<td style="text-align:right;">

0.0111078

</td>

</tr>

<tr>

<td style="text-align:left;">

breeding area

</td>

<td style="text-align:left;">

bec

</td>

<td style="text-align:right;">

0.8521875

</td>

<td style="text-align:right;">

\-12.25374

</td>

<td style="text-align:right;">

32.50748

</td>

<td style="text-align:right;">

34.09906

</td>

<td style="text-align:right;">

25.49040

</td>

<td style="text-align:right;">

7

</td>

<td style="text-align:right;">

0.0012986

</td>

<td style="text-align:right;">

0.0012986

</td>

</tr>

<tr>

<td style="text-align:left;">

breeding area

</td>

<td style="text-align:left;">

landcover

</td>

<td style="text-align:right;">

0.8521508

</td>

<td style="text-align:right;">

\-12.28148

</td>

<td style="text-align:right;">

32.56296

</td>

<td style="text-align:right;">

34.15454

</td>

<td style="text-align:right;">

25.48945

</td>

<td style="text-align:right;">

7

</td>

<td style="text-align:right;">

0.0013760

</td>

<td style="text-align:right;">

0.0013760

</td>

</tr>

<tr>

<td style="text-align:left;">

maximum range

</td>

<td style="text-align:left;">

canopy

</td>

<td style="text-align:right;">

0.7669576

</td>

<td style="text-align:right;">

\-13.83455

</td>

<td style="text-align:right;">

35.66910

</td>

<td style="text-align:right;">

37.26068

</td>

<td style="text-align:right;">

23.17215

</td>

<td style="text-align:right;">

7

</td>

<td style="text-align:right;">

0.1754269

</td>

<td style="text-align:right;">

0.1754269

</td>

</tr>

<tr>

<td style="text-align:left;">

home range

</td>

<td style="text-align:left;">

canopy

</td>

<td style="text-align:right;">

0.8125618

</td>

<td style="text-align:right;">

\-14.56471

</td>

<td style="text-align:right;">

37.12943

</td>

<td style="text-align:right;">

38.72101

</td>

<td style="text-align:right;">

24.66720

</td>

<td style="text-align:right;">

7

</td>

<td style="text-align:right;">

0.0683694

</td>

<td style="text-align:right;">

0.0872097

</td>

</tr>

<tr>

<td style="text-align:left;">

maximum range

</td>

<td style="text-align:left;">

gap

</td>

<td style="text-align:right;">

0.6867231

</td>

<td style="text-align:right;">

\-14.59044

</td>

<td style="text-align:right;">

37.18087

</td>

<td style="text-align:right;">

38.77245

</td>

<td style="text-align:right;">

22.73834

</td>

<td style="text-align:right;">

7

</td>

<td style="text-align:right;">

0.2175312

</td>

<td style="text-align:right;">

0.3491489

</td>

</tr>

<tr>

<td style="text-align:left;">

home range

</td>

<td style="text-align:left;">

gap

</td>

<td style="text-align:right;">

0.7780373

</td>

<td style="text-align:right;">

\-14.86615

</td>

<td style="text-align:right;">

37.73229

</td>

<td style="text-align:right;">

39.32387

</td>

<td style="text-align:right;">

23.48769

</td>

<td style="text-align:right;">

7

</td>

<td style="text-align:right;">

0.1534620

</td>

<td style="text-align:right;">

0.1534620

</td>

</tr>

<tr>

<td style="text-align:left;">

PFA

</td>

<td style="text-align:left;">

suitable

</td>

<td style="text-align:right;">

0.8602586

</td>

<td style="text-align:right;">

\-15.01004

</td>

<td style="text-align:right;">

38.02007

</td>

<td style="text-align:right;">

39.23041

</td>

<td style="text-align:right;">

23.13689

</td>

<td style="text-align:right;">

6

</td>

<td style="text-align:right;">

0.0262632

</td>

<td style="text-align:right;">

0.0262632

</td>

</tr>

<tr>

<td style="text-align:left;">

PFA

</td>

<td style="text-align:left;">

gap

</td>

<td style="text-align:right;">

0.8376815

</td>

<td style="text-align:right;">

\-15.44816

</td>

<td style="text-align:right;">

38.89632

</td>

<td style="text-align:right;">

40.48790

</td>

<td style="text-align:right;">

25.11269

</td>

<td style="text-align:right;">

7

</td>

<td style="text-align:right;">

0.0317450

</td>

<td style="text-align:right;">

0.0317450

</td>

</tr>

<tr>

<td style="text-align:left;">

breeding area

</td>

<td style="text-align:left;">

canopy

</td>

<td style="text-align:right;">

0.8498888

</td>

<td style="text-align:right;">

\-15.51174

</td>

<td style="text-align:right;">

39.02348

</td>

<td style="text-align:right;">

40.61506

</td>

<td style="text-align:right;">

25.43098

</td>

<td style="text-align:right;">

7

</td>

<td style="text-align:right;">

0.0061443

</td>

<td style="text-align:right;">

0.0061443

</td>

</tr>

<tr>

<td style="text-align:left;">

breeding area

</td>

<td style="text-align:left;">

gap

</td>

<td style="text-align:right;">

0.8367779

</td>

<td style="text-align:right;">

\-15.54709

</td>

<td style="text-align:right;">

39.09418

</td>

<td style="text-align:right;">

40.68576

</td>

<td style="text-align:right;">

25.08895

</td>

<td style="text-align:right;">

7

</td>

<td style="text-align:right;">

0.0336310

</td>

<td style="text-align:right;">

0.0336310

</td>

</tr>

<tr>

<td style="text-align:left;">

PFA

</td>

<td style="text-align:left;">

canopy

</td>

<td style="text-align:right;">

0.8501617

</td>

<td style="text-align:right;">

\-16.09738

</td>

<td style="text-align:right;">

40.19476

</td>

<td style="text-align:right;">

41.78634

</td>

<td style="text-align:right;">

25.43804

</td>

<td style="text-align:right;">

7

</td>

<td style="text-align:right;">

0.0055695

</td>

<td style="text-align:right;">

0.0055695

</td>

</tr>

<tr>

<td style="text-align:left;">

home range

</td>

<td style="text-align:left;">

suitable

</td>

<td style="text-align:right;">

0.8073010

</td>

<td style="text-align:right;">

\-16.17567

</td>

<td style="text-align:right;">

40.35134

</td>

<td style="text-align:right;">

41.94292

</td>

<td style="text-align:right;">

24.29998

</td>

<td style="text-align:right;">

7

</td>

<td style="text-align:right;">

0.0944592

</td>

<td style="text-align:right;">

0.0944592

</td>

</tr>

<tr>

<td style="text-align:left;">

maximum range

</td>

<td style="text-align:left;">

suitable

</td>

<td style="text-align:right;">

0.8372722

</td>

<td style="text-align:right;">

\-16.30641

</td>

<td style="text-align:right;">

40.61281

</td>

<td style="text-align:right;">

42.20439

</td>

<td style="text-align:right;">

25.10194

</td>

<td style="text-align:right;">

7

</td>

<td style="text-align:right;">

0.0325996

</td>

<td style="text-align:right;">

0.0325996

</td>

</tr>

<tr>

<td style="text-align:left;">

breeding area

</td>

<td style="text-align:left;">

suitable

</td>

<td style="text-align:right;">

0.8364350

</td>

<td style="text-align:right;">

\-16.79490

</td>

<td style="text-align:right;">

41.58980

</td>

<td style="text-align:right;">

43.18138

</td>

<td style="text-align:right;">

25.07993

</td>

<td style="text-align:right;">

7

</td>

<td style="text-align:right;">

0.0343465

</td>

<td style="text-align:right;">

0.0343465

</td>

</tr>

</tbody>

</table>
