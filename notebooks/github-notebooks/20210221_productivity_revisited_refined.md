Return to reproductive success - done better
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
library(AICcmodavg)
library(MuMIn)
library(knitr)
library(kableExtra)

# Load in the habitat variables prepped earlier.
ls <- read_csv('../data/processed/landscape_metrics_full.csv')

# And load in the productivity data.
pd <- read_csv('../data/raw/productivity.csv')

# Join them together, keeping just sites with productivity data.
df <- left_join(pd, ls)

# Look at them.
df %>% distinct(site, quality.index, year, n.fledge)
```

    ## # A tibble: 12 x 4
    ##    site   year n.fledge quality.index
    ##    <chr> <dbl>    <dbl>         <dbl>
    ##  1 TCR    2019        2         0.833
    ##  2 MTC    2019        1         1    
    ##  3 UTZ    2019        2         0.5  
    ##  4 TMC    2019        0         1    
    ##  5 MTF    2019        2         0.75 
    ##  6 RLK    2019        3         1    
    ##  7 MTF    2020        1         0.75 
    ##  8 PCR    2020        1         1    
    ##  9 PNC    2020       NA         1    
    ## 10 GOW    2020        1         1    
    ## 11 FMT    2020        1         1    
    ## 12 STV    2020        1         0.8

When I first ran this I used `site` as the random effect, which caused
convergence issues. After talking it over with the amazing statsbeerz
folks, I realized this is probably because, since there is a single
quality index value per each site, using `site` as the random effect is
the same as using `quality.index` as the random effect–essentially using
it as both the explanatory variable *and* the random effect.

Here’s a single model as a test:

Create a bunch of functions for creating a bunch of models. These are
(mostly) the same I used for the glms, except I didn’t bother to
calculat HSI diversity so this is missing `m.suitable.diversity` and
`m.suitable.sink`.

``` r
# Proportion suitable
proportion.suitable.model <- function(df) {
  lmer(n.fledge ~ proportion.suitable + (1|year), data=df)
}

# Proportion suitable + suitable edge density
suitable.edge.density.model <- function(df) {
  lmer(n.fledge ~ proportion.suitable + suitable.edge.density + (1|year), data=df)
}

# Proportion mature forest
proportion.mature.model <- function(df) {
  lmer(n.fledge ~ proportion.cover.mature + (1|year), data=df)
}

# Proportion mature + landcover diversity
mature.diversity.model <- function(df) {
  lmer(n.fledge ~ proportion.cover.mature + cover.diversity + (1|year), data=df)
}

# Proportion mature + gap edge density
mature.edge.density.model <- function(df) {
  lmer(n.fledge ~ proportion.cover.mature + gap.edge.density + (1|year), data=df)
}

# Proportion mature + gap edge density + landcover diversity
mature.sink.model <- function(df) {
  lmer(n.fledge ~ proportion.cover.mature + 
        gap.edge.density + cover.diversity + (1|year), data=df)
}

# Null
null.model <- function(df) {
  lmer(n.fledge ~ (1|year), data=df)
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
    m.mature.edge.density=map(data, mature.edge.density.model),
    m.mature.sink=map(data, mature.sink.model),
    m.null=map(data, null.model)
         )
```

Now error-free\!

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
  arrange(desc(R2m)) %>% 
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

m.mature.sink

</td>

<td style="text-align:right;">

0.3519534

</td>

<td style="text-align:right;">

\-11.81741

</td>

<td style="text-align:right;">

35.63481

</td>

<td style="text-align:right;">

38.02219

</td>

<td style="text-align:right;">

11.02421

</td>

<td style="text-align:right;">

5

</td>

<td style="text-align:right;">

0.3426189

</td>

<td style="text-align:right;">

0.9317148

</td>

</tr>

<tr>

<td style="text-align:left;">

maximum range

</td>

<td style="text-align:left;">

m.mature.sink

</td>

<td style="text-align:right;">

0.5649766

</td>

<td style="text-align:right;">

\-14.06884

</td>

<td style="text-align:right;">

40.13768

</td>

<td style="text-align:right;">

42.52505

</td>

<td style="text-align:right;">

18.64885

</td>

<td style="text-align:right;">

5

</td>

<td style="text-align:right;">

0.2844187

</td>

<td style="text-align:right;">

0.7616946

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

0.6486018

</td>

<td style="text-align:right;">

\-13.22710

</td>

<td style="text-align:right;">

36.45421

</td>

<td style="text-align:right;">

38.44369

</td>

<td style="text-align:right;">

22.32147

</td>

<td style="text-align:right;">

6

</td>

<td style="text-align:right;">

0.2101877

</td>

<td style="text-align:right;">

0.6502397

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

0.7164629

</td>

<td style="text-align:right;">

\-17.84551

</td>

<td style="text-align:right;">

45.69102

</td>

<td style="text-align:right;">

47.68050

</td>

<td style="text-align:right;">

22.85901

</td>

<td style="text-align:right;">

6

</td>

<td style="text-align:right;">

0.1783236

</td>

<td style="text-align:right;">

0.4325598

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

0.7533775

</td>

<td style="text-align:right;">

\-17.76095

</td>

<td style="text-align:right;">

45.52189

</td>

<td style="text-align:right;">

47.51137

</td>

<td style="text-align:right;">

22.79199

</td>

<td style="text-align:right;">

6

</td>

<td style="text-align:right;">

0.1770295

</td>

<td style="text-align:right;">

0.2955693

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

0.7273408

</td>

<td style="text-align:right;">

\-13.68948

</td>

<td style="text-align:right;">

37.37896

</td>

<td style="text-align:right;">

39.36843

</td>

<td style="text-align:right;">

23.88825

</td>

<td style="text-align:right;">

6

</td>

<td style="text-align:right;">

0.1428064

</td>

<td style="text-align:right;">

0.4991436

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

0.7874511

</td>

<td style="text-align:right;">

\-17.81944

</td>

<td style="text-align:right;">

45.63888

</td>

<td style="text-align:right;">

47.62835

</td>

<td style="text-align:right;">

23.50913

</td>

<td style="text-align:right;">

6

</td>

<td style="text-align:right;">

0.1353813

</td>

<td style="text-align:right;">

0.2322210

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

0.7566752

</td>

<td style="text-align:right;">

\-16.04446

</td>

<td style="text-align:right;">

40.08892

</td>

<td style="text-align:right;">

41.68050

</td>

<td style="text-align:right;">

24.34879

</td>

<td style="text-align:right;">

7

</td>

<td style="text-align:right;">

0.0999949

</td>

<td style="text-align:right;">

0.2490382

</td>

</tr>

<tr>

<td style="text-align:left;">

breeding area

</td>

<td style="text-align:left;">

m.mature.sink

</td>

<td style="text-align:right;">

0.8181785

</td>

<td style="text-align:right;">

\-17.75731

</td>

<td style="text-align:right;">

47.51461

</td>

<td style="text-align:right;">

49.90199

</td>

<td style="text-align:right;">

24.71423

</td>

<td style="text-align:right;">

5

</td>

<td style="text-align:right;">

0.0963342

</td>

<td style="text-align:right;">

0.4313278

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

0.7591655

</td>

<td style="text-align:right;">

\-16.20808

</td>

<td style="text-align:right;">

40.41616

</td>

<td style="text-align:right;">

42.00775

</td>

<td style="text-align:right;">

24.54037

</td>

<td style="text-align:right;">

7

</td>

<td style="text-align:right;">

0.0898435

</td>

<td style="text-align:right;">

0.2551252

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

0.7712692

</td>

<td style="text-align:right;">

\-16.07518

</td>

<td style="text-align:right;">

40.15035

</td>

<td style="text-align:right;">

41.74194

</td>

<td style="text-align:right;">

24.54669

</td>

<td style="text-align:right;">

7

</td>

<td style="text-align:right;">

0.0839002

</td>

<td style="text-align:right;">

0.2091500

</td>

</tr>

<tr>

<td style="text-align:left;">

breeding area

</td>

<td style="text-align:left;">

m.mature.edge.density

</td>

<td style="text-align:right;">

0.8100178

</td>

<td style="text-align:right;">

\-19.11676

</td>

<td style="text-align:right;">

48.23351

</td>

<td style="text-align:right;">

50.22299

</td>

<td style="text-align:right;">

25.15745

</td>

<td style="text-align:right;">

6

</td>

<td style="text-align:right;">

0.0588903

</td>

<td style="text-align:right;">

0.2941309

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

0.8281541

</td>

<td style="text-align:right;">

\-17.55672

</td>

<td style="text-align:right;">

45.11344

</td>

<td style="text-align:right;">

47.10292

</td>

<td style="text-align:right;">

24.94083

</td>

<td style="text-align:right;">

6

</td>

<td style="text-align:right;">

0.0536924

</td>

<td style="text-align:right;">

0.1981908

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

0.8575757

</td>

<td style="text-align:right;">

\-19.81125

</td>

<td style="text-align:right;">

49.62250

</td>

<td style="text-align:right;">

51.61198

</td>

<td style="text-align:right;">

25.01276

</td>

<td style="text-align:right;">

6

</td>

<td style="text-align:right;">

0.0390560

</td>

<td style="text-align:right;">

0.1049547

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

0.8022135

</td>

<td style="text-align:right;">

\-16.70843

</td>

<td style="text-align:right;">

41.41686

</td>

<td style="text-align:right;">

43.00844

</td>

<td style="text-align:right;">

25.32812

</td>

<td style="text-align:right;">

7

</td>

<td style="text-align:right;">

0.0290010

</td>

<td style="text-align:right;">

0.1514875

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

0.8050903

</td>

<td style="text-align:right;">

\-16.22884

</td>

<td style="text-align:right;">

40.45768

</td>

<td style="text-align:right;">

42.04926

</td>

<td style="text-align:right;">

25.35343

</td>

<td style="text-align:right;">

7

</td>

<td style="text-align:right;">

0.0260637

</td>

<td style="text-align:right;">

0.1424629

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

0.8568910

</td>

<td style="text-align:right;">

\-15.00921

</td>

<td style="text-align:right;">

38.01842

</td>

<td style="text-align:right;">

39.22876

</td>

<td style="text-align:right;">

23.18522

</td>

<td style="text-align:right;">

6

</td>

<td style="text-align:right;">

0.0249078

</td>

<td style="text-align:right;">

0.0375055

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

0.9144433

</td>

<td style="text-align:right;">

\-16.98533

</td>

<td style="text-align:right;">

43.97065

</td>

<td style="text-align:right;">

45.48358

</td>

<td style="text-align:right;">

23.19813

</td>

<td style="text-align:right;">

5

</td>

<td style="text-align:right;">

0.0219703

</td>

<td style="text-align:right;">

0.0395262

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

0.8615107

</td>

<td style="text-align:right;">

\-19.35444

</td>

<td style="text-align:right;">

48.70889

</td>

<td style="text-align:right;">

50.69836

</td>

<td style="text-align:right;">

25.31242

</td>

<td style="text-align:right;">

6

</td>

<td style="text-align:right;">

0.0215793

</td>

<td style="text-align:right;">

0.1108448

</td>

</tr>

<tr>

<td style="text-align:left;">

PFA

</td>

<td style="text-align:left;">

m.mature.sink

</td>

<td style="text-align:right;">

0.9038091

</td>

<td style="text-align:right;">

\-18.03698

</td>

<td style="text-align:right;">

48.07396

</td>

<td style="text-align:right;">

50.46133

</td>

<td style="text-align:right;">

25.45775

</td>

<td style="text-align:right;">

5

</td>

<td style="text-align:right;">

0.0211395

</td>

<td style="text-align:right;">

0.1786293

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

0.8394945

</td>

<td style="text-align:right;">

\-15.82305

</td>

<td style="text-align:right;">

41.64609

</td>

<td style="text-align:right;">

43.63557

</td>

<td style="text-align:right;">

25.75686

</td>

<td style="text-align:right;">

6

</td>

<td style="text-align:right;">

0.0204061

</td>

<td style="text-align:right;">

0.2389805

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

0.8086808

</td>

<td style="text-align:right;">

\-17.01093

</td>

<td style="text-align:right;">

42.02186

</td>

<td style="text-align:right;">

43.61344

</td>

<td style="text-align:right;">

25.81296

</td>

<td style="text-align:right;">

7

</td>

<td style="text-align:right;">

0.0038449

</td>

<td style="text-align:right;">

0.1690513

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

0.8590765

</td>

<td style="text-align:right;">

\-16.02010

</td>

<td style="text-align:right;">

42.04020

</td>

<td style="text-align:right;">

44.02968

</td>

<td style="text-align:right;">

25.84893

</td>

<td style="text-align:right;">

6

</td>

<td style="text-align:right;">

0.0012074

</td>

<td style="text-align:right;">

0.1670642

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

0.8126172

</td>

<td style="text-align:right;">

\-17.14679

</td>

<td style="text-align:right;">

42.29357

</td>

<td style="text-align:right;">

43.88515

</td>

<td style="text-align:right;">

25.79639

</td>

<td style="text-align:right;">

7

</td>

<td style="text-align:right;">

0.0011413

</td>

<td style="text-align:right;">

0.1506272

</td>

</tr>

<tr>

<td style="text-align:left;">

PFA

</td>

<td style="text-align:left;">

m.null

</td>

<td style="text-align:right;">

0.7698004

</td>

<td style="text-align:right;">

\-13.12990

</td>

<td style="text-align:right;">

32.25980

</td>

<td style="text-align:right;">

33.45349

</td>

<td style="text-align:right;">

25.85007

</td>

<td style="text-align:right;">

8

</td>

<td style="text-align:right;">

0.0000000

</td>

<td style="text-align:right;">

0.1608392

</td>

</tr>

<tr>

<td style="text-align:left;">

breeding area

</td>

<td style="text-align:left;">

m.null

</td>

<td style="text-align:right;">

0.7698004

</td>

<td style="text-align:right;">

\-13.12990

</td>

<td style="text-align:right;">

32.25980

</td>

<td style="text-align:right;">

33.45349

</td>

<td style="text-align:right;">

25.85007

</td>

<td style="text-align:right;">

8

</td>

<td style="text-align:right;">

0.0000000

</td>

<td style="text-align:right;">

0.1608392

</td>

</tr>

<tr>

<td style="text-align:left;">

home range

</td>

<td style="text-align:left;">

m.null

</td>

<td style="text-align:right;">

0.7698004

</td>

<td style="text-align:right;">

\-13.12990

</td>

<td style="text-align:right;">

32.25980

</td>

<td style="text-align:right;">

33.45349

</td>

<td style="text-align:right;">

25.85007

</td>

<td style="text-align:right;">

8

</td>

<td style="text-align:right;">

0.0000000

</td>

<td style="text-align:right;">

0.1608392

</td>

</tr>

<tr>

<td style="text-align:left;">

maximum range

</td>

<td style="text-align:left;">

m.null

</td>

<td style="text-align:right;">

0.7698004

</td>

<td style="text-align:right;">

\-13.12990

</td>

<td style="text-align:right;">

32.25980

</td>

<td style="text-align:right;">

33.45349

</td>

<td style="text-align:right;">

25.85007

</td>

<td style="text-align:right;">

8

</td>

<td style="text-align:right;">

0.0000000

</td>

<td style="text-align:right;">

0.1608392

</td>

</tr>

</tbody>

</table>

Well, they all have terrible R2s. Let’s look at the real AICs, since
these AICs are moderately useless.

``` r
all.models <- nf %>% pivot_longer(-c(size, data), names_to='modname', values_to='model') %>% 
  mutate(name=paste(size, modname))

aictab(all.models$model, modnames=all.models$name) %>% 
  kable() %>% kable_styling(bootstrap_options=c('striped'))
```

<table class="table table-striped" style="margin-left: auto; margin-right: auto;">

<thead>

<tr>

<th style="text-align:left;">

</th>

<th style="text-align:left;">

Modnames

</th>

<th style="text-align:right;">

K

</th>

<th style="text-align:right;">

AICc

</th>

<th style="text-align:right;">

Delta\_AICc

</th>

<th style="text-align:right;">

ModelLik

</th>

<th style="text-align:right;">

AICcWt

</th>

<th style="text-align:right;">

Res.LL

</th>

<th style="text-align:right;">

Cum.Wt

</th>

</tr>

</thead>

<tbody>

<tr>

<td style="text-align:left;">

7

</td>

<td style="text-align:left;">

PFA m.null

</td>

<td style="text-align:right;">

3

</td>

<td style="text-align:right;">

35.68838

</td>

<td style="text-align:right;">

0.00000

</td>

<td style="text-align:right;">

1.0000000

</td>

<td style="text-align:right;">

0.2482561

</td>

<td style="text-align:right;">

\-13.12990

</td>

<td style="text-align:right;">

0.2482561

</td>

</tr>

<tr>

<td style="text-align:left;">

14

</td>

<td style="text-align:left;">

breeding area m.null

</td>

<td style="text-align:right;">

3

</td>

<td style="text-align:right;">

35.68838

</td>

<td style="text-align:right;">

0.00000

</td>

<td style="text-align:right;">

1.0000000

</td>

<td style="text-align:right;">

0.2482561

</td>

<td style="text-align:right;">

\-13.12990

</td>

<td style="text-align:right;">

0.4965123

</td>

</tr>

<tr>

<td style="text-align:left;">

21

</td>

<td style="text-align:left;">

home range m.null

</td>

<td style="text-align:right;">

3

</td>

<td style="text-align:right;">

35.68838

</td>

<td style="text-align:right;">

0.00000

</td>

<td style="text-align:right;">

1.0000000

</td>

<td style="text-align:right;">

0.2482561

</td>

<td style="text-align:right;">

\-13.12990

</td>

<td style="text-align:right;">

0.7447684

</td>

</tr>

<tr>

<td style="text-align:left;">

28

</td>

<td style="text-align:left;">

maximum range m.null

</td>

<td style="text-align:right;">

3

</td>

<td style="text-align:right;">

35.68838

</td>

<td style="text-align:right;">

0.00000

</td>

<td style="text-align:right;">

1.0000000

</td>

<td style="text-align:right;">

0.2482561

</td>

<td style="text-align:right;">

\-13.12990

</td>

<td style="text-align:right;">

0.9930246

</td>

</tr>

<tr>

<td style="text-align:left;">

1

</td>

<td style="text-align:left;">

PFA m.proportion.suitable

</td>

<td style="text-align:right;">

4

</td>

<td style="text-align:right;">

46.01842

</td>

<td style="text-align:right;">

10.33004

</td>

<td style="text-align:right;">

0.0057130

</td>

<td style="text-align:right;">

0.0014183

</td>

<td style="text-align:right;">

\-15.00921

</td>

<td style="text-align:right;">

0.9944428

</td>

</tr>

<tr>

<td style="text-align:left;">

24

</td>

<td style="text-align:left;">

maximum range m.proportion.mature

</td>

<td style="text-align:right;">

4

</td>

<td style="text-align:right;">

46.75558

</td>

<td style="text-align:right;">

11.06721

</td>

<td style="text-align:right;">

0.0039517

</td>

<td style="text-align:right;">

0.0009810

</td>

<td style="text-align:right;">

\-16.04446

</td>

<td style="text-align:right;">

0.9954239

</td>

</tr>

<tr>

<td style="text-align:left;">

15

</td>

<td style="text-align:left;">

home range m.proportion.suitable

</td>

<td style="text-align:right;">

4

</td>

<td style="text-align:right;">

46.81702

</td>

<td style="text-align:right;">

11.12865

</td>

<td style="text-align:right;">

0.0038322

</td>

<td style="text-align:right;">

0.0009514

</td>

<td style="text-align:right;">

\-16.07518

</td>

<td style="text-align:right;">

0.9963752

</td>

</tr>

<tr>

<td style="text-align:left;">

17

</td>

<td style="text-align:left;">

home range m.proportion.mature

</td>

<td style="text-align:right;">

4

</td>

<td style="text-align:right;">

47.08283

</td>

<td style="text-align:right;">

11.39445

</td>

<td style="text-align:right;">

0.0033553

</td>

<td style="text-align:right;">

0.0008330

</td>

<td style="text-align:right;">

\-16.20808

</td>

<td style="text-align:right;">

0.9972082

</td>

</tr>

<tr>

<td style="text-align:left;">

22

</td>

<td style="text-align:left;">

maximum range m.proportion.suitable

</td>

<td style="text-align:right;">

4

</td>

<td style="text-align:right;">

47.12434

</td>

<td style="text-align:right;">

11.43597

</td>

<td style="text-align:right;">

0.0032863

</td>

<td style="text-align:right;">

0.0008159

</td>

<td style="text-align:right;">

\-16.22884

</td>

<td style="text-align:right;">

0.9980240

</td>

</tr>

<tr>

<td style="text-align:left;">

8

</td>

<td style="text-align:left;">

breeding area m.proportion.suitable

</td>

<td style="text-align:right;">

4

</td>

<td style="text-align:right;">

48.08353

</td>

<td style="text-align:right;">

12.39515

</td>

<td style="text-align:right;">

0.0020344

</td>

<td style="text-align:right;">

0.0005050

</td>

<td style="text-align:right;">

\-16.70843

</td>

<td style="text-align:right;">

0.9985291

</td>

</tr>

<tr>

<td style="text-align:left;">

18

</td>

<td style="text-align:left;">

home range m.mature.diversity

</td>

<td style="text-align:right;">

5

</td>

<td style="text-align:right;">

48.45421

</td>

<td style="text-align:right;">

12.76583

</td>

<td style="text-align:right;">

0.0016902

</td>

<td style="text-align:right;">

0.0004196

</td>

<td style="text-align:right;">

\-13.22710

</td>

<td style="text-align:right;">

0.9989487

</td>

</tr>

<tr>

<td style="text-align:left;">

10

</td>

<td style="text-align:left;">

breeding area m.proportion.mature

</td>

<td style="text-align:right;">

4

</td>

<td style="text-align:right;">

48.68852

</td>

<td style="text-align:right;">

13.00015

</td>

<td style="text-align:right;">

0.0015033

</td>

<td style="text-align:right;">

0.0003732

</td>

<td style="text-align:right;">

\-17.01093

</td>

<td style="text-align:right;">

0.9993219

</td>

</tr>

<tr>

<td style="text-align:left;">

3

</td>

<td style="text-align:left;">

PFA m.proportion.mature

</td>

<td style="text-align:right;">

4

</td>

<td style="text-align:right;">

48.96024

</td>

<td style="text-align:right;">

13.27186

</td>

<td style="text-align:right;">

0.0013124

</td>

<td style="text-align:right;">

0.0003258

</td>

<td style="text-align:right;">

\-17.14679

</td>

<td style="text-align:right;">

0.9996477

</td>

</tr>

<tr>

<td style="text-align:left;">

25

</td>

<td style="text-align:left;">

maximum range m.mature.diversity

</td>

<td style="text-align:right;">

5

</td>

<td style="text-align:right;">

49.37896

</td>

<td style="text-align:right;">

13.69058

</td>

<td style="text-align:right;">

0.0010645

</td>

<td style="text-align:right;">

0.0002643

</td>

<td style="text-align:right;">

\-13.68948

</td>

<td style="text-align:right;">

0.9999120

</td>

</tr>

<tr>

<td style="text-align:left;">

11

</td>

<td style="text-align:left;">

breeding area m.mature.diversity

</td>

<td style="text-align:right;">

5

</td>

<td style="text-align:right;">

53.64609

</td>

<td style="text-align:right;">

17.95771

</td>

<td style="text-align:right;">

0.0001260

</td>

<td style="text-align:right;">

0.0000313

</td>

<td style="text-align:right;">

\-15.82305

</td>

<td style="text-align:right;">

0.9999432

</td>

</tr>

<tr>

<td style="text-align:left;">

4

</td>

<td style="text-align:left;">

PFA m.mature.diversity

</td>

<td style="text-align:right;">

5

</td>

<td style="text-align:right;">

54.04020

</td>

<td style="text-align:right;">

18.35182

</td>

<td style="text-align:right;">

0.0001035

</td>

<td style="text-align:right;">

0.0000257

</td>

<td style="text-align:right;">

\-16.02010

</td>

<td style="text-align:right;">

0.9999689

</td>

</tr>

<tr>

<td style="text-align:left;">

20

</td>

<td style="text-align:left;">

home range m.mature.sink

</td>

<td style="text-align:right;">

6

</td>

<td style="text-align:right;">

56.63481

</td>

<td style="text-align:right;">

20.94644

</td>

<td style="text-align:right;">

0.0000283

</td>

<td style="text-align:right;">

0.0000070

</td>

<td style="text-align:right;">

\-11.81741

</td>

<td style="text-align:right;">

0.9999760

</td>

</tr>

<tr>

<td style="text-align:left;">

23

</td>

<td style="text-align:left;">

maximum range m.suitable.edge.density

</td>

<td style="text-align:right;">

5

</td>

<td style="text-align:right;">

57.11344

</td>

<td style="text-align:right;">

21.42506

</td>

<td style="text-align:right;">

0.0000223

</td>

<td style="text-align:right;">

0.0000055

</td>

<td style="text-align:right;">

\-17.55672

</td>

<td style="text-align:right;">

0.9999815

</td>

</tr>

<tr>

<td style="text-align:left;">

26

</td>

<td style="text-align:left;">

maximum range m.mature.edge.density

</td>

<td style="text-align:right;">

5

</td>

<td style="text-align:right;">

57.52189

</td>

<td style="text-align:right;">

21.83352

</td>

<td style="text-align:right;">

0.0000182

</td>

<td style="text-align:right;">

0.0000045

</td>

<td style="text-align:right;">

\-17.76095

</td>

<td style="text-align:right;">

0.9999860

</td>

</tr>

<tr>

<td style="text-align:left;">

16

</td>

<td style="text-align:left;">

home range m.suitable.edge.density

</td>

<td style="text-align:right;">

5

</td>

<td style="text-align:right;">

57.63888

</td>

<td style="text-align:right;">

21.95050

</td>

<td style="text-align:right;">

0.0000171

</td>

<td style="text-align:right;">

0.0000043

</td>

<td style="text-align:right;">

\-17.81944

</td>

<td style="text-align:right;">

0.9999902

</td>

</tr>

<tr>

<td style="text-align:left;">

19

</td>

<td style="text-align:left;">

home range m.mature.edge.density

</td>

<td style="text-align:right;">

5

</td>

<td style="text-align:right;">

57.69102

</td>

<td style="text-align:right;">

22.00265

</td>

<td style="text-align:right;">

0.0000167

</td>

<td style="text-align:right;">

0.0000041

</td>

<td style="text-align:right;">

\-17.84551

</td>

<td style="text-align:right;">

0.9999944

</td>

</tr>

<tr>

<td style="text-align:left;">

2

</td>

<td style="text-align:left;">

PFA m.suitable.edge.density

</td>

<td style="text-align:right;">

5

</td>

<td style="text-align:right;">

58.97065

</td>

<td style="text-align:right;">

23.28228

</td>

<td style="text-align:right;">

0.0000088

</td>

<td style="text-align:right;">

0.0000022

</td>

<td style="text-align:right;">

\-16.98533

</td>

<td style="text-align:right;">

0.9999966

</td>

</tr>

<tr>

<td style="text-align:left;">

12

</td>

<td style="text-align:left;">

breeding area m.mature.edge.density

</td>

<td style="text-align:right;">

5

</td>

<td style="text-align:right;">

60.23351

</td>

<td style="text-align:right;">

24.54514

</td>

<td style="text-align:right;">

0.0000047

</td>

<td style="text-align:right;">

0.0000012

</td>

<td style="text-align:right;">

\-19.11676

</td>

<td style="text-align:right;">

0.9999977

</td>

</tr>

<tr>

<td style="text-align:left;">

5

</td>

<td style="text-align:left;">

PFA m.mature.edge.density

</td>

<td style="text-align:right;">

5

</td>

<td style="text-align:right;">

60.70889

</td>

<td style="text-align:right;">

25.02051

</td>

<td style="text-align:right;">

0.0000037

</td>

<td style="text-align:right;">

0.0000009

</td>

<td style="text-align:right;">

\-19.35444

</td>

<td style="text-align:right;">

0.9999986

</td>

</tr>

<tr>

<td style="text-align:left;">

27

</td>

<td style="text-align:left;">

maximum range m.mature.sink

</td>

<td style="text-align:right;">

6

</td>

<td style="text-align:right;">

61.13768

</td>

<td style="text-align:right;">

25.44930

</td>

<td style="text-align:right;">

0.0000030

</td>

<td style="text-align:right;">

0.0000007

</td>

<td style="text-align:right;">

\-14.06884

</td>

<td style="text-align:right;">

0.9999994

</td>

</tr>

<tr>

<td style="text-align:left;">

9

</td>

<td style="text-align:left;">

breeding area m.suitable.edge.density

</td>

<td style="text-align:right;">

5

</td>

<td style="text-align:right;">

61.62250

</td>

<td style="text-align:right;">

25.93412

</td>

<td style="text-align:right;">

0.0000023

</td>

<td style="text-align:right;">

0.0000006

</td>

<td style="text-align:right;">

\-19.81125

</td>

<td style="text-align:right;">

1.0000000

</td>

</tr>

<tr>

<td style="text-align:left;">

13

</td>

<td style="text-align:left;">

breeding area m.mature.sink

</td>

<td style="text-align:right;">

6

</td>

<td style="text-align:right;">

68.51461

</td>

<td style="text-align:right;">

32.82624

</td>

<td style="text-align:right;">

0.0000001

</td>

<td style="text-align:right;">

0.0000000

</td>

<td style="text-align:right;">

\-17.75731

</td>

<td style="text-align:right;">

1.0000000

</td>

</tr>

<tr>

<td style="text-align:left;">

6

</td>

<td style="text-align:left;">

PFA m.mature.sink

</td>

<td style="text-align:right;">

6

</td>

<td style="text-align:right;">

69.07396

</td>

<td style="text-align:right;">

33.38558

</td>

<td style="text-align:right;">

0.0000001

</td>

<td style="text-align:right;">

0.0000000

</td>

<td style="text-align:right;">

\-18.03698

</td>

<td style="text-align:right;">

1.0000000

</td>

</tr>

</tbody>

</table>

hahahahaha yes the nulls beat everything by a truly astonishing margin.
Mind you, this does throw an error so I need to look into that, but it
sure looks like these models are pretty useless. But the code can be
entirely recycled for `proportion.squirrel` which is more logical and
might turn up something different.
