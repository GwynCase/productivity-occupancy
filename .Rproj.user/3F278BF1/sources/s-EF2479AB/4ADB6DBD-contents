
rv.vars <- tribble(
  ~nest, ~prop.sq.mass, ~prop.sq.count,
'MTC2019',	0.7172089,	0.5660377,
'MTF2019',	0.7167476,	0.6153846,
'RLK2019',	0.7755876,	0.6486486,
'TCR2019',	0.8112324,	0.6451613,
'TMC2019',	0.4872569,	0.3200000,
'UTZ2019',	0.7271142,	0.7600000)
rv

rv.vars <- camera.sites.2019 %>% select(nest, site, n_fledge) %>% 
  full_join(rv, by=c('nest'))

rv.vars

lm(n_fledge ~ prop.sq.mass, data=rv.vars) %>% summary()
