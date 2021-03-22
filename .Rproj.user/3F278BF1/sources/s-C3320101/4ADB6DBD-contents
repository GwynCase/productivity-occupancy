

temp <- left_join(diet.items, centroids.sf, by=c('site')) %>% 
  filter(source == 'C') %>% group_by(nest) %>% 
  mutate(total=sum(mass)) %>% filter(class == 'Mammalia') %>% 
  mutate(mmass=sum(mass), cmass=mmass/total*100) %>% 
  distinct(zone, nest, cmass)

t.test(cmass ~ zone, data=percent.mammal.biomass.zone.nest, var.equal=TRUE) %>% 
  glance() %>% select(p.value) %>% peretty()


percent.mammal.biomass.zone.nest <- left_join(diet.items, centroids.sf, by=c('site')) %>% 
  filter(source == 'C') %>% group_by(nest) %>% 
  mutate(total=sum(mass)) %>% filter(class == 'Mammalia') %>% 
  mutate(mmass=sum(mass), cmass=mmass/total) %>% 
  distinct(zone, nest, cmass)
