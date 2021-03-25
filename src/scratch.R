
temp <- diet.items %>% group_by(class, binomial) %>% mutate(count=n()) %>% ungroup() %>% 
  group_by(source) %>% mutate(total.source.count=n(), 
                              total.source.mass=sum(mass)) %>% ungroup() %>% 
  group_by(source, class, binomial) %>% mutate(source.count=n(), 
                                               source.mass=sum(mass),
                                               per.source.count=source.count/total.source.count*100,
                                               per.source.mass=source.mass/total.source.mass*100) %>% 
  distinct(source, class, binomial, count, per.source.count, per.source.mass) %>% 
  mutate(source2=paste0(source, '2')) %>% 
  pivot_wider(names_from=source, values_from=per.source.count) %>% 
  pivot_wider(names_from=source2, values_from=per.source.mass) %>% ungroup() %>% 
  group_by(class, binomial) %>% 
  fill(everything(), .direction='downup') %>% 
  distinct() %>% left_join(prey.list, by=c('class', 'binomial')) %>% 
  select(class, common, binomial, C, C2, P, P2, R, R2) %>% 
  mutate(common=replace_na(common, 'unknown'),
         binomial=str_replace(binomial, 'Unidentified item', NA_character_),
         common=str_to_sentence(common, locale='en'))

