filter(diet.items, source == 'C') %>% mutate(total=sum(mass)) %>% 
  group_by(group) %>% mutate(mass=sum(mass), gmass=mass/total) %>% 
  distinct(group, gmass) %>% arrange(desc(gmass))
