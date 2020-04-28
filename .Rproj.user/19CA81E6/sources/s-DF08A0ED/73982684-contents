
area.95

area.95 %>% filter(id %in% c('HAR04', 'HAR10', 'HAR08'))# %>% 
  pivot_longer(cols=1:3) %>% 
  summarize(mean(value)) %>% 
  as.numeric() %>% round(digits=2)

area.95 %>% filter(id %within% c('HAR07', 'HAR05', 'HAR09'))
  pivot_longer(cols=1:3) %>% 
    summarize(mean(value)) %>% 
    as.numeric() %>% round(digits=2)
  
  area.95 %>% pivot_longer(cols=1:6) %>% 
    filter(name %in% c('HAR04', 'HAR10', 'HAR08')) %>% 
    summarize(sd(value)) %>% 
    as.numeric() %>% round(digits=2)
  