
select(captures, id, sex) %>% 
  right_join(homerange, by=c('id')) %>% 
  filter(sex =='m') %>% distinct(id)



select(captures, id, sex) %>% 
  right_join(homerange, by=c('id')) %>% 
  filter(sex =='f') %>% distinct(id)

select(captures, id, sex) %>% 
  right_join(homerange, by=c('id')) %>% 
  filter(id == 'HAR07' & method == 'mcp')

captures
