zone.counts

test <- zone.counts %>% filter(source == 'C') %>%
  pivot_wider(names_from=group, values_from=count, values_fill=0) %>% 
  ungroup() %>% select(!source) %>% 
  column_to_rownames(var='zone') %>% 
  chisq.test(., correct=FALSE, simulate.p.value=TRUE)



zone.counts %>% group_by(source) %>% nest() %>% 
  mutate(ch=map(data, function(data) {
    data %>% pivot_wider(names_from=group, values_from=count, values_fill=0) %>% 
      column_to_rownames(var='zone') %>% 
      chisq.test(., correct=FALSE, simulate.p.value=TRUE)
  })) 


