zone.chi %>% mutate(gl=map(ch, glance)) %>% 
  unnest(gl)

zone.chi %>% mutate(td=map(ch, tidy)) %>% 
  unnest(td)

# New, throws an error message
zone.counts %>% pivot_wider(names_from=source, values_from=count, values_fill=list(count = 0)) %>% 
  mutate(RP=R + P) %>% 
  pivot_longer(cols=c('R', 'C', 'P', 'RP'), names_to='source', values_to='count') %>% 
  group_by(source) %>% nest() %>% 
  mutate(ch=map(data, function(data) {
    data %>% pivot_wider(names_from=group, values_from=count, values_fill=0) %>% 
      column_to_rownames(var='zone') %>% 
      chisq.test(., correct=FALSE, simulate.p.value=TRUE)
  }),
        gl=map(ch, glance))

# Old, no error.
zone.counts %>% group_by(source) %>% nest() %>% 
  mutate(ch=map(data, function(data) {
    data %>% pivot_wider(names_from=group, values_from=count, values_fill=0) %>% 
      column_to_rownames(var='zone') %>% 
      chisq.test(., correct=FALSE, simulate.p.value=TRUE)
  })) 
