new.data %>% group_by(site, year=year(datetime)) %>% 
  summarize(first=min(datetime), last=max(datetime))#, days=difftime(max, min), n=n())

grab.some(new.data)
