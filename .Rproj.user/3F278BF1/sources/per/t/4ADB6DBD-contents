
# Make a happy data set for the models to use.
fake.data <- productivity %>% mutate(nest=paste(site, year, sep='')) %>% 
  left_join(nest.diversity, by=c('nest')) %>% 
  left_join(nest.mass, by=c('nest')) %>%
  select(site, year, nest, n.fledge, diet.diversity, Aves) %>% 
  filter_all(all_vars(!is.na(.)))

# Make the models.

fake.model <- lm(n.fledge ~ Aves, data=fake.data)
summary(fake.model)
