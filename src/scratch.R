diet.items %>% filter(genus == 'Larus')

# Create a function to calculate n.
give.n <- function(x){
  return(c(y = mean(x), label = length(x)))
}

ggplot(diamonds, aes(cut, price)) + 
  geom_boxplot() + 
  stat_summary(fun.data = give.n, geom = "text")


# Calculate biomass and counts for mammalian prey.
diet.items %>% 
  filter(method == 'remains') %>% 
  mutate(total.mass=sum(mass, na.rm=TRUE)) %>% 
  filter(genus == 'Tamiasciurus') %>% 
  mutate(mass.sq=sum(mass), prop.sq.mass=mass.sq/total.mass) %>% 
  select(prop.sq.mass) %>% distinct() 
