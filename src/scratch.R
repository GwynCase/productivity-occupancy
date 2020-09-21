diet.items %>% filter(genus == 'Larus')

# Create a function to calculate n.
give.n <- function(x){
  return(c(y = mean(x), label = length(x)))
}

ggplot(diamonds, aes(cut, price)) + 
  geom_boxplot() + 
  stat_summary(fun.data = give.n, geom = "text")


test <- diet.items %>% filter(binomial != 'Unidentified item' & method == 'camera') %>% 
  mutate(count=1, index=1:nrow(.)) %>%
  dplyr::select(site, index, binomial, count) %>%
  pivot_wider(names_from=binomial, values_from=count,
              values_fill=list(count = 0))

temp <- test %>% group_by(site) %>% 
  group_map(~specaccum(.x[3:17], method="random", permutations=100))

temp %>% map(~plot(.$sites, .$richness,
              xlab="Number of Items",
              ylab="Species Richness",
              main="Camera data"))
library(ggplot2)

temp %>% ggplot()

diet.items %>% filter(binomial != 'Unidentified item' & method == 'remains') %>% 
  group_by(site) %>% 
  summarize(n.items=n())
