diet.items %>% filter(genus == 'Larus')

# Create a function to calculate n.
give.n <- function(x){
  return(c(y = mean(x), label = length(x)))
}

ggplot(diamonds, aes(cut, price)) + 
  geom_boxplot() + 
  stat_summary(fun.data = give.n, geom = "text")


sankeyNetwork(Links = links, Nodes = nodes,
              Source = "IDsource", Target = "IDtarget",
              Value = "value", NodeID = "name", 
              sinksRight=FALSE, fontFamily='Lato',
              height=750, width=1000, fontSize=14)
