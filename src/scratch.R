diet.items %>% filter(genus == 'Larus')

# Create a function to calculate n.
give.n <- function(x){
  return(c(y = mean(x), label = length(x)))
}

ggplot(diamonds, aes(cut, price)) + 
  geom_boxplot() + 
  stat_summary(fun.data = give.n, geom = "text")

tt <- test %>% mutate(model=map(data, function(df) lmer(n.fledge ~ canopy.high + (1|site), data=df)))

productivity.by.cover.diversity %>% map(glance) %>% 
  bind_rows(.id='size') 

productivity.models <- c(productivity.by.cover.diversity,
                         productivity.by.canopy.high)

# Nest data.
ns <- df %>% group_by(size) %>% nest()

# Look at it.
ns$data[[1]]

# Create model-fitting function.
canopy.model <- function(df) {
  lmer(n.fledge ~ canopy.high + (1|site), data=df)
}

# Apply model to each size.
models <- map(ns$data, canopy.model)

# Or add it into data frame.
ns <- ns %>% 
  mutate(canopy=map(data, canopy.model))

# Look at it.
ns

# Add residuals.
ns <- ns %>% 
  mutate(can.resids=map2(data, can.models, add_residuals))

# Look at it.
ns

# Convert back to a data frame for plotting residuals.
can.resids <- unnest(ns, can.resids)

# Plot the residuals.
can.resids %>% 
  ggplot(aes(canopy.high, resid)) +
  geom_line(alpha = 1 / 3) + 
  facet_wrap(~size)

# Let's get some model quality metrics.
ns %>% mutate(can.glance=map(can.models, broom::glance)) %>% 
  unnest(can.glance, .drop=TRUE)

## Can we do this with multiple models?

# Make another model function.
cover.model <- function(df) {
  lmer(n.fledge ~ cover.diversity + (1|site), data=df)
}

# Add it to the data frame.
ns <- ns %>% 
  mutate(cover=map(data, cover.model))

# Twist the data frame.
ns.long <- ns %>% pivot_longer(-c(size, data), names_to='modname', values_to='model')

# Can we get quality metrics?
ns.long %>% mutate(glance=map(model, broom::glance)) %>% 
  unnest(glance, .drop=TRUE) %>% 
  arrange(AIC)

## Can I use an external function to extract something else?

# Pull out one model.
ns.long$model[[1]]

# Try it.
r.squaredGLMM(ns.long$model[[1]])

# On many models.
tt <- ns.long %>% ungroup() %>% 
  mutate(rsq=map(model, r.squaredGLMM)) %>% 
  #rowwise() %>% 
  mutate(rsq=as.data.frame(rsq)) 

t3 <- ns.long %>% ungroup() %>% 
  mutate(rsq=map(model, r.squaredGLMM)) %>% 
  mutate(rsq=map(rsq, as.data.frame)) %>% 
  unnest(rsq)

nf.long %>% unnest(glance) %>% 
  unnest(rsq) %>% 
  select(-model, -data)















