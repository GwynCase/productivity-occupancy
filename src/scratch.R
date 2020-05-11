ggplot(class.area, aes(x=amt.tz, y=prop.ml, label=site)) +
  geom_point() +
  geom_text(hjust='inward', nudge_y = 0.5) +
  theme_classic()
  