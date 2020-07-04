ggplot(ml.tz, aes(x=amt.tz, y=ratio, label=site)) +
  geom_point() +
  geom_text(hjust='inward', nudge_y = 0.5) +
  theme_classic()

vri.class <- vri %>% mutate(class=case_when(
  # Non-vegetated and water
  BCLCS_LV_1 == 'N' & BCLCS_LV_2 == 'W' ~ 1, 
  # Non-vegetated and not water
  BCLCS_LV_1 == 'N' & BCLCS_LV_2 != 'W' ~ 2, 
  # Vegetated and not trees
  BCLCS_LV_1 == 'V' & BCLCS_LV_2 == 'N' ~ 3,
  # Vegetated and mixed or deciduous trees
  BCLCS_LV_1 == 'V' & BCLCS_LV_4 %in% c('TD', 'TM') ~ 4,
  # Vegetated and coniferous trees
  BCLCS_LV_4 == 'TC' & PROJ_AGE_1 < 20 ~ 5,
  BCLCS_LV_4 == 'TC' & PROJ_AGE_1 >= 20 & PROJ_AGE_1 < 60 ~ 6,
  BCLCS_LV_4 == 'TC' & PROJ_AGE_1 >= 60 & PROJ_AGE_1 < 140 ~ 7,
  BCLCS_LV_4 == 'TC' & PROJ_AGE_1 >= 140 ~ 8,
  TRUE ~ 0
))

buffers <- st_buffer(sites.sf, r.hr.m)
crop(r.vri, buffers) %>% plot()

buffers$site %>% plot()

show_landscape(r.vri, discrete=TRUE)
show_shareplot(r.vri, sites.sf, 10, 50) 

show_shareplot(classified_landscape, new_point, 10, 50)
