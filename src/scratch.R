calculate_lsm(utz.r, what=c('lsm_c_area_mn', 'lsm_c_ca')) %>% 
  pivot_wider(id_cols=class, names_from=metric, values_from=value) %>% 
  left_join(bec.levels, by=c('class'='ID'))
