df %>% mutate(size=case_when(
  genus == 'Catharus' ~ 'S',
  species == 'pubescens' ~ 'S',
  species == 'villosus' ~ 'M',
  genus == 'Lepus' & is.na(size) ~ 'L',
  genus == 'Neotoma' ~ 'M',
  genus == 'Rattus' ~ 'M',
  genus == 'Neotamias' ~ 'S',
  genus == 'Tamiasciurus' & is.na(size) ~ 'M',
  TRUE ~ size
))

df %>% select(class, order, family, genus, species, common, size) %>% distinct() %>% 
  arrange(class, order, family, genus, species, common, size) %>% view()

df %>% filter(genus == 'Tamiasciurus' & is.na(size)) %>% view()

df %>% mutate(family=case_when(genus == 'Rattus' & !is.na(comments) & size == 'L' ~ 'Aplodontiidae',
                               TRUE ~ family),
              genus=case_when(genus == 'Rattus' & !is.na(comments) & size == 'L' ~ 'Aplodontia',
                              TRUE ~ genus),
              species=case_when(genus == 'Rattus' & !is.na(comments) & size == 'L' ~ 'rufa',
                                TRUE ~ species),
              common=case_when(genus == 'Rattus' & !is.na(comments) & size == 'L' ~ 'mountain beaver',
                               TRUE ~ common))
