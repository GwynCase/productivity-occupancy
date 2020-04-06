# This code is for filling in a table of all prey items observed
# with their group, mass, etc.

# load

df <- read.csv('../data/interim/camera_corrected.csv', stringsAsFactors=FALSE)

## ---- what_sp ----

# Filter out just the prey items delivered.
# Remove complete unknowns (ie, no class or size).
items <- df %>% filter(class != '') %>%
  filter(size != 'Unknown') %>%
  dplyr::select(class, family, genus, species, common, size) %>% 
  arrange(class, family, genus, species, size)

# prey_attributes

# Classify each avian item into a size class.
items <- items %>% mutate(group=case_when(
  # Band-tailed pigeon
  species == 'fasciata' ~ 'Large bird',
  # Ruffed grouse
  species == 'umbellus' ~ 'Large bird',
  # Sooty grouse
  species == 'fulignosus' ~ 'Large bird',
  # Steller's jay
  species == 'stelleri' ~ 'Medium bird',
  # Gray jay
  species == 'canadensis' ~ 'Medium bird',
  # Swainson's thrush
  species == 'ustulatus' ~ 'Small bird',
  # Varied thrush
  species == 'naevius' ~ 'Medium bird',
  # American robin
  species == 'migratorius' ~ 'Medium bird',
  # Unknown small bird
  class == 'Aves' & species == 'unknown' & size == 'Small' ~ 'Small bird',
  # Unknown medium bird
  class == 'Aves' & species == 'unknown' & size == 'Medium' ~ 'Medium bird',
  # Unknown large bird
  class == 'Aves' & species == 'unknown' & size == 'Large' ~ 'Large bird',
  TRUE ~ 'group'))

# Add a common name for items not IDed to species.
items <- items %>% mutate(common=case_when(
  class == 'Aves' & species == 'unknown' & size == 'Small' ~ 'average small bird',
  class == 'Aves' & species == 'unknown' & size == 'Medium' ~ 'average medium bird',
  class == 'Aves' & species == 'unknown' & size == 'Large' ~ 'average large bird',
  TRUE ~ common))

# Fil in mass for known bird species.
items <- items %>% mutate(mass=case_when(
  # Band-tailed pigeon
  species == 'fasciata' ~ 392,
  # Ruffed grouse
  species == 'umbellus' ~ 600,
  # Sooty grouse
  species == 'fulignosus' ~ 600,
  # Steller's jay
  species == 'stelleri' ~ 120,
  # Gray jay
  species == 'canadensis' ~ 67.5,
  # Swainson's thrush
  species == 'ustulatus' ~ 34,
  # Varied thrush
  species == 'naevius' ~ 82.5,
  # American robin
  species == 'migratorius' ~ 77,
  TRUE ~ 0))

# Assign each mammal to a size group.
items <- items %>% mutate(group=case_when(
  # Snowshoe hare
  species == 'americanus' ~ 'Large mammal',
  # Rat sp.
  genus == 'Rattus' ~ 'Medium mammal',
  # Bushy-tailed woodrat
  species == 'cinerea' ~ 'Medium mammal',
  # Northern flying squirrel
  species == 'sabrinus' ~ 'Small mammal',
  # Chipmunk
  genus == 'Neotamias' ~ 'Small mammal',
  # Douglas & red squirrels
  genus == 'Tamiasciurus' ~ 'Medium mammal',
  # Bat
  genus == 'Myotis' ~ 'Small mammal',
  # Unknown small mammal
  class == 'Mammalia' & species == 'unknown' & size == 'Small' ~ 'Small mammal',
  # Unknown medium mammal
  class == 'Mammalia' & species == 'unknown' & size == 'Medium' ~ 'Medium mammal',
  # Unknown large mammal
  class == 'Mammalia' & species == 'unknown' & size == 'Large' ~ 'Large mammal',
  TRUE ~ group))

# Add common name for mammals not IDed to species.
items <- items %>% mutate(common=case_when(
  class == 'Mammalia' & species == 'unknown' & size == 'Small' ~ 'average small mammal',
  class == 'Mammalia' & species == 'unknown' & size == 'Medium' ~ 'average medium mammal',
  class == 'Mammalia' & species == 'unknown' & size == 'Large' ~ 'average large mammal',
  TRUE ~ common))

# Fil in mass for known mammal species.
items <- items %>% mutate(mass=case_when(
  # Snowshoe hare
  species == 'americanus' ~ 1340,
  # Rat
  genus == 'Rattus' ~ 269.8,
  # Bush-tailed woodrat
  species == 'cinerea' ~ 374.7,
  # Northern flying-squirrel
  species == 'sabrinus' ~ 155.5,
  # Chipmunk
  genus == 'Neotamias' ~ 66.4,
  # Red squirrel
  species == 'hudsonicus' ~ 224.5,
  # Douglas squirrel
  species == 'douglasii' ~ 203.5,
  # Unknown tree squirrel
  genus == 'Tamiasciurus' & species == 'sp' ~ 214,
  # Bat
  genus == 'Myotis' ~ 5.8,
  TRUE ~ mass))

# Assign a group for items of unknown class.
items <- items %>% mutate(group=case_when(
  class == 'Unknown' & size == 'Medium' ~ 'Medium item',
  class == 'Unknown' & size == 'Small' ~ 'Small item',
  TRUE ~ group))

# Add a common name for items of unknown class.
items <- items %>% mutate(common=case_when(
  class == 'Unknown' & size == 'Medium' ~ 'average medium item',
  class == 'Unknown' & size == 'Small' ~ 'average small item',
  TRUE ~ common))

# Calculate the mean mass of each class/size.
m.mass <- items %>% filter(mass != 0) %>%
  group_by(group) %>%
  summarise(mean=mean(mass))

# Add to the list of prey items.
items <- left_join(items, m.mass, by='group') %>%
  mutate(mass = ifelse(mass > 1, mass, mean)) %>%
  dplyr::select(-mean)

# Calculate average small item.
sm <- items %>% filter(group %in% c('Small mammal', 'Small bird')) %>%
  distinct(mass) %>%
  summarize(mean(mass))

# Calculate average medium item.
md <- items %>% filter(group %in% c('Medium mammal', 'Medium bird')) %>%
  distinct(mass) %>%
  summarize(mean(mass))

# Insert into table.
items <- items %>% mutate(mass=replace(mass, group=='Small item', sm), 
                          mass=replace(mass, group=='Medium item', md))
