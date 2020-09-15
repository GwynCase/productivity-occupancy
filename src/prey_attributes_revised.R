 setwd('C:/Users/gwync/sfuvault/productivity-occupancy/notebooks')
library(tidyverse)
library(lubridate)

#################
# Tidy data
#################

# Import camera data from 2019...
camera.data.2019 <- read_csv('../data/interim/camera_corrected.csv', guess_max=7000) %>% 
  
  ## drop records with no prey items identified...
  drop_na(class) %>% 
  
  ## and select only relevant columns.
  select(site, datetime, class, family, genus, species, common, size)


# Add a unique site/year identifier.
camera.data.2019$nest <- paste(camera.data.2019$site, year(camera.data.2019$datetime), sep='')


# Filter only items that at least have a size assigned...
camera.data.2019 <- camera.data.2019 %>% filter(size != 'Unknown') %>% 
  
  ## and tidy up the columns.
  select(site, nest, everything(), -datetime)
  

# Change the case on the size column to make it look nicer.
camera.data.2019$size <- str_to_lower(camera.data.2019$size)


# Bring in a list of all known prey.
prey.list <- read_csv('../data/interim/prey_attributes.csv')


# Fill in order based on family...
camera.data.2019 <- prey.list %>% select(order, family) %>% 
  right_join(camera.data.2019, by='family') %>% 
  
  ## then reorder columns so everything is pretty...
  select(site, nest, class, everything()) %>% 
  
  ## and fill in NAs in Order column with "Unknown"...
  replace_na(list(order = 'Unknown')) %>% 
  
  ## and finally add a marker for method.
  mutate(method='camera')

# Let's also make sure common names are lowercase.
camera.data.2019$common <- str_to_lower(camera.data.2019$common)


# Import prey remains data from 2019 & 2020...
remains.data <- read_csv('../data/raw/20200910_specimens.csv', guess_max=7000) %>% 
  
  ## filter only records with at least size assigned...
  filter(size != 'U') %>% 
  
  ## and rename a confusing column.
  rename(name=site)


# Bring in a list of site abbreviations and site names.
nest.list <- read_csv('../data/processed/site_abbreviations.csv')


# Join the site list to the remains data by site name.
remains.data <- left_join(remains.data, nest.list, by='name')


# Add a unique site/year identifier.
remains.data$nest <- paste(remains.data$site, year(remains.data$date), sep='')


# Select just the relevant columns...
remains.data <- remains.data %>% 
  select(site, nest, class, order, family, genus, species, common, size) %>% 
  
  ## then change the size column wording to make it look nicer...
  mutate(size=case_when(
    size == 'S' ~ 'small',
    size == 'M' ~ 'medium',
    size == 'L' ~ 'large'
  )) %>% 
  
  ## and replace all the "U"s with "Unknown"...
  mutate_all(str_replace_all, "U", "Unknown") %>% 
  
  ## and finally add a marker for method.
  mutate(method='remains')
  

# Make sure species is lowercase.
remains.data$species <- str_to_lower(remains.data$species)


# Join all the data together.
diet.items <- bind_rows(camera.data.2019, remains.data)
  

#################
# Add mass
#################

# Join the biomass data to the list of diet items.
diet.items <- prey.list %>% select(genus, species, binomial, category, mass) %>% 
  right_join(diet.items, by=c('genus', 'species'))


# For unidentified items, classify them by size and class.
diet.items <- diet.items %>% mutate(category=case_when(
  is.na(category) & class == 'Mammalia' & size == 'small' ~ 'small mammal',
  is.na(category) & class == 'Mammalia' & size == 'medium' ~ 'medium mammal',
  is.na(category) & class == 'Mammalia' & size == 'large' ~ 'large mammal',
  is.na(category) & class == 'Aves' & size == 'small' ~ 'small bird',
  is.na(category) & class == 'Aves' & size == 'medium' ~ 'medium bird',
  is.na(category) & class == 'Aves' & size == 'large' ~ 'large bird',
  is.na(category) & class == 'Unknown' ~ paste(size, 'item'),
  TRUE ~ category))


# For unidentified items, fill in the binomial column.
diet.items <- diet.items %>% replace_na(list(binomial = 'Unidentified item'))


# Create a table with average masses by collecting all the items of known mass...
average.sizes <- diet.items %>% drop_na(mass) %>% 
  group_by(category) %>% 
  
  ## averaging the mass for each size & class category...
  summarize(average=mean(mass)) %>% 
  pivot_wider(names_from=category, values_from=average) %>% 
  
  ## calculating the average mass for complete unknowns...
  mutate(`large item` = mean(`large bird`, `large mammal`),
         `medium item` = mean(`medium bird`, `medium mammal`),
         `small item` = mean(`small bird`, `small mammal`)) %>% 
  
  ## and reassembling it in a tidy format.
  pivot_longer(everything(), names_to='category', values_to='average')


# Join average mass to diet items...
diet.items <- left_join(diet.items, average.sizes, by='category') %>% 
  
  ## and fill in missing mass with average values
  mutate(mass=coalesce(mass, average)) %>% 
  
  ## then drop no longer needed average column and rearrange.
  select(site, nest, class, order, family, genus, species, binomial, common, 
         category, size, mass, method)