setwd('C:/Users/gwync/sfuvault/Data/External')
library(tidyverse)
library(sf)
library(raster)

df %>% 
  filter(SiteName == 'WrayCreek') %>% 
  #filter(SignType == 'CR') %>% 
  view()

df %>% select(SiteName) %>% distinct() %>% view()
