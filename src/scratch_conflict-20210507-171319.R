
# 95% MCP
rlk.f.mcp.95 <- telemetry.sf %>%
  filter(id == 'HAR02') %>% 
  select(id, geometry) %>%
  as_Spatial() %>%
  mcp(percent=95, unin='m', unout='ha') %>% 
  st_as_sf()

# 50% MCP
rlk.f.mcp.50 <- telemetry.sf %>%
  filter(id == 'HAR02') %>% 
  select(id, geometry) %>%
  as_Spatial() %>%
  mcp(percent=50, unin='m', unout='ha') %>% 
  st_as_sf()

# Telemetry points.
rlk.f.points <- telemetry.sf %>% 
  filter(id == 'HAR02')

# RLK nest.
rlk.nest.2020 <- telemetry.sites %>% 
  filter(nest == 'RLK2020') %>% 
  st_as_sf(coords=c('x_coord', 'y_coord'), remove=FALSE) %>%
  st_set_crs('+proj=utm +zone=10 +datum=WGS84 +units=m +no_defs')

area <- 200

radius <- sqrt(area*10000/pi)

buffer.2020 <- st_buffer(rlk.nest.2020, radius)


ggplot() +
  geom_sf(data=rlk.f.points, alpha=0.25) +
  geom_sf(data=rlk.f.mcp.50, fill=NA) +
  geom_sf(data=rlk.f.mcp.95, fill=NA) +
  geom_sf(data=buffer.2020, fill=NA, linetype='dashed') +
  geom_sf(data=rlk.nest.2020, color='red', size=2) +
  annotation_scale(location='br') +
  theme_void()


### HAR04 RLK - M
rlk.m.mcp.95 <- telemetry.sf %>%
  filter(id == 'HAR04') %>% 
  select(id, geometry) %>%
  as_Spatial() %>%
  mcp(percent=95, unin='m', unout='ha') %>% 
  st_as_sf()

rlk.m.mcp.50 <- telemetry.sf %>%
  filter(id == 'HAR04') %>% 
  select(id, geometry) %>%
  as_Spatial() %>%
  mcp(percent=50, unin='m', unout='ha') %>% 
  st_as_sf()

rlk.m.points <- telemetry.sf %>% 
  filter(id == 'HAR04')

rlk.nest.2019 <- telemetry.sites %>% 
  filter(nest == 'RLK2019') %>% 
  st_as_sf(coords=c('x_coord', 'y_coord'), remove=FALSE) %>%
  st_set_crs('+proj=utm +zone=10 +datum=WGS84 +units=m +no_defs')

ggplot() +
  geom_sf(data=rlk.m.mcp.95) +
  geom_sf(data=rlk.m.mcp.50) +
  geom_sf(data=rlk.m.points) +
  geom_sf(data=rlk.nest.2019) +
  theme_classic()







