library(sf)
library(dplyr)
proj <- "+proj=utm +zone=17 +datum=WGS84 +units=km"
PA <- USAboundaries::us_states(states = "Pennsylvania")
PA <- st_transform(PA, proj)
species <- c('Setophaga caerulescens', 'Setophaga magnolia', 'Setophaga fusca')
eBird <- rgbif::occ_data(datasetKey = '4fa7b334-ce0d-4e88-aaae-2e0c138d049e',
                      geometry = st_bbox(st_transform(PA, crs = '+proj=longlat +datum=WGS84 +no_defs')),
                      scientificName = species, year = '2005,2009',
                      limit = 10000) #Time
data_sp <- list()
for (spec in species) {

  data_sp[[spec]] <- st_transform(st_as_sf(
  x = eBird[[spec]]$data[, names(eBird[[spec]]$data) %in% c("decimalLatitude", "decimalLongitude", 'scientificName', 'year')],
  coords = c('decimalLongitude', 'decimalLatitude'),
  crs = '+proj=longlat +datum=WGS84 +no_defs'), proj)
  data_sp[[spec]]$scientificName <- sub(' ', '_',spec)

}
eBird <- do.call(rbind, data_sp)
eBird <- eBird %>% rename(Species_name = scientificName,
                          Year = year)
eBird <- eBird[unlist(st_intersects(PA, eBird)),]

##Download Data here
 #Not sure how to download this automatically
#https://datadryad.org/stash/dataset/doi:10.5061/dryad.t4g871v
#PA_BirdAtlas_bird_counts <- read.csv("PA_BirdAtlas_bird_counts.csv")
#PA_BirdAtlas_pointcount_location_and_conditions <- read.csv('PA_BirdAtlas_pointcount_location_and_conditions.csv')
BBA <- PA_BirdAtlas_bird_counts %>% 
  filter(SP_code %in% c('BTBW', 'BLBW', 'MAWA')) %>%
  mutate(total = dplyr::select(., c(3,5,7,9,11)) %>% rowSums(na.rm = TRUE)) %>%
  mutate(NPres = if_else(total == 0, 0, 1)) %>% 
  left_join(PA_BirdAtlas_pointcount_location_and_conditions) %>%
  mutate(Year = as.numeric(format(as.Date(.$Date, "%m/%d/%Y"), '%Y')) + 2000) %>%
  filter(Year >= 2005, Year <= 2009) %>%
  select('BBA_ID', 'SP_code', 'NPres', 'GPS_N', 'GPS_W', 'Year') %>%
  mutate(SP_code = case_when(
    SP_code == "BTBW" ~ 'Setophaga_caerulescens',
    SP_code == 'BLBW' ~ 'Setophaga_fusca',
    SP_code == 'MAWA' ~ 'Setophaga_magnolia')) %>%
  rename(Species_name = 'SP_code') %>%
  st_as_sf(., coords = c('GPS_W','GPS_N'), 
           crs = '+proj=longlat +datum=WGS84 +no_defs') %>%
    st_transform(., crs = proj)
BBA$BBA_ID <- NULL

routes.url <- 'https://www.sciencebase.gov/catalog/file/get/625f151ed34e85fa62b7f926?f=__disk__95%2F4e%2F1a%2F954e1a5ec1112c34bba91ad1fce6bd2d3c9df90e'
routes.file <- 'Data/routes.zip'

routes <- download.file(routes.url, routes.file)

routes <- read.csv(unzip(routes.file)) %>% 
  filter(StateNum == 72) %>% select(Longitude, Latitude, Route)

BBS.url <- 'https://www.sciencebase.gov/catalog/file/get/625f151ed34e85fa62b7f926?f=__disk__b5%2Fde%2F6b%2Fb5de6b3d221373125e90a85b5fcba4b7059c7f94'
BBS.file <- 'Data/50_stops.zip'

stops <- download.file(BBS.url, BBS.file, ) 

stops <- unzip(BBS.file)

BBS_Wren <- read.csv(unzip(stops[grep('Fifty8.zip', stops)]))  %>% 
  filter(AOU %in% c(06540, 06570, 06620), Year >= 2005, Year <= 2009, StateNum == 72) #06570 Mag #06620 Fus #06540 Cae

BBS_Wren <- BBS_Wren %>% mutate(Counts = rowSums(dplyr::select(., starts_with("stop"))),
                                NPres = rowSums(dplyr::select(., starts_with("stop")) > 0)) %>%
  mutate(Ntrials = rowSums(!is.na(dplyr::select(., starts_with("stop"))))) %>%
  mutate(AOU = case_when(
    AOU == 06540 ~ 'Setophaga_caerulescens',
    AOU == 06620 ~ 'Setophaga_fusca',
    AOU == 06570 ~ 'Setophaga_magnolia'))

BBS_Wren <- BBS_Wren %>% group_by(Route, AOU) %>%
  summarise(
    Year = Year,
    NPres = NPres,
    Counts = sum(Counts)) %>%
  rename(Species_name = AOU)

routes_in <- intersect(BBS_Wren$Route, routes$Route)
routes <- routes[routes$Route %in% routes_in,]

BBS_Wren <- BBS_Wren %>% left_join(routes)
BBS_Wren$Route <- NULL

BBS <- st_as_sf(x = BBS_Wren,
                coords = c('Longitude', 'Latitude'),
                crs = '+proj=longlat +datum=WGS84 +no_defs') %>%
  st_transform(., proj)
BBS$Trials <- 50
  

saveRDS(list(eBird = eBird,
             BBA = BBA, 
             BBS = BBS), file = 'Data/Vignette_setophaga/SetophagaData.rds')
