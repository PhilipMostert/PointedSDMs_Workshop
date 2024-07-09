# Load necessary libraries
library(tidycensus)
library(sf)
library(ggplot2)
library(viridis)

# Set your Census API key
# You need to sign up for a free API key at http://api.census.gov/data/key_signup.html
census_api_key("Your_API_key", install = TRUE, overwrite=TRUE)

# Download population data for Pennsylvania counties
pa_pop <- get_acs(
  geography = "tract",
  variables = "B01003_001",  # Total population estimate
  state = "PA",
  year = 2019,
  geometry = TRUE
)

# Calculate area of each county in square kilometers
pa_pop$area_sqkm <- st_area(pa_pop) / 1e6

# Calculate population density
pa_pop$density <- pa_pop$estimate / as.numeric(pa_pop$area_sqkm)
write_sf(pa_pop, "Data/Vignette_setophaga/PA_pop_density.shp")
