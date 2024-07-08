# Load necessary libraries
library(terra)
library(sf)
library(tidyverse)
library(FedData)

# import other covariates for reference
covariates <- scale(terra::rast(system.file('extdata/SetophagaCovariates.tif', package = "PointedSDMs")))

# Define the area of interest (Pennsylvania)
pa_boundary <- USAboundaries::us_states(states = "Pennsylvania")

# Download the latest NLCD data (2019) for Pennsylvania
nlcd <- get_nlcd(template = pa_boundary, label = "PA", year = 2019)

# Extract only the coniferous forest class (value 42 in NLCD classification)
conif_forest <- nlcd == 42

# Resample the original raster to match the template
conif_cover <- terra::project(conif_forest, covariates, method="average")

# scale
conif_cover <- scale(conif_cover)  

# assign name
names(conif_cover) <- "conif_cover"

# Save the result
writeRaster(conif_cover, "Data/Vignette_setophaga/pa_conif_cover.tiff", overwrite = TRUE)
