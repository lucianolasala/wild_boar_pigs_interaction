#------------------------------------------------------------------
# Transforma rasters base a GTiff y estandariza a rango 0-1
#------------------------------------------------------------------

rm(list=ls())

library(magrittr)
library(dplyr)
library(mc2d)  # rpert function
library(Matrix)
library(matrixcalc)
library(tidyverse)
library(raster)

#------------------------------------------------------------------
# Transformar rasters de ascii a GTiff
#------------------------------------------------------------------

rm(list=ls())

path = ("D:/CIC/Analisis/Wild_boar_pigs_interaction/Rasters/Base")

# List all .asc files in the folder
asc_files <- list.files(path = path, pattern = "//.asc$", full.names = TRUE)
asc_files

# Loop through each .asc file
for (asc_file in asc_files) {
  # Read the .asc file as a raster
  raster_data <- raster(asc_file)
  
  # Create the output .tif file name
  tif_file <- sub("//.asc$", ".tif", asc_file)
  
  # Write the raster data to a .tif file
  writeRaster(raster_data, filename = tif_file, format = "GTiff", overwrite = TRUE)
  
  # Print a message to indicate progress
  cat("Converted:", basename(asc_file), "to", basename(tif_file), "/n")
}

#------------------------------------------------------------------
# Standardize rasters layers to have values between 0-1
#------------------------------------------------------------------

setwd("D:/CIC/Analisis/Wild_boar_pigs_interaction/Rasters")

rm(list = ls())

bs1 <- raster("./Base/BS1.tif")
bs2 <- raster("./Base/BS2.tif")
bs3 <- raster("./Base/BS3.tif")
wb <- raster("./Base/WB.tif")

path = ("./Base/")

files <- list.files(path = path, pattern = ".tif$", full.names = TRUE)
files

# Raster list
raster_list <- list(bs1, bs2, bs3, wb)
variables <- as.factor(c("BS1","BS2","BS3","WB"))

# Create an empty list to store the standardized raster layers
standardized_raster_layers <- list()

# Loop over each raster in the raster list

for(i in seq_along(raster_list)) {
  # Get the current raster
  raster <- raster_list[[i]]
  
  # Calculate the maximum value of the current raster
  max_value <- maxValue(raster)
  
  # Standardize the current raster by dividing by its maximum value
  standardized_raster <- raster/max_value
  
  # Add the standardized raster to the list
  standardized_raster_layers[[i]] <- standardized_raster
}
standardized_raster_layers

# Save
for(i in seq_along(standardized_raster_layers)) {
  writeRaster(standardized_raster_layers[[i]], 
              filename = paste0("./Standardized/", variables[i], ".tif"), 
              format = "GTiff",
              overwrite=T)
}
