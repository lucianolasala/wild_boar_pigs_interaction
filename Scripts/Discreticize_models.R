#--------------------------------------------------------------
# Discretizar modelos finales
#--------------------------------------------------------------

rm(list=ls())

avg <- raster("D:/CIC/Analisis/Wild_boar_pigs_interaction/Model_outputs/average.tif")
perc_5 <- raster("D:/CIC/Analisis/Wild_boar_pigs_interaction/Model_outputs/perc_5.tif")
perc_95 <- raster("D:/CIC/Analisis/Wild_boar_pigs_interaction/Model_outputs/perc_95.tif")

# path <- ("D:/CIC/Analisis/Wild_boar_pigs_interaction/Model_outputs") 
# files <- list.files(path = path, pattern = ".tif$", full.names = TRUE)
# files

# Raster list
raster_list <- list(avg, perc_5, perc_95)
variables <- as.factor(c("Average","Percentile_5","Percentile_95"))

# Empty list to store discretized models
discrete_models <- list()

# Discretize raster into categories
for(i in 1:length(files)){
  # Get the current raster
  raster <- raster(files[i])
  
  # Breakpoints
  breaks <- quantile(values(raster), probs = seq(0, 1, by = 0.2), na.rm = TRUE)
  
  # Raster categories
  raster_categories <- cut(values(raster), breaks = breaks, labels = c("Very Low", "Low", "Medium", "High", "Very High"))
  
  # Reclassify raster with category labels
  reclassified <- setValues(raster, raster_categories)
  
  # Save reclassified raster
  writeRaster(reclassified, filename = paste(getwd(), "/Reclass/", basename(files[i]), sep = ''), format = "GTiff", overwrite = TRUE)
}

#--------------------------------------------------------------
# Discretizar modelos. Alternativa similar 
#--------------------------------------------------------------

rm(list=ls())

# Define file paths
avg_path <- "D:/CIC/Analisis/Wild_boar_pigs_interaction/Model_outputs/average.tif"
perc_5_path <- "D:/CIC/Analisis/Wild_boar_pigs_interaction/Model_outputs/perc_5.tif"
perc_95_path <- "D:/CIC/Analisis/Wild_boar_pigs_interaction/Model_outputs/perc_95.tif"

# Load raster layers
avg <- raster(avg_path)
perc_5 <- raster(perc_5_path)
perc_95 <- raster(perc_95_path)

# Raster list
raster_list <- list(avg, perc_5, perc_95)
variables <- c("Average", "Percentile_5", "Percentile_95")

# Discretize raster into categories

for(i in seq_along(raster_list)){
  # Get the current raster
  raster <- raster_list[[i]]
  
  # Breakpoints
  breaks <- quantile(values(raster), probs = seq(0, 1, by = 0.2), na.rm = TRUE)
  
  # Raster categories
  raster_categories <- cut(values(raster), breaks = breaks, labels = c("Very Low", "Low", "Medium", "High", "Very High"))
  
  # Reclassify raster with category labels
  reclassified <- setValues(raster, raster_categories)
  
  # Save reclassified raster
  writeRaster(reclassified, filename = paste(getwd(), "/Reclass/", variables[i], sep = ''), format = "GTiff", overwrite = TRUE)
}
