rm(list=ls(all=TRUE))
if(!require('devtools')) install.packages('devtools')
devtools::install_github('luismurao/ntbox', force = TRUE)

library("ntbox")
run_ntbox()
library("raster")

# Load raster
r <- raster("D:/CIC/Analisis/Wild_boar_pigs_interaction/Model_outputs/Rasters_modificados_continuos/average.tif")
hist(r)

# Load your presence points
occ_data <- read.csv("D:/CIC/Analisis/Wild_boar_pigs_interaction/Datos/SENASA/Procesadas/Encuestas_positivos_ntbox_R.csv")  # Replace with your actual data
head(occ_data)
length(occ_data$longitude)
summary(occ_data)

# Extract raster values at occurrence points
occ_values <- raster::extract(r, occ_data)

# Check if extraction was successful
print(occ_values)

# Get the valid indices where occ_values are not zero
valid_occ_indices <- which(occ_values != -9999)

# Filter the occurrence values and coordinates based on valid indices
occ_values <- occ_values[valid_occ_indices]           # This is now a vector of valid occurrence values
occ_coords <- occ_data[valid_occ_indices, , drop = FALSE]  # Filter coordinates accordingly

# Step 6: Perform partial ROC validation
# Perform partial ROC validation
p_roc_result <- pROC(continuous_mod = r,
     test_data = occ_coords,
     n_iter = 10000,
     E_percent = 5,
     boost_percent = 50,
     parallel = FALSE,
     ncores = 4,
     rseed = FALSE)

p_roc_result$pROC_summary


# Extract AUC ratios from the result
auc_ratios <- p_roc_result$pROC_results$auc_ratio
head(auc_ratios)

# Base R histogram
hist(auc_ratios, 
     main = "Partial ROC AUC Ratios", 
     xlab = "AUC Ratios", 
     col = "lightblue", 
     border = "black")

# Optional: Add density plot
plot(density(auc_ratios), 
     main = "Density of AUC Ratios", 
     xlab = "AUC Ratios", 
     col = "blue", 
     lwd = 2)


#-----------------------------------------------------
# Example from author
#-----------------------------------------------------

# Load a continuous model
conti_model <- raster::raster(system.file("extdata",
                                          "ambystoma_model.tif",
                                          package="ntbox"))
# Read validation (test) data
test_data <- read.csv(system.file("extdata",
                                  "ambystoma_validation.csv",
                                  package = "ntbox"))

# Filter only presences as the Partial ROC only needs occurrence data
test_data <- dplyr::filter(test_data, presence_absence==1)
test_data <- test_data[,c("longitude","latitude")]
head(test_data)

partial_roc <- pROC(continuous_mod=conti_model,
                    test_data = test_data,
                    n_iter=1000,E_percent=5,
                    boost_percent=50,
                    parallel=FALSE)
partial_roc

# View results
print(partial_roc)

# Optional: Plot the pROC curve
plot(partial_roc)
