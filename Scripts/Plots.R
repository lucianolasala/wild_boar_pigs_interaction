
#--------------------------------------------------------------
# Plot distribucion de S entre positivos y negativos (NC)
#--------------------------------------------------------------

rm(list=ls())
library(magrittr)
library(dplyr)
library(ggplot2)

senasa <- read.csv("D:/CIC/Analisis/Wild_boar_pigs_interaction/Datos/SENASA/Encuesta_final.csv")
head(senasa)

# Filter points with Value = 0 and 1
absent <- senasa %>% filter(WB == 0) %>% dplyr::select(Lon, Lat)
present <- senasa %>% filter(WB == 1) %>% dplyr::select(Lon, Lat)

# Random sample of 1000 points from each group and extract values
points_value_0 <- sample_n(absent, 1000, replace = TRUE)
points_value_1 <- sample_n(present, 1000, replace = TRUE)

# Load the raster
model_avg <- raster("./Model_outputs/average.tif")

# Extract values
values_0 <- raster::extract(model_avg, absent)  # points_value_0
values_1 <- raster::extract(model_avg, present)  # points_value_1

# Plot the distribution of values
df <- data.frame(values = c(values_0, values_1), 
                 group = c(rep("Absent", length(values_0)), 
                           rep("Present", length(values_1))))

ggplot(df, aes(x = values, fill = group)) + 
  geom_density(alpha = 0.5) + 
  theme_minimal() + 
  labs(title = "Distribucion de valores", y = "Densidad")


#--------------------------------------------------------------
# Plot distribucion de S entre positivos y negativos (LFLS)
#--------------------------------------------------------------

rm(list=ls())

model_avg <- raster("D:/CIC/Analisis/Wild_boar_pigs_interaction/Model_outputs/average.tif")
senasa <- read.csv("D:/CIC/Analisis/Wild_boar_pigs_interaction/Datos/SENASA/Encuesta_final.csv")
head(senasa)

# Filter points with Value = 0 and 1
points_value_0 <- senasa[senasa$WB == 0, 1:2]
points_value_1 <- senasa[senasa$WB == 1, 1:2]

# Extract raster values for points with Value = 0 and 1
extracted_values_value_0 <- extract(model_avg, points_value_0)
extracted_values_value_1 <- extract(model_avg, points_value_1)

wb_abs <- as.data.frame(extracted_values_value_0) 
head(wb_abs)
wb_abs$WB <- "Absent"

wb_pres <- as.data.frame(extracted_values_value_1) 
head(wb_pres)
wb_pres$WB <- "Present"

# Rename columns in wb_pres and wb_abs data frames so we can use rbind
names(wb_pres) <- c("Values", "WB")
names(wb_abs) <- c("Values", "WB")

new_df <- rbind(wb_abs, wb_pres)
head(new_df)

# Convert the "WB" column to a factor to ensure correct plotting order
new_df$WB <- factor(new_df$WB, levels = c("Absent", "Present"))

# Create a histogram
histogram <- ggplot(new_df, aes(x = Values, fill = WB)) +
  geom_histogram(position = "identity", alpha = 0.5, bins = 100) +
  labs(x = "Extracted Values", y = "Frequency", title = "Distribution of values") +
  scale_fill_manual(values = c("blue", "red"), labels = c("Absent", "Present")) +
  theme_minimal()

# Print the histogram
print(histogram)

# Convert character columns to numeric, handling non-numeric values
points$Lat <- as.numeric(as.character(points$Lat))
points$Lon <- as.numeric(as.character(points$Lon))

# Check for NAs
if (any(is.na(points$Lon)) || any(is.na(points$Lat))) {
  warning("Some values in 'Lon' or 'Lat' columns could not be converted to numeric.")
}

# Identify problematic values
problematic_lon <- points[is.na(points$Lon), "Lon"]
problematic_lat <- points[is.na(points$Lat), "Lat"]

# Print problematic values
print(problematic_lon)
print(problematic_lat)


