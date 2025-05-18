
# This script generates three rasters, each with the number of farms
# per 100 sq-km de acuerdo a tres niveles de bioseguridad (BS).
# The analysis uses two inputs:
# (1) A file "nodes.csv" with the geographic coordinates of each farm and biosecurity level as factors:
# High = 1; Medium = 2; Low = 3. These farm data was sourced from SENASA. 
# Contact Med. Vet. Andrea Marcos (SENASA) if needed.  
# (2) A file "ENM_argentina.tif", which is a raster file corresponding to 
# the wild boar ecological niche model developed in La Sala et al. (2023):  
# "Wild pigs and their widespread threat to biodiversity conservation in
# South America. Journal for Nature Conservation, 73, 126393".
# Contact the corresponding author if needed.
#-------------------------------------------------------------------------------

# Loading packages and libraries
rm(list=ls(all=TRUE))
pkgs <- c("raster","rgdal","sf")
sapply(pkgs, function(x) library(x, character.only = TRUE)) 
options(digits = 8)
options(max.print = 1000)

# Using the raster of wild boar ENM final model as base raster for Argentina
arg_ras <- raster("D:/CIC/Analisis/MNE_jabali/Modelling/Final_model_rasters/ENM_argentina.tif")

# Replacing no NAs with 0
arg_ras[!is.na(arg_ras[])] <- 0  
writeRaster(arg_ras, filename = "D:/CIC/Analisis/Wild_boar_pigs_interaction/Rasters/Raster_10km", format = "ascii", overwrite = TRUE)
 
#-------------------------------------------------------------------------------
# Processing farm data by biosecurity level
#-------------------------------------------------------------------------------

# Load farm data
nodes <- read.csv("D:/CIC/Analisis/Wild_boar_pigs_interaction/Datos/nodes.csv", sep = ",")

#-------------------------------------------------------------------------------
# Subset high biosecurity farms (BS1) 
#-------------------------------------------------------------------------------

BS_1 <- subset(nodes, BS == "High", select = c(Lat, Lon, BS))
which(is.na(BS_1$Lat|BS_2$Lon))

# Create a new variable that includes the difference between latitude and longitude 
# to make sure that each farm record is unique

BS_1$COORDS_COMB <- BS_1$Lat-BS_1$Lon; head(BS_1)

# Check
length(BS_1$COORDS_COMB) 
length(unique(BS_1$COORDS_COMB))
rep_coords <- BS_1[duplicated(BS_1$COORDS_COMB),] 
length(rep_coords$COORDS_COMB)

# Crete a SpatialPointsDataFrame and save as ESRI shapefile 
coords <- BS_1[,c("Lon", "Lat")]
which(is.na(coords$LAT|coords$LONG))

data <- as.data.frame(BS_1$BS)
colnames(data) <- "BS"

crs <- CRS("+init=epsg:4326")
spdf <- SpatialPointsDataFrame(coords = coords, data = data, proj4string = crs)

writeOGR(spdf,"D:/CIC/Analisis/Wild_boar_pigs_interaction/Vectors","BS_1", driver = "ESRI Shapefile", overwrite_layer = T)

# Read Argentina's raster created above
arg_ras <- raster("D:/CIC/Analisis/Wild_boar_pigs_interaction/Rasters/Raster_10km.asc")

# Function to count farms/pixel
pointcount = function(arg_ras, spdf){
  
  # Raster as input:
  r2 = arg_ras
  
  # Get cell index for each point and make a table:
  counts = table(cellFromXY(arg_ras, spdf))
  
  # Fill in raster with counts from the cell index:
  r2[as.numeric(names(counts))] = counts
  return(r2)
}

# Result
BS1 = pointcount(arg_ras, spdf)
writeRaster(BS1, filename = "D:/CIC/Analisis/Wild_boar_pigs_interaction/Rasters/BS1", format = "ascii", overwrite = TRUE)

#-------------------------------------------------------------------------------
# Subset BS2
#-------------------------------------------------------------------------------

BS_2 <- subset(nodes, BS == "Medium", select = c(Lat, Lon, BS))
which(is.na(BS_2$Lat|BS_2$Lon))

# Create a new variable that includes the difference between latitude and longitude 
# to make sure that each farm record is unique

BS_2$COORDS_COMB <- BS_2$Lat-BS_2$Lon
head(BS_2)
  
# Crete a SpatialPointsDataFrame and save as ESRI shapefile 

coords <- BS_2[,c("Lon", "Lat")]
which(is.na(coords$LAT|coords$LONG))

datos <- as.data.frame(BS_2[,3])
head(datos)
colnames(datos) <- "BS"

crs <- CRS("+init=epsg:4326")
spdf <- SpatialPointsDataFrame(coords = coords, data = datos, proj4string = crs)

writeOGR(spdf,"D:/CIC/Analisis/Wild_boar_pigs_interaction/Vectors","BS_2", driver = "ESRI Shapefile", overwrite_layer = T)

# Read Argentina's raster created above
arg_ras <- raster("D:/CIC/Analisis/Wild_boar_pigs_interaction/Rasters/Raster_10km.asc")

# Function to count farms/pixel
pointcount = function(arg_ras, spdf){
  
  # Raster as input:
  r2 = arg_ras
  
  # Get the cell index for each point and make a table:
  counts = table(cellFromXY(arg_ras, spdf))
  
  # Fill in the raster with the counts from the cell index:
  r2[as.numeric(names(counts))] = counts
  return(r2)
}

# Result
BS2 = pointcount(arg_ras, spdf)
writeRaster(BS2, "D:/CIC/Analisis/Wild_boar_pigs_interaction/Rasters/BS2", format = "ascii", overwrite = TRUE)

#-------------------------------------------------------------------------------
# Subset BS3
#-------------------------------------------------------------------------------

# Subset each BS level
BS_3 <- subset(nodes, BS == "Low", select = c(Lat, Lon, BS))

length(BS_3$Lat)
which(is.na(BS_3$Lat|BS_3$Lon))

# Create a new variable that includes the difference between latitude and longitude 
# to make sure that each farm record is unique 
BS_3$COORDS_COMB <- BS_3$Lat - BS_3$Lon

length(BS_3$COORDS_COMB)
length(unique(BS_3$COORDS_COMB))

rep_coords <- BS_3[duplicated(BS_3$COORDS_COMB),] 
length(rep_coords$COORDS_COMB)

# Note. Do not delete repeated records. They are farms with same coordinates 
# because they were assigned to the centroid of a county 

# Crete a SpatialPointsDataFrame to save as ESRI shapefile 
coords <- BS_3[,c("Lon", "Lat")]

which(is.na(coords$Lat|coords$Lon))  # No hay NAs

datos <- as.data.frame(BS_3[,c("BS")])
colnames(datos) <- "BS"

crs <- CRS("+init=epsg:4326")

spdf <- SpatialPointsDataFrame(coords = coords, data = datos, proj4string = crs)

writeOGR(spdf,"D:/CIC/Analisis/Wild_boar_pigs_interaction/Vectors","BS_3", driver = "ESRI Shapefile", overwrite_layer = T)

# Function to count farms/pixel
pointcount = function(arg_ras, spdf){
  
  # Raster as input:
  r2 = arg_ras
  
  # Get the cell index for each point and make a table:
  counts = table(cellFromXY(arg_ras, spdf))
  
  # Fill in the raster with the counts from the cell index:
  r2[as.numeric(names(counts))] = counts
  return(r2)
}

# Result
BS3 = pointcount(arg_ras, spdf)
writeRaster(BS3, filename = "D:/CIC/Analisis/Wild_boar_pigs_interaction/Rasters/BS3", format = "ascii", overwrite = TRUE)
