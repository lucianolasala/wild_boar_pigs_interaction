
This script generates three rasters, each with the number of farms
per 100 sq-km de acuerdo a tres niveles de bioseguridad (BS).

# The snalysis uses two inputs:
# (1) A file called "nodes.csv" has lon/lat of each farm and biosafety level as:
# High = 1; Medium = 2; Low = 3. These farm data was sourced from SENASA. Please
# contact Andrea Marcos (SENASA) if needed.  
# (2) A file called "ENM_argentina.tif", which is a raster file corresponding to 
# the wild boar ecological niche model developed in La Sala et al. (2023):  
# "Wild pigs and their widespread threat to biodiversity conservation in
# South America. Journal for Nature Conservation, 73, 126393". 
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
plot(arg_ras)
summary(arg_ras) 
summary(arg_ras@data@values)

writeRaster(arg_ras, filename = "D:/CIC/Analisis/Wild_boar_pigs_interaction/Rasters/Raster_10km", format = "ascii", overwrite = TRUE)

which(arg_ras@data@values == 0)  
which(is.na(arg_ras@data@values))  

#-------------------------------------------------------------------------------
# Processing of farms data by biosefty level
#-------------------------------------------------------------------------------

# Load farm data
nodes <- read.csv("D:/CIC/Analisis/Wild_boar_pigs_interaction/Datos/nodes.csv", sep = ",")
colnames(nodes)
head(nodes)

#-------------------------------------------------------------------------------
# Subset BS1 
#-------------------------------------------------------------------------------

BS_1 <- subset(nodes, BS == "High", select = c(Lat, Lon, BS))
length(BS_1$Lat)  # 37
which(is.na(BS_1$Lat | BS_1$Lon))

# Create a new variable that combines lat and lon (difference) to make sure that 
# each farm record is unique.

BS_1$COORDS_COMB <- BS_1$Lat-BS_1$Lon; head(BS_1)

length(BS_1$COORDS_COMB) 
length(unique(BS_1$COORDS_COMB))

rep_coords <- BS_1[duplicated(BS_1$COORDS_COMB),] 
length(rep_coords$COORDS_COMB)

# Crete a SpatialPointsDataFrame to save as ESRI shapefile 

coords <- BS_1[,c("Lon", "Lat")]
head(coords)
is.data.frame(coords)
which(is.na(coords$LAT|coords$LONG))

data <- as.data.frame(BS_1$BS)
head(data)
colnames(data) <- "BS"
is.data.frame(data)

crs <- CRS("+init=epsg:4326")
spdf <- SpatialPointsDataFrame(coords = coords, data = data, proj4string = crs)

class(spdf)
spdf@coords
spdf@data

writeOGR(spdf,"D:/CIC/Analisis/Wild_boar_pigs_interaction/Vectors","BS_1", driver = "ESRI Shapefile", overwrite_layer = T)

# Read raster

rm(list=ls(all=TRUE))

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

# Result and raster exploration

BS1 = pointcount(arg_ras, spdf)
plot(BS1)

writeRaster(BS1, filename = "D:/CIC/Analisis/Wild_boar_pigs_interaction/Rasters/BS1", format = "ascii", overwrite = TRUE)

BS1 <- raster("D:/CIC/Analisis/Wild_boar_pigs_interaction/Rasters/BS1.asc")
str(BS1)

setMinMax(BS1) 

table(BS1[], useNA = "always")

# Lengths
length(which(BS1[]==0))
length(which(BS1[]==1))   
length(which(BS1[]==2)) 

minValue(BS1)  
maxValue(BS1)   

BS1@data@min
BS1@data@max   

xyFromCell(BS1, 1)

hist(BS1)
hist(BS1[BS1!=0])  
sum(values(BS1!=0), na.rm = TRUE)  
canProcessInMemory(BS1)
length(which(values(BS1) %in% c(1,2)))

# Frequency table
table(BS1[])


#-------------------------------------------------------------------------------
# Subset BS2
#-------------------------------------------------------------------------------

BS_2 <- subset(nodes, BS == "Medium", select = c(Lat, Lon, BS))
head(BS_2)
length(BS_2$Lat)

which(is.na(BS_2$Lat|BS_2$Lon))

# Create a new variable that combines lat and lon (difference) to make sure that 
# each farm record is unique.

BS_2$COORDS_COMB <- BS_2$Lat-BS_2$Lon
head(BS_2)

length(BS_2$COORDS_COMB)
length(unique(BS_2$COORDS_COMB))

rep_coords <- BS_2[duplicated(BS_2$COORDS_COMB),] 
length(rep_coords$COORDS_COMB)
  
# Crete a SpatialPointsDataFrame to save as ESRI shapefile 

coords <- BS_2[,c("Lon", "Lat")]
is.data.frame(coords)
head(coords)

which(is.na(coords$LAT|coords$LONG))

datos <- as.data.frame(BS_2[,3])
head(datos)
colnames(datos) <- "BS"

crs <- CRS("+init=epsg:4326")

spdf <- SpatialPointsDataFrame(coords = coords, data = datos, proj4string = crs)

class(spdf) 
spdf@coords
spdf@data

writeOGR(spdf,"D:/CIC/Analisis/Wild_boar_pigs_interaction/Vectors","BS_2", driver = "ESRI Shapefile", overwrite_layer = T)

# Read raster
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


# Result and raster exploration
BS2 = pointcount(arg_ras, spdf)
plot(BS2)

writeRaster(BS2, "D:/CIC/Analisis/Wild_boar_pigs_interaction/Rasters/BS2", format = "ascii", overwrite = TRUE)

BS2 <- raster("D:/CIC/Analisis/Wild_boar_pigs_interaction/Rasters/BS2.asc")
class(BS2)

setMinMax(BS2)

BS2@data@values[1]
xyFromCell(BS2, 1)

hist(BS2)  

hist(BS2[BS2!=0]) 

sum(values(BS2!=0), na.rm = TRUE)  

length(which(values(BS2) %in% c(0,1,2,3,4))) 

# Frequency table

table(BS2[])

#-------------------------------------------------------------------------------
# Subset BS3
#-------------------------------------------------------------------------------

# Subset each BS level
BS_3 <- subset(nodes, BS == "Low", select = c(Lat, Lon, BS))

length(BS_3$Lat)
which(is.na(BS_3$Lat|BS_3$Lon))

# Create a new variable that combines lat and lon (difference) to make sure that 
# each farm record is unique. 
BS_3$COORDS_COMB <- BS_3$Lat - BS_3$Lon
head(BS_3)

length(BS_3$COORDS_COMB)
length(unique(BS_3$COORDS_COMB))

rep_coords <- BS_3[duplicated(BS_3$COORDS_COMB),] 
length(rep_coords$COORDS_COMB)

# Nota. Do not delete repeated records. They are farms with same coordinates 
# because they were assigned to the centroid of a county 

# Crete a SpatialPointsDataFrame to save as ESRI shapefile 
coords <- BS_3[,c("Lon", "Lat")]
is.data.frame(coords)
head(coords)

which(is.na(coords$Lat|coords$Lon))  # No hay NAs

datos <- as.data.frame(BS_3[,c("BS")])
head(datos)
colnames(datos) <- "BS"

crs <- CRS("+init=epsg:4326")

spdf <- SpatialPointsDataFrame(coords = coords, data = datos, proj4string = crs)

class(spdf)  # "SpatialPointsDataFrame"
spdf@coords
spdf@data

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

# Result and raster exploratio
BS3 = pointcount(arg_ras, spdf)
plot(BS3)

writeRaster(BS3, filename = "D:/CIC/Analisis/Wild_boar_pigs_interaction/Rasters/BS3", format = "ascii", overwrite = TRUE)

BS3 <- raster("D:/CIC/Analisis/Wild_boar_pigs_interaction/Rasters/BS3.asc")
class(BS3)

setMinMax(BS3)
hist(BS3)  

hist(BS3[BS3!=0]) 
sum(values(BS3!=0), na.rm = TRUE)
length(which(values(BS3) %in% c(1:20)))

# Frequancy table
table(BS3[])

