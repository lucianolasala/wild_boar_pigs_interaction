#---------------------------------------------------------------------
# En este script se generan tres rasters con densidad de granjas/10 km2.
# de acuerdo a tres niveles de bioseguridad (BS).

# En el archivo "nodes.csv", tenemos coordenadas de cada granja y nivel de BS como:
# Alto = 1
# Medio = 2
# Bajo = 3

# El raster del modelo de jabalí (ENM_argentina) se utiliza como "base".
#---------------------------------------------------------------------

# Paquetes y librerías

rm(list=ls(all=TRUE))

pkgs <- c("raster","rgdal","sf")

sapply(pkgs, function(x) library(x, character.only = TRUE)) 

options(digits = 8)
options(max.print = 1000)

#---------------------------------------------------------------------
# Uso del raster de modelo final para jabalí
#---------------------------------------------------------------------

arg_ras <- raster("D:/CIC/Analisis/MNE_jabali/Modelling/Final_model_rasters/ENM_argentina.tif")

# Reemplazo no NA's por ceros

arg_ras[!is.na(arg_ras[])] <- 0  # Estos ceros reemplazan el valor inicial de cada pixel con valor
plot(arg_ras)
summary(arg_ras)  # min and max = 0
summary(arg_ras@data@values)  # Todos 0 y 47895 NAs

writeRaster(arg_ras, filename = "D:/CIC/Analisis/Wild_boar_pigs_interaction/Rasters/Raster_10km", format = "ascii", overwrite = TRUE)

which(arg_ras@data@values == 0)  # 34467
which(is.na(arg_ras@data@values))  # 47965 NAs

#---------------------------------------------------------------------
# BS1
#---------------------------------------------------------------------
# Carga de existencias porcinas
#---------------------------------------------------------------------

rm(list=ls(all=TRUE))

options(digits = 8)

nodos <- read.csv("D:/CIC/Analisis/Wild_boar_pigs_interaction/Datos/nodes.csv", sep = ",")
colnames(nodos)
head(nodos)

# Subset each BS level

BS_1 <- subset(nodos, BS == "Alto", select = c(Lat, Lon, BS))

length(BS_1$Lat)  # 37

which(is.na(BS_1$Lat | BS_1$Lon))  # 0

#---------------------------------------------------------------------  
# Creo nueva variable que combine Lat y Lon para asegurarme de tener 
# registros unicos. Se usa la diferencia entre Lat y Lon para generar 
# un numero unico
#---------------------------------------------------------------------

BS_1$COORDS_COMB <- BS_1$Lat-BS_1$Lon
head(BS_1)

length(BS_1$COORDS_COMB) # 37
length(unique(BS_1$COORDS_COMB))  # 37

rep_coords <- BS_1[duplicated(BS_1$COORDS_COMB),] 
length(rep_coords$COORDS_COMB)  # 0

#-------------------------------------------------------------------------------
# Creo un SpatialPointsDataFrame para guardar como ESRI shp. 
#-------------------------------------------------------------------------------

coords <- BS_1[,c("Lon", "Lat")]
head(coords)
is.data.frame(coords)

which(is.na(coords$LAT|coords$LONG))  # No hay NAs

datos <- as.data.frame(BS_1$BS)
head(datos)
colnames(datos) <- "BS"
is.data.frame(datos)

crs <- CRS("+init=epsg:4326")
spdf <- SpatialPointsDataFrame(coords = coords, data = datos, proj4string = crs)

class(spdf)  # "SpatialPointsDataFrame"
spdf@coords
spdf@data

writeOGR(spdf,"D:/CIC/Analisis/Wild_boar_pigs_interaction/Vectors","BS_1", driver = "ESRI Shapefile", overwrite_layer = T)

#-----------------------------------------------------------------------------
# Leer shapefile de existencias y raster
#-----------------------------------------------------------------------------

rm(list=ls(all=TRUE))

BS1 <- readOGR("D:/CIC/Analisis/Wild_boar_pigs_interaction/Vectors/BS_1.shp")
arg_ras <- raster("D:/CIC/Analisis/Wild_boar_pigs_interaction/Rasters/Raster_10km.asc")

#-----------------------------------------------------------------------------
# Transformar shapefile en data frame
#-----------------------------------------------------------------------------

BS1.df <- as(BS1, "data.frame"); colnames(BS1.df)

names(BS1.df)[1:3] <- paste(c("BS","x","y")); head(BS1.df)

#-------------------------------------------------------------------------------
# Creo un SpatialPointsDataFrame 
#-------------------------------------------------------------------------------

coordenadas <- BS1.df[, c("x", "y")]
is.data.frame(coordenadas)

which(is.na(coordenadas$x|coordenadas$y))  # No hay NAs

datos <- as.data.frame(BS1.df$BS)
head(datos)
is.data.frame(datos)
colnames(datos) <- "BS"

crs <- CRS("+init=epsg:4326")

#-----------------------------------------------------------------------------
# Crear objeto SpatialPointsDataFrame
#-----------------------------------------------------------------------------

spdf <- SpatialPointsDataFrame(coords = coordenadas, data = datos, proj4string = crs)

#-------------------------------------------------------------------------------
# Funcion para contar granjas/celda
#-------------------------------------------------------------------------------

pointcount = function(arg_ras, spdf){
  
  # Raster como input:
  r2 = arg_ras
  
  # Get the cell index for each point and make a table:
  counts = table(cellFromXY(arg_ras, spdf))
  
  # Fill in the raster with the counts from the cell index:
  r2[as.numeric(names(counts))] = counts
  return(r2)
}

#-------------------------------------------------------------------------------
# Resultado y exploracion del raster
#-------------------------------------------------------------------------------

BS1 = pointcount(arg_ras, spdf)
plot(BS1)

writeRaster(BS1, filename = "D:/CIC/Analisis/Wild_boar_pigs_interaction/Rasters/BS1", format = "ascii", overwrite = TRUE)

BS1 <- raster("D:/CIC/Analisis/Wild_boar_pigs_interaction/Rasters/BS1.asc")
str(BS1)

setMinMax(BS1) # 0, 2  

table(BS1[], useNA = "always")

# Lengths
  
length(which(BS1[]==0))  # 34430
length(which(BS1[]==1))   # 37
length(which(BS1[]==2))   # 0

minValue(BS1)  # 0
maxValue(BS1)  # 2  

BS1@data@min  # 0
BS1@data@max  # 2  

xyFromCell(BS1, 1)

hist(BS1)  # Gran frecuencia de ceros

hist(BS1[BS1!=0])  # Ademas de ceros, solo existen 1 y 2

sum(values(BS1!=0), na.rm = TRUE)  # 36 pixels with value
canProcessInMemory(BS1)

length(which(values(BS1) %in% c(1,2)))  # 36

# Tabla de frecuencias para granjas BS1 por celda

table(BS1[])

#---------------------------------------------------------------------
# BS2
#---------------------------------------------------------------------
# Carga de existencias porcinas
#---------------------------------------------------------------------

rm(list=ls(all=TRUE))

nodos <- read.csv("D:/CIC/Analisis/Wild_boar_pigs_interaction/Datos/nodes.csv", sep = ",")
colnames(nodos)
head(nodos)

# Subset each BS level

BS_2 <- subset(nodos, BS == "Medio", select = c(Lat, Lon, BS))
head(BS_2)
length(BS_2$Lat)  # 213

which(is.na(BS_2$Lat|BS_2$Lon))  # 0

#---------------------------------------------------------------------  
# Creo nueva variable que combine LAT y LONG para asegurarme de tener 
# registros unicos. Se usa la diferencia entre LAT y LONG para generar 
# un numero unico
#---------------------------------------------------------------------

BS_2$COORDS_COMB <- BS_2$Lat-BS_2$Lon
head(BS_2)

length(BS_2$COORDS_COMB) # 213
length(unique(BS_2$COORDS_COMB))  # 213

rep_coords <- BS_2[duplicated(BS_2$COORDS_COMB),] 
length(rep_coords$COORDS_COMB)  # 0
  
#-------------------------------------------------------------------------------
# Creo un SpatialPointsDataFrame para guardar como ESRI shp. 
#-------------------------------------------------------------------------------

coords <- BS_2[,c("Lon", "Lat")]
is.data.frame(coords)
head(coords)

which(is.na(coords$LAT|coords$LONG))  # No hay NAs

datos <- as.data.frame(BS_2[,3])
head(datos)
colnames(datos) <- "BS"

crs <- CRS("+init=epsg:4326")

spdf <- SpatialPointsDataFrame(coords = coords, data = datos, proj4string = crs)

class(spdf)  # "SpatialPointsDataFrame"
spdf@coords
spdf@data

writeOGR(spdf,"D:/CIC/Analisis/Wild_boar_pigs_interaction/Vectors","BS_2", driver = "ESRI Shapefile", overwrite_layer = T)

#-----------------------------------------------------------------------------
# Leer shapefile de existencias y raster
#-----------------------------------------------------------------------------

rm(list=ls(all=TRUE))

BS2 <- readOGR("D:/CIC/Analisis/Wild_boar_pigs_interaction/Vectors/BS_2.shp")
arg_ras <- raster("D:/CIC/Analisis/Wild_boar_pigs_interaction/Rasters/Raster_10km.asc")

#-----------------------------------------------------------------------------
# Transformar shapefile en data frame
#-----------------------------------------------------------------------------

BS2.df <- as(BS2, "data.frame")
colnames(BS2.df)

names(BS2.df)[1:3] <- paste(c("BS","x","y"))
head(BS2.df)

#-------------------------------------------------------------------------------
# Creo un SpatialPointsDataFrame 
#-------------------------------------------------------------------------------

coordenadas <- BS2.df[,c("x", "y")]
is.data.frame(coordenadas)

which(is.na(coordenadas$x | coordenadas$y))  # No hay NAs

datos <- as.data.frame(BS2.df[,c("BS")])
head(datos)
is.data.frame(datos)
colnames(datos) <- "BS"

crs <- CRS("+init=epsg:4326")

#-----------------------------------------------------------------------------
# Crear objeto SpatialPointsDataFrame
#-----------------------------------------------------------------------------

spdf <- SpatialPointsDataFrame(coords = coordenadas, data = datos, proj4string = crs)

#-------------------------------------------------------------------------------
# Funcion para contar granjas / celda
#-------------------------------------------------------------------------------

pointcount = function(arg_ras, spdf){
  
  # Raster como input:
  r2 = arg_ras
  
  # Get the cell index for each point and make a table:
  counts = table(cellFromXY(arg_ras, spdf))
  
  # Fill in the raster with the counts from the cell index:
  r2[as.numeric(names(counts))] = counts
  return(r2)
}

#-------------------------------------------------------------------------------
# Resultado y exploracion del raster
#-------------------------------------------------------------------------------

BS2 = pointcount(arg_ras, spdf)
plot(BS2)

writeRaster(BS2, "D:/CIC/Analisis/Wild_boar_pigs_interaction/Rasters/BS2", format = "ascii", overwrite = TRUE)

BS2 <- raster("D:/CIC/Analisis/Wild_boar_pigs_interaction/Rasters/BS2.asc")
class(BS2)

setMinMax(BS2)  # 0, 4

BS2@data@values[1]
xyFromCell(BS2, 1)

hist(BS2)  # Gran frecuencia de ceros

hist(BS2[BS2!=0])  # Ademas de ceros, solo existen 1 y 2

sum(values(BS2!=0), na.rm = TRUE)  # 203 pixels with value

length(which(values(BS2) %in% c(0,1,2,3,4))) # 34468

# Tabla de frecuencias para granjas BS1 por celda

table(BS2[])

# 0       1       2     3     4 
# 34273   182     9     3     1

#---------------------------------------------------------------------
# BS3
#---------------------------------------------------------------------
# Carga de existencias porcinas
#---------------------------------------------------------------------

rm(list=ls(all=TRUE))

options(digits = 8)

nodos <- read.csv("D:/CIC/Analisis/Wild_boar_pigs_interaction/Datos/nodes.csv", sep = ",")
head(nodos)

# Subset each BS level

BS_3 <- subset(nodos, BS == "Bajo", select = c(Lat, Lon, BS))

length(BS_3$Lat)  # 4969

which(is.na(BS_3$Lat|BS_3$Lon))  # 0

#---------------------------------------------------------------------  
# Creo nueva variable que combine LAT y LONG para asegurarme de tener 
# registros unicos. Se usa la diferencia entre LAT y LONG para generar 
# un numero unico
#---------------------------------------------------------------------

BS_3$COORDS_COMB <- BS_3$Lat - BS_3$Lon
head(BS_3)

length(BS_3$COORDS_COMB) # 4969
length(unique(BS_3$COORDS_COMB))  # 4969

rep_coords <- BS_3[duplicated(BS_3$COORDS_COMB),] 
length(rep_coords$COORDS_COMB)  # 20

# Nota. No se eliminan. Son granjas con misma coordenadas porque deben asignarse 
# al centro de algún depto./municipio. 

#-------------------------------------------------------------------------------
# Creo un SpatialPointsDataFrame para guardar como ESRI shp. 
#-------------------------------------------------------------------------------

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

#-----------------------------------------------------------------------------
# Leer shapefile de existencias y raster
#-----------------------------------------------------------------------------

rm(list=ls(all=TRUE))

BS3 <- readOGR("D:/CIC/Analisis/Wild_boar_pigs_interaction/Vectors/BS_3.shp")
arg_ras <- raster("D:/CIC/Analisis/Wild_boar_pigs_interaction/Rasters/Raster_10km.asc")

#-----------------------------------------------------------------------------
# Transformar shapefile en data frame
#-----------------------------------------------------------------------------

BS3.df <- as(BS3, "data.frame")
colnames(BS3.df)

names(BS3.df)[2:3] <- paste(c("x","y"))
head(BS3.df)

#-------------------------------------------------------------------------------
# Creo un SpatialPointsDataFrame 
#-------------------------------------------------------------------------------

coordenadas <- BS3.df[,c("x", "y")]
is.data.frame(coordenadas)

which(is.na(coordenadas$x | coordenadas$y))  # No hay NAs

datos <- as.data.frame(BS3.df[,c("BS")])
head(datos)
is.data.frame(datos)
colnames(datos) <- "BS"

crs <- CRS("+init=epsg:4326")


#-----------------------------------------------------------------------------
# Crear objeto SpatialPointsDataFrame
#-----------------------------------------------------------------------------

spdf <- SpatialPointsDataFrame(coords = coordenadas, data = datos, proj4string = crs)

#-------------------------------------------------------------------------------
# Funcion para contar granjas/celda
#-------------------------------------------------------------------------------

pointcount = function(arg_ras, spdf){
  
  # Raster como input:
  r2 = arg_ras
  
  # Get the cell index for each point and make a table:
  counts = table(cellFromXY(arg_ras, spdf))
  
  # Fill in the raster with the counts from the cell index:
  r2[as.numeric(names(counts))] = counts
  return(r2)
}


#-------------------------------------------------------------------------------
# Resultado y exploracion del raster
#-------------------------------------------------------------------------------

BS3 = pointcount(arg_ras, spdf)
plot(BS3)

writeRaster(BS3, filename = "D:/CIC/Analisis/Wild_boar_pigs_interaction/Rasters/BS3", format = "ascii", overwrite = TRUE)

BS3 <- raster("D:/CIC/Analisis/Wild_boar_pigs_interaction/Rasters/BS3.asc")
class(BS3)

setMinMax(BS3)  # 0, 20

hist(BS3)  # Gran frecuencia de ceros

hist(BS3[BS3!=0])  # Ademas de ceros, solo existen 1 y 2

sum(values(BS3!=0), na.rm = TRUE)  # 2762 pixels with value

length(which(values(BS3) %in% c(1:20))) # 2762

# Tabla de frecuencias para granjas BS1 por celda

table(BS3[])

