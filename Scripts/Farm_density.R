
#---------------------------------------------------------------------
# This script creates a raster of farm density in Argentina
#---------------------------------------------------------------------

# Load libraries and set options
pkgs <- c("raster","rgdal","leaflet","sf","ggplot2")

sapply(pkgs, function(x) library(x, character.only = TRUE)) 

options(digits = 8)
options(max.print = 1000)

#----------------------------------------------------------
# Creation of raster for Argentina (10.000 sq-m resolution)
#----------------------------------------------------------

arg_ras <- raster(ncol = 224, nrow = 380, xmn = -73.5810049222300080, xmx = -53.5845066977294593, ymn = -55.4799519472216502, ymx = -21.3978700677270020)

values(arg_ras) <- 1:ncell(arg_ras)  # Asign values to cells
hasValues(arg_ras)
summary(arg_ras@data@values)
dim(arg_ras)

writeRaster(arg_ras, filename = ".../Rasters/Raster_10km", format = "ascii", overwrite = TRUE)

#-----------------------------------------------------------------------
# Load polygon (SpatialPolygonsDataFrame) of Argentina avilable at:
# https://www.ign.gob.ar/NuestrasActividades/InformacionGeoespacial/CapasSIG#descarga
#-----------------------------------------------------------------------

arg_poly <- readOGR(".../Shapefiles","ARG_adm0")

# Transform ESRI shapefile into SpatialPolygons
arg_poly <- as(arg_poly, "SpatialPolygons")


# Use a mask to hide pixels outside of the polygon and transform to NAs
arg_ras <- raster(".../Rasters/Raster_10km.asc")


#-----------------------------------------------------------------------
# Prueba
#-----------------------------------------------------------------------

arg_raster <- mask(arg_ras, arg_poly)
plot(arg_raster1)

summary(arg_raster1@data@values)  # Min 531 to max 83860, with 50203 NAs after masking

writeRaster(arg_raster1, filename = "C:/Users/User/Documents/Analyses/Wild boar-pig disease transmission/Rasters/Arg_raster1", format = "ascii", overwrite = TRUE)

# The raster has NA outside and data inside
# Replace NAs with 0

arg_raster1[!is.na(arg_raster1[])] <- 0  # Estos ceros reemplazan el valor inicial de cada pixel con valor
plot(arg_raster1)

writeRaster(arg_raster1, filename = "C:/Users/User/Documents/Analyses/Network analysis pigs/Data/Rasters/Arg_raster1a", format = "ascii", overwrite = TRUE)

# Cargo el raster

summary(arg_raster1@data@values)  # Todos 0 y 50203 NAs
which(arg_raster1@data@values == 0)
which(is.na(arg_raster1@data@values))  # 50203 NAs


#---------------------------------------------------------------------
# Carga de existencias porcinas
#---------------------------------------------------------------------

rm(list=ls(all=TRUE))

options(digits = 8)

nodos <- read.csv("C:/Users/User/Documents/Analyses/Network analysis pigs/Data/Nodos_3092020.csv", sep = ",")
colnames(nodos)
levels(nodos$TIPO)
head(nodos)

# Subset granjas familiares

familiar <- subset(nodos, TIPO == "FAMILIAR" | TIPO == "TRASPATIO", select = c(ESTAB, LAT, LONG, RENSPA, CERDAS))

head(familiar)
is.data.frame(familiar)
length(familiar$RENSPA)  # 1657 filas totales

summary(familiar)  # Hay 1 NA para coordenadas, las cuales 
                   # hay que excluir porque impiden crear el SpatialPoinsDataFrame

length(familiar$LONG)  # 1657

which(is.na(familiar$LAT|familiar$LONG))  # 770
which(familiar$LAT == 0.00) # No hay 0.00s

# Crear subset solo con coords existentes

familiar1 <- familiar[-c(770),]
                
summary(familiar1)  # Ahora lat y long no tienen NAs

length(familiar1$LONG)  # 1656 (se elimino el NA)

#------------------------------------------------------------------
# Exploracion de RENSPAs
#------------------------------------------------------------------

length(familiar1$RENSPA)  # 1656
length(unique(familiar1$RENSPA))  # 1643 granjas familiares con RENSPAs unicos 
                                 # Deberia haber 13 granjas con RENSPA que no son unicos  

# Comprobacion

familiar1[duplicated(familiar1$RENSPA),]  # 13 duplicados, que son NA para RENSPA y datos de animales  

familiar1[duplicated(familiar1$ESTAB),]  # No hay reps.

# Se identifican: 
# RENSPA 13.004.0.00312/02 x 4 
# RENSPA 21.009.0.03586/02 x 2 

# Se separan para explorar

#RENSPA_rep <- subset(familiar1, RENSPA == "13.004.0.00312/02" | RENSPA == "21.009.0.03586/02")
#RENSPA_rep

#ID_RENSPA_rep <- familiar1[which(familiar1$RENSPA == "13.004.0.00312/02" | familiar1$RENSPA == "21.009.0.03586/02"),]
#ID_RENSPA_rep

# Estas granjas deben quedar con un solo registro cada una. Para no eliminar por row ID 
# (no recomendable), creo nueva columna con ID unico

familiar1$id <- 1:nrow(familiar1)

which(is.na(familiar1$RENSPA))  # Identifico los ID a eliminar

# ID_RENSPA_rep <- familiar1[which(familiar1$RENSPA == "13.004.0.00312/02" | familiar1$RENSPA == "21.009.0.03586/02"),]
# ID_RENSPA_rep

familiar2 <- familiar1[!(familiar1$id %in% c(52,99,155,167,209,222,320,493,598,605,718,1122,1198,1640)), ]
length(unique(familiar2$RENSPA))  # 1642
length(familiar2$RENSPA)  # 1642 
length(unique(familiar1$RENSPA))  # 1643


#---------------------------------------------------------------------  
# Creo nueva variable que combine LAT y LONG para asegurarme de tener 
# registros unicos. Se usa la diferencia entre LAT y LONG para generar 
# un numero unico
#---------------------------------------------------------------------

familiar2$COORDS_COMB <- familiar2$LAT-familiar2$LONG
head(familiar2)

length(familiar2$RENSPA) # 1642 RENSPAs totales
length(unique(familiar2$RENSPA))  # 1642

length(unique(familiar2$COORDS_COMB))  # 1637

rep_coords <- familiar2[duplicated(familiar2$COORDS_COMB),] 
length(rep_coords$COORDS_COMB)  # 5

hist(rep_coords$COORDS_COMB, col = "red", las = 1, xlab = "Coordenadas unicas", 
     main = "Frecuencia de \n coordenadas repetidas")

rep_coords1 <- familiar2[duplicated(familiar2$COORDS_COMB) | duplicated(familiar2$RENSPA),]
length(rep_coords1$RENSPA)  # 5
rep_coords1

rep_coords2 <- familiar2[!duplicated(familiar2$COORDS_COMB) | !duplicated(familiar2$RENSPA),]
length(rep_coords2$RENSPA)  # 1642

# La funcion devuelve todos los repetidos. Si una coordenada aparece 1 vez, 
# entonces hay una mas repetida, si aparece dos o mas, lo mismo. 
# O sea que las enumeradas son las repetidas. 

# Nota. -36.000000 -64.000000, que sale como repetida, habria que eliminarla.

# Son dos granjas, una en Cordoba y otra en Bs. As, con mismas coords.

familiar2[familiar2$LAT == -36.000000 | familiar2$LONG == -64.000000,]

length(familiar2$COORDS_COMB)

sum(familiar2$COORDS_COMB == 28)  # 2

familiar2 <- familiar2[!(familiar2$COORDS_COMB %in% 28),] 

length(familiar2$COORDS_COMB)  # 1640 >> Se eliminaron las 2 coordenadas invalidas


#-------------------------------------------------------------------------------
# Creo un SpatialPointsDataFrame para guardar como ESRI shp. 
#-------------------------------------------------------------------------------

coordenadas <- familiar2[,c("LONG", "LAT")]
is.data.frame(coordenadas)

which(is.na(coordenadas$LONG | coordenadas$LONG))  # No hay NAs

datos <- as.data.frame(familiar2[,c("RENSPA","CERDAS")])
is.data.frame(datos)

crs <- CRS("+init=epsg:4326")

spdf <- SpatialPointsDataFrame(coords = coordenadas, data = datos, proj4string = crs)

class(spdf)
spdf@coords
spdf@data

writeOGR(spdf,"C:/Users/User/Documents/Analyses/Network analysis pigs/Data","Familiar", driver = "ESRI Shapefile", overwrite_layer = T)


#-----------------------------------------------------------------------------
# Leer shapefile de existencias y raster
#-----------------------------------------------------------------------------

rm(list=ls(all=TRUE))

granjas <- readOGR("C:/Users/User/Documents/Analyses/Network analysis pigs/Data/Familiar.shp")

arg_ras <- raster("C:/Users/User/Documents/Analyses/Network analysis pigs/Data/Rasters/Arg_raster1a.asc")


#-----------------------------------------------------------------------------
# Transformar shapefile en data frame
#-----------------------------------------------------------------------------

farms.df <- as(granjas, "data.frame")
colnames(farms.df)

names(farms.df)[2:4] <- paste(c("Cerdas","x","y"))
head(farms.df)


#-------------------------------------------------------------------------------
# Creo un SpatialPointsDataFrame 
#-------------------------------------------------------------------------------

coordenadas <- farms.df[,c("x", "y")]
is.data.frame(coordenadas)

which(is.na(coordenadas$x | coordenadas$y))  # No hay NAs

datos <- as.data.frame(farms.df[,c("RENSPA","Cerdas")])
is.data.frame(datos)

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
  #r2[] = 0

# Get the cell index for each point and make a table:
  counts = table(cellFromXY(arg_ras, spdf))

# Fill in the raster with the counts from the cell index:
  r2[as.numeric(names(counts))] = counts
  return(r2)
 }
