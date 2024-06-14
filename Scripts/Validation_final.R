#----------------------------------------------------------
# MODEL VALIDATION USING PERMUTATION TESTS
#----------------------------------------------------------

rm(list=ls())

library(raster)
library(rgdal)
library(sf)
library(readr)

# Cases: coordenadas de granjas de SENASA con casos positivos de interaccion
cases <- read.table("D:/CIC/Analisis/Wild_boar_pigs_interaction/Datos/SENASA/Encuesta_positivos.csv", sep = ",", header = TRUE)
head(cases)
length(cases$Lon) # 320 granjas


# Convert lon and lat to spatial points to extract posittive counties below
coordinates <- st_as_sf(cases, coords = c("Lon", "Lat"), crs = 4326); head(coordinates)

# Perform spatial join
# join = st_intersects: This parameter specifies the type of 
# spatial join to be performed. In this case, st_intersects 
# indicates that the join will be based on whether geometries 
# from the two datasets intersect with each other.
# So, st_join(coordinates, dep, join = st_intersects) performs 
# a spatial join between the points in coordinates and the polygons 
# in dep, and it returns a new sf object containing the attributes 
# from both datasets where the geometries intersect. 
# In this case, it returns an sf object where each point is associated 
# with the department it intersects with.

# Read departments shapefile

shapefile <- "D:/CIC/Analisis/Wild_boar_pigs_interaction/Vectors/ARG_adm2.shp"
dep <- st_read(shapefile)
dep$NAME_2  # 503
length(unique(dep$NAME_2))  # 424: la diferencia de 79 deben ser poligonos
# correspondientes a deptos. multiparte (islas).   

posit_county <- st_join(coordinates, dep, join = st_intersects)
head(posit_county)
length(posit_county$NAME_2)  # 320 filas de deptos. con interaccion positiva
length(unique(posit_county$NAME_2))  # 139 deptos unicos

# Dejar en cases coordenadas y columna Id con nombre de depto.

posit_counties <- posit_county %>%
  mutate(lon = st_coordinates(.)[, 1],
         lat = st_coordinates(.)[, 2]) %>%
  select(lon, lat, NAME_2) %>%
  rename(Id = NAME_2) %>%
  st_drop_geometry(); head(posit_counties)

write.csv(cases, "D:/CIC/Analisis/Wild_boar_pigs_interaction/Datos/SENASA/posits_dtos.csv")

# Load raster model

r <- raster("D:/CIC/Analisis/Wild_boar_pigs_interaction/Model_outputs/average.tif")

pr <- raster(r)
pr[] <- NA
pr[!is.na(r[])] <- 1
plot(pr)
summary(pr@data@values)

# Get the id of the departments. Taking the id from dep
posit_counties$IdNum <- dep$ID_2[match(posit_counties$Id, dep$NAME_2)] 
length(posit_counties$IdNum)  # 320
length(unique(posit_counties$IdNum))

dep$ID <- as.factor(dep$ID_2)  # ID_2 son int entre 1 y 503 (deptos.)
dep.id <- rasterize(dep, r, field = "ID")

kd <- posit_counties$IdNum  # Id de posit. counties
kv <- which(dep.id[] %in% kd & !is.na(pr[])) # Cells in deps with observations
ko <- which(!(dep.id[] %in% kd) & !is.na(dep.id[]) & !is.na(pr[])) # Cells in the other deps
ka <- c(kv, ko) # All cells

# Rasterize the shapefile with the field "ID"

dep_raster <- rasterize(dep, r, field = "ID_2"); plot(dep_raster)
writeRaster(dep_raster, "D:/CIC/Analisis/Wild_boar_pigs_interaction/Rasters/Deptos/Counties.tif", overwrite = TRUE)


#-------------------------------------------------------------------------------
# Permutations by department
#-------------------------------------------------------------------------------

# H0: mean interaction index is the same for positive and negative states 
# H1: mean interaction index is not the same for positive and negative states

mrisk <- tapply(r[ka], INDEX = dep.id[ka], mean, na.rm = TRUE)
length(mrisk)  # Si mrisk tiene 503 elementos, por que la longitud da 497??
head(mrisk)

risk <- data.frame(id = 1:497, r = mrisk)  # Error in data.frame(id = 1:503, r = mrisk) : 
# arguments imply differing number of rows: 503, 497
head(risk)
length(risk$r)  # 497

hist(risk$r, breaks = 200, xlab = "", main = "All species", col = "gray")
abline(v = mean(risk$r), col = "red")

sequence <- 1:497
excluded <- c(as.numeric(kd))  # Assuming kd (postitive counties) is a vector of elements you want to exclude
ko <- sequence[setdiff(seq_along(sequence), excluded)]
length(ko)  # 385

pos_risk_r <- risk %>% filter(., id %in% kd) %>% select(r); length(pos_risk_r$r)  # 139
neg_risk_r <- risk %>% filter(., id %in% ko) %>% select(r); length(neg_risk_r$r)  # 358

#-------------------------------------------------------------
# Test statistic usin abs value: 
# This is the absolute difference in means between positive 
# and negative states
#-------------------------------------------------------------

test_stat_abs <- abs(mean(pos_risk_r$r, na.rm = T) - mean(neg_risk_r$r, na.rm = T)); test_stat_abs

# La diferencia absoluta observada es de 0.0005696599, muy pequeña, 
# lo cual se condice con un valor de p muy alto

#-------------------------------------------------------------
# Test statistic using real value:
# This is the difference in means between positive and negative states
#-------------------------------------------------------------

test_stat_real <- mean(pos_risk_r$r, na.rm = T) - mean(neg_risk_r$r, na.rm = T); test_stat_1

nperm <- 10000  # Generate 10000 perms
set.seed(1974)

difs1 <- rep(NA, nperm)

for(i in 1:nperm){
  k1 <- sort(sample(1:497, 320, replace = FALSE))  # Saco 320, correspondiente al nro. de granjas positivas  
  k2 <- (1:497)[-k1]  # Negativos
  
  difs1[i] <- abs(mean(risk$r[k1], na.rm = T) - mean(risk$r[k2], na.rm = T))
}

difs1[1:10]  # Null distributions from 1:10

plot.new()

hist(difs1)  # Permuted diffs. distribution under the null
abline(v = test_stat_1_abs, col = "red")  # Observed difference between null distribution generated with permutations (mean = 0) 
# and the test statistic (absolute value) 


#----------------------------------------------
# Approximate p-value (two-tail test)
#----------------------------------------------

# In a two-tail test, you're interested in extreme values in both tails 
# of the distribution. By using abs(test_stat_1_abs), you ensure that you're 
# capturing extreme values in both tails, regardless of whether they're 
# higher or lower than the test statistic from your actual data. 
# This ensures that you're accounting for extreme values in both directions, 
# making the p-value calculation appropriate for a two-tail test.

# The question being asked is: the p value represents the probability
# of getting what we observed in our sample (test statistic) or something
# more extreme if the null distribution is true.

# Calculate the absolute difference in means

# Calculate the permutation two-tail p-value
two_tail_p <- sum(difs1 >= abs(test_stat_1))/nperm; two_tail_p

# Calculo equivalente
table(difs1 >= abs(test_stat_1))  # 1ro ver tabla  
two_tail_p <- table(difs1 >= abs(test_stat_1))[2]/(sum(table(difs1 >= abs(test_stat_1))[1]) + table(difs1 >= abs(test_stat_1))[2]); two_tail_p

# Calculo equivalente
mean(difs1 >= abs(test_stat_1))  


#----------------------------------------------
# P-value (one-tail p value as in t-test)
#----------------------------------------------

# Permutation t-test
# The p-values are calculated based on how often the observed difference 
# is as extreme as or more extreme than the permuted differences (null).

table(difs1 >= test_stat_1)
mean(difs1 >= test_stat_1)  # 0.677

# Calculate the permutation one-tail p-value
one_tail_p1 <- sum(difs1 >= test_stat_1)/nperm; one_tail_p
