#------------------------------------------------------------------
# Expert opinion. Analytic Hierarchy Process
#------------------------------------------------------------------

# El archivo "datosCrudosExpertos" tiene resultados crudos de 
# la encuesta de opinión de cuatro expertos. Estos son expetos cuyas  
# respuestas en matriz superaron el umbral de confidence index de 0.1.
# En realidad, un solo de los siete expertos consultados tuvo CI < 0.1, por 
# lo cual se decidió incluir a aquellos con CI < 1 luego de redondeo decimal
# y uno con CI = 0.168. 

# En datosCrudosExpertos: 
# inv = número de investigador 
# pert = "l" representa valor mínimo, "m" valor más probable, y 
# "h" valor más alto correspondientes a los parámetros de distribución Pert. 
# var = comparación de pares, donde "_" separa las varaibes comparadas, por ejemplo:
# BSmedia_BSalta compara bioseguridad media vs. bioseguridad alta. 

rm(list = ls())

setwd("D:/CIC/Analisis/Wild_boar_pigs_interaction")

# Functions to generate Pert distributions and plot combined experts
source("./Scripts/PertDistr.r")
source("./Scripts/Overlays.r")

df <- read.csv('./Scripts/datosCrudosExpertos.csv')
head(df)

# Crea distribuciones de Pert a partir de remuestreos (1000) y gegera 
# distribuciones de probabilidad para cada comparación de la matrix.
# Son seis comparaciones posibles, ya que es una matriz ortogonal donde
# la diagonal es 1 y los valores recíprocos (arriba de diagonal) son calculados 
# mediante la inversa "1/comparación del par en cuestion".

iters = 10000  # Numero de muestras aleatorias a tomar de la distribucion de Pert
count <- 1  # Contador

for(i in unique(df$var)) {
  
  # Creo una matriz de 10000 valores para cada experto
  experts.dat <- c()
  for (j in unique(df$inv)) {
    l = df$value[df$var==i & df$inv==j & df$pert=='l']
    m = df$value[df$var==i & df$inv==j & df$pert=='m']
    h = df$value[df$var==i & df$inv==j & df$pert=='h']
    
    pert <- rpert(iters, min = l, mode = m, max = h)
    
    experts.dat <- cbind(experts.dat, pert)
  }
  
  # Unifico las matrices de los expertos en una sola tomando valores al azar
  experts.samples <- c()
  
  for(k in 1:iters){  # 10000 iters
    which.expert = sample(1:length(unique(df$inv)), 1)  # Select randomly one expert
    experts.samples[k] = experts.dat[k, which.expert] 
  }
  
  # Plot
  hist(experts.samples, freq = FALSE, las = 1, ylim = c(0,1), main = "Histogram of expert samples", xlab = "Probability")
  abline(v = median(experts.samples), col = "red", lwd = 2, lty = 2)
  abline(v = quantile(experts.samples, c(0.025, 0.975)), col = "blue", lwd=1, lty=1)  # 95% quantile (5% of data left out, 2.5% on each side)
  box(lty = "solid", col = 'black')
  
  for(x in 1:length(unique(df$inv))){
    lines(density(experts.dat[,x]), col=x)
  }
  
  # Save
  write.csv(as.data.frame(experts.samples), paste('Pairwise_comps/', count, '_', i,'.csv', sep = ''))
  count <- count + 1
}


#------------------------------------------------------------------
# Analisis final de cada matriz
#------------------------------------------------------------------

# Se arman 3 funciones para calculo de autovector, indice de 
# consistencia (CI), ratio de consistencia (CR), y armar matrices

rm = list(ls())

# Function 1. Calculate principal eigenvector
calculate_eigenvector <- function(matrix) {
  eigen_result <- eigen(matrix)
  eigenvector <- eigen_result$vectors[, 1]
  return(eigenvector/sum(eigenvector))
}

# Function 2. Calculate consistency index (CI)
calculate_CI <- function(matrix) {
  n <- nrow(matrix)
  eigenvector <- calculate_eigenvector(matrix)
  lambda_max <- max(Re(eigen(matrix)$values))
  CI <- (lambda_max - n)/(n - 1)
  return(CI)
}

# Function 3. Calculate consistency ratio (CR)
calculate_CR <- function(matrix) {
  n <- nrow(matrix)
  RI <- 0.9  # Index value for a 4 by 4 matrix
  CI <- calculate_CI(matrix)
  CR <- CI/RI
  return(CR)
}

# Function 4. Armar matrices 
createMatrix <- function(vector, matrixSize){
  # Valores de la mitad inferior
  valores_inferiores <- vector
  
  # Tamaño de la matriz cuadrada
  tamano_matriz <- matrixSize
  
  # Matriz cuadrada con todos los elementos inicializados a 0
  matriz <- matrix(0.0, nrow = tamano_matriz, ncol = tamano_matriz)
  
  # Lleno la mitad inferior de la matriz con los valores del vector
  matriz[lower.tri(matriz)] <- valores_inferiores
  
  # Invierto y transpongo la matriz 
  m2 <- t(1/matriz)
  
  # Lleno la mitad inferior de la nueva matriz con los valores del vector
  m2[lower.tri(m2)] <- valores_inferiores
  
  # Agrego unos en la diagonal
  diag(m2) <- 1
  
  # Return
  return(m2)
}


# Leer datos de las seis comparaciones de pares (10000 obs. c/u)
archivos <- list.files(path = './Pairwise_comps', pattern = ".csv$", full.names = TRUE)
dataframes <- list()

for(archivo in archivos) {
  
  # Leer el archivo CSV y almacenarlo en la lista con el nombre del archivo como clave
  nombre_archivo <- basename(archivo)
  dataframes[[nombre_archivo]] <- read.csv(archivo)
}

# Initialize an empty list to store consistent matrices
consistent_matrices <- list()

# Number of consistent matrices needed
target_consistent_matrices <- 10000  
counter <- 1

# Loop until target number of consistent matrices is reached
while(length(consistent_matrices) < target_consistent_matrices) {
  print(counter)
  
  # Sample one value from each dataframe
  sampled_values <- sapply(dataframes, function(df) sample(df$experts.samples, 1))
  
  # Combine sampled values into a pairwise comparison matrix
  pairwise_matrix <- createMatrix(sampled_values, matrixSize = 4)
  
  # Calculate consistency ratio
  CR <- calculate_CR(pairwise_matrix)
  
  # If CR < 0.1, save the matrix in the list
  if(CR < 0.1) {
    consistent_matrices[[length(consistent_matrices) + 1]] <- pairwise_matrix
    counter <- counter + 1
  }
}

# Chequeo de CI para algunas matrices
mat1 <- consistent_matrices[[1]]; calculate_CR(mat1)
mat2300 <- consistent_matrices[[2300]]; calculate_CR(mat2300)
mat10000 <- consistent_matrices[[10000]]; calculate_CR(mat10000)


#------------------------------------------------------------------
# Procesamiento de matrices seleccionadas (consistentes)
#------------------------------------------------------------------

# Function to calculate criterion weights
calculate_criterion_weights <- function(matrix_list) {
  # Initialize an empty list to store criterion weights for each matrix
  criterion_weights_list <- list()
  
  # Iterate over each matrix in the list
  for(matrix in matrix_list) {
    # Calculate the sum of each column (variable) in the matrix
    col_sums <- colSums(matrix)
    
    # Normalize the column sums to obtain criterion weights
    criterion_weights <- col_sums/sum(col_sums)
    
    # Append criterion weights to the list
    criterion_weights_list[[length(criterion_weights_list) + 1]] <- criterion_weights
  }
  
  # Return the list of criterion weights for each matrix
  return(criterion_weights_list)
}

# Calculate criterion weights for each matrix in the list
criterion_weights_list <- calculate_criterion_weights(consistent_matrices)

# Print criterion weights for each of the 10.000 matrices
for(i in 1:length(criterion_weights_list)) {
  cat("Matrix", i, "Criterion Weights:")
  print(criterion_weights_list[[i]])
}

saveRDS(criterion_weights_list, file = "D:/CIC/Analisis/Wild_boar_pigs_interaction/Scripts/criterion_weights_list.rds")

# Output: una lista con 10.000 vectores, correspondientes a las 10.000 matrices
# consistentes generadas. Cada vector tiene 4 elementos, cada uno
# de los cuales corresponde al peso de cada criterio o variable.  
# Por ejemplo, 

criterion_weights_list[[1]]

# [1] 0.55156588 0.32894797 0.06211478 0.05737137
# Corresponde a la matriz 1, y los pesos de BS alta, BS media, BS baja, y WB


#-------------------------------------------------------------------------------
# Generacion de 3000 combinaciones
#-------------------------------------------------------------------------------

# Corresponde a la sección del Glanville et al. (2014) donde dice:

# To incorporate the full range of possible combinations
# of W for each criterion, as well as the uncertainty associated with 
# the standardization of suitability layers, the
# process was repeated over 10.000 iterations, with a specific
# weight combination randomly selected by Monte Carlo
# sampling for each iteration. Hence, each sampled point
# was associated with 10.000 possible estimates of suitability
# (S) for each objective based on the linear combination of
# values of x. Three thousand iterations were used to allow
# the incorporation of the full range of weights (W) generated through 
# the iterative pairwise comparison procedure (n = 10.000).
#-------------------------------------------------------------------------------

#--------------------------------------------------
# Muestreo sobre la lista
#--------------------------------------------------

criterion_weights_list <- readRDS("D:/CIC/Analisis/Wild_boar_pigs_interaction/Scripts/criterion_weights_list.rds")
head(criterion_weights_list)

# Convierto a matriz la lista
mat <- do.call(rbind, criterion_weights_list)
head(mat)
is.matrix(mat)

# Shuffle by columns. 
shuffled_matrix <- apply(mat, 2, sample) # 'sample' argument randomly permutes column elements
head(shuffled_matrix)

shuffled_matrix_df <- as.data.frame(shuffled_matrix)

write.csv(shuffled_matrix_df, "D:/CIC/Analisis/Wild_boar_pigs_interaction/Scripts/shuffled_matrix.csv")

#-------------------------------------------------------------------------------
# Validacion: al hacer muestreo sin reemplazo, si sumo las columnas de la matriz 
# original y comparo los resultados con las sumas de las columnas 
# de la nueva matriz, las sumas deberian ser iguales.
#-------------------------------------------------------------------------------

# Suma columnas en lista de pesos: criterion_weights_list. 
# Como la convertía a matriz uso la matriz, que es lo mismo 

# Column-wise sums (original matrix)
column_sums_a <- colSums(mat)
column_sums_a

# Calculate the column-wise sums (shuffled matrix)
column_sums_b <- colSums(shuffled_matrix)
column_sums_b

# Check for equality between column-wise sums with a tolerance of 0.0001
isEqual <- all.equal(column_sums_a, column_sums_b, tolerance = 0.0001)
isEqual  # TRUE


#---------------------------------------------------------------------
# Suitability index calculation using weighted linear combination
#---------------------------------------------------------------------

rm(list=ls())
library(raster)

setwd("D:/CIC/Analisis/Wild_boar_pigs_interaction")

# Rasters estandarizados
BS_alta <- raster("./Rasters/Standardized/BS1.tif")
BS_media <- raster("./Rasters/Standardized/BS2.tif")
BS_baja <- raster("./Rasters/Standardized/BS3.tif")
Suit_jaba <- raster("./Rasters/Standardized/WB.tif")

# Resample raster2 to match the resolution and extent of raster1
Suit_jaba_resampled <- resample(Suit_jaba, BS_baja, method = "bilinear")
writeRaster(Suit_jaba_resampled, "./Rasters/Standardized/WB.tif", overwrite = TRUE)

# Mean, 5% and 95% percentiles
promedios <- colMeans(shuffled_matrix)
percentiles5 <- apply(shuffled_matrix, 2, function(x) quantile(x, probs = 0.05))
percentiles95 <- apply(shuffled_matrix, 2, function(x) quantile(x, probs = 0.95))

# Models
mod_avr <- BS_alta*promedios[[1]] + BS_media*promedios[[2]] + BS_baja*promedios[[3]] + Suit_jaba*promedios[[4]]
mod_per5 <- BS_alta*percentiles5[[1]] + BS_media*percentiles5[[2]] + BS_baja*percentiles5[[3]] + Suit_jaba*percentiles5[[4]]
mod_per95 <- BS_alta*percentiles95[[1]] + BS_media*percentiles95[[2]] + BS_baja*percentiles95[[3]] + Suit_jaba*percentiles95[[4]]

writeRaster(mod_avr, filename = "./Model_Outputs/average.tif",format = "GTiff", overwrite = TRUE)
writeRaster(mod_per5, filename = "./Model_Outputs/perc_5.tif",format = "GTiff", overwrite = TRUE)
writeRaster(mod_per95, filename = "./Model_Outputs/perc_95.tif",format = "GTiff", overwrite = TRUE)


#--------------------------------------------------------------
# Discretizar modelos
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


