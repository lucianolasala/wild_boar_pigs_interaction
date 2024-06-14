#--------------------------------------------------------------
# Evaluacion de consistencia de matriz de cada experto
#--------------------------------------------------------------

#----------------------------------------------------------
# 1. BFP 
#----------------------------------------------------------

rm(list = ls())

RI <- 0.9  # Index for a 4x4 matrix

bfp <- matrix(c(1,0.25,0.125,0.25,
                4,1,0.25,0.2,
                8,4,1,0.125,
                4,5,8,1), 4, 4, byrow = TRUE)

eigen.1 <- eigen(bfp)
eigenvalue.1 <- eigen.1$values[1] 
eigenvector.1 <- eigen.1$vectors[,1] 
sum.eigenvector.1 <- sum(eigen.1$vectors[,1])

weights.1 <- function(bfp){
  for(a in eigenvector.1){
    normalization.1 <- eigenvector.1/sum.eigenvector.1
  }
  return(normalization.1)
}
weights.1(eigenvector.1)

n <- nrow(bfp)  # Number of criteria
CI <- (eigenvalue.1-n)/(n-1)
CR <- CI / RI
CR

check_consistency <- function(CR) {
  real_CR <- Re(CR)  # Extract the real part of CR
  if (real_CR <= 0.1) {
    print("The matrix is consistent")
  } else {
    print("The matrix is inconsistent")
  }
}
check_consistency(CR)


#----------------------------------------------------------
# 2. Cane
#----------------------------------------------------------

rm(list=ls())

RI <- 0.9  # Random Index for a 4x4 matrix

cane <- matrix(c(1,0.5,0.25,0.2,
                 2,1,0.167,0.125,
                 4,6,1,0.111,
                 5,8,9,1), 4, 4, byrow = TRUE)

eigen.1 <- eigen(cane)
eigenvalue.1 <- eigen.1$values[1] 
eigenvector.1 <- eigen.1$vectors[,1] 
sum.eigenvector.1 <- sum(eigen.1$vectors[,1])

weights.1 <- function(cane){
  for(a in eigenvector.1){
    normalization.1 <- eigenvector.1/sum.eigenvector.1
  }
  return(normalization.1)
}
weights.1(eigenvector.1)

n <- nrow(cane)  # Number of criteria
CI <- (eigenvalue.1-n)/(n-1)
CR <- CI/RI
CR

check_consistency <- function(CR) {
  real_CR <- Re(CR)  # Extract the real part of CR
  if (real_CR <= 0.1) {
    print("The matrix is consistent")
  } else {
    print("The matrix is inconsistent")
  }
}

check_consistency(CR)

#----------------------------------------------------------
# 3. Poli
#----------------------------------------------------------

rm(list=ls())

RI <- 0.9  # Random Index for a 4x4 matrix

poli <- matrix(c(1,0.333,0.143,0.2,
                 3,1,0.167,0.333,
                 7,6,1,1,
                 5,3,1,1), 4, 4, byrow = TRUE)

eigen.1 <- eigen(poli)
eigenvalue.1 <- eigen.1$values[1] 
eigenvector.1 <- eigen.1$vectors[,1] 
sum.eigenvector.1 <- sum(eigen.1$vectors[,1])

weights.1 <- function(poli){
  for(a in eigenvector.1){
    normalization.1 <- eigenvector.1/sum.eigenvector.1
  }
  return(normalization.1)
}
weights.1(eigenvector.1)

# Alternativa simple: weights <- eigenvector.1 / sum.eigenvector.1

n <- nrow(poli)  # Number of criteria
CI <- (eigenvalue.1-n)/(n-1)
CR <- CI/RI
CR  # 0.04301525+0i

check_consistency <- function(CR) {
  real_CR <- Re(CR)  # Extract the real part of CR
  if (real_CR <= 0.1) {
    print("The matrix is consistent")
  } else {
    print("The matrix is inconsistent")
  }
}

check_consistency(CR)

#----------------------------------------------------------
# 4. Alarcón 
#----------------------------------------------------------

rm(list=ls())

RI <- 0.9  # Random Index for a 4x4 matrix

la <- matrix(c(1,0.143,0.167,0.2,
               7,1,0.143,0.2,
               6,7,1,0.143,
               5,5,7,1), 4, 4, byrow = TRUE)

eigen.1 <- eigen(la)
eigenvalue.1 <- eigen.1$values[1] 
eigenvector.1 <- eigen.1$vectors[,1] 
sum.eigenvector.1 <- sum(eigen.1$vectors[,1])

weights.1 <- function(la){
  for(a in eigenvector.1){
    normalization.1 <- eigenvector.1/sum.eigenvector.1
  }
  return(normalization.1)
}
weights.1(eigenvector.1)

# Alternativa simple: weights <- eigenvector.1 / sum.eigenvector.1

n <- nrow(la)  # Number of criteria
CI <- (eigenvalue.1-n)/(n-1)
CR <- CI/RI
CR

check_consistency <- function(CR) {
  real_CR <- Re(CR)  # Extract the real part of CR
  if (real_CR <= 0.1) {
    print("The matrix is consistent")
  } else {
    print("The matrix is inconsistent")
  }
}

check_consistency(CR)

#----------------------------------------------------------
# 5. LFLS
#----------------------------------------------------------

rm(list=ls())

RI <- 0.9  # Random Index for a 4x4 matrix

lfls <- matrix(c(1,0.25,0.125,0.125,
                 4,1,0.2,0.2,
                 8,5,1,0.25,
                 8,5,4,1), 4, 4, byrow = TRUE)

eigen.1 <- eigen(lfls)
eigenvalue.1 <- eigen.1$values[1] 
eigenvector.1 <- eigen.1$vectors[,1] 
sum.eigenvector.1 <- sum(eigen.1$vectors[,1])

weights.1 <- function(lfls){
  for(a in eigenvector.1){
    normalization.1 <- eigenvector.1/sum.eigenvector.1
  }
  return(normalization.1)
}
weights.1(eigenvector.1)

# Alternativa simple: weights <- eigenvector.1 / sum.eigenvector.1

n <- nrow(lfls)  # Number of criteria
CI <- (eigenvalue.1-n)/(n-1)
CR <- CI/RI
CR

check_consistency <- function(CR) {
  real_CR <- Re(CR)  # Extract the real part of CR
  if (real_CR <= 0.1) {
    print("The matrix is consistent")
  } else {
    print("The matrix is inconsistent")
  }
}

check_consistency(CR)

#----------------------------------------------------------
# 6. AM Perez
#----------------------------------------------------------

rm(list=ls())

RI <- 0.9  # Random Index for a 4x4 matrix

amp <- matrix(c(1,0.2,0.125,0.111,
                5,1,0.2,0.167,
                8,5,1,0.2,
                9,6,5,1), 4, 4, byrow = TRUE)

eigen.1 <- eigen(amp)
eigenvalue.1 <- eigen.1$values[1] 
eigenvector.1 <- eigen.1$vectors[,1] 
sum.eigenvector.1 <- sum(eigen.1$vectors[,1])

weights.1 <- function(amp){
  for(a in eigenvector.1){
    normalization.1 <- eigenvector.1/sum.eigenvector.1
  }
  return(normalization.1)
}
weights.1(eigenvector.1)

# Alternativa simple: weights <- eigenvector.1 / sum.eigenvector.1

n <- nrow(amp)  # Number of criteria
CI <- (eigenvalue.1-n)/(n-1)
CR <- CI/RI
CR

check_consistency <- function(CR) {
  real_CR <- Re(CR)  # Extract the real part of CR
  if (real_CR <= 0.1) {
    print("The matrix is consistent")
  } else {
    print("The matrix is inconsistent")
  }
}

check_consistency(CR)

#----------------------------------------------------------
# 7. amarcos
#----------------------------------------------------------

rm(list=ls())

RI <- 0.9  # Index for a 4x4 matrix

amarcos <- matrix(c(1,0.25,0.167,0.143,
                    4,1,0.25,0.167,
                    6,4,1,0.2,
                    7,6,5,1), 4, 4, byrow = TRUE)

eigen.1 <- eigen(amarcos)
eigenvalue.1 <- eigen.1$values[1] 
eigenvector.1 <- eigen.1$vectors[,1] 
sum.eigenvector.1 <- sum(eigen.1$vectors[,1])

weights.1 <- function(amarcos){
  for(a in eigenvector.1){
    normalization.1 <- eigenvector.1/sum.eigenvector.1
  }
  return(normalization.1)
}
weights.1(eigenvector.1)

# Alternativa simple: weights <- eigenvector.1 / sum.eigenvector.1

n <- nrow(amarcos)  # Number of criteria
CI <- (eigenvalue.1-n)/(n-1)
CR <- CI/RI
CR

check_consistency <- function(CR) {
  real_CR <- Re(CR)  # Extract the real part of CR
  if (real_CR <= 0.1) {
    print("The matrix is consistent")
  } else {
    print("The matrix is inconsistent")
  }
}

check_consistency(CR)

#----------------------------------------------------------
# 8. Alejandro Perez
#----------------------------------------------------------

rm(list=ls())

RI <- 0.9  # Random Index for a 4x4 matrix

aperez <- matrix(c(1,0.25,0.2,0.25,
                   4,1,0.125,0.167,
                   5,8,1,0.125,
                   4,6,8,1), 4, 4, byrow = TRUE)

eigen.1 <- eigen(aperez)
eigenvalue.1 <- eigen.1$values[1] 
eigenvector.1 <- eigen.1$vectors[,1] 
sum.eigenvector.1 <- sum(eigen.1$vectors[,1])

weights.1 <- function(aperez){
  for(a in eigenvector.1){
    normalization.1 <- eigenvector.1/sum.eigenvector.1
  }
  return(normalization.1)
}
weights.1(eigenvector.1)

# Alternativa simple: weights <- eigenvector.1 / sum.eigenvector.1

n <- nrow(aperez)  # Number of criteria
CI <- (eigenvalue.1-n)/(n-1)
CR <- CI/RI
CR

check_consistency <- function(CR) {
  real_CR <- Re(CR)  # Extract the real part of CR
  if (real_CR <= 0.1) {
    print("The matrix is consistent")
  } else {
    print("The matrix is inconsistent")
  }
}

check_consistency(CR)

