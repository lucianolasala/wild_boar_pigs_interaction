# This script streamlines the analysis of the consistency index (CI) for multiple experts. 

# Random index for a 4x4 matrix
RI <- 0.9  

# Define matrices for each expert
experts <- list(
  expert_1 = matrix(c(1,0.25,0.125,0.25,
                      4,1,0.25,0.2,
                      8,4,1,0.125,
                      4,5,8,1), 4, 4, byrow = TRUE),
  
  expert_2 = matrix(c(1,0.5,0.25,0.2,
                      2,1,0.167,0.125,
                      4,6,1,0.111,
                      5,8,9,1), 4, 4, byrow = TRUE),
  
  expert_3 = matrix(c(1,0.333,0.143,0.2,
                      3,1,0.167,0.333,
                      7,6,1,1,
                      5,3,1,1), 4, 4, byrow = TRUE),
  
  expert_4 = matrix(c(1,0.143,0.167,0.2,
                      7,1,0.143,0.2,
                      6,7,1,0.143,
                      5,5,7,1), 4, 4, byrow = TRUE),
  
  expert_5 = matrix(c(1,0.25,0.125,0.125,
                      4,1,0.1666667,0.2,
                      8,6,1,1,
                      8,5,1,1), 4, 4, byrow = TRUE),
  
  expert_6 = matrix(c(1,0.2,0.125,0.111,
                      5,1,0.2,0.167,
                      8,5,1,0.2,
                      9,6,5,1), 4, 4, byrow = TRUE),
  
  expert_7 = matrix(c(1,0.25,0.167,0.143,
                      4,1,0.25,0.167,
                      6,4,1,0.2,
                      7,6,5,1), 4, 4, byrow = TRUE),
  
  expert_8 = matrix(c(1,0.25,0.2,0.25,
                      4,1,0.125,0.167,
                      5,8,1,0.125,
                      4,6,8,1), 4, 4, byrow = TRUE),
  
  expert_9 = matrix(c(1,0.5,0.167,0.143,
                      2,1,0.25,0.25,
                      6,4,1,2,
                      7,4,0.5,1), 4, 4, byrow = TRUE),
  
  expert_10 = matrix(c(1,0.333,0.2,0.143,
                       3,1,0.5,0.2,
                       5,2,1,0.5,
                       7,5,2,1), 4, 4, byrow = TRUE))
experts

# Function to calculate CI and check consistency
calculate_consistency <- function(matrix) {
  eigen_result <- eigen(matrix)
  
  # Taking the real part of the eigenvalue
  eigenvalue <- Re(eigen_result$values[1]) 
  n <- nrow(matrix)
  CI <- (eigenvalue - n) / (n - 1)
  CR <- CI / RI
  
  # Standardizing eigenvector weights
  eigenvector <- eigen_result$vectors[, 1]
  weights <- Re(eigenvector / sum(eigenvector))  # Normalization
  
  list(CI = CI, CR = CR, weights = weights)
}

# Initialize result list
results <- data.frame(Expert = character(), CI = numeric(), CR = numeric(), Consistent = character(), stringsAsFactors = FALSE)

# Loop through each expert's matrix
for (expert_name in names(experts)) {
  consistency <- calculate_consistency(experts[[expert_name]])
  CI <- as.numeric(consistency$CI)
  CR <- as.numeric(consistency$CR)
  consistency_result <- ifelse(CR <= 0.1, "Consistent", "Inconsistent")
  
  results <- rbind(results, data.frame(Expert = expert_name, CI = CI, CR = CR, Consistent = consistency_result))
}

# Filter results for consistent matrices
consistent_results <- results[results$CR <= 0.1, ]

# Display final output
print(consistent_results)

