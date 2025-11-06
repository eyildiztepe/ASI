library(NbClust)
library(parallel)
library(mvtnorm)
library(ggplot2)

k <- 3
R <- 1000

# Sim Cases
# Uncomment to use the corresponding case and comment the others

#a1
 std <- rep(10,times=k)
 n <- rep(40, times=k)

#a2
# n_all <- list(
#     "2" = c(40, 60),
#     "3" = c(40, 60, 80),
#     "4" = c(40, 60, 80, 100),
#     "5" = c(40, 60, 80, 100, 80),
#     "6" = c(40, 60, 80, 100, 80, 60),
#     "7" = c(40, 60, 80, 100, 80, 60, 40),
#     "8" = c(40, 60, 80, 100, 80, 60, 40, 60),
#     "9" = c(40, 60, 80, 100, 80, 60, 40, 60, 80),
#     "10" = c(40, 60, 80, 100, 80, 60, 40, 60, 80, 100))
# 
# n <- n_all[[as.character(k)]]
# std <- rep(10,times=k)

# b1
# std_all <- list(
#    "2" = c(10, 15),
#    "3" = c(10, 15, 20),
#    "4" = c(10, 15, 20, 25),
#    "5" = c(10, 15, 20, 25, 20),
#    "6" = c(10, 15, 20, 25, 20, 15),
#    "7" = c(10, 15, 20, 25, 20, 15, 10),
#    "8" = c(10, 15, 20, 25, 20, 15, 10, 15),
#    "9" = c(10, 15, 20, 25, 20, 15, 10, 15, 20),
#    "10" = c(10, 15, 20, 25, 20, 15, 10, 15, 20, 25))
#  std <- std_all[[as.character(k)]]
# 
# n <- rep(40, times=k)

# b2
# n_all <- list(
#   "2" = c(40, 60),
#   "3" = c(40, 60, 80),
#   "4" = c(40, 60, 80, 100),
#   "5" = c(40, 60, 80, 100, 80),
#   "6" = c(40, 60, 80, 100, 80, 60),
#   "7" = c(40, 60, 80, 100, 80, 60, 40),
#   "8" = c(40, 60, 80, 100, 80, 60, 40, 60),
#   "9" = c(40, 60, 80, 100, 80, 60, 40, 60, 80),
#   "10" = c(40, 60, 80, 100, 80, 60, 40, 60, 80, 100))
# n <- n_all[[as.character(k)]]
# std_all <- list(
#   "2" = c(10, 15),
#   "3" = c(10, 15, 20),
#   "4" = c(10, 15, 20, 25),
#   "5" = c(10, 15, 20, 25, 20),
#   "6" = c(10, 15, 20, 25, 20, 15),
#   "7" = c(10, 15, 20, 25, 20, 15, 10),
#   "8" = c(10, 15, 20, 25, 20, 15, 10, 15),
#   "9" = c(10, 15, 20, 25, 20, 15, 10, 15, 20),
#   "10" = c(10, 15, 20, 25, 20, 15, 10, 15, 20, 25))
# std <- std_all[[as.character(k)]]

set.seed(1) #for reproducibility

sim_data <- replicate(R,{
  centers <- generateRandomCenters(k, sc=40*k, min_dist=50) # function in Sim_Funs.R
  df1 <- data.frame()
  for (i in 1:k){
    sigma <- matrix(c(std[i]^2,0,0,std[i]^2), nrow = 2) # cluster covariance matrix
    df1 <- rbind(df1, generateGaussianData(n[i], centers[i,], sigma, i)) # function in Sim_Funs.R
    }
  df1},simplify=F)

#Example plots for the generated data
ggplot(sim_data[[1]], aes(x = x, y = y, color = cluster)) +
  geom_point() +
  theme_minimal() +
  ggtitle("Simulation Clusters")

ggplot(sim_data[[2]], aes(x = x, y = y, color = cluster)) +
  geom_point() +
  theme_minimal() +
  ggtitle("Simulation Clusters")

# Compute indices (assumes sim_data is generated)
sim_results <- applyClustIndex(sim_data, min_nc = 2, max_nc = 10, #function in Sim_Funs.R
                method = "kmeans", distance = "euclidean", use_parallel = TRUE)

# Print results
apply(sim_results[,1:5],2,table)
apply(sim_results[,1:5] == k, 2, sum) / R

  
