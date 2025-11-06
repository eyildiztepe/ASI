# Function to generate random cluster centers with minimum distance constraint
generateRandomCenters <- function(k, sc = 100, min_dist = 50) {
  centers <- matrix(NA, nrow = k, ncol = 2)
  centers[1, ] <- runif(2, -sc, sc)  # First center is fully random
  
  for (i in 2:k) {
    repeat {
      candidate <- runif(2, -sc, sc)
      # Check minimum distance to existing centers
      distances <- apply(centers[1:(i-1), , drop = FALSE], 1, function(cx) sqrt(sum((candidate - cx)^2)))
      if (all(distances >= min_dist)) {
        centers[i, ] <- candidate
        break
      }
    }
  }
  return(centers)
}

generateGaussianData <- function(n, center, sigma, label) {
  data <- mvtnorm::rmvnorm(n, mean = center, sigma = sigma)
  data.frame(x = data[, 1], y = data[, 2], cluster = factor(label))
}

# Function to apply NBClust and k_ASI to sim_data
applyClustIndex <- function(sim_data, min_nc = 3, max_nc = 10, method = "kmeans", distance = "euclidean", use_parallel = TRUE) {
  # Define indices to evaluate
  indices <- c("ch", "db", "dunn", "silhouette", "ASI")
  
  # Helper function to run NBClust and k_ASI for a single simulation
  run_clustering <- function(df) {
    df_subset <- df[, 1:2]  # For simulation select only x and y columns
    # df_subset <- df #For real data
    results <- numeric(2*length(indices))
    names(results) <- c(indices,paste0(indices,"_Index"))
    
    # Run NBClust for standard indices
    for (idx in indices[1:4]) {
      nb <- NbClust(df_subset, distance = distance, min.nc = min_nc, max.nc = max_nc, 
                    method = method, index = idx)
      results[idx] <- nb$Best.nc[1]
      results[paste0(idx,"_Index")] <- nb$Best.nc[2]
    }
    
    # Run k_ASI 
    k_asi <- k_ASI(df_subset, kmin = min_nc, kmax = max_nc, algorithm = method)
    results["ASI"] <- k_asi$Best_nc[1]
    results[paste0("ASI","_Index")] <- k_asi$Best_nc[2]
    return(results)
  }
  
  # Parallel or serial 
  if (use_parallel) {
    n_cores <- parallel::detectCores() - 5 
    cl <- parallel::makeCluster(n_cores)
    on.exit(parallel::stopCluster(cl))  # Ensure cluster is closed
    
    # Load required packages and export custom functions to cluster
    parallel::clusterEvalQ(cl, {
      library(NbClust)
      library(dplyr)
    })
    parallel::clusterExport(cl, c("k_ASI", "ASI"), envir = .GlobalEnv)  # Export custom functions
    
    # Run in parallel
    results <- parallel::parLapply(cl, sim_data, run_clustering)
  } else {
    # Run serially
    results <- lapply(sim_data, run_clustering)
  }
  
  # Convert results to a data frame
  results_df <- do.call(rbind, results)
  colnames(results_df) <- c(indices,paste0(indices,"_Index"))
  return(as.data.frame(results_df))
}
