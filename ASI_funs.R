ASI <- function(df, k, algorithm, nstart = 25) {
  if (!algorithm %in% c("kmeans", "pam")) {
    stop("Error: invalid clustering algorithm. Please specify 'kmeans' or 'pam'.")
  }
  if (k < 2) {
    stop("Cluster number k must be greater than 1")
  }
  
  if (!is.matrix(df)) {
    df <- as.matrix(df)
  }
  
  # calculate clusters and medoids according to the clustering algorithm
  if (algorithm == "kmeans") {
    # kmeans
    kmeans_result <- stats::kmeans(x = df, centers = k, nstart = nstart)
    clusters <- kmeans_result$cluster
    
    kcenters <- do.call(rbind, lapply(1:k, function(i) {
      apply(df[clusters == i, , drop = FALSE], 2, median)
    }))
    kcenters <- as.data.frame(kcenters) 
  } else if (algorithm == "pam") {
    # pam
    pam_cl <- cluster::pam(x = df, k = k, nstart = nstart, cluster.only = FALSE) 
    clusters <- pam_cl$clustering
    kcenters <- as.data.frame(pam_cl$medoids) 
  }
  
  # calculation of cluster variance
  medoids_for_obs <- kcenters[clusters, , drop = FALSE]
  squared_diffs <- (df - medoids_for_obs)^2
  dist_to_medoid_sq <- rowSums(squared_diffs)
  # for each cluster, finding the average distance between all of the observations and the median of the cluster which they are belong to.
  a_per_cluster <- tapply(dist_to_medoid_sq, clusters, mean)
  #WCV
  a <- mean(a_per_cluster)
  
  #finding the distance between clusters
  dist_matrix_kcenters <- as.matrix(dist(kcenters))
  diag(dist_matrix_kcenters) <- Inf
  
  # finding the nearest cluster for each cluster
  nearestcluster <- apply(dist_matrix_kcenters, 1, which.min)
  
  nearest_medoids_for_obs <- kcenters[nearestcluster[clusters], , drop = FALSE]
  
  # for each cluster, finding the average distance between all of the observation in the cluster and the nearest cluster's centroid
  squared_diffs_b <- (df - nearest_medoids_for_obs)^2
  dist_to_nearest_medoid_sq <- rowSums(squared_diffs_b)
  b_per_cluster <- tapply(dist_to_nearest_medoid_sq, clusters, mean)
  # BCV
  b <- mean(b_per_cluster)
  
  asi <- (b - a) / max(c(a, b))
  names(asi) <- k
  
  return(asi)
}


k_ASI <- function(df, kmin = 2, kmax = 10, algorithm) {  
  if (kmin >= kmax) {
    stop("kmax must be greater than kmin")
  }
  All_index <- sapply(kmin:kmax, function(i) ASI(df, i, algorithm))
  max_index <- which.max(All_index)  
  max_value <- All_index[max_index]  
  max_name <- as.integer(names(All_index)[max_index])  
  Best_nc <- round(c(max_name, max_value),4)  
  names(Best_nc) <- c("Number_clusters", "Value_Index")  
  result <- list("All_index"=round(All_index,4), "Best_nc"=Best_nc)
  result
}

