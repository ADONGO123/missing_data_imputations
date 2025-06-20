library(filling)

evaluate_k <- function(data, k, missing_pct = 0.3, reps = 5) {
  nrmse_vals <- numeric(reps)
  
  for (i in 1:reps) {
    # Introduce missing values
    data_miss <- produce_NA(data, proportion = missing_pct)
    mask <- is.na(data_miss)
    
    # Try imputation
    knn_result <- try(fill.KNNimpute(as.matrix(data_miss), k = k), silent = TRUE)
    
    if (inherits(knn_result, "try-error") || any(is.nan(knn_result$X_filled))) {
      nrmse_vals[i] <- NA
    } else {
      imputed <- knn_result$X
      
      # Replace any remaining NaNs only where imputations were attempted
      if (any(is.nan(imputed[mask]))) {
        for (j in 1:ncol(imputed)) {
          bad <- is.nan(imputed[, j]) & mask[, j]
          imputed[bad, j] <- mean(data[, j], na.rm = TRUE)
        }
      }
      
      # Calculate NRMSE
      nrmse_vals[i] <- nrmse(imputed, data, mask)
    }
  }
  
  return(mean(nrmse_vals, na.rm = TRUE))
}
ks <- 2:20  # Try k from 2 to 20
nrmse_scores <- sapply(ks, function(k) evaluate_k(NewPark, k = k, missing_pct = 0.3, reps = 5))

# Plot to visualize
plot(ks, nrmse_scores, type = "b", col = "blue", pch = 19,
     xlab = "k (number of neighbors)", ylab = "Average NRMSE",
     main = "Choosing Optimal k for fill.KNNimpute")

best_k <- ks[which.min(nrmse_scores)]
cat("Optimal k:", best_k, "\n")



miss.dat <- produce_NA(NewPark, 0.1)
filldat <- fill.KNNimpute(as.matrix(miss.dat), k=11)
mask <- is.na(miss.dat)
