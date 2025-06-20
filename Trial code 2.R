library(filling)
library(VIM)
set.seed(2025)
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



knn_out <- fill.KNNimpute(as.matrix(data_miss), k = 6)
# Copy imputed data
knn_imp <- knn_out$X
# Replace NaNs only where data was missing
nan_locs <- which(is.nan(knn_imp) & is.na(data_miss), arr.ind = TRUE)
if (length(nan_locs) > 0) {
  for (i in 1:nrow(nan_locs)) {
    row <- nan_locs[i, 1]
    col <- nan_locs[i, 2]
    # Replace with column mean from original (or overall mean)
    knn_imp[row, col] <- mean(NewPark[, col], na.rm = TRUE)
  }
  
}
nrmse(knn_imp, NewPark, is.na(data_miss))
?knn()
?kNN
impt <- kNN(miss.dat, k=10)
table(is.na(impt))
view(impt)
summary(impt)
?fill.KNNimpute
