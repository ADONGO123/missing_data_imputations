library(tidyverse)
library(rpart)
library(impute)
library(rpart.plot)
library(caret)
library(e1071)
library(filling)
library(ROCR)
library(missForestPredict)
url <- "https://archive.ics.uci.edu/ml/machine-learning-databases/parkinsons/parkinsons.data"
parkinsons_data <- read.csv(url)
NewPark <- parkinsons_data[,-c(1,18)]

dat_missing <- function(data) {
  missingLevels <- c(0.1, 0.2, 0.3)
  missing_datasets <- list()
  
  for (i in missingLevels) {
    data_missing <- produce_NA(data, proportion = i)
    
    missing_datasets[[paste0("missing_", i * 100)]] <- data_missing
  }
  
  return(missing_datasets)
}

# Normalized Root Mean Squared Error
nrmse <- function(imputed, original, mask) {
  sqrt(mean((imputed[mask] - original[mask])^2)) / sd(original[mask])
}

set.seed(2025)

missingLevels <- c(0.1, 0.2, 0.3)
reps <- 10
methods <- c("KNN", "missForest")

results_list <- list()

for (level in missingLevels) {
  cat("\nMissing Level:", level * 100, "%\n")
  
  rep_results <- matrix(NA, nrow = reps, ncol = length(methods))
  colnames(rep_results) <- methods
  
  for (r in 1:reps) {
    cat("  Rep", r, "\n")
    
    # Generate fresh missing datasets using your function
    data_missing_list <- dat_missing(NewPark)
    data_miss <- data_missing_list[[paste0("missing_", level * 100)]]
    mask <- is.na(data_miss)
    
    # KNN
    knn_out <- fill.KNNimpute(as.matrix(data_miss), k = 20)
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
    rep_results[r, "KNN"] <- nrmse(knn_imp, NewPark, is.na(data_miss))
    
    # missForest
    mf_try <- missForest(data_miss)
      rep_results[r, "missForest"] <- nrmse(mf_try$ximp, NewPark, mask)
  }
  results_list[[paste0("missing_", level * 100)]] <- rep_results
}

summary_df <- data.frame()

for (name in names(results_list)) {
  res <- results_list[[name]]
  level <- gsub("missing_", "", name)
  
  for (method in methods) {
    method_data <- res[, method]
    method_data <- method_data[!is.na(method_data)]
    
    avg <- mean(method_data)
    se <- sd(method_data) / sqrt(length(method_data))
    
    summary_df <- rbind(summary_df, data.frame(
      MissingLevel = paste0(level, "%"),
      Method = method,
      MeanNRMSE = round(avg, 5),
      SE = round(se, 5)
    ))
  }
}


summary_df$Signif <- ""

for (lvl in unique(summary_df$MissingLevel)) {
  res <- results_list[[paste0("missing_", gsub("%", "", lvl))]]
  
  # KNN vs missForest
  if (all(!is.na(res[, "KNN"])) && all(!is.na(res[, "missForest"]))) {
    p_knn <- wilcox.test(res[, "missForest"], res[, "KNN"], paired = TRUE)$p.value
    summary_df$Signif[summary_df$MissingLevel == lvl & summary_df$Method == "KNN"] <- 
      if (mean(res[, "KNN"]) > mean(res[, "missForest"])) {
        if (p_knn < 0.001) "###" else if (p_knn < 0.01) "##" else if (p_knn < 0.05) "#" else ""
      } else {
        if (p_knn < 0.001) "***" else if (p_knn < 0.01) "**" else if (p_knn < 0.05) "*" else ""
      }
  }
}

print(summary_df)
