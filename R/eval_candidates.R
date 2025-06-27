#' Evaluate candidate municipalities using a single cross-validation scheme
#' @name eval_candidates
#' @title Evaluate candidates via out-of-sample corr² from multilinear regression
#' @description This function evaluates each candidate area by fitting a linear regression model to predict case counts in a target area using cross-validation. For each cross-validation scheme, it calculates the out-of-sample corr² for each candidate (in combination with any previously selected candidates) and stores the predicted values across all time points.
#' 
#'
#' @param goal_vec Vector of time-stratified (e.g., weekly) cases in the target area
#' @param candidate_mat Matrix of candidate cases (time units × candidate areas)
#' @param block_labels Character vector of block labelings for a single cross-validation scheme
#' @param previously_selected Integer vector of indices of previously selected areas
#'
#' @return A list with:
#' \itemize{
#'   \item \code{corr²}: Vector of corr² values for each (block × candidate)
#'   \item \code{selected}: Vector of candidate names corresponding to each corr²
#'   \item \code{oos_est}: Matrix of out-of-sample predictions (same shape as \code{candidate_mat})
#' }

#'   
#' @export
eval_candidates <- function(goal_vec, candidate_mat, block_labels, previously_selected = numeric(0)) {
  n_candidates <- ncol(candidate_mat)  # Total number of candidate municipalities
  candidate_names <- colnames(candidate_mat)  # Names of candidate municipalities
  
  corr2_vec <- c()             # To store R² values for each candidate × number of block
  selected_vec <- c()       # To store candidate names corresponding to each corr²
  oos_matrix <- matrix(NA, nrow = length(goal_vec), ncol = n_candidates)  # Placeholder for OOS predictions
  colnames(oos_matrix) <- candidate_names
  
  # Identify the unique block labels used in this CV scheme (e.g., "A", "B", "C")
  unique_blocks <- unique(block_labels)
  
  # Loop over each block (excluding the last one, which may be incomplete)
  for (b in unique_blocks[1:(length(unique_blocks) - 1)]) {
    
    # Define test and train indices based on the current block label
    test_idx <- which(block_labels == b)
    train_idx <- setdiff(seq_along(goal_vec), test_idx)
    
    # Subset training and testing data
    goal_train <- goal_vec[train_idx]
    goal_test <- goal_vec[test_idx]
    data_train <- candidate_mat[train_idx, , drop = FALSE]
    data_test <- candidate_mat[test_idx, , drop = FALSE]
    
    # Identify candidates not already selected
    candidate_indices <- setdiff(1:n_candidates, previously_selected)
    
    # Loop over remaining candidate municipalities
    for (p in candidate_indices) {
      current_cols <- c(p, previously_selected)  # Use current candidate + previously selected
      X_train <- data_train[, current_cols, drop = FALSE]
      X_test <- data_test[, current_cols, drop = FALSE]
      
      # Fit linear model to training data
      model <- lm(goal_train ~ ., data = as.data.frame(X_train))
      
      # If model fit is successful (i.e., all coefficients are non-NA)
      coefs <- coef(model)
      if (all(!is.na(coefs))) {
        intercept <- coefs[1]
        slopes <- coefs[-1]
        
        # Generate predictions on the held-out block
        preds <- intercept + as.matrix(X_test) %*% slopes
        
        # Store predictions in the OOS matrix
        oos_matrix[test_idx, p] <- preds
        
        # Compute out-of-sample R²
        if (sum(goal_test, na.rm = TRUE) == 0) {
          corr2 <- 0  # Avoid division by zero or undefined correlation
        } else {
          corr2 <- cor(preds, goal_test, use = "complete.obs")^2
        }
        
        # Record R² and candidate name
        corr2_vec <- c(corr2_vec, corr2)
        selected_vec <- c(selected_vec, candidate_names[p])
      }
    }
  }
  
  return(list(
    corr2 = corr2_vec,              # Vector of length (#blocks × #candidates)
    selected = selected_vec,        # Candidate name associated with each corr²
    oos_est = oos_matrix            # Matrix of predictions: time × candidate
  ))
}


