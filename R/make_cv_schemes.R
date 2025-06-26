#' Generate multiple cross-validation block labelings over a time series
#'
#' @name make_cv_schemes
#' @title Generate Cross-Validation Schemes
#' @description
#' This function generates multiple randomized cross-validation labeling schemes for a time series of outcomes, where each scheme divides the timeline into repeating blocks (e.g., "A", "B", "C") used for testing and training in assessing candidates. 
#' 
#' @param tot_ts Total number of time points (e.g., total weeks of data)
#' @param n_cv_schemes Number of replicate cross-validation block labelings schemes to generate
#' @param block_length Number of years per block (default: 3). Each block is used as a test set once.
#' @param freq Number of time points per year (e.g., 52 for weekly data)
#'
#' @return A list of length \code{n_cv_schemes}. Each element is a character vector of length \code{tot_ts},
#' assigning each time point to a block label (e.g., "A", "B", "C").
#'         
#' @export         
make_cv_schemes <- function(tot_ts, n_cv_schemes, block_length = 3, freq = 52) {
  
  # Compute the number of full years represented in the time series
  years <- floor(tot_ts / freq)
  
  # Create a repeating sequence of block labels (e.g., "A", "B", "C"),
  # where each label spans `block_length` years (converted to weeks)
  base_labels <- rep(LETTERS[1:(years / block_length)], each = freq * block_length)
  
  # Initialize the list to hold n_cv_schemes different labelings
  cv_schemes <- vector("list", n_cv_schemes)
  
  for (i in 1:n_cv_schemes) {
    # Start by filling the full time series with a placeholder label (unused)
    block_labels <- rep(LETTERS[(years / block_length) + 1], tot_ts)
    
    # Randomly select a starting point for inserting the base_labels
    start <- sample(1:freq, 1)
    
    if (start == 1) {
      # If start is 1, assign base_labels directly
      block_labels <- base_labels
    } else {
      # If not, insert base_labels starting at `start`, trimming to fit
      rm_end <- abs(length(start:tot_ts) - length(base_labels)) - 1
      block_labels[start:tot_ts] <- base_labels[-((length(base_labels) - rm_end):length(base_labels))]
    }
    
    # Store block labels in the list of cv schemes
    cv_schemes[[i]] <- block_labels
  }
  
  return(cv_schemes)
}
