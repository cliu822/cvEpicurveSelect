
#' @name greedy_select_candidate
#' @title Select candidate areas using greedy selection algorithm
#' @description This function selects candidate areas using a greedy selection algorithm by comparing the out-of-sample corr² averaged across test blocks and cross-validation schemes
#' 
#' @param goal_vec Vector of time-stratified (e.g., weekly) cases in the target area
#' @param candidate_mat Matrix of candidate cases (time units × candidate areas)
#' @param cv_schemes List of length \code{n_cv_schemes}, each element is a vector of block labels (e.g., "A", "B", "C") of length \code{tot_ts}. Output of \code{make_cv_schemes()}
#' @param n_select Number of candidates to select
#' @param rcpp Whether to use Rcpp compiler function or R function to evaluate candidates
#' 
#' @return A list with:
#' \itemize{
#'   \item \code{selected_areas}: Vector of \code{n_select} selected areas
#'   \item \code{corr2_per_step}: Vector of performance with each additional selection
#' }

#' 
#' @export
greedy_select_candidate <- function(goal_vec, candidate_mat, cv_schemes, n_select = 10, rcpp=TRUE) {
  
  if (length(goal_vec) != nrow(candidate_mat)) {
    stop("Length of 'goal_vec' does not match the number of rows in 'candidate_mat'. Number of time series observations between goal and candidate should be the same.")
  }
  
  if (nrow(candidate_mat) == 0 || ncol(candidate_mat) == 0) {
    stop("Candidate matrix is empty. It must have at least one row and one column.")
  }
  
  if (all(is.na(candidate_mat) | candidate_mat == 0)) {
    stop("Candidate matrix contains only 0s or NAs. At least one non-zero, non-NA value is required.")
  }
  
  if (any(sapply(cv_schemes, function(scheme) length(unique(scheme)) <= 1))) {
    stop("At least one cross-validation scheme contains fewer than two unique blocks. Not enough data. ")
  }
  
  if (any(sapply(cv_schemes, function(scheme) length(unique(scheme)) < 3))) {
    warning("At least one cross-validation scheme contains fewer than three unique blocks. Consider shortening block length or adding more time steps.")
  }
  
  selected_names <- c()
  corr2_per_step <- c()
  
  for (i in 1:n_select) {
    candidate_names <- colnames(candidate_mat)
    remaining_names <- setdiff(candidate_names, selected_names)
    
    # Store average corr2 per candidate
    candidate_corr2_scores <- c()
    
    for (cand in remaining_names) {
      current_set <- c(selected_names, cand)
      current_mat <- candidate_mat[, current_set, drop = FALSE]
      
      # Accumulate corr2s across all CV schemes
      all_corr2 <- c()
      for (scheme in cv_schemes) {
        
        if(rcpp==TRUE){
          res <- eval_candidates_rcpp(
            goal_vec = as.numeric(goal_vec),
            candidate_mat = as.matrix(current_mat),
            block_labels = scheme,
            unique_blocks=unique(scheme),
            previously_selected = (which(colnames(current_mat) %in% selected_names)-1),
            candidate_names = colnames(current_mat))
          
        }else{
            res <- eval_candidates(
              goal_vec = goal_vec,
              candidate_mat = current_mat,
              block_labels = scheme,
              previously_selected = which(colnames(current_mat) %in% selected_names))
        }
        
        # Average corr2 across blocks (res$corr2 will have length = #blocks x #new candidates)
        all_corr2 <- c(all_corr2, res$corr2[res$selected == cand])
      }
      avg_corr2 <- mean(all_corr2, na.rm = TRUE)
      candidate_corr2_scores[cand] <- avg_corr2
    }
    
    #best_cand <- names(which.max(candidate_corr2_scores))  ##Selects first alphabetical
    
    
    # ---- tie-break: fair coin flip among true ties ----
    best_val <- suppressWarnings(max(candidate_corr2_scores, na.rm = TRUE))
    if (!is.finite(best_val)) {
      stop(sprintf("All candidate scores are NA at step %d. Check CV schemes or eval function outputs.", i))
    }
    ties <- names(candidate_corr2_scores)[!is.na(candidate_corr2_scores) &
                                            candidate_corr2_scores == best_val]
    
  
    best_cand <- sample(ties, 1)
    
    selected_names <- c(selected_names, best_cand)
    corr2_per_step <- c(corr2_per_step, candidate_corr2_scores[best_cand])
    
    message(sprintf("Step %d: selected %s with avg corr2 = %.3f", i, best_cand, candidate_corr2_scores[best_cand]))
  }
  
  return(list(
    selected_areas = selected_names,
    corr2_per_step = corr2_per_step
  ))
}
