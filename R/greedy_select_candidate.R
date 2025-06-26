
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
#' @return List of names of selected candidates and corr² values
#' 
#' @export
greedy_select_controls <- function(goal_vec, candidate_mat, cv_schemes, n_select = 10, rcpp=TRUE) {
  
  
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
    
    best_cand <- names(which.max(candidate_corr2_scores))
    selected_names <- c(selected_names, best_cand)
    corr2_per_step <- c(corr2_per_step, candidate_corr2_scores[best_cand])
    
    message(sprintf("Step %d: selected %s with avg corr2 = %.3f", i, best_cand, candidate_corr2_scores[best_cand]))
  }
  
  return(list(
    selected_controls = selected_names,
    corr2_per_step = corr2_per_step
  ))
}
