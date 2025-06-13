
#' Greedy selection of control municipalities to optimize out-of-sample R2
#' 
#' @param goal_vec Vector of weekly cases in the goal municipality
#' @param candidate_mat Matrix of weekly cases for all candidate municipalities
#' @param cv_schemes List of length-n_splits, each a vector of block labels (e.g., "A", "B", "C") of length nrow(goal_vec)
#' @param n_select Number of municipalities to select
#' @param rcpp Whether to use rcpp compiler function for evaluating candidates or R function
#' 
#' @return List with selected municipality names and R2 per step
#' 
#' @export
greedy_select_controls <- function(goal_vec, candidate_mat, cv_schemes, n_select = 10, rcpp=TRUE) {
  
  
  selected_names <- c()
  r2_per_step <- c()
  
  for (i in 1:n_select) {
    candidate_names <- colnames(candidate_mat)
    remaining_names <- setdiff(candidate_names, selected_names)
    
    # Store average R2 per candidate
    candidate_r2_scores <- c()
    
    for (cand in remaining_names) {
      current_set <- c(selected_names, cand)
      current_mat <- candidate_mat[, current_set, drop = FALSE]
      
      # Accumulate R2s across all CV schemes
      all_r2 <- c()
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
        
        # Average R2 across blocks (res$R2 will have length = #blocks x #new candidates)
        all_r2 <- c(all_r2, res$R2[res$selected == cand])
      }
      avg_r2 <- mean(all_r2, na.rm = TRUE)
      candidate_r2_scores[cand] <- avg_r2
    }
    
    best_cand <- names(which.max(candidate_r2_scores))
    selected_names <- c(selected_names, best_cand)
    r2_per_step <- c(r2_per_step, candidate_r2_scores[best_cand])
    
    message(sprintf("Step %d: selected %s with avg R2 = %.3f", i, best_cand, candidate_r2_scores[best_cand]))
  }
  
  return(list(
    selected_controls = selected_names,
    r2_per_step = r2_per_step
  ))
}
