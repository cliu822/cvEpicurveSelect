#' Evaluate candidate municipalities using Rcpp
#'
#' @param goal_vec Vector of weekly cases in the goal municipality
#' @param candidate_mat Matrix of candidate cases (time × municipality)
#' @param block_labels Character vector of block labels (e.g., "A", "B", "C")
#' @param unique_blocks Unique labels for each block
#' @param previously_selected Integer vector of selected candidate indices
#' @param candidate_names Character vector of candidate names
#'
#' @return A list with R2 values, selected candidates, and out-of-sample predictions
#' @name eval_candidates_rcpp
#' @export
#' @useDynLib cvEpicurveSelect, .registration = TRUE
#' @importFrom Rcpp sourceCpp
NULL
