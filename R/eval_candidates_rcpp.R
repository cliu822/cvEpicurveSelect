#' Evaluate candidate municipalities using Rcpp
#' @name eval_candidates_rcpp
#' @title Evaluate candidates via out-of-sample corr² from multilinear regression (Rcpp version)
#' @description This Rcpp function evaluates each candidate area by fitting a linear regression model to predict case counts in a target area using cross-validation. For each cross-validation scheme, it calculates the out-of-sample corr² for each candidate (in combination with any previously selected candidates) and stores the predicted values across all time points.
#' 

#' @param goal_vec Vector of time-stratified (e.g., weekly) cases in the target area
#' @param candidate_mat Matrix of candidate cases (time units × candidate areas)
#' @param block_labels Character vector of block labelings for a single cross-validation scheme
#' @param unique_blocks Character vector of unique block labels
#' @param previously_selected Integer vector of indices of previously selected areas
#' @param candidate_names Character vector of candidate names
#'
#'@return A list with:
#' \itemize{
#'   \item \code{corr²}: Vector of corr² values for each (block × candidate)
#'   \item \code{selected}: Vector of candidate names corresponding to each corr²
#'   \item \code{oos_est}: Matrix of out-of-sample predictions (same shape as \code{candidate_mat})
#' }



#' @export
#' @useDynLib cvEpicurveSelect, .registration = TRUE
#' @importFrom Rcpp sourceCpp
NULL
