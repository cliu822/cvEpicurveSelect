# Generated by using Rcpp::compileAttributes() -> do not edit by hand
# Generator token: 10BE3573-1514-4C36-9D1C-5A225CD40393

eval_candidates_rcpp <- function(goal_vec, candidate_mat, block_labels, unique_blocks, previously_selected, candidate_names) {
    .Call(`_cvEpicurveSelect_eval_candidates_rcpp`, goal_vec, candidate_mat, block_labels, unique_blocks, previously_selected, candidate_names)
}

