// Generated by using Rcpp::compileAttributes() -> do not edit by hand
// Generator token: 10BE3573-1514-4C36-9D1C-5A225CD40393

#include <RcppArmadillo.h>
#include <Rcpp.h>

using namespace Rcpp;

#ifdef RCPP_USE_GLOBAL_ROSTREAM
Rcpp::Rostream<true>&  Rcpp::Rcout = Rcpp::Rcpp_cout_get();
Rcpp::Rostream<false>& Rcpp::Rcerr = Rcpp::Rcpp_cerr_get();
#endif

// eval_candidates_rcpp
List eval_candidates_rcpp(const arma::vec& goal_vec, const arma::mat& candidate_mat, const CharacterVector& block_labels, const CharacterVector& unique_blocks, const IntegerVector& previously_selected, const CharacterVector& candidate_names);
RcppExport SEXP _cvEpicurveSelect_eval_candidates_rcpp(SEXP goal_vecSEXP, SEXP candidate_matSEXP, SEXP block_labelsSEXP, SEXP unique_blocksSEXP, SEXP previously_selectedSEXP, SEXP candidate_namesSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< const arma::vec& >::type goal_vec(goal_vecSEXP);
    Rcpp::traits::input_parameter< const arma::mat& >::type candidate_mat(candidate_matSEXP);
    Rcpp::traits::input_parameter< const CharacterVector& >::type block_labels(block_labelsSEXP);
    Rcpp::traits::input_parameter< const CharacterVector& >::type unique_blocks(unique_blocksSEXP);
    Rcpp::traits::input_parameter< const IntegerVector& >::type previously_selected(previously_selectedSEXP);
    Rcpp::traits::input_parameter< const CharacterVector& >::type candidate_names(candidate_namesSEXP);
    rcpp_result_gen = Rcpp::wrap(eval_candidates_rcpp(goal_vec, candidate_mat, block_labels, unique_blocks, previously_selected, candidate_names));
    return rcpp_result_gen;
END_RCPP
}

static const R_CallMethodDef CallEntries[] = {
    {"_cvEpicurveSelect_eval_candidates_rcpp", (DL_FUNC) &_cvEpicurveSelect_eval_candidates_rcpp, 6},
    {NULL, NULL, 0}
};

RcppExport void R_init_cvEpicurveSelect(DllInfo *dll) {
    R_registerRoutines(dll, NULL, CallEntries, NULL, NULL);
    R_useDynamicSymbols(dll, FALSE);
}
