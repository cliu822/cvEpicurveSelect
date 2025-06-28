#include <RcppArmadillo.h>
using namespace Rcpp;

// [[Rcpp::depends(RcppArmadillo)]]

// [[Rcpp::export]]
List eval_candidates_rcpp(const arma::vec& goal_vec,                  //Vector of cases in target area
                          const arma::mat& candidate_mat,             //Matrix of candidate area values (each area as columns over #rows for each time step)
                          const CharacterVector& block_labels,           //Vector of CV block labels (ex. "A", "B", "C")
                          const CharacterVector& unique_blocks,         //Unique block label values
                          const IntegerVector& previously_selected,   //Indices of already selected candidates areas
                          const CharacterVector& candidate_names) {   //Names of candidate areas
  
  int n = goal_vec.n_elem;                                            //Number of time steps
  int n_candidates = candidate_mat.n_cols;                            //Number of candidate areas
  
  arma::mat oos_matrix(n, n_candidates, arma::fill::value(NA_REAL));  //Initialize out-of-sample (OOS) prediction matrix with NAs with n rows and n_candidate columns
  std::vector<double> corr2_vec;                                         //Initialize vectors to store corr2 values (candidates x number blocks)                             
  std::vector<std::string> selected_vec;                              //Initialize vector to store names of selected areas  
  
  //Loop over each block (excluding last one, which may be incomplete)
  for (int b = 0; b < unique_blocks.size() - 1; ++b) {                  
    String test_block = unique_blocks[b];                                //Identifies the current block label being used as the test block
    
    
    std::vector<int> test_idx, train_idx;                             //Initialize vectors for test_idx and train_idx
   
    //For each time point, if label in block_labels matches the test_block, then added to test_idk, otherwise added to train_idx
    for (int i = 0; i < n; ++i) {
      if (block_labels[i] == test_block) {
        test_idx.push_back(i);                                        //test_idx contains indices of rows belonging to the test set
      } else {
        train_idx.push_back(i);                                       //train_idx contains indices of rows belonging to the training set
      }
    }
    
    //Convert to Armadillo unsigned integer vectors
    arma::uvec train_u(train_idx.size());
    for (size_t i = 0; i < train_idx.size(); ++i) train_u[i] = train_idx[i];
    arma::uvec test_u(test_idx.size());
    for (size_t i = 0; i < test_idx.size(); ++i) test_u[i] = test_idx[i];
    
    //Extract values for the goal targets for training and testing
    arma::vec y_train = goal_vec.elem(train_u);
    arma::vec y_test = goal_vec.elem(test_u);
    
    //Loop over all candidate areas
    for (int p = 0; p < n_candidates; ++p) {
      bool already_selected = false;                                //Initializes a flag to track whether current candidate (p) already selected
      for (int k = 0; k < previously_selected.size(); ++k) {        //Loops through vector of previously selected
        if (p == previously_selected[k]) {
          already_selected = true;                                  //If a match is found, set already_selected to true
          break;
        }
      }
      if (already_selected) continue;                               //If candidate p already selected, then skip candidate and go to next one
      
      arma::mat X_train(train_idx.size(), 1 + 1 + previously_selected.size(), arma::fill::ones);  //Initializes matrices for trainings and testing, and +1 for intercept
      arma::mat X_test(test_idx.size(), 1 + 1 + previously_selected.size(), arma::fill::ones);
      
      X_train.col(1) = candidate_mat.submat(train_u, arma::uvec{(unsigned int)p}); //Add current candidate as first column, use submat to subset row and column at same time
      X_test.col(1)  = candidate_mat.submat(test_u,  arma::uvec{(unsigned int)p});
      
      for (int k = 0; k < previously_selected.size(); ++k) {                      //Add previously selected candidates to the remaining columns
        X_train.col(k + 2) = candidate_mat.submat(train_u, arma::uvec{(unsigned int)previously_selected[k]});
        X_test.col(k + 2)  = candidate_mat.submat(test_u,  arma::uvec{(unsigned int)previously_selected[k]});
        
      }
      
      // Skip candidate if design matrix is rank-deficient
      if (arma::rank(X_train) < X_train.n_cols) continue;
      
      // Fit linear regression: solve for coefficients
      arma::vec coef;
      bool success = arma::solve(coef, X_train, y_train);
      
      //if (!success || coef.has_nan()) {
      //  return R_NilValue; // Return NULL if model failed
      //}
      
      if (!success || coef.has_nan()) continue;
      
      // Predict on test data
      arma::vec preds = X_test * coef;
      
      //Compute corr2
      //double ss_res = arma::accu(arma::square(y_test - preds));
      //double ss_tot = arma::accu(arma::square(y_test - arma::mean(y_test)));
      //double corr2 = (ss_tot == 0) ? 0.0 : 1.0 - ss_res / ss_tot;

      //Compute cor^2
      double corr2;
      if (arma::accu(y_test) == 0) {
        corr2 = 0.0;
      } else {
        double r = arma::as_scalar(arma::cor(y_test, preds));
        corr2 = std::isfinite(r) ? r * r : 0.0;  // extra safety
      }
      //double r = arma::as_scalar(arma::cor(y_test, preds));
      //double corr2 = r * r;
      
      corr2_vec.push_back(corr2);
      selected_vec.push_back(as<std::string>(candidate_names[p]));
      
      // Fill OOS matrix
      for (int i = 0; i < test_idx.size(); ++i) {
        oos_matrix(test_idx[i], p) = preds[i];
      }
    }
  }
  
  return List::create(
    Named("corr2") = corr2_vec,
    Named("selected") = selected_vec,
    Named("oos_est") = oos_matrix
  );
}
