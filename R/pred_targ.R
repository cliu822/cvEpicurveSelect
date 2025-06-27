#' @name pred_targ
#' @title Predict target area from selected areas
#' @description This function runs multivariate linear regression on time-series of values from selected areas to predict the time-series of values from target areas
#' 
#' @param goal_vec Vector of time-stratified (e.g., weekly) cases in the target area
#' @param mat_sel Matrix of selected areas. Rows represent selections in each iterations

#' @return A data frame with columns of area name and the frequency of selection, ordered by the most commonly-selected areas
#' #' @return A list with:
#' \itemize{
#'   \item \code{pred}: Vector of time-series of predicted values for target area
#'   \item \code{corr2}: Corr² value between predicted and target
#' }
#'        
#' @export 
pred_targ <- function(goal_vec, mat_sel){
  mod <- lm(goal_vec ~.,data= mat_sel)
  pred <- predict(mod, newdata=mat_sel)
  corr2 <- cor(goal_vec, pred)^2
  return(list(pred=pred,corr2=corr2))
}