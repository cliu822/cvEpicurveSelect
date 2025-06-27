#' @name order_select
#' @title Count number of times candidate areas are selected
#' @description This function counts the number of times a candidate area is selected across all iterations
#' 
#' @param mat_sel Matrix of selected areas. Rows represent selections in each iterations
#'
#'
#' @return A data frame with columns of area name and the frequency of selection, ordered by the most commonly-selected areas
#'         
#' @export 
order_select<-function(mat_sel){
  # Flatten the matrix into a single vector of all selected areas
  selected_all <- as.vector(as.matrix(mat_sel))
  
  selection_counts <- table(factor(selected_all))
  
  # Convert to data frame for easy viewing/sorting
  selection_df <- data.frame(
    Area = names(selection_counts),
    Count = as.integer(selection_counts)
  )
  # Sort by most selected
  selection_df <- selection_df[order(-selection_df$Count), ]
  
  return(selection_df)
}