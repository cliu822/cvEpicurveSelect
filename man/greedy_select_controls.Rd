% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/greedy_select_controls.R
\name{greedy_select_controls}
\alias{greedy_select_controls}
\title{Greedy selection of control municipalities to optimize out-of-sample R2}
\usage{
greedy_select_controls(
  goal_vec,
  candidate_mat,
  cv_schemes,
  n_select = 10,
  rcpp = TRUE
)
}
\arguments{
\item{goal_vec}{Vector of weekly cases in the goal municipality}

\item{candidate_mat}{Matrix of weekly cases for all candidate municipalities}

\item{cv_schemes}{List of length-n_splits, each a vector of block labels (e.g., "A", "B", "C") of length nrow(goal_vec)}

\item{n_select}{Number of municipalities to select}

\item{rcpp}{Whether to use rcpp compiler function for evaluating candidates or R function}
}
\value{
List with selected municipality names and R2 per step
}
\description{
Greedy selection of control municipalities to optimize out-of-sample R2
}
