% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/element_covariate_plot.R
\name{element_covariate_network}
\alias{element_covariate_network}
\title{Plot the network of elements and covariates based on the long format of sequences}
\usage{
element_covariate_network(
  sequences_long,
  cutoff = 3,
  element,
  covariate,
  n_permutations = 1000,
  pvalue = 0.01,
  clusters = FALSE
)
}
\arguments{
\item{sequences_long}{A data frame containing the sequences, with columns for elements and contexts.}

\item{cutoff}{minimum number of occurrences for which element or covariate should be included}

\item{element}{A string specifying the column name for elements in the sequences data frame.}

\item{covariate}{A string specifying the column name for contexts in the sequences data frame.}

\item{n_permutations}{An integer specifying the number of permutations for the bootstrapping process.}

\item{pvalue}{cutoff pvalue to include combination}

\item{clusters}{should clusters be calculated and added?}
}
\value{
plot of bimodal network containing the elements and covariates
}
\description{
Plot the network of elements and covariates based on the long format of sequences
}
