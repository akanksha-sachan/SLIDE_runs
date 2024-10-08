% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/runPCR.R
\name{runPCR}
\alias{runPCR}
\title{Run principal components regression for cross-validation.}
\usage{
runPCR(train_y, train_x, valid_x)
}
\arguments{
\item{train_y}{a vector of numeric values; the response (training set)}

\item{train_x}{a matrix or data frame of numeric values; the data (training set)}

\item{valid_x}{a matrix or data frame of numeric values; the data (validation set)}
}
\value{
a list containing the training model and the predicted values of the response
using \code{valid_x} and \code{train_x}
}
\description{
Creates a linear regression model on the principal components of the data
set and predicts response values for the validation set.
}
