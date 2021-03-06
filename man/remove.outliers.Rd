% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/remove_outliers.R
\name{remove.outliers}
\alias{remove.outliers}
\title{Sets outliers to NA}
\usage{
remove.outliers(v, n.sd = 3)
}
\arguments{
\item{v}{vector of values}

\item{n.sd}{number of standard deviation (default: 3)}
}
\value{
a vector having outliers set to NA
}
\description{
Sets to NA the values greater/smaller than n.sd standard deviation from
the values' mean.
}
\examples{
x <- rexp(100)
remove.outliers(x)
}
\author{
Alessia Visconti
}
