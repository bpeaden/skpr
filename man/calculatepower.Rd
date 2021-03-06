% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/calculatepower.R
\name{calculatepower}
\alias{calculatepower}
\title{Calculate Power}
\usage{
calculatepower(X, L, lambda, alpha, degrees_of_freedom)
}
\arguments{
\item{X}{The model matrix}

\item{L}{The parameter matrix/vector}

\item{lambda}{The non-centrality parameter for the F test}

\item{alpha}{the specified type-I error}

\item{degrees_of_freedom}{The number of degrees of freedom.}
}
\value{
The power for a given parameter L, given the
}
\description{
Calculates the power of the model given the non-centrality parameter
}
\keyword{internal}
