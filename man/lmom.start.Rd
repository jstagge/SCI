% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/distrstart.r
\name{lmom.start}
\alias{lmom.start}
\title{Lmoment fitting function}
\usage{
lmom.start(
  x,
  distr = c("gamma", "genlog", "gev", "gumbel", "lnorm", "norm", "pe3", "weibull"),
  ...
)
}
\arguments{
\item{x}{Data vector.}

\item{A}{character string "name" naming a distribution for which the corresponding density function ('dname'), the corresponding distribution function ('pname') and the quantile function ('qname') must be defined (see for example 'GammaDist'}
}
\value{
An Lmoment fit.
}
\description{
Estimates parameters of distributions using L-moments
}
