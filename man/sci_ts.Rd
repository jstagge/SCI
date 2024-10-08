% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/classes.r
\name{sci_ts}
\alias{sci_ts}
\title{Create sci_ts object}
\usage{
sci_ts(ts, resolution, stage = "orig_ts")
}
\arguments{
\item{ts}{A zoo object.}

\item{resolution}{Character in lowercase, daily or monthly.}

\item{stage}{Stage in the SCI calculation process. Options include orig_ts, roll_mean, sci}
}
\value{
An sci_ts object.
}
\description{
Wrapper to create an sci_ts object from a zoo time series
}
