% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/sumstats.R
\name{mean_wet}
\alias{mean_wet}
\title{Mean rainfall on wet days}
\usage{
mean_wet(x, cutoff = 0)
}
\arguments{
\item{x}{Numeric vector of rainfall totals}

\item{cutoff}{If cutoff>0, rain day defined as day where rain>=cutoff, otherwise rain day defined as day where rain>0}
}
\value{
Mean of rainfall on rain days
}
\description{
Mean rainfall on wet days
}

