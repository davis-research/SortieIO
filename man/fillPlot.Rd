% Generated by roxygen2 (4.1.1.9000): do not edit by hand
% Please edit documentation in R/fillPlot.R
\name{fillPlot}
\alias{fillPlot}
\title{Fill A Plot In With Similar Values}
\usage{
fillPlot(currentDf, corners, speciesDf, retainCurrent = FALSE)
}
\arguments{
\item{currentDf}{This is a data.frame with (x,y) columns representing your
current points.}

\item{corners}{This is a vector set up as (minimum x, maximum x, minimum y,
maximum y) representing the size plot you want to be filled.}

\item{speciesDf}{This is a data.frame with species, density, mean dbh and sd
  of dbh. DBH could represent any numerical value needed.}
}
\value{
This function should return a data.frame with Species, X, Y, and
  DBH that contains new records to fill in the rest of the plot space.
}
\description{
Fill A Plot In With Similar Values
}

