% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/functions.R
\name{gwc2mwc}
\alias{gwc2mwc}
\title{Calculate match winning chance from game winning chances}
\usage{
gwc2mwc(pwin, x, y, cube, met)
}
\arguments{
\item{pwin}{game winning chances (cubeless)}

\item{x}{number of points that player needs}

\item{y}{number of points that opponent needs}

\item{cube}{cube value}

\item{met}{match equity table}
}
\value{
double
}
\description{
Given the game winning probability, match score, and cube
value, what is a players match winning probability?
}
\examples{
met <- get_met() # Get the default Kazaross XG2 table
gwc2mwc(0.5, 3, 5, 1, met)

}
