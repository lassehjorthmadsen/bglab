% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/functions.R
\name{tp_info}
\alias{tp_info}
\title{Take points in different flavors}
\usage{
tp_info(x, y, probs, cube, met, cube_eff = 2/3, last_roll = FALSE)
}
\arguments{
\item{x}{number of points that player needs}

\item{y}{number of points that opponent needs}

\item{probs}{numeric vector of length 6, representing outcome
probabilities (must always sum to 1 or 100)}

\item{cube}{cube value (before doubling)}

\item{met}{match equity table}

\item{cube_eff}{Cube efficiency, defaults to 0.68}

\item{last_roll}{this as a last roll position; no automatic redouble available.
Defaults to FALSE}
}
\value{
List of take points in different flavors, along with informative
metrics from the calculation
}
\description{
Calculate cubeless and cubeful take points at different scores, as a function of gammons,
and backgammons, cube level, cube efficiency, and match equity table
}
