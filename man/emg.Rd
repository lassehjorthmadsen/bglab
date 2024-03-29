% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/functions.R
\name{emg}
\alias{emg}
\title{Scale match winning chances to money game}
\usage{
emg(mwc, a, b, cube, met)
}
\arguments{
\item{mwc}{current match winning chances for player}

\item{a}{number of points that player needs}

\item{b}{number of points that opponent needs}

\item{cube}{cube value}

\item{met}{match equity table}
}
\value{
double
}
\description{
This scales a match winning chance at a certain score
to the more familiar [-1, +1] money game-like interval.
}
\examples{
# If I have 0.7 to win the match at 3-away, 5-away,
# how good is this on an money game scale?
met <- get_met() # Get the default Kazaross XG2 table
emg(0.7, 3, 5, 1, met)

}
