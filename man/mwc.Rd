% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/functions.R
\name{mwc}
\alias{mwc}
\title{Match winning chances at score}
\usage{
mwc(x, y, met)
}
\arguments{
\item{x}{number of points that player needs}

\item{y}{number of points that opponent needs}

\item{met}{match equity table}
}
\value{
double. Match winning chance
}
\description{
Look up match winning chances at different scores in a given
match equity table
}
\examples{
met <- get_met() # Get the default Kazaross XG2 table
mwc(3, 5, met)   # Match winning chance at 3-away, 5-away

}
