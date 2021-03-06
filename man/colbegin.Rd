% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/new.R
\name{colbegin}
\alias{colbegin}
\title{Giving column names begin with pattern}
\usage{
colbegin(df, pattern)
}
\arguments{
\item{df}{dataframes with column names containing string}

\item{pattern}{string pattern}
}
\description{
Giving column names begin with pattern
}
\examples{
colbegin(mtcars$make, 'to')
}
\keyword{string}
