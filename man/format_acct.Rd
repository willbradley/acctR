% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/format_acct.R
\name{format_acct}
\alias{format_acct}
\title{Function to simply format currency values}
\usage{
format_acct(x, digits = 2, shown.in = NULL)
}
\arguments{
\item{x}{numeric vector}

\item{digits}{numeric to denote number of decimal places}

\item{shown.in}{character to express value in thousands, millions or billions.
Accepts \code{c('k', 'm', 'b')}.}
}
\value{
character vector of \code{length(x)} with desired formatting
}
\description{
Formatting will follow excel accounting style
}
\examples{

\dontrun{

format_acct(100, digits = 0)

library(dplyr)
mtcars \%>\% mutate(msrp = runif(nrow(.), 20000, 30000), msrp_format = format_acct(msrp, shown.in = 'k'))

}

}
