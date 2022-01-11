\name{pr_sim}
\alias{pr_sim}
\docType{data}
\title{
A data table containing extracted values of precipitation
}
\description{
Extracted from the .nc precipitation file, the dataset shows the simulated values as a data table. Product of the rastr_extract function.
}
\usage{data("pr_sim")}
\format{
  A data table with 55115 observations on the following 2 variables.
  \describe{
    \item{\code{pr}}{values of simulated precipitation}
    \item{\code{DTM}}{Dates for the values YYYY-MM-DD}
  }
}
\examples{
data(pr_sim)
}
\keyword{datasets}
