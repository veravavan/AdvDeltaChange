\name{pr_sim_small}
\alias{pr_sim_small}
\docType{data}
\title{
A data table containing only some of the extracted values of precipitation
}
\description{
Extracted from the .nc precipitation file, the dataset shows the simulated values as a data table (already converted). Product of the rastr_extract_small function.
}
\usage{data("pr_sim_small")}
\format{
  A data table with 55115 observations on the following variables:
  \describe{
    \item{\code{value}}{a numeric vector, the simulated precipitation}
    \item{\code{DTM}}{Dates of the observations YYYY-MM-DD}
  }
}
\examples{
data(pr_sim_small)
}
\keyword{datasets}
