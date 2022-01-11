\name{tas_sim_small}
\alias{tas_sim_small}
\docType{data}
\title{
A data table containing only some of the extracted values of temperature
}
\description{
Extracted from the .nc temperature file, the dataset shows the simulated values as a data table (already converted). Product of the rastr_extract_small function.
}
\usage{data("tas_sim_small")}
\format{
  A data table with 55115 observations on the following variables:
  \describe{
    \item{\code{value}}{a numeric vector, the simulated temperature}
    \item{\code{DTM}}{Dates of the observations YYYY-MM-DD}
  }
}
\examples{
data(tas_sim_small)
}
\keyword{datasets}
