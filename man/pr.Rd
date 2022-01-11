\name{pr}
\alias{pr}
\docType{data}
\title{
A dataset containing precipitation values from Klementinum
}
\description{
A dataset containing the observed values of precipitation from Klementinum.
}
\usage{data("pr")}
\format{
  A data frame with 73322 observations on the following 6 variables.
  \describe{
    \item{\code{SOUID}}{Source identifier}
    \item{\code{DATE}}{Unformatted date YYYYMMDD}
    \item{\code{RR}}{Precipitation amount in 0.1 mm}
    \item{\code{Q_RR}}{Quality code for RR (0='valid'; 1='suspect'; 9='missing')}
    \item{\code{pr_o}}{Precipitation amount in mm}
    \item{\code{DTM}}{Formatted date YYYY-MM-DD}
  }
}
\source{
http://www.ecad.eu
}

\examples{
data(pr)
str(pr)
}
\keyword{datasets}
