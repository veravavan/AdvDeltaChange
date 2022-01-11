\name{tas}
\alias{tas}
\docType{data}
\title{
A dataset containing temperature values from Klementinum
}
\description{
A dataset containing the observed values of average temperature from Klementinum.
}
\usage{data("tas")}
\format{
  A data frame with 90031 observations on the following 6 variables.
  \describe{
    \item{\code{SOUID}}{Source identifier}
    \item{\code{DATE}}{Unformatted date YYYYMMDD}
    \item{\code{TG}}{Mean temperature in 0.1 degree Celcius}
    \item{\code{Q_TG}}{Quality code for TG (0='valid'; 1='suspect'; 9='missing')}
    \item{\code{tas_o}}{Mean temperature in degree Celcius}
    \item{\code{DTM}}{Formatted date YYYY-MM-DD}
  }
}

\source{
http://www.ecad.eu
}
\examples{
data(tas)
str(tas)
}
\keyword{datasets}
