% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/whole_adc.R
\name{adc_tas}
\alias{adc_tas}
\title{Transformation for temperature values (whole method)}
\usage{
adc_tas(data_sim, data_obs)
}
\arguments{
\item{data_sim}{the data with the simulated temperature values (already extracted)}

\item{data_obs}{the data with the observed temperature values}
}
\value{
The observed temperature values with an added column for transformed values
}
\description{
Transformation for temperature values (whole method)
}
\examples{
data_sim <- data('tas_sim')
data_obs <- data('tas')
tas_trans <- adc_tas(data_sim, data_obs)
}
