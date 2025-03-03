% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/rastr_extract.R
\name{rastr_extract_small}
\alias{rastr_extract_small}
\title{Extract some values from raster .nc file}
\usage{
rastr_extract_small(file_path, station_lon, station_lat)
}
\arguments{
\item{file_path}{A path to the .nc file you want to extract}

\item{station_lon}{A number representing the longitude of the location to extract}

\item{station_lat}{A number representing the latitude of the location to extract}
}
\value{
A data table of the extracted values (just the first 100)
}
\description{
Extract some values from raster .nc file
}
\examples{
pr_filepath <- 'data-raw/EUR_pr_day_CanESM2_historical_rcp85_r25i1p1_19500101-21001231.nc'
stat_lon <- 14 + 24/60 + 59/60/100
stat_lat <- 50 + 05/60 + 11/60/100
pr_sim_small <- rastr_extract_small(pr_filepath, stat_lon, stat_lat)
}
