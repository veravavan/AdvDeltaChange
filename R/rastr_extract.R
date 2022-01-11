#' Extract some values from raster .nc file
#'
#' @param file_path A path to the .nc file you want to extract
#' @param station_lon A number representing the longitude of the location to extract
#' @param station_lat A number representing the latitude of the location to extract
#' @return A data table of the extracted values (just the first 100)
#' @examples
#' pr_filepath <- 'data-raw/EUR_pr_day_CanESM2_historical_rcp85_r25i1p1_19500101-21001231.nc'
#' stat_lon <- 14 + 24/60 + 59/60/100
#' stat_lat <- 50 + 05/60 + 11/60/100
#' pr_sim_small <- rastr_extract_small(pr_filepath, stat_lon, stat_lat)
rastr_extract_small <- function(file_path, station_lon, station_lat){
  rastr_data <- rast(file_path)
  brick_data <- brick(file_path)
  station <- vect(data.frame(lon = station_lon, lat = station_lat))
  ce <- cells(rastr_data, station)
  extr_data <- terra::extract(rastr_data[[1:100]], ce[1, 2])
  extr_data <- data.table(value = as.matrix(extr_data)[1, -1], DTM = brick_data@z$Date)
  return(extr_data)
}


#' Extract all values from raster .nc file
#'
#' @param file_path A path to the .nc file you want to extract
#' @param station_lon A number representing the longitude of the location to extract
#' @param station_lat A number representing the latitude of the location to extract
#' @return A data table of the extracted values
#' @examples
#' \dontrun{
#' pr_filepath <- 'data-raw/EUR_pr_day_CanESM2_historical_rcp85_r25i1p1_19500101-21001231.nc'
#' stat_lon <- 14 + 24/60 + 59/60/100
#' stat_lat <- 50 + 05/60 + 11/60/100
#' pr_sim <- rastr_extract(pr_filepath, stat_lon, stat_lat)
#' }
rastr_extract <- function(file_path, station_lon, station_lat){
  rastr_data <- rast(file_path)
  brick_data <- brick(file_path)
  station <- vect(data.frame(lon = station_lon, lat = station_lat))
  extr_data <- terra::extract(rastr_data, station)
  extr_data <- data.table(value = as.matrix(extr_data)[1, -1], DTM = brick_data@z$Date)
  return(extr_data)
}
