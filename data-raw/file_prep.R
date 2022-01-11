library(data.table)
library(raster)
library(ncdf4)
pr <- fread('data-raw/RR_STAID000027.txt', na.strings = '-9999')
tas <- fread('data-raw/TG_STAID000027.txt', na.strings = '-9999')

pr = pr[!is.na(RR)]
pr = pr[, pr_o := RR/10]
pr[, DTM := as.Date(as.character(DATE), format = '%Y%m%d')]

tas[, tas_o := TG/10]
tas = tas[!is.na(tas_o)]
tas[, DTM := as.Date(as.character(DATE), format = '%Y%m%d')]

usethis::use_data(pr, tas, overwrite = T)

tas_filepath <- 'data-raw/EUR_tas_day_CanESM2_historical_rcp85_r50i1p1_19500101-21001231.nc'
pr_filepath <- 'data-raw/EUR_pr_day_CanESM2_historical_rcp85_r25i1p1_19500101-21001231.nc'
stat_lon <- 14 + 24/60 + 59/60/100
stat_lat <- 50 + 05/60 + 11/60/100
pr_sim_small <- rastr_extract_small(pr_filepath, stat_lon, stat_lat)
tas_sim_small <- rastr_extract_small(tas_filepath, stat_lon, stat_lat)

usethis::use_data(pr_sim_small, tas_sim_small, overwrite = T)

#downloaded pr_sim and tas_sim from the teacher, supposed to be a data table with a date as well
pr_brick <- brick('data-raw/EUR_pr_day_CanESM2_historical_rcp85_r25i1p1_19500101-21001231.nc')
tas_brick <- brick('data-raw/EUR_tas_day_CanESM2_historical_rcp85_r50i1p1_19500101-21001231.nc')
pr_sim <- data.table(pr = as.matrix(pr_sim)[1, -1], DTM = pr_brick@z$Date)
tas_sim <- data.table(tas = as.matrix(tas_sim)[1, -1], DTM = tas_brick@z$Date)
usethis::use_data(pr_sim, tas_sim, overwrite = T)
