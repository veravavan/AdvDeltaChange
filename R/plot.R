
#' Visualization for temperature values
#'
#' @param tas TAS with transformed values (obtained with adc_tas())
#' @param start_yr The starting year for the data, if not supplied = 1981
#' @param end_yr The ending year for the data, if not supplied = 1990
#' @return A plot of the transformed values
#' @examples
#' load('data/tas_sim.rda')
#' tas_sim[, tas := tas-273.15]
#' tas_ctrl = tas_sim[year(DTM) %in% 1981:2010]
#' tas_scen = tas_sim[year(DTM) %in% 2071:2100]
#' load('data/tas.rda')
#' tas_trans <- tas_transf(tas, tas_scen, tas_ctrl)
#' tas_plot(tas_trans)
tas_plot <- function(tas, start_yr = 1981, end_yr = 1990) {
  tas[year(DTM)%in%start_yr:end_yr, plot(DTM, tas_o, type = 'l', ylim = range(tas_o, tas_trans))]
  tas[year(DTM)%in%start_yr:end_yr, lines(DTM, tas_trans, col = 'red')]
}


#' Visualization for precipitation values
#'
#' @param pr PR with transformed values (obtained with adc_pr())
#' @param start_yr The starting year for the data, if not supplied = 1981
#' @param end_yr The ending year for the data, if not supplied = 1990
#' @return A plot of the transformed values
#' @examples
#' data_sim <- data('pr_sim')
#' data_obs <- data('pr')
#' pr_trans <- adc_pr(data_sim, data_obs)
#' pr_plot(pr_trans)
pr_plot <- function(pr, start_yr = 1981, end_yr = 1990) {
  pr[year(DTM)%in%start_yr:end_yr, plot(DTM, pr_trans, type = 'l', ylim = range(pr_o, pr_trans))]
  pr[year(DTM)%in%start_yr:end_yr, lines(DTM, pr_o, col = 'red')]

}
