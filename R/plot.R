
#' Visualization for temperature values
#'
#' @param tas TAS with transformed values (obtained with adc_tas())
#' @return A plot of the transformed values
#' @examples
#' file_path_sim <- 'data/tas_sim.rda'
#' file_path_obs <- 'data/tas.rda'
#' tas_trans <- adc_tas(file_path_sim, file_path_obs)
#' tas_plot(tas_trans)
tas_plot <- function(tas) {
  tas[year(DTM)%in%1981:1990, plot(DTM, tas_o, type = 'l', ylim = range(tas_o, tas_trans))]
  tas[year(DTM)%in%1981:1990, lines(DTM, tas_trans, col = 'red')]
}


#' Visualization for precipitation values
#'
#' @param pr PR with transformed values (obtained with adc_pr())
#' @return A plot of the transformed values
#' @examples
#' file_path_sim <- 'data/pr_sim.rda'
#' file_path_obs <- 'data/pr.rda'
#' pr_trans <- adc_pr(file_path_sim, file_path_obs)
#' pr_plot(pr_trans)
pr_plot <- function(pr) {
  pr[year(DTM)%in%1981:1990, plot(DTM, pr_o, type = 'l', ylim = range(pr_o, pr_trans))]
  pr[year(DTM)%in%1981:1990, lines(DTM, pr_trans, col = 'red')]
}
