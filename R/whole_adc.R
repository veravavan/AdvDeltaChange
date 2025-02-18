#the whole method

#' Transformation for precipitation values (whole method)
#'
#' @param data_sim the data with the simulated precipitation values (already extracted)
#' @param data_obs the data with the observed precipitation values
#' @return The observed precipitation values with an added column for transformed values
#' @examples
#' data_sim <- data('pr_sim')
#' data_obs <- data('pr')
#' trans <- adc_pr(data_sim, data_obs)
adc_pr <- function(data_sim, data_obs) {
  assign('pr_sim', get(data_sim))
  assign('pr', get(data_obs))
  pr_sim[, pr5 := 60*60*24*frollsum(pr, 5, align = 'c')]
  pr_sim <- pr_sim[!is.na(pr5)]
  pr_ctrl <- pr_sim[year(DTM) %in% 1981:2010]
  pr_scen <- pr_sim[year(DTM) %in% 2071:2100]
  ctrl_q <- quantile(pr_ctrl$pr5, c(.6,.9))
  scen_q <- quantile(pr_scen$pr5, c(.6,.9))
  pr[, pr_o5 := frollsum(pr_o, 5)]
  pr <- pr[year(DTM) %in% 1981:2010]
  obs_q <- quantile(pr$pr_o5, c(.6,.9))
  pr_trans <- pr_transf(pr_ctrl, pr_scen, ctrl_q, scen_q, pr, obs_q)
  return(pr_trans)
}

#' Transformation for temperature values (whole method)
#'
#' @param data_sim the data with the simulated temperature values (already extracted)
#' @param data_obs the data with the observed temperature values
#' @return The observed temperature values with an added column for transformed values
#' @examples
#' data_sim <- data('tas_sim')
#' data_obs <- data('tas')
#' tas_trans <- adc_tas(data_sim, data_obs)
adc_tas <- function(data_sim, data_obs) {
  assign('tas_sim', get(data_sim))
  tas_sim[, tas := tas-273.15]
  tas_ctrl = tas_sim[year(DTM) %in% 1981:2010]
  tas_scen = tas_sim[year(DTM) %in% 2071:2100]
  assign('tas', get(data_obs))
  tas <- tas_transf(tas, tas_scen, tas_ctrl)
  return(tas)
}

