#transformation for large P

#' Transformation for precipitation values
#'
#' @param pr_ctrl Simulated precipitation values for the control period (must have a column named 'pr5' with five day sums)
#' @param pr_scen Simulated precipitation values for the scenario period (must have a column named 'pr5' with five day sums)
#' @param ctrl_q 60 and 90 quantiles of the control period precipitation data
#' @param scen_q 60 and 90 quantiles of the scenario period precipitation data
#' @param pr Observed precipitation values in the control period timeline
#' @param obs_q 60 and 90 quantiles of the observed precipitation data in the control period timeline
#' @return The observed precipitation values with an added column for transformed values
#' @examples
#' load('data/pr_sim.rda')
#' pr_sim[, pr5 := 60*60*24*frollsum(pr, 5, align = 'c')]
#' pr_sim <- pr_sim[!is.na(pr5)]
#' pr_ctrl <- pr_sim[year(DTM) %in% 1981:2010]
#' pr_scen <- pr_sim[year(DTM) %in% 2071:2100]
#' ctrl_q <- quantile(pr_ctrl$pr5, c(.6,.9))
#' scen_q <- quantile(pr_scen$pr5, c(.6,.9))
#' load('data/pr.rda')
#' pr[, pr_o5 := frollsum(pr_o, 5)]
#' pr <- pr[year(DTM) %in% 1981:2010]
#' obs_q <- quantile(pr$pr_o5, c(.6,.9))
#' print(pr_transf(pr_ctrl, pr_scen, ctrl_q, scen_q, pr, obs_q))
pr_transf <- function(pr_ctrl, pr_scen, ctrl_q, scen_q, pr, obs_q) {
  Ec <- pr_ctrl[pr5>ctrl_q[2], mean(pr5 - ctrl_q[2])]
  Es <- pr_scen[pr5>scen_q[2], mean(pr5 - scen_q[2])]
  b <- pr_b(scen_q, ctrl_q, obs_q)
  a <- pr_a(scen_q, ctrl_q, obs_q, b)
  pr[pr_o5<obs_q[2], pr_trans := a *pr_o^b]
  pr[pr_o5>=obs_q[2], pr_trans := Es/Ec * (pr_o - obs_q[2]) + a * obs_q[2] ^b]
  return(pr)
}

#' Transformation for temperature values
#'
#' @param tas Observed temperature values (must contain a column named 'tas_o' for the values)
#' @param tas_scen Simulated scenario temperature values (the values column must be named 'tas')
#' @param tas_ctrl Simulated control temperature values (the values column must be named 'tas')
#' @return The observed temperature values with an added column for transformed values
#' @examples
#' load('data/tas_sim.rda')
#' tas_sim[, tas := tas-273.15]
#' tas_ctrl = tas_sim[year(DTM) %in% 1981:2010]
#' tas_scen = tas_sim[year(DTM) %in% 2071:2100]
#' load('data/tas.rda')
#' tas <- tas_transf(tas, tas_scen, tas_ctrl)
tas_transf <- function(tas, tas_scen, tas_ctrl) {
  tas[, tas_mean_trans := tas_o + tas_scen[, mean(tas)] - tas_ctrl[, mean(tas)]]
  tas[, tas_trans := tas_mean_trans + (tas_o - mean(tas_o)) * tas_scen[, sd(tas)] / tas_ctrl[, sd(tas)]]
  return(tas)
}

