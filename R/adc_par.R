
#no bias correction

#' Find the a parameter with no bias correction for precipitation
#'
#' @param scen_q 60 and 90 quantiles of the scenario period precipitation data
#' @param ctrl_q 60 and 90 quantiles of the control period precipitation data
#' @param b Already calculated b parameter with pr_b_nb
#' @return The a parameter value
#' @examples
#' load('data/pr_sim.rda')
#' pr_sim[, pr5 := 60*60*24*frollsum(pr, 5, align = 'c')]
#' pr_sim = pr_sim[!is.na(pr5)]
#' pr_ctrl = pr_sim[year(DTM) %in% 1981:2010]
#' pr_scen = pr_sim[year(DTM) %in% 2071:2100]
#' ctrl_q = quantile(pr_ctrl$pr5, c(.6,.9))
#' scen_q = quantile(pr_scen$pr5, c(.6,.9))
#' pr_b <- pr_b_nb(scen_q, ctrl_q)
#' pr_a <- pr_a_nb(scen_q, ctrl_q, pr_b)
pr_a_nb <- function(scen_q, ctrl_q, b) {
  a <- scen_q[1] / (ctrl_q[1]^b)
  return(a)
}

#' Find the b parameter with no bias correction for precipitation
#'
#' @param scen_q 60 and 90 quantiles of the scenario period precipitation data
#' @param ctrl_q 60 and 90 quantiles of the control period precipitation data
#' @return The b parameter value
#' @examples
#' load('data/pr_sim.rda')
#' pr_sim[, pr5 := 60*60*24*frollsum(pr, 5, align = 'c')]
#' pr_sim = pr_sim[!is.na(pr5)]
#' pr_ctrl = pr_sim[year(DTM) %in% 1981:2010]
#' pr_scen = pr_sim[year(DTM) %in% 2071:2100]
#' ctrl_q = quantile(pr_ctrl$pr5, c(.6,.9))
#' scen_q = quantile(pr_scen$pr5, c(.6,.9))
#' pr_b <- pr_b_nb(scen_q, ctrl_q)
pr_b_nb <- function(scen_q, ctrl_q) {
  b <- log(scen_q[2]/scen_q[1]) / log(ctrl_q[2]/ctrl_q[1])
  return(b)
}

#with bias correction


#' Find the a parameter with bias correction for precipitation
#'
#' @param scen_q 60 and 90 quantiles of the scenario period precipitation data
#' @param ctrl_q 60 and 90 quantiles of the control period precipitation data
#' @param obs_q 60 and 90 quantiles of the observed precipitation data in the control period timeline
#' @param b Already calculated b parameter with pr_b
#' @return The a parameter value
#' @examples
#' load('data/pr_sim.rda')
#' pr_sim[, pr5 := 60*60*24*frollsum(pr, 5, align = 'c')]
#' pr_sim = pr_sim[!is.na(pr5)]
#' pr_ctrl = pr_sim[year(DTM) %in% 1981:2010]
#' pr_scen = pr_sim[year(DTM) %in% 2071:2100]
#' ctrl_q = quantile(pr_ctrl$pr5, c(.6,.9))
#' scen_q = quantile(pr_scen$pr5, c(.6,.9))
#' load('data/pr.rda')
#' pr[, pr_o5 := frollsum(pr_o, 5)]
#' pr = pr[year(DTM) %in% 1981:2010]
#' obs_q = quantile(pr$pr_o5, c(.6,.9))
#' b <- pr_b(scen_q, ctrl_q, obs_q)
#' a <- pr_a(scen_q, ctrl_q, obs_q, b)
pr_a <- function(scen_q, ctrl_q, obs_q, b) {
  g1 = obs_q[1] / ctrl_q[1]
  a = scen_q[1] / (ctrl_q[1]^b) * g1^(1-b)
  return(a)
}

#' Find the b parameter with bias correction for precipitation
#'
#' @param scen_q 60 and 90 quantiles of the scenario period precipitation data
#' @param ctrl_q 60 and 90 quantiles of the control period precipitation data
#' @param obs_q 60 and 90 quantiles of the observed precipitation data in the control period timeline
#' @return The b parameter value
#' @examples
#' load('data/pr_sim.rda')
#' pr_sim[, pr5 := 60*60*24*frollsum(pr, 5, align = 'c')]
#' pr_sim = pr_sim[!is.na(pr5)]
#' pr_ctrl = pr_sim[year(DTM) %in% 1981:2010]
#' pr_scen = pr_sim[year(DTM) %in% 2071:2100]
#' ctrl_q = quantile(pr_ctrl$pr5, c(.6,.9))
#' scen_q = quantile(pr_scen$pr5, c(.6,.9))
#' load('data/pr.rda')
#' pr[, pr_o5 := frollsum(pr_o, 5)]
#' pr = pr[year(DTM) %in% 1981:2010]
#' obs_q = quantile(pr$pr_o5, c(.6,.9))
#' b <- pr_b(scen_q, ctrl_q, obs_q)
pr_b <- function(scen_q, ctrl_q, obs_q) {
  g1 = obs_q[1] / ctrl_q[1]
  g2 = obs_q[2] / ctrl_q[2]
  b = log( (g2 * scen_q[2])/(g1 * scen_q[1])) / log( (g2 * ctrl_q[2])/(g1*ctrl_q[1]) )
}
