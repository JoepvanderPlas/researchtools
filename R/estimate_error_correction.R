#' @name estimate_error_correction
#' @title Error correction estimator
#'
#' @param y Character
#' @param X Character (vector)
#' @param data Data.table
#'
#' @return Returns the long-run estimates in a data.table format
#' @export

# function used to used data.table functions
usethis::use_data_table()

estimate_error_correction <- function(y, X, data, cross_section) {

  # create lag variables
  data[, paste0(c(y, X), "_lag1") := shift(.SD), .SDcols = c(y, X), by = cross_section] 

  # create diff variables
  data[, paste0(c(y, X), "_diff1") := .SD - shift(.SD), .SDcols = c(y, X), by = cross_section] 

  # specify formula
  formula <- paste0(paste0(y, "_diff1"), " ~ ", paste0(X, "_diff1", collapse = " + "), " + ", paste0(y, "_lag1")," + ", paste0(X, "_lag1", collapse = " + "))

  # estimate model with OLS
  mod <- stats::lm(formula, data = data)

  # take coeffs
  DT_coeffs <- setDT(broom::tidy(mod))

  ## Long-term effect
  # create a data table with all statistics required to compute the (i) LT effect and (ii) significance
  DT_LT_effect <- data.table(term = X,
                          coef_indep_var = DT_coeffs[term %chin% paste0(X, "_lag1"), estimate],
                          SE_indep_var = DT_coeffs[term %chin% paste0(X, "_lag1"), std.error],
                          coef_speed_of_adj = DT_coeffs[term == paste0(y, "_lag1"), estimate],
                          SE_speed_of_adj = DT_coeffs[term == paste0(y, "_lag1"), std.error],
                          cov_speed_of_adj_and_indep_var = stats::vcov(mod)[paste0(y, "_lag1"), paste0(X, "_lag1")])


  # compute SE -- analytical approach
  DT_LT_effect[, SE := sqrt( (1/(-coef_speed_of_adj)^2*SE_indep_var^2) + (coef_indep_var^2/(-coef_speed_of_adj)^4*SE_speed_of_adj^2) - (2*coef_indep_var/(-coef_speed_of_adj)^3*(-cov_speed_of_adj_and_indep_var)) )]

  # the analytical approach == the delta method -- verified twice. See delta method below and benchmarked with the package ARDL.
  #DT_LT_effect[1, SE_delta_rule := sqrt( matrix(c(-1/coef_speed_of_adj, coef_indep_var/coef_speed_of_adj^2), ncol = 2) %*%
  #                                   matrix(c(SE_indep_var^2, cov_speed_of_adj_and_indep_var, cov_speed_of_adj_and_indep_var, SE_speed_of_adj^2), ncol = 2, nrow = 2) %*%
  #                                   matrix(c(-1/coef_speed_of_adj, coef_indep_var/coef_speed_of_adj^2), nrow = 2) )]


  # compute (i) LT effect, (ii) t_statistic, and (iii) p-value
  DT_LT_effect[, effect := coef_indep_var/-coef_speed_of_adj]
  DT_LT_effect[, t_statistic := effect/SE]
  DT_LT_effect[, p_value := 2 * stats::pt(-abs(t_statistic), df = stats::df.residual(mod))] # df = n - # of estimated coefficients


  # keep only some columns
  DT_LT_effect <- DT_LT_effect[, .(term, effect, SE, t_statistic, p_value)]

  # return coeffs
  return(DT_LT_effect)

}


