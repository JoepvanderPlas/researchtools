#  1 functie en die  een SUR EC model schat + reshape van de data

# Copula transformation function (see Park and Gupta 2012), based on a variable's ECDF
make_copula <- function(x) {
  if (length(unique(x)) %in% c(0,1)) return(as.numeric(rep(NA, length(x))))
  return(ifelse(ecdf(x)(x)==1, qnorm(1-.0000001), qnorm(ecdf(x)(x))))
}

# main function
estimate_error_correction_SUR <- function(data, y_name, X_name,  time, cross_section, add_copulas = T, praise_winsten_correction = F) {
  
  ## (1) transform the variables
  # create lag variables
  data[, paste0(c(y_name, X_name), "_lag1") := shift(.SD), .SDcols = c(y_name, X_name), by = cross_section] 
  
  # create diff variables
  data[, paste0(c(y_name, X_name), "_diff1") := .SD - shift(.SD), .SDcols = c(y_name, X_name), by = cross_section] 
  
  ## (2) delete rows with missing values. Missing are introduced because of lag/diff.
  data <- na.omit(data)
  
  ## (3) add gaussian copulas
  if(add_copulas == T) { data[, paste0(X_name, "_copula") := lapply(.SD, make_copula), by = cross_section, .SDcols = X_name] }
  
  ## (4) reshape the data to (TO DO: document)
  # stacked X data.table
  formula <- paste0(cross_section, " + ", time, " + ", paste0(y_name, "_diff1"), " ~ ", cross_section)
  # create a vector with (i) all lagged and diffs X's and (ii) lag y
  if(add_copulas == T) {
  vec_all_Xs <- c(CJ(X_name, c("_lag1", "_diff1", "_copula"))[, paste0(X_name, c("_lag1", "_diff1", "_copula"))], paste0(y_name, "_lag1")) 
  } else {
    vec_all_Xs <- c(CJ(X_name, c("_lag1", "_diff1"))[, paste0(X_name, c("_lag1", "_diff1"))], paste0(y_name, "_lag1")) 
  }
  DT_input_SUR <- dcast(data, formula, value.var = vec_all_Xs, fill = 0)
  
  # add cross_section dummies
  inds <- unique(unlist(DT_input_SUR[, ..cross_section]))
  DT_input_SUR[, paste0(cross_section, "_", inds) := lapply(inds, function(x) as.numeric(get(cross_section) == x))]
  

  ## (5) create the objects: (i) index (ii) y and (iii) X. These are needed as input for the function itersur.
  # (i)
  index <- cbind(DT_input_SUR[, ..time], DT_input_SUR[, ..cross_section]) # dit kan ook beter..
  # (ii)
  y <- as.matrix(DT_input_SUR[, get(paste0(y_name, "_diff1"))])
  # (iii)
  # take (i) all lagged and diffs X's and (ii) lag y, and (iii) cross-section specific dummies
  X_collapsed_for_grep <- paste0(paste0("^", c(X_name, paste0(y_name, "_lag1"), paste0(cross_section, "_", inds, "$"))), collapse = "|") #this is not super robust..
  X <- as.matrix(DT_input_SUR[, .SD, .SDcols = grep(X_collapsed_for_grep, names(DT_input_SUR))]) 
  
  ## (6) call itersur
  if (praise_winsten_correction == F) {
    mod <- itersur(X = X, Y = y, index = index)
  } else {
    mod <- itersur(X = X, Y = y, index = index, method = "FGLS-Praise-Winsten")
  }

  
  ## (7) compute LT effect 
  # take the coeffs from the object returned by itersur
  DT_coeffs <- setDT(mod@coefficients)
  # drop the z-scores column (redundant)
  DT_coeffs[, z := NULL]
  # change the variable name to a character (was a factor)
  DT_coeffs[, variable := as.character(variable)]
  # create variable_id combo column. The column variable is actually a variable_id combo name.
  DT_coeffs[, variable_id := variable]
  # extract variable # everything before the last underscore
  DT_coeffs[, variable := sub("_[^_]+$", "", variable_id)]
  # extract the brand # everything after the last underscore
  DT_coeffs[, (cross_section) := gsub('.*_ ?(\\w+)', '\\1', variable_id)]
  
  
  # create a new DT with only the information about the lagged X
  DT_LT_effect <- DT_coeffs[variable %chin% paste0(X_name, "_lag1")]
  # rename cols
  setnames(DT_LT_effect, c("coef", "se"), c("coef_iv_lag1", "se_iv_lag1"))
  
  # create a new DT with speed of adjustment (i) coef & (ii) SE
  DT_speed_of_adj_temp <- DT_coeffs[variable %like% paste0(y_name, "_lag1")]
  # remove redudant columns
  DT_speed_of_adj_temp[, c("variable", "variable_id" ) := NULL]
  # rename
  setnames(DT_speed_of_adj_temp, c("coef", "se"), c("coef_speed_of_adj", "se_speed_of_adj"))
  
  # merge information lagged X & information speed of adjustment 
  DT_LT_effect <- DT_LT_effect[DT_speed_of_adj_temp, on = cross_section]
  
  # take the variance covariance matrix of the coeffs from the object returned by itersur 
  mat_var_covar <- mod@varcovar
  # give the rows and columns of the matrix a name
  colnames(mat_var_covar) <- DT_coeffs[, variable_id]
  rownames(mat_var_covar) <- DT_coeffs[, variable_id]

  
  # goal: add relevant information from the mat_var_covar to DT_coeffs. Loop over cross sections.
  for(id_counter in unique(DT_LT_effect$id)){
    # select row with lagged y for the cross section "id_counter"
    row_to_select <- paste0(y_name, "_lag1_", id_counter)
    # select columnS with lagged X for the cross section "id_counter"
    cols_to_select <- DT_LT_effect[id == id_counter, variable_id]
    # extra info from the matrix by subsetting on a row and columnS and store in a vector
    vec_var_cov <- mat_var_covar[row_to_select, cols_to_select]
    # merge with the DT_LT_effect
    DT_LT_effect[id == id_counter, cov_speed_of_adj_and_indep_var := vec_var_cov]
  }
  
  
  # 
  # delete "log_lag1" in the variable name
  DT_LT_effect[, variable := gsub('.{0,9}$', '', variable)]
  # compute the LT effect (= point estimate)
  DT_LT_effect[, effect := coef_iv_lag1 /-coef_speed_of_adj]
  # compute SE -- analytical approach
  DT_LT_effect[, se := sqrt( (1/(-coef_speed_of_adj)^2*se_iv_lag1^2) + (coef_iv_lag1^2/(-coef_speed_of_adj)^4*se_speed_of_adj^2) - (2*coef_iv_lag1/(-coef_speed_of_adj)^3*(-cov_speed_of_adj_and_indep_var)) )]
  # compute t_stat
  DT_LT_effect[, t_statistic := effect/se]
  # compute p_value
  #DT_LT_effect[, p_value := 2 * stats::pt(-abs(t_statistic), df = stats::df.residual(mod))] # df = n - # of estimated coefficients
  # keep only rows about LT effect
  DT_LT_effect <- cbind(DT_LT_effect[, .(variable, effect, se, t_statistic)], DT_LT_effect[, ..cross_section]) #maybe add p_value
  

                   
                   
                   
                   
                   
                   
                   
  ## estimate with OLS to verify the results
  #data[, id := as.factor(id)]
  #formula <- paste0(paste0(y_name, "_diff1"), " ~ ", " 0 + id + ", paste0(X_name, "_diff1:id", collapse = " + "), " + ", paste0(y_name, "_lag1:id")," + ", paste0(X_name, "_lag1:id", collapse = " + "))
  #mod_lm <- stats::lm(formula, data = data)
  #summary(mod_lm)
  
  
  
  ## (x) return (TO DO: document) 
  return(DT_LT_effect) 

}


mod <- estimate_error_correction_SUR(data = DT_sales_and_prices, y_name = "sales_log", X_name = c("own_price_log", "comp_price1_log", "comp_price2_log"), time = "week", cross_section = "id", add_copulas = T, praise_winsten_correction = T)



# to do:
# - vergelijk resultaten met systemfit
# - hoe werkt het als we echt data gebruiken
# -- singular variables 
# -- ander soort te weinig variatie in de dataset









