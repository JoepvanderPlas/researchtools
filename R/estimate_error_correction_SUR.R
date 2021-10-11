#  1 functie en die  een SUR EC model schat + reshape van de data

function_temp_name <- function(data, y_name, X_name,  time, cross_section) {
  
  ## (1) transform the variables
  # create lag variables
  data[, paste0(c(y_name, X_name), "_lag1") := shift(.SD), .SDcols = c(y_name, X_name), by = cross_section] 
  
  # create diff variables
  data[, paste0(c(y_name, X_name), "_diff1") := .SD - shift(.SD), .SDcols = c(y_name, X_name), by = cross_section] 
  
  ## (3) delete rows with missing values. Missing are introduced because of lag/diff.
  data <- na.omit(data)
  
  ## (3) reshape the data to (TO DO: document)
  # stacked X data.table
  formula <- paste0(cross_section, " + ", time, " + ", paste0(y_name, "_diff1"), " ~ ", cross_section)
  # create a vector with (i) all lagged and diffs X's and (ii) lag y
  vec_all_X_name_and_tranformation_combinations <- c(CJ(X_name, c("_lag1", "_diff1"))[, paste0(X_name, c("_lag1", "_diff1"))], paste0(y_name, "_lag1")) 
  DT_input_SUR <- dcast(data, formula, value.var = vec_all_X_name_and_tranformation_combinations, fill = 0)
  
  # add cross_section dummies
  inds <- unique(unlist(DT_input_SUR[, ..cross_section]))
  DT_input_SUR[, paste0(cross_section, "_", inds) := lapply(inds, function(x) as.numeric(get(cross_section) == x))]
  

  ## (4) create the objects: (i) index (ii) y and (iii) X. These are needed as input for the function itersur.
  # (i)
  index <- cbind(DT_input_SUR[, ..time], DT_input_SUR[, ..cross_section]) # dit kan ook beter..
  # (ii)
  y <- as.matrix(DT_input_SUR[, get(paste0(y_name, "_diff1"))])
  # (iii)
  # take (i) all lagged and diffs X's and (ii) lag y, and (iii) cross-section specific dummies
  X_collapsed_for_grep <- paste0(paste0("^", c(X_name, paste0(y_name, "_lag1"), paste0(cross_section, "_", inds, "$"))), collapse = "|") #this is not super robust..
  X <- as.matrix(DT_input_SUR[, .SD, .SDcols = grep(X_collapsed_for_grep, names(DT_input_SUR))]) 
  
  ## (5) call itersur
  mod <- itersur(X = X, Y = y, index = index)  #method="FGLS-Praise-Winsten"
  
  
  ## (6) take LT effect 
  # take the coeffs from the object returned by itersur
  DT_coeffs <- setDT(mod@coefficients)
  # change the variable name to a character (was a factor)
  DT_coeffs[, variable := as.character(variable)]
  # create variable_brand combo column. The column variable is actually a variable brand combo name.
  DT_coeffs[, variable_brand := variable]
  # extract variable # everything before the last underscore
  DT_coeffs[, variable := sub("_[^_]+$", "", variable_brand)]
  # extract the brand # everything after the last underscore
  DT_coeffs[, (cross_section) := gsub('.*_ ?(\\w+)', '\\1', variable_brand)]

  
  
  #mod@varcovar
  
  
  # DUS PAK DE lag X, voeg daar de lag y aan toe, BELANGRIJK: wat doe je van de var-cov matrix?
  
  
  
  # create a data table with all statistics required to compute the (i) LT effect and (ii) significance
  DT_LT_effect <- data.table(variable = DT_coeffs[variable %chin% paste0(X_name, "_lag1"), variable],
                             coef_indep_var = DT_coeffs[variable %chin% paste0(X_name, "_lag1"), coef],
                             SE_indep_var = DT_coeffs[variable %chin% paste0(X_name, "_lag1"), se],
                             coef_speed_of_adj = DT_coeffs[variable %like% paste0(y_name, "_lag1"), coef],
                             SE_speed_of_adj = DT_coeffs[variable == paste0(y_name, "_lag1"), se],
                             cov_speed_of_adj_and_indep_var = stats::vcov(mod)[paste0(y_name, "_lag1"), paste0(X_name, "_lag1")])
  
  
  ## estimate with OLS to verify the results
  #data[, id := as.factor(id)]
  #formula <- paste0(paste0(y_name, "_diff1"), " ~ ", " 0 + id + ", paste0(X_name, "_diff1:id", collapse = " + "), " + ", paste0(y_name, "_lag1:id")," + ", paste0(X_name, "_lag1:id", collapse = " + "))
  #mod_lm <- stats::lm(formula, data = data)
  #summary(mod_lm)
  
  
  
  ## (x) return (TO DO: document) 
  return(mod) 

}


mod <- function_temp_name(data = DT_sales_and_prices, y_name = "sales_log", X_name = c("own_price_log", "comp_price1_log", "comp_price2_log"), time = "week", cross_section = "id")





# to do:
# - create a better function name
# - add copulas
# - vergelijk resultaten met systemfit









