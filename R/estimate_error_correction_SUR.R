# only send relevant data in the function <- because we omit rows with NA



# Copula transformation function (see Park and Gupta 2012), based on a variable's ECDF
make_copula <- function(x) {
  if (length(unique(x)) %in% c(0,1)) return(as.numeric(rep(NA, length(x))))
  return(ifelse(ecdf(x)(x)==1, qnorm(1-.0000001), qnorm(ecdf(x)(x))))
}


# function to keep only columns with variation. Input need to be a numeric vector. Output is T if variation, F otherwise.
keep_cols_with_variation <- function(vec_ts){
  # keep only the data regarding 1 cross-section. If the data only has zero's retunr false
  cross_section_vec_ts <- vec_ts[vec_ts != 0]
  # if empty vec -- no variation -- return False
  if(length(cross_section_vec_ts) < 1) {return(F)}
  # return True if there is variation in the.
  return(sd(cross_section_vec_ts)>0)
}

# function to detect linear dependent columns. Input is a DT. Output is numeric vector with linear depdent columns -- exclude these columns.
fun_linear_dependence <- function(DT) {
  # covert to matrix
  mat <- as.matrix(DT)
  # do rank reduction to detect lin. dep. columns
  rank_rec <- sapply(1:ncol(mat), function(col) qr(mat[ , -col])$rank)
  # if there is linear dependence: give column numbers
  if (diff(range(rank_rec)) != 0) {
    num <- which(rank_rec == max(rank_rec))
    return(num)
  }
  # if there is no lin. dep. columns: return NULL
}

# main function
estimate_error_correction_SUR <- function(data, y_name, X_name, X_exo_name, time_FE = c(), time, cross_section, add_copulas = T, praise_winsten_correction = F, maxiter = 1000) {
  
  ## (1) transform the variables
  # create lag variables
  data[, paste0(c(y_name, X_name), "_lag1") := shift(.SD), .SDcols = c(y_name, X_name), by = cross_section] 
  
  # create diff variables
  data[, paste0(c(y_name, X_name), "_diff1") := .SD - shift(.SD), .SDcols = c(y_name, X_name), by = cross_section] 
  
  ## (2) delete rows with missing values. Missing are introduced because of lag/diff.
  data <- na.omit(data)
  
  ## (3) add cross-section specific gaussian copulas
  if(add_copulas == T) { 
    data[, paste0(X_name, "_copula") := lapply(.SD, make_copula), by = cross_section, .SDcols = X_name]
    # for some cross-sections no variation for a variable -- only NAs introduced -- set these to 0 - these will be remove later when checking when there is variation in the data
    cols <- data[, names(.SD), .SDcols = grepl("_copula", colnames(data))]
    data[, (cols) := lapply(.SD, nafill, fill=0), .SDcols = cols]
  }
  
  ## (4) reshape the data to (TO DO: document)
  # stacked X data.table
  if(length(time_FE) > 0) { #if time_FE added
    formula <- paste0(cross_section, " + ", time, " + ", time_FE, " + ", paste0(y_name, "_diff1"), " ~ ", cross_section) 
  } else {
    formula <- paste0(cross_section, " + ", time, " + ", paste0(y_name, "_diff1"), " ~ ", cross_section)
  }
  
  # create a vector with (i) all lagged and diffs X's, (ii) copulas of X, (iii) lag y, and (iiii) exogenous X
  if(add_copulas == T) {
    vec_all_Xs <- c(CJ(X_name, c("_lag1", "_diff1", "_copula"))[, paste0(X_name, c("_lag1", "_diff1", "_copula"))], paste0(y_name, "_lag1"), X_exo_name) 
  } else { # with the copulas
    vec_all_Xs <- c(CJ(X_name, c("_lag1", "_diff1"))[, paste0(X_name, c("_lag1", "_diff1"))], paste0(y_name, "_lag1"), X_exo_name) 
  }
  DT_input_SUR <- dcast(data, formula, value.var = vec_all_Xs, fill = 0, sep = ".")
  
  
  
  ## (5) (a) remove columns with no variation and (b) that are linearly dependent (=singular)
  # (a)
  # send all X variables <- they all have a "." in their column name
  vec_keep_cols <- unlist(DT_input_SUR[, lapply(.SD, keep_cols_with_variation), .SDcols = grep("\\.", names(DT_input_SUR))])
  # keep names in vector that are true
  keep_cols <- names(vec_keep_cols[vec_keep_cols == T])
  # keep only columns with variation and bind with column that do not have a "."
  DT_input_SUR <- cbind(DT_input_SUR[, .SD, .SDcols = !grep("\\.", names(DT_input_SUR))], DT_input_SUR[, .SD, .SDcols = grep("\\.", names(DT_input_SUR))][, ..vec_keep_cols])
  
  
  # (b)
  # do rank reduction to detect lin. dep. columns
  # call function that returns a vector of numbers corresponding to columns to delete
  col_nums_to_remove <- fun_linear_dependence(DT_input_SUR[, .SD, .SDcols = grep("\\.", names(DT_input_SUR))]) 
  # if there are linear dependent columns: remove these columns
  if (!is.null(col_nums_to_remove)) {  DT_input_SUR <- cbind(DT_input_SUR[, .SD, .SDcols = !grep("\\.", names(DT_input_SUR))], DT_input_SUR[, .SD, .SDcols = grep("\\.", names(DT_input_SUR))][, !..col_nums_to_remove]) }
  
  
  ## (6) add (i) cross_section dummies and (ii) cross_section specific time dummies
  # add cross_section dummies
  inds <- unique(unlist(DT_input_SUR[, ..cross_section]))
  DT_input_SUR[, paste0(cross_section, ".", inds) := lapply(inds, function(x) as.numeric(get(cross_section) == x))]
  
  # add time fixed effects
  if(length(time_FE) > 0) {
    ## set the first time period to NA
    # get first time period
    first_period <- DT_input_SUR[min(get(time_FE)), get(time_FE)]
    # create time_id combo FE, without using the first time period
    DT_input_SUR[get(time_FE) != first_period, time_id_FE := .GRP, by = c(time_FE, cross_section)]
    
    ## create the dummies
    # vector with unique time_id_FE
    inds_time_id <- DT_input_SUR[!is.na(time_id_FE), unique(time_id_FE)]
    # create time_id_FE dummies in the DT
    DT_input_SUR[, paste0(time_FE, ".", inds_time_id) := lapply(inds_time_id, function(x) as.numeric(time_id_FE == x))]   
    # replace NA's with 0's
    cols <- DT_input_SUR[, names(.SD), .SDcols = is.numeric]
    DT_input_SUR[ , (cols) := lapply(.SD, nafill, fill=0), .SDcols = cols]
    
  }
  
  
  ## (7) create the objects: (i) index (ii) y and (iii) X. These are needed as input for the function itersur.
  # (i)
  index <- cbind(DT_input_SUR[, ..time], DT_input_SUR[, ..cross_section]) # dit kan ook beter..
  # (ii)
  y <- as.matrix(DT_input_SUR[, get(paste0(y_name, "_diff1"))])
  # (iii)
  # take (i) all lagged and diffs X's and (ii) lag y, and (iii) cross-section specific dummies
  if(length(time_FE) > 0) {
    X_collapsed_for_grep <- paste0(paste0("^", c(X_name, paste0(y_name, "_lag1"), X_exo_name, paste0(cross_section, ".", inds, "$"), paste0(time_FE, ".", inds_time_id, "$"))), collapse = "|")
  } else {
    X_collapsed_for_grep <- paste0(paste0("^", c(X_name, paste0(y_name, "_lag1"), X_exo_name, paste0(cross_section, ".", inds, "$"))), collapse = "|") 
  }
  X <- as.matrix(DT_input_SUR[, .SD, .SDcols = grep(X_collapsed_for_grep, names(DT_input_SUR))]) 
  
  
  ## (8) We test for the presence of endogeneity per marketing mix instrument 
  if(add_copulas == T) { 
    for(x_name in X_name) {
      
      # colnames copula focal x
      col_nums_copulas_focal_x <- grep(paste0("^", x_name, "_copula."), colnames(X))
      
      # colnames copula all x
      col_nums_copulas_all_X <- grep("_copula.", colnames(X))
      
      # colnames_to_remove in this iteraction (i.e., non focal copulas)
      col_nums_to_remove_in_this_iteration <- setdiff(col_nums_copulas_all_X, col_nums_copulas_focal_x)
      
      # estimate SUR by testing only for the presence of endogeneity per marketing mix instrument 
      try (
        if (praise_winsten_correction == F) {
          mod <- itersur(X = X[,-col_nums_to_remove_in_this_iteration], Y = y, index = index, maxiter = maxiter)
        } else {
          mod <- itersur(X = X[,-col_nums_to_remove_in_this_iteration], Y = y, index = index, maxiter = maxiter, method = "FGLS-Praise-Winsten")
        }
      )
      
      # if the model is not estimable due to the Guassian copulas, remove the copulas
      if(inherits(mod, "try-error")) {
        # 
        col_names_to_remove <- colnames(X[, col_nums_copulas_focal_x])
      } else { # else get the insignificant copulas
        # get coeffs
        DT_coeffs_endo_test <- setDT(mod@coefficients)
        # take insignificant coeffs (at alpha = 0.1)
        col_names_to_remove <- DT_coeffs_endo_test[variable %like% "_copula." & abs(z) < 1.645, variable]
      }
      
      
      
      # remove copula columns that are not significant
      X <- X[, !colnames(X) %in% col_names_to_remove]
    }
  }
  
  ## (9) call itersur
  if (praise_winsten_correction == F) {
    mod <- itersur(X = X, Y = y, index = index, maxiter = maxiter)
  } else {
    mod <- itersur(X = X, Y = y, index = index, maxiter = maxiter, method = "FGLS-Praise-Winsten")
  }
  
  
  ## (10) compute LT effect 
  # take the coeffs from the object returned by itersur
  DT_coeffs <- setDT(mod@coefficients)
  # drop the z-scores column (redundant)
  DT_coeffs[, z := NULL]
  # change the variable name to a character (was a factor)
  DT_coeffs[, variable := as.character(variable)]
  # create variable_id combo column. The column variable is actually a variable_id combo name.
  DT_coeffs[, variable_id := variable]
  # extract variable # everything before the last underscore
  DT_coeffs[, variable := gsub("\\..*", "", variable_id)]
  # extract the brand # everything after the last underscore
  DT_coeffs[, (cross_section) := gsub(".*\\.", "", variable_id)]
  
  
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
  DT_LT_effect <- DT_LT_effect[DT_speed_of_adj_temp, on = cross_section, nomatch=0]
  
  # take the variance covariance matrix of the coeffs from the object returned by itersur 
  mat_var_covar <- mod@varcovar
  # give the rows and columns of the matrix a name
  colnames(mat_var_covar) <- DT_coeffs[, variable_id]
  rownames(mat_var_covar) <- DT_coeffs[, variable_id]
  
  
  # goal: add relevant information from the mat_var_covar to DT_coeffs. Loop over cross sections.
  for(id_counter in unlist(unique(DT_LT_effect[, ..cross_section]))){
    # select row with lagged y for the cross section "id_counter"
    row_to_select <- paste0(y_name, "_lag1.", id_counter)
    # select columnS with lagged X for the cross section "id_counter"
    cols_to_select <- DT_LT_effect[get(cross_section) == id_counter, variable_id]
    # extra info from the matrix by subsetting on a row and columnS and store in a vector
    vec_var_cov <- mat_var_covar[row_to_select, cols_to_select]
    # merge with the DT_LT_effect
    DT_LT_effect[get(cross_section) == id_counter, cov_speed_of_adj_and_indep_var := vec_var_cov]
  }
  
  
  # 
  # delete "lag1" in the variable name
  DT_LT_effect[, variable := gsub('.{0,5}$', '', variable)]
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
  
  
  
  
  ## (x) return LT effect
  return(DT_LT_effect) 
  
}



### Hieronder probeer ik de functie
# create a trend variable # do not call it week because week is already used as an index # same for time_FE
DT_sales_and_prices[, trend := week]
# create  time fixed effects
DT_sales_and_prices[week <= 50, period := 1]
DT_sales_and_prices[week > 50 & week <= 100, period := 2]
DT_sales_and_prices[week > 100 & week <= 150, period := 3]
DT_sales_and_prices[week > 150 & week <= 200, period := 4]
DT_sales_and_prices[week > 200 & week <= 250, period := 5]




## TEST TEMP ---
#data = DT_sales_and_prices
#y_name = "sales_log"
#X_name = c("own_price_log", "comp_price1_log", "comp_price2_log")
#X_exo_name = c("trend")
#time_FE = "period"
#time = "week"
#cross_section = "id"
#add_copulas = T
#praise_winsten_correction = T

#####

# call the function
mod <- estimate_error_correction_SUR(data = DT_sales_and_prices, y_name = "sales_log", X_name = c("own_price_log", "comp_price1_log", "comp_price2_log"), X_exo_name = c("trend"), time_FE = "period", time = "week", cross_section = "id", add_copulas = T, praise_winsten_correction = T)















