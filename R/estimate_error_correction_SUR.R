# to do: maakt 1 functie en schat een SUR EC model.. + reshape

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
  # create a vector with all lagged and diffs X's and lag y
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
  X_collapsed_for_grep <- paste0(paste0("^", c(X_name, paste0(y_name, "_lag1"))), collapse = "|") #this is not super robust..
  X <- as.matrix(DT_input_SUR[, .SD, .SDcols = grep(X_collapsed_for_grep, names(DT_input_SUR))]) 
  
  ## (5) call itersur
  mod <- itersur(X = X, Y = y, index = index)  #method="FGLS-Praise-Winsten"
  
  formula <- paste0(paste0(y, "_diff1"), " ~ ", paste0(X, "_diff1", collapse = " + "), " + ", paste0(y, "_lag1")," + ", paste0(X, "_lag1", collapse = " + "))

  # estimate model with OLS
  mod <- stats::lm(formula, data = data)
  
  
  
  ## (x) return (TO DO: document) 
  return(mod) 

}


mod <- function_temp_name(data = DT_sales_and_prices, y_name = "sales_log", X_name = c("own_price_log", "comp_price1_log", "comp_price2_log"), time = "week", cross_section = "id")



# waarom komen er vreemde resultaten uit?? Merk 1 heeft steeds een rare coeffs.
# vergelijk met lm
# vergelijk met systemfit



# to do:
# - create a better function name
# - add copulas










