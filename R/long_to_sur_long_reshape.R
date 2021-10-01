

long_to_sur_long_reshape <- function(y, X, data, time, cross_section) {
  # stacked X data.table
  formula <- paste0(cross_section, " + ", time, " + ", y, " ~ ", cross_section)
  DT_input_SUR <- dcast(data, formula, value.var = X, fill = 0)

  # add dummies
  inds <- unique(unlist(DT_input_SUR[, ..cross_section]))
  DT_input_SUR[, paste0(cross_section, "_", inds) := lapply(inds, function(x) as.numeric(get(cross_section) == x))]
  
  # create the objects: (i) index (ii) y and (iii) X. These are needed as input for the function itersur.
  # (i)
  index <- cbind(DT_input_SUR[, ..time], DT_input_SUR[, ..cross_section]) # dit kan ook beter..
  # (ii)
  y <- as.matrix(DT_input_SUR[, ..y])
  # (iii)
  X_collapsed_for_grep <- paste0(paste0("^", X), collapse = "|") #this is not super robust..
  X <- as.matrix(DT_input_SUR[, .SD, .SDcols = grep(X_collapsed_for_grep, names(DT_input_SUR))]) 
  

  return(list(index, y, X)) #index, y, X
  
}


list_of_index_y_X <- long_to_sur_long_reshape(y = "sales_log", X = c("own_price_log", "comp_price1_log", "comp_price2_log"), data = DT_sales_and_prices, time = "week", cross_section = "id")
index <- list_of_index_y_X[[1]]
y <- list_of_index_y_X[[2]]
X <- list_of_index_y_X[[3]]




