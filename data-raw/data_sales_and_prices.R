set.seed(1)

n_obs <- 1000                                         # Specify sample size
mu <- c(10, 10, 10)                                   # Specify the means of the variables

# own price = 10
# comp price 1 = 10
# comp price 2 = 10

sigma <- matrix(c(1, 0.1, 0.2, 0.1, 1, 0.3, 0.2, 0.3, 1),  ncol = 3)  # Specify the covariance matrix of the variables

# competitive reaction = 01, 0.2, 0.3 = collinearity

# create dataset
DT_sales_and_prices <- as.data.table(MASS::mvrnorm(n = n_obs, mu = mu, Sigma = sigma))  # Random sample from bivariate normal distribution
setnames(DT_sales_and_prices, c("own_price", "comp_price1", "comp_price2"))


# take the log
DT_sales_and_prices[, `:=` (own_price_log = log(own_price), comp_price1_log = log(comp_price1), comp_price2_log = log(comp_price2))]

# add random error
DT_sales_and_prices[, error := MASS::mvrnorm(n = n_obs, mu = 1, Sigma = 0.2)]

## add  sales (=y) #and an AR(1) term
# first time period
DT_sales_and_prices[1, sales_log := 100 + -2.5*own_price_log + 0.5*comp_price1_log + 0.8*comp_price2_log +  error]
for (i in 2:nrow(DT_sales_and_prices)) {
  DT_sales_and_prices[i, sales_log := 100 + -2.5*own_price_log + 0.5*comp_price1_log + 0.8*comp_price2_log + DT_sales_and_prices[i-1, sales_log*0.5] + error]
}

# add 4 brands 
DT_sales_and_prices[1:250, id := 1]
DT_sales_and_prices[251:500, id := 2]
DT_sales_and_prices[501:750, id := 3]
DT_sales_and_prices[751:1000, id := 4]

# add weeks
DT_sales_and_prices[, week := 1:.N, by = id]

# keep only some columns
DT_sales_and_prices <- DT_sales_and_prices[, .(sales_log, own_price_log, comp_price1_log, comp_price2_log, id)]

usethis::use_data(DT_sales_and_prices, overwrite = T)
