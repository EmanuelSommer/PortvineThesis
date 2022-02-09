### conditional performance measures PART II

# Forecasting window 250
# Marginal window 50 -> 5
# vine window 25 -> 10

# Risk measures: VaR, ES_mean
# alpha: 0.01, 0.025
# cond_u level: 0.05, 0.5

# Data set size for the performance measurements: 1000
# load data set
load("msci_spain_data_clean.RData")

data_10vars <- msci_spain_complete_data[1:1000, 2:11]

library(portvine)
library(future)

cond_marg <- marginal_settings(
  train_size = 750,
  refit_size = 50
)
cond_vine <- vine_settings(
  train_size = 200,
  refit_size = 25,
  family_set = "parametric",
  vine_type = "dvine"
)

# estimate risk measures with different sample sizes (of the risk measure
# estimation) on different nested settings

results_df <- data.frame(
  first_level_parallel = 1,
  second_level_parallel = 1,
  vars = 10,
  family = "parametric",
  n_samples = 1,
  time_minutes = 0
)


## now different numbers of variables with fixed sample size of 10000
data_5vars <- data_10vars[, 1:5]
library(dplyr)
set.seed(2)
data_20vars <- data_10vars %>%
  rowwise() %>%
  mutate(across(everything(), ~rnorm(1, .x, sd = 1))) %>%
  bind_cols(data_10vars)
colnames(data_20vars) <- paste0("var", 1:20)
data_30vars <- data_20vars %>%
  rowwise() %>%
  mutate(across(everything(), ~rnorm(1, .x, sd = 0.5))) %>%
  bind_cols(data_10vars)
colnames(data_30vars) <- paste0("var", 1:30)

future::plan(list(future::tweak(future::multicore, workers = 10),
                  future::tweak(future::multicore, workers = 2)))
temp <- estimate_risk_roll(
  data = data_5vars,
  weights = NULL,
  marginal_settings = cond_marg,
  vine_settings = cond_vine,
  alpha = c(0.01, 0.025),
  risk_measures = c("VaR", "ES_mean"),
  n_samples = 10000,
  cond_vars = "iberdrola",
  cond_u = c(0.05, 0.5)
)
results_df <- rbind(results_df,
                    c(10, 2, 5, "parametric", 10000, temp@time_taken))

temp <- estimate_risk_roll(
  data = data_20vars,
  weights = NULL,
  marginal_settings = cond_marg,
  vine_settings = cond_vine,
  alpha = c(0.01, 0.025),
  risk_measures = c("VaR", "ES_mean"),
  n_samples = 10000,
  cond_vars = "var1",
  cond_u = c(0.05, 0.5)
)
results_df <- rbind(results_df,
                    c(10, 2, 20, "parametric", 10000, temp@time_taken))

temp <- estimate_risk_roll(
  data = data_30vars,
  weights = NULL,
  marginal_settings = cond_marg,
  vine_settings = cond_vine,
  alpha = c(0.01, 0.025),
  risk_measures = c("VaR", "ES_mean"),
  n_samples = 10000,
  cond_vars = "var1",
  cond_u = c(0.05, 0.5)
)
results_df <- rbind(results_df,
                    c(10, 2, 30, "parametric", 10000, temp@time_taken))
future::plan("sequential")

## now different copula families
future::plan(list(future::tweak(future::multicore, workers = 10),
                  future::tweak(future::multicore, workers = 2)))
temp <- estimate_risk_roll(
  data = data_10vars,
  weights = NULL,
  marginal_settings = cond_marg,
  vine_settings = vine_settings(200, 25, family_set = "gaussian",
                                vine_type = "dvine"),
  alpha = c(0.01, 0.025),
  risk_measures = c("VaR", "ES_mean"),
  n_samples = 10000,
  cond_vars = "telefonica",
  cond_u = c(0.05, 0.5)
)
results_df <- rbind(results_df,
                    c(10, 2, 10, "gaussian", 10000, temp@time_taken))

temp <- estimate_risk_roll(
  data = data_10vars,
  weights = NULL,
  marginal_settings = cond_marg,
  vine_settings = vine_settings(200, 25, family_set = "t",
                                vine_type = "dvine"),
  alpha = c(0.01, 0.025),
  risk_measures = c("VaR", "ES_mean"),
  n_samples = 10000,
  cond_vars = "telefonica",
  cond_u = c(0.05, 0.5)
)
results_df <- rbind(results_df,
                    c(10, 2, 10, "t", 10000, temp@time_taken))

temp <- estimate_risk_roll(
  data = data_10vars,
  weights = NULL,
  marginal_settings = cond_marg,
  vine_settings = vine_settings(200, 25, family_set = "onepar",
                                vine_type = "dvine"),
  alpha = c(0.01, 0.025),
  risk_measures = c("VaR", "ES_mean"),
  n_samples = 10000,
  cond_vars = "telefonica",
  cond_u = c(0.05, 0.5)
)
results_df <- rbind(results_df,
                    c(10, 2, 10, "onepar", 10000, temp@time_taken))
future::plan("sequential")

results_df

save(results_df,
     file = "performance_cond2.RData")

Sys.info()
R.version
