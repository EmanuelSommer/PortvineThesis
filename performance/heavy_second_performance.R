### Test the performance with the 1-25 strategy

# Forecasting window 250
# Marginal window 50 -> 5
# vine window 25 -> 10

# Risk measures: VaR, ES_mean
# alpha: 0.01, 0.025

# Data set size for the performance measurements: 1000
# load data set
load("msci_spain_data_clean.RData")

data_10vars <- msci_spain_complete_data[1:1000, 2:11]

library(portvine)
library(future)

## unconditional
uncond_marg <- marginal_settings(
  train_size = 750,
  refit_size = 50
)
uncond_vine <- vine_settings(
  train_size = 200,
  refit_size = 25,
  family_set = "parametric",
  vine_type = "rvine"
)

## conditional

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


results_df <- data.frame(
  first_level_parallel = 1,
  second_level_parallel = 1,
  vars = 10,
  family = "parametric",
  n_samples = 1,
  time_minutes = 0,
  cond = NA
)


future::plan(list(future::tweak(future::multicore, workers = 1),
                  future::tweak(future::multicore, workers = 25)))
# unconditional
temp <- estimate_risk_roll(
  data = data_10vars,
  weights = NULL,
  marginal_settings = uncond_marg,
  vine_settings = uncond_vine,
  alpha = c(0.01, 0.025),
  risk_measures = c("VaR", "ES_mean"),
  n_samples = 1000
)
results_df <- rbind(results_df,
                    c(1, 25, 10, "parametric", 1000, temp@time_taken, "no"))
temp@time_taken
temp <- estimate_risk_roll(
  data = data_10vars,
  weights = NULL,
  marginal_settings = uncond_marg,
  vine_settings = uncond_vine,
  alpha = c(0.01, 0.025),
  risk_measures = c("VaR", "ES_mean"),
  n_samples = 10000
)
results_df <- rbind(results_df,
                    c(1, 25, 10, "parametric", 10000, temp@time_taken, "no"))
temp@time_taken
temp <- estimate_risk_roll(
  data = data_10vars,
  weights = NULL,
  marginal_settings = uncond_marg,
  vine_settings = uncond_vine,
  alpha = c(0.01, 0.025),
  risk_measures = c("VaR", "ES_mean"),
  n_samples = 100000
)
results_df <- rbind(results_df,
                    c(1, 25, 10, "parametric", 100000, temp@time_taken, "no"))
temp@time_taken
# conditional
temp <- estimate_risk_roll(
  data = data_10vars,
  weights = NULL,
  marginal_settings = cond_marg,
  vine_settings = cond_vine,
  alpha = c(0.01, 0.025),
  risk_measures = c("VaR", "ES_mean"),
  n_samples = 1000,
  cond_vars = "telefonica",
  cond_u = c(0.05, 0.5)
)
results_df <- rbind(results_df,
                    c(1, 25, 10, "parametric", 1000, temp@time_taken, "yes"))
temp@time_taken
temp <- estimate_risk_roll(
  data = data_10vars,
  weights = NULL,
  marginal_settings = cond_marg,
  vine_settings = cond_vine,
  alpha = c(0.01, 0.025),
  risk_measures = c("VaR", "ES_mean"),
  n_samples = 10000,
  cond_vars = "telefonica",
  cond_u = c(0.05, 0.5)
)
results_df <- rbind(results_df,
                    c(1, 25, 10, "parametric", 10000, temp@time_taken, "yes"))
temp@time_taken
temp <- estimate_risk_roll(
  data = data_10vars,
  weights = NULL,
  marginal_settings = cond_marg,
  vine_settings = cond_vine,
  alpha = c(0.01, 0.025),
  risk_measures = c("VaR", "ES_mean"),
  n_samples = 100000,
  cond_vars = "telefonica",
  cond_u = c(0.05, 0.5)
)
results_df <- rbind(results_df,
                    c(1, 25, 10, "parametric", 100000, temp@time_taken, "yes"))

future::plan("sequential")

results_df

save(results_df,
     file = "performance_heavy_second.RData")

Sys.info()
R.version
