### conditional performance measures PART III

# Forecasting window 250
# Marginal window 50 -> 5
# vine window 25 -> 10

# Risk measures: VaR, ES_mean
# alpha: 0.01, 0.025
# cond_u level: 0.05, 0.5

# Data set size for the performance measurements: 1000
# load data set

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

future::plan(list(future::tweak(future::multicore, workers = 10),
                  future::tweak(future::multicore, workers = 4)))
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
                    c(10, 4, 10, "parametric", 1000, temp@time_taken))
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
                    c(10, 4, 10, "parametric", 10000, temp@time_taken))
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
                    c(10, 4, 10, "parametric", 100000, temp@time_taken))
# more cond_u values 5, 10
temp <- estimate_risk_roll(
  data = data_10vars,
  weights = NULL,
  marginal_settings = cond_marg,
  vine_settings = cond_vine,
  alpha = c(0.01, 0.025),
  risk_measures = c("VaR", "ES_mean"),
  n_samples = 10000,
  cond_vars = "telefonica",
  cond_u = seq(0.05, 0.95, 0.2)
)
cond_u_5_10000 <- temp@time_taken
temp <- estimate_risk_roll(
  data = data_10vars,
  weights = NULL,
  marginal_settings = cond_marg,
  vine_settings = cond_vine,
  alpha = c(0.01, 0.025),
  risk_measures = c("VaR", "ES_mean"),
  n_samples = 10000,
  cond_vars = "telefonica",
  cond_u = seq(0.05, 0.95, 0.1)
)
cond_u_10_10000 <- temp@time_taken
future::plan("sequential")

future::plan(list(future::tweak(future::multicore, workers = 4),
                  future::tweak(future::multicore, workers = 6)))
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
                    c(4, 6, 10, "parametric", 1000, temp@time_taken))
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
                    c(4, 6, 10, "parametric", 10000, temp@time_taken))
future::plan("sequential")

results_df

save(results_df, cond_u_5_10000, cond_u_10_10000,
     file = "performance_cond3.RData")

Sys.info()
R.version
