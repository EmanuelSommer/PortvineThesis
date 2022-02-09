### conditional performance measures

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


uncond_marg <- marginal_settings(
  train_size = 750,
  refit_size = 50
)
uncond_vine <- vine_settings(
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

# just one layer of parallelization with 5 and 10 workers
future::plan(multicore, workers = 5)
temp <- estimate_risk_roll(
  data = data_10vars,
  weights = NULL,
  marginal_settings = uncond_marg,
  vine_settings = uncond_vine,
  alpha = c(0.01, 0.025),
  risk_measures = c("VaR", "ES_mean"),
  n_samples = 1000,
  cond_vars = "telefonica",
  cond_u = c(0.05, 0.5)
)
results_df <- rbind(results_df,
                    c(5, 1, 10, "parametric", 1000, temp@time_taken))

temp <- estimate_risk_roll(
  data = data_10vars,
  weights = NULL,
  marginal_settings = uncond_marg,
  vine_settings = uncond_vine,
  alpha = c(0.01, 0.025),
  risk_measures = c("VaR", "ES_mean"),
  n_samples = 10000,
  cond_vars = "telefonica",
  cond_u = c(0.05, 0.5)
)
results_df <- rbind(results_df,
                    c(5, 1, 10, "parametric", 10000, temp@time_taken))
future::plan("sequential")

future::plan(multicore, workers = 10)
temp <- estimate_risk_roll(
  data = data_10vars,
  weights = NULL,
  marginal_settings = uncond_marg,
  vine_settings = uncond_vine,
  alpha = c(0.01, 0.025),
  risk_measures = c("VaR", "ES_mean"),
  n_samples = 1000,
  cond_vars = "telefonica",
  cond_u = c(0.05, 0.5)
)
results_df <- rbind(results_df,
                    c(10, 1, 10, "parametric", 1000, temp@time_taken))

temp <- estimate_risk_roll(
  data = data_10vars,
  weights = NULL,
  marginal_settings = uncond_marg,
  vine_settings = uncond_vine,
  alpha = c(0.01, 0.025),
  risk_measures = c("VaR", "ES_mean"),
  n_samples = 10000,
  cond_vars = "telefonica",
  cond_u = c(0.05, 0.5)
)
results_df <- rbind(results_df,
                    c(10, 1, 10, "parametric", 10000, temp@time_taken))
future::plan("sequential")


# nested parallelization
future::plan(list(future::tweak(future::multicore, workers = 5),
                  future::tweak(future::multicore, workers = 4)))
temp <- estimate_risk_roll(
  data = data_10vars,
  weights = NULL,
  marginal_settings = uncond_marg,
  vine_settings = uncond_vine,
  alpha = c(0.01, 0.025),
  risk_measures = c("VaR", "ES_mean"),
  n_samples = 1000,
  cond_vars = "telefonica",
  cond_u = c(0.05, 0.5)
)
results_df <- rbind(results_df,
                    c(5, 4, 10, "parametric", 1000, temp@time_taken))
temp <- estimate_risk_roll(
  data = data_10vars,
  weights = NULL,
  marginal_settings = uncond_marg,
  vine_settings = uncond_vine,
  alpha = c(0.01, 0.025),
  risk_measures = c("VaR", "ES_mean"),
  n_samples = 10000,
  cond_vars = "telefonica",
  cond_u = c(0.05, 0.5)
)
results_df <- rbind(results_df,
                    c(5, 4, 10, "parametric", 10000, temp@time_taken))
future::plan("sequential")

future::plan(list(future::tweak(future::multicore, workers = 10),
                  future::tweak(future::multicore, workers = 2)))
temp <- estimate_risk_roll(
  data = data_10vars,
  weights = NULL,
  marginal_settings = uncond_marg,
  vine_settings = uncond_vine,
  alpha = c(0.01, 0.025),
  risk_measures = c("VaR", "ES_mean"),
  n_samples = 1000,
  cond_vars = "telefonica",
  cond_u = c(0.05, 0.5)
)
results_df <- rbind(results_df,
                    c(10, 2, 10, "parametric", 1000, temp@time_taken))
temp <- estimate_risk_roll(
  data = data_10vars,
  weights = NULL,
  marginal_settings = uncond_marg,
  vine_settings = uncond_vine,
  alpha = c(0.01, 0.025),
  risk_measures = c("VaR", "ES_mean"),
  n_samples = 10000,
  cond_vars = "telefonica",
  cond_u = c(0.05, 0.5)
)
results_df <- rbind(results_df,
                    c(10, 2, 10, "parametric", 10000, temp@time_taken))
temp <- estimate_risk_roll(
  data = data_10vars,
  weights = NULL,
  marginal_settings = uncond_marg,
  vine_settings = uncond_vine,
  alpha = c(0.01, 0.025),
  risk_measures = c("VaR", "ES_mean"),
  n_samples = 100000,
  cond_vars = "telefonica",
  cond_u = c(0.05, 0.5)
)
results_df <- rbind(results_df,
                    c(10, 2, 10, "parametric", 100000, temp@time_taken))
future::plan("sequential")


future::plan(list(future::tweak(future::multicore, workers = 10),
                  future::tweak(future::multicore, workers = 3)))
temp <- estimate_risk_roll(
  data = data_10vars,
  weights = NULL,
  marginal_settings = uncond_marg,
  vine_settings = uncond_vine,
  alpha = c(0.01, 0.025),
  risk_measures = c("VaR", "ES_mean"),
  n_samples = 1000,
  cond_vars = "telefonica",
  cond_u = c(0.05, 0.5)
)
results_df <- rbind(results_df,
                    c(10, 3, 10, "parametric", 1000, temp@time_taken))
temp <- estimate_risk_roll(
  data = data_10vars,
  weights = NULL,
  marginal_settings = uncond_marg,
  vine_settings = uncond_vine,
  alpha = c(0.01, 0.025),
  risk_measures = c("VaR", "ES_mean"),
  n_samples = 10000,
  cond_vars = "telefonica",
  cond_u = c(0.05, 0.5)
)
results_df <- rbind(results_df,
                    c(10, 3, 10, "parametric", 10000, temp@time_taken))
future::plan("sequential")


results_df

save(results_df,
     file = "performance_cond.RData")

Sys.info()
R.version
