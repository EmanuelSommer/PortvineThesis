### Single conditional msci spain risk estimates for the case studies 2016-2019


# load the data
load("msci_spain_data_clean.RData")

# required packages
library(portvine)
library(future)

# hyperparameters
alpha_values <- c(0.01, 0.025, 0.05, 0.25, 0.5, 0.75, 0.95)
cond_u_values <- c(0.05, 0.1, 0.25, 0.5)
risk_measure_values <- c("VaR", "ES_mean")
# for the weights use the market cap from 29.10.2021. Changing weights would
# be possible on the vine level but here for simplicity one will stick to
# constant weights
weights_values <- c(
  iberdrola = 60.48,
  banco_santander = 56.82,
  bbv_argentaria = 40.42,
  inditex = 34.08,
  cellnex_telecom = 27.09,
  amadeus_it_group = 26.06,
  telefonica = 19.37,
  repsol_ypf = 16.04,
  ferrovial = 12.99
)


# choose the parallel strategy
future::plan(list(future::tweak(future::multicore, workers = 6),
                 future::tweak(future::multicore, workers = 8)))


# for now fix the lengths of the marginal window to 50, of the vine window to 50
# and of the vine training window to 500 as suggested by the unconditional
# results

## 2016-2019 data set

# conditioning on the sp500
cond_risk_roll_16_19_sp500 <- estimate_risk_roll(
 data = msci_spain_16_19[, 3:12],
 weights = c(weights_values, "sp500" = 0),
 marginal_settings = marginal_settings(
   train_size = 750,
   refit_size = 50
 ),
 vine_settings = vine_settings(
   train_size = 500,
   refit_size = 50,
   family_set = "parametric",
   vine_type = "dvine"
 ),
 alpha = alpha_values,
 risk_measures = risk_measure_values,
 n_samples = 100000,
 n_mc_samples = 10000,
 cond_vars = "sp500",
 cond_u = cond_u_values
)
cond_risk_roll_16_19_sp500@time_taken

future::plan("sequential")

save(
 cond_risk_roll_16_19_sp500,
 file = "msci_spain_cond1_1619sp500.RData"
)

# choose the parallel strategy
future::plan(list(future::tweak(future::multicore, workers = 6),
                  future::tweak(future::multicore, workers = 8)))

# conditioning on the eurostoxx50 index
cond_risk_roll_16_19_eurostoxx50 <- estimate_risk_roll(
  data = msci_spain_16_19[, c(3:11, 13)],
  weights = c(weights_values, "eurostoxx50" = 0),
  marginal_settings = marginal_settings(
    train_size = 750,
    refit_size = 50
  ),
  vine_settings = vine_settings(
    train_size = 500,
    refit_size = 50,
    family_set = "parametric",
    vine_type = "dvine"
  ),
  alpha = alpha_values,
  risk_measures = risk_measure_values,
  n_samples = 100000,
  n_mc_samples = 10000,
  cond_vars = "eurostoxx50",
  cond_u = cond_u_values
)


future::plan("sequential")

save(
  cond_risk_roll_16_19_eurostoxx50,
  file = "msci_spain_cond1_1619euro.RData"
)


### Now the prior residual strategy to get a sense of how this one performs

# conditioning on the sp500
cond_risk_roll_16_19_sp50_prior_resid <- estimate_risk_roll(
  data = msci_spain_16_19[, 3:12],
  weights = c(weights_values, "sp500" = 0),
  marginal_settings = marginal_settings(
    train_size = 750,
    refit_size = 50
  ),
  vine_settings = vine_settings(
    train_size = 500,
    refit_size = 50,
    family_set = "parametric",
    vine_type = "dvine"
  ),
  alpha = alpha_values,
  risk_measures = risk_measure_values,
  n_samples = 100000,
  n_mc_samples = 10000,
  cond_vars = "sp500",
  cond_u = cond_u_values,
  prior_resid_strategy = TRUE
)
cond_risk_roll_16_19_sp500@time_taken

future::plan("sequential")

save(
  cond_risk_roll_16_19_sp500_prior_resid,
  file = "msci_spain_cond1_1619sp500_prior_resid.RData"
)

# choose the parallel strategy
future::plan(list(future::tweak(future::multicore, workers = 6),
                  future::tweak(future::multicore, workers = 8)))

# conditioning on the eurostoxx50 index
cond_risk_roll_16_19_eurostoxx50_prior_resid <- estimate_risk_roll(
  data = msci_spain_16_19[, c(3:11, 13)],
  weights = c(weights_values, "eurostoxx50" = 0),
  marginal_settings = marginal_settings(
    train_size = 750,
    refit_size = 50
  ),
  vine_settings = vine_settings(
    train_size = 500,
    refit_size = 50,
    family_set = "parametric",
    vine_type = "dvine"
  ),
  alpha = alpha_values,
  risk_measures = risk_measure_values,
  n_samples = 100000,
  n_mc_samples = 10000,
  cond_vars = "eurostoxx50",
  cond_u = cond_u_values,
  prior_resid_strategy = TRUE
)


future::plan("sequential")

save(
  cond_risk_roll_16_19_eurostoxx50_prior_resid,
  file = "msci_spain_cond1_1619euro_prior_resid.RData"
)




