### Unconditional msci spain risk estimates for the case studies


# load the data
load("msci_spain_data_clean.RData")

# required packages
library(portvine)
library(future)

# hyperparameters
alpha_values <- c(0.01, 0.025, 0.05, 0.95)
cond_u_values <- seq(0.1, 0.9, 0.1)
risk_measure_values <- c("VaR", "ES_mean", "ES_mc", "ES_median")
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
future::plan(list(future::tweak(future::multicore, workers = 12),
                  future::tweak(future::multicore, workers = 13)))
## 2016-2019 data set
uncond_risk_roll_16_19_g50_k25_p500 <- estimate_risk_roll(
  data = msci_spain_16_19[, 3:11],
  weights = weights_values,
  marginal_settings = marginal_settings(
    train_size = 750,
    refit_size = 50
  ),
  vine_settings = vine_settings(
    train_size = 500,
    refit_size = 25,
    family_set = "parametric"
  ),
  alpha = alpha_values,
  risk_measures = risk_measure_values,
  n_samples = 100000,
  n_mc_samples = 10000
)

uncond_risk_roll_16_19_g50_k50_p500 <- estimate_risk_roll(
  data = msci_spain_16_19[, 3:11],
  weights = weights_values,
  marginal_settings = marginal_settings(
    train_size = 750,
    refit_size = 50
  ),
  vine_settings = vine_settings(
    train_size = 500,
    refit_size = 50,
    family_set = "parametric"
  ),
  alpha = alpha_values,
  risk_measures = risk_measure_values,
  n_samples = 100000,
  n_mc_samples = 10000
)

uncond_risk_roll_16_19_g50_k25_p250 <- estimate_risk_roll(
  data = msci_spain_16_19[, 3:11],
  weights = weights_values,
  marginal_settings = marginal_settings(
    train_size = 750,
    refit_size = 50
  ),
  vine_settings = vine_settings(
    train_size = 250,
    refit_size = 25,
    family_set = "parametric"
  ),
  alpha = alpha_values,
  risk_measures = risk_measure_values,
  n_samples = 100000,
  n_mc_samples = 10000
)

uncond_risk_roll_16_19_g50_k50_p250 <- estimate_risk_roll(
  data = msci_spain_16_19[, 3:11],
  weights = weights_values,
  marginal_settings = marginal_settings(
    train_size = 750,
    refit_size = 50
  ),
  vine_settings = vine_settings(
    train_size = 250,
    refit_size = 50,
    family_set = "parametric"
  ),
  alpha = alpha_values,
  risk_measures = risk_measure_values,
  n_samples = 100000,
  n_mc_samples = 10000
)

## 2020-21 data set
uncond_risk_roll_20_21_g50_k25_p100 <- estimate_risk_roll(
  data = msci_spain_20_21[, 3:11],
  weights = weights_values,
  marginal_settings = marginal_settings(
    train_size = 300,
    refit_size = 50
  ),
  vine_settings = vine_settings(
    train_size = 100,
    refit_size = 25,
    family_set = "parametric"
  ),
  alpha = alpha_values,
  risk_measures = risk_measure_values,
  n_samples = 100000,
  n_mc_samples = 10000
)

uncond_risk_roll_20_21_g50_k50_p100 <- estimate_risk_roll(
  data = msci_spain_20_21[, 3:11],
  weights = weights_values,
  marginal_settings = marginal_settings(
    train_size = 300,
    refit_size = 50
  ),
  vine_settings = vine_settings(
    train_size = 100,
    refit_size = 50,
    family_set = "parametric"
  ),
  alpha = alpha_values,
  risk_measures = risk_measure_values,
  n_samples = 100000,
  n_mc_samples = 10000
)


uncond_risk_roll_20_21_g50_k25_p200 <- estimate_risk_roll(
  data = msci_spain_20_21[, 3:11],
  weights = weights_values,
  marginal_settings = marginal_settings(
    train_size = 300,
    refit_size = 50
  ),
  vine_settings = vine_settings(
    train_size = 200,
    refit_size = 25,
    family_set = "parametric"
  ),
  alpha = alpha_values,
  risk_measures = risk_measure_values,
  n_samples = 100000,
  n_mc_samples = 10000
)

uncond_risk_roll_20_21_g50_k50_p200 <- estimate_risk_roll(
  data = msci_spain_20_21[, 3:11],
  weights = weights_values,
  marginal_settings = marginal_settings(
    train_size = 300,
    refit_size = 50
  ),
  vine_settings = vine_settings(
    train_size = 200,
    refit_size = 50,
    family_set = "parametric"
  ),
  alpha = alpha_values,
  risk_measures = risk_measure_values,
  n_samples = 100000,
  n_mc_samples = 10000
)


future::plan("sequential")

save(
  uncond_risk_roll_16_19_g50_k25_p250,
  uncond_risk_roll_16_19_g50_k50_p250,
  uncond_risk_roll_16_19_g50_k25_p500,
  uncond_risk_roll_16_19_g50_k50_p500,
  uncond_risk_roll_20_21_g50_k25_p100,
  uncond_risk_roll_20_21_g50_k50_p100,
  uncond_risk_roll_20_21_g50_k25_p200,
  uncond_risk_roll_20_21_g50_k50_p200,
  file = "msci_spain_uncond_res.RData"
)
