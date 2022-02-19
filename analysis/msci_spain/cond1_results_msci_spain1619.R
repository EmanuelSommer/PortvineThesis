### Single conditional msci spain risk estimates for the case studies 2016-2019


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
future::plan(list(future::tweak(future::multicore, workers = 6),
                 future::tweak(future::multicore, workers = 8)))


# for now fix the lengths of the marginal window to 50, of the vine window to 25
# and of the vine training window to 250

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
   train_size = 250,
   refit_size = 25,
   family_set = "parametric",
   vine_type = "dvine"
 ),
 alpha = alpha_values,
 risk_measures = risk_measure_values,
 n_samples = 100000,
 n_mc_samples = 10000,
 cond_vars = "sp500",
 cond_u = seq(0.1, 0.9, 0.1)
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
    train_size = 250,
    refit_size = 25,
    family_set = "parametric",
    vine_type = "dvine"
  ),
  alpha = alpha_values,
  risk_measures = risk_measure_values,
  n_samples = 100000,
  n_mc_samples = 10000,
  cond_vars = "eurostoxx50",
  cond_u = seq(0.1, 0.9, 0.1)
)


future::plan("sequential")

save(
  cond_risk_roll_16_19_eurostoxx50,
  file = "msci_spain_cond1_1619euro.RData"
)







