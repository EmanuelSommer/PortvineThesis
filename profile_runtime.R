# profile runtime

library(portvine)
library(profvis)

# load the data
load(here::here("data", "msci_spain_data_clean.RData"))

reduced_train_data <- msci_spain_complete_data[1:1000, 3:6]
dim(reduced_train_data)

# unconditional
missl_marginal_settings <- marginal_settings(
  train_size = 800,
  refit_size = 100
)

missl_vine_settings <- vine_settings(
  train_size = 200,
  refit_size = 50,
  family_set = "parametric",
  vine_type = "rvine"
)

profvis({
  missl_risk_roll_seq <- estimate_risk_roll(
    reduced_train_data,
    weights = NULL,
    marginal_settings = missl_marginal_settings,
    vine_settings = missl_vine_settings,
    alpha = c(0.01, 0.05, 0.1),
    risk_measures = c("VaR", "ES_mean", "ES_median", "ES_mc"),
    n_samples = 1000,
    n_mc_samples = 1000,
    trace = TRUE
  )
})

# conditional 1 var

missl_cond_vine_settings <- vine_settings(
  train_size = 200,
  refit_size = 100,
  family_set = "parametric",
  vine_type = "dvine"
)

profvis({
  missl_risk_roll_seq_c1 <- estimate_risk_roll(
    reduced_train_data,
    weights = NULL,
    marginal_settings = missl_marginal_settings,
    vine_settings = missl_cond_vine_settings,
    alpha = c(0.01, 0.05, 0.1),
    risk_measures = c("VaR", "ES_mean", "ES_median", "ES_mc"),
    n_samples = 1000,
    n_mc_samples = 1000,
    cond_vars = "inditex",
    cond_alpha = c(0.1, 0.5),
    trace = TRUE
  )
})


# cond 2 vars
profvis({
  missl_risk_roll_seq_c2 <- estimate_risk_roll(
    reduced_train_data,
    weights = NULL,
    marginal_settings = missl_marginal_settings,
    vine_settings = missl_cond_vine_settings,
    alpha = c(0.01, 0.05, 0.1),
    risk_measures = c("VaR", "ES_mean", "ES_median", "ES_mc"),
    n_samples = 1000,
    n_mc_samples = 1000,
    cond_vars = c("inditex", "iberdrola"),
    cond_alpha = c(0.1, 0.5),
    trace = TRUE
  )
})


# internal conditional sampling function

profvis({
  int_test <- portvine:::r1conddvine(
    100, c(0.05, 0.5),
    fitted_vine = fitted_vines(missl_risk_roll_seq_c1)[[1]]
  )
})

profvis({
  int_test <- portvine:::r2conddvine(
    100, c(0.05, 0.5),
    fitted_vine = fitted_vines(missl_risk_roll_seq_c2)[[1]]
  )
})
