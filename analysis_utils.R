###############################################################################
# Utility functions for the Backtesting of stock portfolios with the portvine
# package (Mainly visualizations)
###############################################################################

# -----------------------------------------------------------------------------
# NOTE: these functions are not intended for the general usage they do not cover
# input checks, unit tests and are documented quite minimal. If you would like
# to use these functions do this with care. Some of the visualization functions
# can be found in a cleaner setup in the Get Started vignette of the portvine
# package
# -----------------------------------------------------------------------------

# Extract for each asset the residuals of a certain marginal roll and visualize
# Returns named list with visualization for each asset
marg_resid_viz_list <- function(roll, asset_names = NULL, marg_window = 1,
                                squared = FALSE) {
  fitted_marginals <- fitted_marginals(roll)
  if (is.null(asset_names)) asset_names <- fitted_vines(roll)[[1]]$names
  sapply(
    asset_names,
    function(asset_name) {
      # use again a utility function from the portvine package
      model_resid <- roll_residuals(
        fitted_marginals[[asset_name]], 1
      )
      if (squared) model_resid <- model_resid^2

      simple_exploratory <- tibble(resid = model_resid) %>%
        rowid_to_column(var = "id") %>%
        ggplot(aes(x = id, y = resid)) +
        geom_line(size = 0.2) +
        labs(x = "t", y = ifelse(squared, expression(z[t]^2),expression(z[t])),
             title = str_to_title(str_replace_all(asset_name, "_", " "))) +
        theme(axis.text.x = element_blank(),
              axis.ticks.x = element_blank())

      acf_plot <- tibble(
        acf = as.numeric(acf(model_resid, type = "cor", lag.max = 20,
                             plot = FALSE)$acf),
        lag = 0:20
      ) %>%
        filter(lag != 0 & lag <= 10) %>%
        ggplot() +
        geom_hline(yintercept = 0, col = "black", size = 0.3) +
        geom_hline(yintercept = qnorm(c(0.025, 0.975)) /
                     sqrt(length(model_resid)),
                   linetype = "longdash", col = custom_colors[1], size = 0.5) +
        geom_segment(aes(x = lag, xend = lag, y = 0, yend = acf)) +
        geom_point(aes(x = lag, y = acf)) +
        scale_x_continuous(breaks = seq(1, 10, 1)) +
        ylim(-1, 1) +
        labs(x = "h", y = "ACF(h)")

      ljungplot <- tibble(
        pval = sapply(
          1:10,
          function(i) Box.test(model_resid, lag = i,
                               type = "Lju")$p.value),
        lag = 1:10) %>%
        ggplot() +
        geom_hline(yintercept = 0, col = "black", size = 0.3) +
        geom_hline(yintercept = 0.05,
                   linetype = "longdash", col = custom_colors[1], size = 0.5) +
        geom_line(aes(x = lag, y = pval)) +
        geom_point(aes(x = lag, y = pval)) +
        scale_x_continuous(breaks = seq(1, 10, 1)) +
        labs(x = "h", y = "p-value of Ljung-Box test at lag h")

      (simple_exploratory / (ljungplot + acf_plot)) +
        plot_layout(nrow = 2)
    }, USE.NAMES = TRUE, simplify = FALSE)
}

# creates a heatmap of Ljung Box test pvalues for a portvine_roll object
ljung_heatmap <- function(roll, roll_num = 1) {
  asset_names <- fitted_vines(roll)[[1]]$names
  roll_marginals <- fitted_marginals(roll)
  ljung_data <- sapply(asset_names, function(asset_name) {
    # use again a utility function from the portvine package
    model_resid <- roll_residuals(
      roll_marginals[[asset_name]], roll_num = roll_num
    )
    sapply(
      1:10,
      function(i) Box.test(model_resid, lag = i, type = "Lju")$p.value
    )
  }, USE.NAMES = TRUE, simplify = TRUE)
  ljung_data %>%
    as_tibble() %>%
    rowid_to_column("lag") %>%
    pivot_longer(-lag, names_to = "asset", values_to = "pval") %>%
    ggplot(aes(x = lag, y = asset, fill = pval)) +
    geom_tile() +
    scale_y_discrete(labels = ~ str_to_title(str_replace_all(.x, "_", " "))) +
    scale_x_continuous(breaks = 1:10) +
    labs(y = "", x = "h", fill = "p-value",
         title = "Results of the Ljung-Box tests",
         caption = paste("Rolling window:", roll_num)) +
    scale_fill_gradientn(colours = c(custom_colors[2],"#C37285" ,
                                     custom_colors[1], "#2a82db"),
                         values = scales::rescale(c(0, 0.05 - 0.01,
                                                    0.05, 1)),
                         breaks = c(0.05),
                         labels = c(0.05),
                         guide = guide_colourbar(nbin = 1000)) +
    theme(legend.position = "right",
          panel.grid.minor.x = element_blank())
}



# animated version of ljung_heatmap that animates over all marginal windows
library(gganimate)
ljung_heatmap_animation <- function(roll) {
  asset_names <- fitted_vines(roll)[[1]]$names
  roll_marginals <- fitted_marginals(roll)
  n_marginal_rolls <- roll_marginals[[1]]@model$n.refits
  anim <- map_df(seq(n_marginal_rolls), function(roll_num) {
    ljung_data <- sapply(asset_names, function(asset_name) {
      # use again a utility function from the portvine package
      model_resid <- roll_residuals(
        roll_marginals[[asset_name]], roll_num = roll_num
      )
      sapply(
        1:10,
        function(i) Box.test(model_resid, lag = i, type = "Lju")$p.value
      )
    }, USE.NAMES = TRUE, simplify = TRUE)
    ljung_data %>%
      as_tibble() %>%
      rowid_to_column("lag") %>%
      pivot_longer(-lag, names_to = "asset", values_to = "pval") %>%
      mutate(roll = roll_num)
  }) %>%
    ggplot(aes(x = lag, y = asset, fill = pval, group = roll)) +
    geom_tile() +
    transition_states(roll,
                      transition_length = 1,
                      state_length = 4) +
    scale_y_discrete(labels = ~ str_to_title(str_replace_all(.x, "_", " "))) +
    scale_x_continuous(breaks = 1:10) +
    labs(y = "", x = "h", fill = "p-value",
         title = "Results of the Ljung-Box tests",
         caption = "Rolling window: {closest_state}") +
    scale_fill_gradientn(colours = c(custom_colors[2],"#C37285" ,
                                     custom_colors[1], "#2a82db"),
                         values = scales::rescale(c(0, 0.05 - 0.01,
                                                    0.05, 1)),
                         breaks = c(0.05),
                         labels = c(0.05),
                         guide = guide_colourbar(nbin = 1000)) +
    theme(legend.position = "right",
          panel.grid.minor.x = element_blank())

  animate(
    anim +
      enter_fade() +
      exit_shrink(),
    width = 600,
    height = 300,
    nframes = 200
  )
}


# utility function for labeled vinecop tree plots
labeled_vinecop_plot <- function(vine, tree = 1) {
  rvinecopulib:::plot.vinecop(vine, tree = tree,
                              var_names = "use",
                              edge_labels = "family_tau")
}
# utility for table of used bivariate copula families
bicops_used <- function(vine) {
  table(unlist(vine$pair_copulas)[names(unlist(vine$pair_copulas)) == "family"])
}

# utility to get the copula famlies with their kendalls tau for the copulas
# that are directly associated with the conditional asset i.e. the rightmost
# bicop of of every tree level.
index_copulas <- function(vine) {
  n_trees <- length(vine$pair_copulas)
  res <- data.frame("Tree" = seq(n_trees),
                    "Copula_familiy" = "",
                    "Kendalls_tau" = 0)
  for (i in seq(n_trees)) {
    res[i, 2] <- vine$pair_copulas[[i]][[1]]$family
    res[i, 3] <- rvinecopulib::par_to_ktau(vine$pair_copulas[[i]][[1]])
  }
  res
}




# -----------------------------------------------------------------------------
# backtesting utilities
# -----------------------------------------------------------------------------

# traditional backtests for a portvine_roll object
get_traditional_backtests_uncond <- function(roll, alphas) {
  res <- tribble(
    ~"Risk measure", ~"Backtest",
    "VaR", "unconditional coverage",
    "VaR", "conditional coverage",
    "ES", "exceedance residuals (two sided)",
    "ES", "exceedance residuals (one sided)",
    "ES", "simple conditional calibration (two sided)",
    "ES", "simple conditional super-calibration (one sided)",
    "ES", "strict ESR test (two sided)",
    "ES", "one sided intercept ESR test",
  )
  for (alpha in alphas) {
    res_vec <- numeric(8)
    ### VaR
    temp <- rugarch::VaRTest(
      alpha = alpha,
      actual = risk_estimates(roll, "VaR", alpha) %>%
        pull(realized),
      VaR = risk_estimates(roll, "VaR", alpha) %>%
        pull(risk_est)
    )[c("uc.LRp", "cc.LRp")]
    res_vec[1] <- temp[[1]]
    res_vec[2] <- temp[[2]]
    ### ES
    # exceedance residuals
    temp <- esback::er_backtest(
      r = risk_estimates(roll, "VaR", alpha) %>%
        pull(realized),
      q = risk_estimates(roll, "VaR", alpha) %>%
        pull(risk_est),
      e = risk_estimates(roll, "ES_mean", alpha) %>%
        pull(risk_est)
    )
    res_vec[3] <- temp$pvalue_twosided_simple
    res_vec[4] <- temp$pvalue_onesided_simple
    # conditional calibration
    temp <- esback::cc_backtest(
      r = risk_estimates(roll, "VaR", alpha) %>%
        pull(realized),
      q = risk_estimates(roll, "VaR", alpha) %>%
        pull(risk_est),
      e = risk_estimates(roll, "ES_mean", alpha) %>%
        pull(risk_est),
      alpha = alpha
    )
    res_vec[5] <- temp$pvalue_twosided_simple
    res_vec[6] <- temp$pvalue_onesided_simple
    # ESR tests
    temp <- try(
      esback::esr_backtest(
        r = risk_estimates(roll, "VaR", alpha) %>%
          pull(realized),
        q = risk_estimates(roll, "VaR", alpha) %>%
          pull(risk_est),
        e = risk_estimates(roll, "ES_mean", alpha) %>%
          pull(risk_est),
        alpha = alpha,
        version = 1
      ), silent = TRUE)
    if ("try-error" %in% class(temp)) {
      res_vec[7] <- -1
    } else {
      res_vec[7] <- temp$pvalue_twosided_asymptotic
    }

    temp <- try(
      esback::esr_backtest(
        r = risk_estimates(roll, "VaR", alpha) %>%
          pull(realized),
        q = risk_estimates(roll, "VaR", alpha) %>%
          pull(risk_est),
        e = risk_estimates(roll, "ES_mean", alpha) %>%
          pull(risk_est),
        alpha = alpha,
        version = 3
      ), silent = TRUE)
    if ("try-error" %in% class(temp)) {
      res_vec[8] <- -1
    } else {
      res_vec[8] <- temp$pvalue_onesided_asymptotic
    }
    res <- res %>%
      bind_cols("temp" = res_vec) %>%
      rename(!!paste("alpha:", alpha) := temp)
  }
  res
}
# the corresponding version dealing with cond_portvine_roll objects
get_traditional_backtests_cond <- function(roll, alphas, cond_u) {
  res <- tribble(
    ~"Risk measure", ~"Backtest",
    "VaR", "unconditional coverage",
    "VaR", "conditional coverage",
    "ES", "exceedance residuals (two sided)",
    "ES", "exceedance residuals (one sided)",
    "ES", "simple conditional calibration (two sided)",
    "ES", "simple conditional super-calibration (one sided)",
    "ES", "strict ESR test (two sided)",
    "ES", "one sided intercept ESR test",
  )
  for (alpha in alphas) {
    res_vec <- numeric(8)
    ### VaR
    temp <- rugarch::VaRTest(
      alpha = alpha,
      actual = risk_estimates(roll, "VaR", alpha, cond_u = cond_u) %>%
        pull(realized),
      VaR = risk_estimates(roll, "VaR", alpha, cond_u = cond_u) %>%
        pull(risk_est)
    )[c("uc.LRp", "cc.LRp")]
    res_vec[1] <- temp[[1]]
    res_vec[2] <- temp[[2]]
    ### ES
    # exceedance residuals
    temp <- esback::er_backtest(
      r = risk_estimates(roll, "VaR", alpha, cond_u = cond_u) %>%
        pull(realized),
      q = risk_estimates(roll, "VaR", alpha, cond_u = cond_u) %>%
        pull(risk_est),
      e = risk_estimates(roll, "ES_mean", alpha, cond_u = cond_u) %>%
        pull(risk_est)
    )
    res_vec[3] <- temp$pvalue_twosided_simple
    res_vec[4] <- temp$pvalue_onesided_simple
    # conditional calibration
    temp <- esback::cc_backtest(
      r = risk_estimates(roll, "VaR", alpha, cond_u = cond_u) %>%
        pull(realized),
      q = risk_estimates(roll, "VaR", alpha, cond_u = cond_u) %>%
        pull(risk_est),
      e = risk_estimates(roll, "ES_mean", alpha, cond_u = cond_u) %>%
        pull(risk_est),
      alpha = alpha
    )
    res_vec[5] <- temp$pvalue_twosided_simple
    res_vec[6] <- temp$pvalue_onesided_simple
    # ESR tests
    temp <- try(
      esback::esr_backtest(
        r = risk_estimates(roll, "VaR", alpha, cond_u = cond_u) %>%
          pull(realized),
        q = risk_estimates(roll, "VaR", alpha, cond_u = cond_u) %>%
          pull(risk_est),
        e = risk_estimates(roll, "ES_mean", alpha, cond_u = cond_u) %>%
          pull(risk_est),
        alpha = alpha,
        version = 1
      ), silent = TRUE)
    if ("try-error" %in% class(temp)) {
      res_vec[7] <- -1
    } else {
      res_vec[7] <- temp$pvalue_twosided_asymptotic
    }

    temp <- try(
      esback::esr_backtest(
        r = risk_estimates(roll, "VaR", alpha, cond_u = cond_u) %>%
          pull(realized),
        q = risk_estimates(roll, "VaR", alpha, cond_u = cond_u) %>%
          pull(risk_est),
        e = risk_estimates(roll, "ES_mean", alpha, cond_u = cond_u) %>%
          pull(risk_est),
        alpha = alpha,
        version = 3
      ), silent = TRUE)
    if ("try-error" %in% class(temp)) {
      res_vec[8] <- -1
    } else {
      res_vec[8] <- temp$pvalue_onesided_asymptotic
    }
    res <- res %>%
      bind_cols("temp" = res_vec) %>%
      rename(!!paste("alpha:", alpha) := temp)
  }
  res
}

### comparative backtesting as in Nolde and Ziegel 2017
# first up the scoring function for the ES
es_scoring <- function(realized, es, var, alpha) {
  if (any(es >= 0)) stop("ES must be always negative for this scoring function")
  ((realized < var) * (realized  - var) / es) +
    alpha * ((var / es) - 1 + log(-es))
}

# interpretation if confidence_result is smaller than the specified nu then the
# model corresponding to roll1 is considered at least as good as the roll2 model
es_comparative_backtest <- function(roll1, roll2, alpha) {
  realized1 <- risk_estimates(roll1, "ES_mean", alpha) %>%
    pull(realized)
  var1 <- risk_estimates(roll1, "VaR", alpha) %>%
    pull(risk_est)
  es1 <- risk_estimates(roll1, "ES_mean", alpha) %>%
    pull(risk_est)
  scoring1 <- es_scoring(
    realized = realized1,
    es = es1,
    var = var1,
    alpha = alpha
  )
  realized2 <- risk_estimates(roll2, "ES_mean", alpha) %>%
    pull(realized)
  var2 <- risk_estimates(roll2, "VaR", alpha) %>%
    pull(risk_est)
  es2 <- risk_estimates(roll2, "ES_mean", alpha) %>%
    pull(risk_est)
  scoring2 <- es_scoring(
    realized = realized2,
    es = es2,
    var = var2,
    alpha = alpha
  )
  n <- length(realized1)
  sigma <- sd(scoring1 - scoring2)
  test_statistic <- mean(scoring1 - scoring2) * sqrt(n) / sigma
  confidence_result <- pnorm(test_statistic)
  c(test_statistic = test_statistic, confidence_result = confidence_result)
}
# the same but roll1 is a cond_portvine_roll object
es_cond_comparative_backtest <- function(cond_roll1, roll2, alpha, cond_u) {
  realized1 <- risk_estimates(cond_roll1, "ES_mean", alpha, cond_u = cond_u) %>%
    pull(realized)
  var1 <- risk_estimates(cond_roll1, "VaR", alpha, cond_u = cond_u) %>%
    pull(risk_est)
  es1 <- risk_estimates(cond_roll1, "ES_mean", alpha, cond_u = cond_u) %>%
    pull(risk_est)
  scoring1 <- es_scoring(
    realized = realized1,
    es = es1,
    var = var1,
    alpha = alpha
  )
  realized2 <- risk_estimates(roll2, "ES_mean", alpha) %>%
    pull(realized)
  var2 <- risk_estimates(roll2, "VaR", alpha) %>%
    pull(risk_est)
  es2 <- risk_estimates(roll2, "ES_mean", alpha) %>%
    pull(risk_est)
  scoring2 <- es_scoring(
    realized = realized2,
    es = es2,
    var = var2,
    alpha = alpha
  )
  n <- length(realized1)
  sigma <- sd(scoring1 - scoring2)
  test_statistic <- mean(scoring1 - scoring2) * sqrt(n) / sigma
  confidence_result <- pnorm(test_statistic)
  c(test_statistic = test_statistic, confidence_result = confidence_result)
}

# heatmap that displays at which quantile level the conditional risk measures
# would pass the traditional backtests. This can be used to infer knowledge
# about the influence of the certain conditional asset on the portfolio level
# risk measures
cond_backtest_heatmap <- function(roll, alpha, cond_u) {
  plot_df <- map_df(cond_u, function(u) {
    get_traditional_backtests_cond(
      roll,
      alphas = alpha,
      cond_u = u
    ) %>%
      mutate(cond_u = u)
  }) %>%
    mutate(test = paste0(`Risk measure`, ": ", Backtest)) %>%
    select(starts_with("alpha"), cond_u, test)
  colnames(plot_df)[1] <- "pval"

  plot_df <- plot_df %>%
    mutate(pval = if_else(is.nan(pval) | pval == -1, NA_real_, pval),
           test = fct_rev(fct_inorder(factor(test))))

  if (all(plot_df$pval >= 0.05 | is.na(plot_df$pval))) {
    legend_scale <- scale_fill_gradient(
      low = "#92B8DE", high = "#2a82db"
    )
  } else {
    legend_scale <- scale_fill_gradientn(
      colours = c("#db4f59","#C37285" ,
                  "#92B8DE", "#2a82db"),
      values = scales::rescale(c(0, 0.05 - 0.01, 0.05, 1)),
      breaks = c(0.05),
      labels = c(0.05),
      guide = guide_colourbar(nbin = 1000))
  }
  plot_df %>%
    ggplot(aes(x = factor(cond_u), y = test, fill = pval)) +
    geom_tile() +
    legend_scale +
    labs(y = "", x = "Quantile level", fill = "P-value",
         title = "Traditional backtests on the conditional risk measures",
         subtitle = paste("Alpha level:", alpha))
}



#' Extract a filtered ugarch model from a uGARCHroll object
#'
#' The [`rugarch::ugarchroll`] class object encompasses fitting information
#' about a number of
#' models fitted in a rolling window fashion. This utility function gives an
#' easy interface to extract the filtered ugarch model for a specified roll.
#'
#' @param ugarchroll Object of class [`rugarch::ugarchroll`].
#' @param roll_num Count that specifies the fitted model to extract the
#' residuals from.
#'
#' @return filtered ugarch model
#' @export
roll_filtered_model <- function(ugarchroll, roll_num = 1) {
  checkmate::assert_class(ugarchroll, classes = "uGARCHroll")
  total_roll_num <- ugarchroll@model$n.refits
  checkmate::assert_integerish(roll_num,
                               lower = 0, upper = total_roll_num,
                               len = 1
  )

  train_end_index <- ugarchroll@model$n.start
  refit_size <- ugarchroll@model$refit.every
  distribution <- ugarchroll@model$spec@model$modeldesc$distribution
  coefs <- ugarchroll@model$coef[[roll_num]]$coef[, 1]
  spec <- rugarch::ugarchspec(
    distribution.model = distribution,
    fixed.pars = coefs,
    variance.model = list(
      model = ugarchroll@model$spec@model$modeldesc$vmodel,
      garchOrder = c(
        marginals$iberdrola@model$spec@model$modelinc[["alpha"]],
        marginals$iberdrola@model$spec@model$modelinc[["beta"]]
      ),
      submodel = NULL,
      external.regressors = NULL,
      variance.targeting = FALSE
    ),
    mean.model = list(
      armaOrder = c(
        marginals$iberdrola@model$spec@model$modelinc[["ar"]],
        marginals$iberdrola@model$spec@model$modelinc[["ma"]]
      ),
      include.mean = marginals$iberdrola@model$spec@model$modelinc[["mu"]] == 1,
      archm = FALSE,
      archpow = 1,
      arfima = FALSE,
      external.regressors = NULL,
      archex = FALSE
    )
  )
  filtered_model <- rugarch::ugarchfilter(
    spec = spec,
    data = ugarchroll@model$data[seq(
      1 + refit_size * (roll_num - 1),
      min(
        refit_size * (roll_num - 1) +
          train_end_index,
        length(ugarchroll@model$data)
      )
    )]
  )
 filtered_model
}
