###############################################################################
# Utility functions for the Backtesting of stock portfolios with the portvine
# package (Mainly visualizations)
###############################################################################



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
                   linetype = "longdash", col = yes_no_cols[1], size = 0.5) +
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
                   linetype = "longdash", col = yes_no_cols[1], size = 0.5) +
        geom_line(aes(x = lag, y = pval)) +
        geom_point(aes(x = lag, y = pval)) +
        scale_x_continuous(breaks = seq(1, 10, 1)) +
        labs(x = "h", y = "p-value of Ljung-Box test at lag h")

      (simple_exploratory / (ljungplot + acf_plot)) +
        plot_layout(nrow = 2)
    }, USE.NAMES = TRUE, simplify = FALSE)
}

# creates a heatmap of Ljung Box test pvalues for a portvineroll object
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
    scale_fill_gradientn(colours = c(yes_no_cols[2],"#C37285" ,
                                     yes_no_cols[1], "#2a82db"),
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
    scale_fill_gradientn(colours = c(yes_no_cols[2],"#C37285" ,
                                     yes_no_cols[1], "#2a82db"),
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
labeled_vinecop_plot <- function(vine) {
  node_labels <- vine$names[vine$structure$order]
  ff <- tempfile()
  png(filename = ff)
  plot <- rvinecopulib:::plot.vinecop(vine)
  dev.off()
  unlink(ff)
  plot$data$name <- str_to_title(str_replace_all(node_labels, "_", " "))
  plot
  # ggraph::geom_node_label(
  #   aes(label = str_to_title(str_replace_all(node_labels, "_", " ")))
  # )
}
# utility for table of used bivariate copula families
bicops_used <- function(vine) {
  table(unlist(vine$pair_copulas)[names(unlist(vine$pair_copulas)) == "family"])
}
