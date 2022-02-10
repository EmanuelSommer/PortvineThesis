### Visualize the performance of the package tested on the LRZ HPC linux cluster
library(tidyverse)
custom_colors <- c("#92B8DE", "#db4f59", "#477042", "#cc72d6")
theme_set(
  theme_minimal() +
    theme(plot.title = element_text(size = 11),
          plot.subtitle = element_text(size = 9))
)
# load the performance results:
load("data/performance_uncond.RData")
results_df
uncond_results_df <- results_df[-1, ] %>%
  mutate(family = as.character(family))
uncond_results_df[20:22, "family"] <- c("gaussian", "t", "onepar")
load("data/performance_heavy_second.RData")
uncond_results_df <- uncond_results_df %>%
  bind_rows(results_df[2:4, 1:6] %>%
              mutate(family = as.character(family)))
load("data/performance_max.RData")
uncond_results_df <- uncond_results_df %>%
  bind_rows(results_df[2:4, 1:6] %>%
              mutate(family = as.character(family)))

uncond_results_df <- uncond_results_df %>%
  mutate(parallel_strategy = paste0(first_level_parallel,
                                    "-",
                                    second_level_parallel),
         time_minutes = as.numeric(time_minutes))

# compare sample sizes
uncond_results_df %>%
  filter(family == "parametric", vars == 10) %>%
  mutate(n_samples = if_else(n_samples == "1e+05", "100000", n_samples)) %>%
  ggplot(aes(x = factor(parallel_strategy,
                        levels = c("5-1", "10-1", "4-6", "5-4", "1-25",
                                   "10-2", "10-3", "10-4", "10-25")),
             y = time_minutes,
             group = n_samples,
             col = n_samples, shape = n_samples)) +
  geom_point(size = 2) +
  geom_line(alpha = 0.4) +
  ylim(0, NA) +
  scale_color_manual(values = custom_colors[2:4], name = "Sample size") +
  labs(x = "Parallel stratgey: first-second level parallelization",
       y = "Runtime in minutes",
       shape = "Sample size",
       title = "Influence of the sample size on runtime",
       subtitle = "Number of variables: 10\nAllowed copula families: all parametric\nEstimation: unconditional")


# compare number of variables
uncond_results_df %>%
  mutate(vars = as.numeric(vars)) %>%
  filter(family == "parametric", n_samples == "10000",
         parallel_strategy == "10-2") %>%
  ggplot(aes(x = vars, y = time_minutes)) +
  geom_point(size = 2) +
  geom_line(alpha = 0.4) +
  scale_x_continuous(breaks = unique(as.numeric(uncond_results_df$vars))) +
  ylim(0, NA) +
  labs(y = "Runtime in minutes",
       x = "Number of variables",
       title = "Influence of the number of variables on runtime",
       subtitle = "Sample size: 10000\nAllowed copula families: all parametric\nEstimation: unconditional\nParallel strategy: 10-2"
       ) +
  theme(panel.grid.minor.x = element_blank())

# compare the use of different copula families
uncond_results_df %>%
  mutate(vars = as.numeric(vars),
         family = factor(family,
                         levels = c("gaussian", "t",
                                    "onepar", "parametric")),
         family = fct_recode(family, "Student t" = "t", "Gaussian" = "gaussian",
                             "Single parametric" = "onepar",
                             "All parametric" = "parametric")) %>%
  filter(n_samples == "10000", vars == 10,
         parallel_strategy == "10-2") %>%
  ggplot(aes(x = family,
             y = time_minutes,
             group = 1)) +
  geom_point(size = 2) +
  geom_segment(aes(y = 0, yend = time_minutes,
                   x = family,
                   xend = family),
               size = 0.5) +
  ylim(0, NA) +
  labs(y = "Runtime in minutes",
       x = "Allowed copula families",
       title = "Influence of the allowed copula families on runtime",
       subtitle = "Sample size: 10000\nNumber of variables: 10\nEstimation: unconditional\nParallel strategy: 10-2"
       )

#-------------------------------------------------------------------------
###  now the same for the conditional approach
# load the performance results:
load("data/performance_cond.RData")
cond_results_df <- results_df[-1, ]
load("data/performance_cond2.RData")
cond_results_df <- cond_results_df %>%
  bind_rows(results_df[-1, ]) %>%
  mutate(family = as.character(family))
cond_results_df[15:17, "family"] <- c("gaussian", "t", "onepar")
load("data/performance_cond3.RData")
cond_results_df <- cond_results_df %>%
  bind_rows(results_df[-1, ] %>% mutate(family = as.character(family)))
load("data/performance_heavy_second.RData")
cond_results_df <- cond_results_df %>%
  bind_rows(results_df[5:7, 1:6] %>%
              mutate(family = as.character(family)))
load("data/performance_max.RData")
cond_results_df <- cond_results_df %>%
  bind_rows(results_df[5:7, 1:6] %>%
              mutate(family = as.character(family)))

cond_results_df <- cond_results_df %>%
  mutate(parallel_strategy = paste0(first_level_parallel,
                                    "-",
                                    second_level_parallel),
         time_minutes = as.numeric(time_minutes))

# compare sample sizes
cond_results_df %>%
  filter(family == "parametric", vars == 10) %>%
  mutate(n_samples = if_else(n_samples == "1e+05", "100000", n_samples)) %>%
  ggplot(aes(x = factor(parallel_strategy,
                        levels = c("5-1", "10-1", "4-6", "5-4", "1-25",
                                   "10-2", "10-3", "10-4", "10-25")),
             y = time_minutes,
             group = n_samples,
             col = n_samples, shape = n_samples)) +
  geom_point(size = 2) +
  geom_line(alpha = 0.4) +
  ylim(0, NA) +
  scale_color_manual(values = custom_colors[2:4], name = "Sample size") +
  geom_segment(aes(x = 7.5, y = 80, xend = 8.9, yend = 35),
               arrow = arrow(length = unit(0.3, "cm")),
               col = "#6e6e6e") +
  annotate("text", x = 6.8, y = 90, size = 3.5, col = "#6e6e6e",
           label = "Only 1 estimated\nquantile level\ninstead of 2") +
  labs(x = "Parallel stratgey: first-second level parallelization",
       y = "Runtime in minutes",
       shape = "Sample size",
       title = "Influence of the sample size on runtime",
       subtitle = "Number of variables: 10\nAllowed copula families: all parametric\nEstimation: conditional")
# without the 100000
cond_results_df %>%
  filter(family == "parametric", vars == 10, n_samples != "1e+05") %>%
  mutate(n_samples = if_else(n_samples == "1e+05", "100000", n_samples)) %>%
  ggplot(aes(x = factor(parallel_strategy,
                        levels = c("5-1", "10-1", "4-6", "5-4", "1-25",
                                   "10-2", "10-3", "10-4", "10-25")),
             y = time_minutes,
             group = n_samples,
             col = n_samples, shape = n_samples)) +
  geom_point(size = 2) +
  geom_line(alpha = 0.4) +
  ylim(0, NA) +
  scale_color_manual(values = custom_colors[2:4], name = "Sample size") +
  labs(x = "Parallel stratgey: first-second level parallelization",
       y = "Runtime in minutes",
       shape = "Sample size",
       title = "Influence of the sample size on runtime",
       subtitle = "Number of variables: 10\nAllowed copula families: all parametric\nEstimation: conditional")


# compare number of variables
cond_results_df %>%
  mutate(vars = as.numeric(vars)) %>%
  filter(family == "parametric", n_samples == "10000",
         parallel_strategy == "10-2") %>%
  ggplot(aes(x = vars, y = time_minutes)) +
  geom_point(size = 2) +
  geom_line(alpha = 0.4) +
  scale_x_continuous(breaks = unique(as.numeric(cond_results_df$vars))) +
  ylim(0, NA) +
  labs(y = "Runtime in minutes",
       x = "Number of variables",
       title = "Influence of the number of variables on runtime",
       subtitle = "Sample size: 10000\nAllowed copula families: all parametric\nEstimation: conditional\nParallel strategy: 10-2"
  ) +
  theme(panel.grid.minor.x = element_blank())

# compare the use of different copula families
cond_results_df %>%
  mutate(vars = as.numeric(vars),
         family = factor(family,
                         levels = c("gaussian", "t",
                                    "onepar", "parametric")),
         family = fct_recode(family, "Student t" = "t", "Gaussian" = "gaussian",
                             "Single parametric" = "onepar",
                             "All parametric" = "parametric")) %>%
  filter(n_samples == "10000", vars == 10,
         parallel_strategy == "10-2") %>%
  ggplot(aes(x = family,
             y = time_minutes,
             group = 1)) +
  geom_point(size = 2) +
  geom_segment(aes(y = 0, yend = time_minutes,
                   x = family,
                   xend = family),
               size = 0.5) +
  ylim(0, NA) +
  labs(y = "Runtime in minutes",
       x = "Allowed copula families",
       title = "Influence of the allowed copula families on runtime",
       subtitle = "Sample size: 10000\nNumber of variables: 10\nEstimation: conditional\nParallel strategy: 10-2"
  )

tibble(x = c(2, 5, 10),
       y = c(cond_results_df$time_minutes[19],
             cond_u_5_10000, cond_u_10_10000)) %>%
  ggplot(aes(x = x, y = y)) +
  geom_point(size = 2) +
  geom_line(alpha = 0.4) +
  scale_x_continuous(breaks = c(2, 5, 10)) +
  ylim(0, NA) +
  labs(y = "Runtime in minutes",
       x = "Number of quantile levels estimated",
       title = "Influence of the number of estimated quantile levels on runtime",
       subtitle = "Sample size: 10000\nAllowed copula families: all parametric\nEstimation: conditional\nParallel strategy: 10-4\nNumber of variables: 10"
  ) +
  theme(panel.grid.minor.x = element_blank())
