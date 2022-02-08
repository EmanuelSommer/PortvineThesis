### Visualize the performance of the package tested on the LRZ HPC linux cluster
library(tidyverse)
custom_colors <- c("#92B8DE", "#db4f59", "#477042", "#cc72d6")
theme_set(
  theme_minimal() +
    theme(plot.title = ggtext::element_markdown(size = 11),
          plot.subtitle = ggtext::element_markdown(size = 9))
)
# load the performance results:
load()
results_df
uncond_results_df <- results_df[-1, ]
uncond_results_df <- uncond_results_df %>%
  mutate(parallel_strategy = paste0(first_level_parallel,
                                    "-",
                                    second_level_parallel),
         time_minutes = as.numeric(time_minutes))

# compare sample sizes
uncond_results_df %>%
  filter(family == "parametric", vars == 10) %>%
  ggplot(aes(x = fct_inorder(parallel_strategy), y = time_minutes,
             group = n_samples,
             col = n_samples, shape = n_samples)) +
  geom_point(size = 2) +
  geom_line(alpha = 0.4) +
  ylim(0, NA) +
  scale_color_manual(values = custom_colors[3:4], name = "Sample size") +
  labs(x = "Parallel stratgey: first-second level parallelization",
       y = "Runtime in minutes",
       shape = "Sample size",
       title = "Influence of the sample size on runtime",
       subtitle = "Number of variables: 10,
       allowed copula families: all parametric")


# compare number of variables
uncond_results_df %>%
  mutate(vars = as.numeric(vars)) %>%
  filter(family == "parametric", n_samples == "10000",
         parallel_strategy == "10-2") %>%
  ggplot(aes(x = vars, y = time_minutes)) +
  geom_point(size = 2) +
  geom_line(alpha = 0.4) +
  ylim(0, NA) +
  labs(y = "Runtime in minutes",
       x = "Number of variables",
       title = "Influence of the number of variables on runtime",
       subtitle = "Sample size: 10000, allowed copula families: all parametric")

# compare the use of different copula families
uncond_results_df %>%
  mutate(vars = as.numeric(vars)) %>%
  filter(n_samples == "10000", vars = 10,
         parallel_strategy == "10-2") %>%
  ggplot(aes(x = family, y = time_minutes)) +
  geom_point(size = 2) +
  geom_line(alpha = 0.4) +
  ylim(0, NA) +
  labs(y = "Runtime in minutes",
       x = "Allowed copula families",
       title = "Influence of the allowed copula families on runtime",
       subtitle = "Sample size: 10000, number of variables: 10")


# if working copy and paste for conditional measures
