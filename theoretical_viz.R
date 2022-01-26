### Explanatory visualizations for theoretical concepts

library(tidyverse)
library(latex2exp)
library(patchwork)
# colors
my_blue <- "#92B8DE"
my_red <- "#db4f59"


### Normal vs Student's t --------------------------------------------
ggplot() +
  stat_function(fun = dnorm, size = 1, n = 100001,
                aes(col = "norm", linetype = "norm")) +
  stat_function(fun = dt, size = 1, n = 100001, args = list(df = 2),
                aes(col = "t", linetype = "t")) +
  scale_color_manual(values = c("norm" = "black", "t" = my_blue),
                     labels = c("N(0,1)", expression(t[2](0,1))),
                     name = "") +
  scale_linetype_manual(values = c("norm" = "solid", "t" = "twodash"),
                        labels = c("N(0,1)", expression(t[2](0,1))),
                        name = "") +
  scale_x_continuous(limits = c(-5, 5), breaks = seq(-6, 6, 2)) +
  labs(y = "Density", x = TeX(""),
       title = TeX("Densities of the standard normal and Student's t distribution")) +
  theme_light() +
  theme(panel.grid.minor.y = element_blank(),
        panel.grid.minor.x = element_blank(),
        panel.border = element_blank(),
        axis.ticks = element_blank(),
        panel.grid.major = element_line(colour = "grey", size = .2),
        axis.text = element_text(size = 10),
        axis.title = element_text(size = 12),
        legend.position = c(0.77, 0.66),
        legend.background = element_blank(),
        legend.text = element_text(size = 12))

### Stationary time series -------------------------------------------

tibble(x = 1:200,
       y = as.numeric(arima.sim(list(ma = 3), n = 200))) %>%
  ggplot(aes(x = x, y = y)) +
  geom_line(size = 0.3, alpha = 0.6) +
  geom_smooth(se = FALSE, col = my_blue, size = 1.3,
              method = "loess", formula = "y ~ x", span = 1000) +
  labs(x = "t", y = expression(x[t]),
       title = "Exemplary weakly stationary time series with LOESS smoothing line") +
  theme_light() +
  theme(panel.grid.minor.y = element_blank(),
        panel.grid.minor.x = element_blank(),
        panel.border = element_blank(),
        axis.ticks = element_blank(),
        panel.grid.major = element_line(colour = "grey", size = .2),
        axis.text = element_text(size = 10),
        axis.title = element_text(size = 12))

### AR(1) sample paths -----------------------------------------------

ar1_sample_paths <- tibble(x = 1:100,
       "0.1" = as.numeric(arima.sim(list(order = c(1, 0, 0), ar = 0.1),
                                n = 100)),
       "0.9" = as.numeric(arima.sim(list(order = c(1, 0, 0), ar = 0.9),
                                n = 100)),
       "-0.9" = as.numeric(arima.sim(list(order = c(1, 0, 0), ar = -0.9),
                                n = 100))
       ) %>%
  pivot_longer(-x, names_to = "phi", values_to = "y") %>%
  mutate(phi = as.factor(phi),
         phi = fct_inorder(phi))
levels(ar1_sample_paths$phi) <- c(
  TeX("$\\phi = 0.1$"),
  TeX("$\\phi = 0.9$"),
  TeX("$\\phi = -0.9$")
)

a1_sample_paths_plot <- ar1_sample_paths %>%
  ggplot() +
  geom_line(aes(x = x, y = y)) +
  facet_wrap(~phi,
             labeller = label_parsed) +
  labs(x = "t", y = expression(x[t]),
       title = TeX("Exemplary AR(1) sample paths and emp. autocovariance function $\\hat{\\gamma}$")) +
  scale_x_continuous(breaks = seq(0, 100, 50)) +
  theme_light() +
  theme(panel.grid.minor.y = element_blank(),
        panel.grid.minor.x = element_blank(),
        panel.border = element_blank(),
        axis.ticks = element_blank(),
        panel.grid.major = element_line(colour = "grey", size = .2),
        axis.text = element_text(size = 10),
        axis.title = element_text(size = 12),
        strip.background = element_rect(color = "grey"),
        strip.text = element_text(size = 10, color = "black"))

a1_sample_acf_plot <- ar1_sample_paths %>%
  group_by(phi) %>%
  summarize(acf = as.numeric(acf(y, type = "cov", plot = FALSE)$acf),
            lag = 0:20) %>%
  ungroup() %>%
  filter(lag != 0 & lag <= 10) %>%
  ggplot() +
  geom_hline(yintercept = 0, col = "black", size = 0.3) +
  geom_segment(aes(x = lag, xend = lag, y = 0, yend = acf)) +
  geom_point(aes(x = lag, y = acf)) +
  scale_x_continuous(breaks = seq(1, 10, 1)) +
  facet_wrap(~phi,
             labeller = label_parsed) +
  labs(x = "h", y = TeX("$\\hat{\\gamma}(h)$")) +
  theme_light() +
  theme(panel.grid.minor.y = element_blank(),
        panel.grid.minor.x = element_blank(),
        panel.border = element_blank(),
        axis.ticks = element_blank(),
        panel.grid.major = element_line(colour = "grey", size = .2),
        axis.text = element_text(size = 10),
        axis.title = element_text(size = 12),
        strip.background = element_rect(color = "grey"),
        strip.text = element_text(size = 10, color = "black"))

a1_sample_paths_plot / a1_sample_acf_plot

### MA(1) sample paths -----------------------------------------------

ma1_sample_paths <- tibble(x = 1:100,
                           "0.1" = as.numeric(arima.sim(list(order = c(0, 0, 1), ma = 0.1),
                                                        n = 100)),
                           "0.9" = as.numeric(arima.sim(list(order = c(0, 0, 1), ma = 0.9),
                                                        n = 100)),
                           "-0.9" = as.numeric(arima.sim(list(order = c(0, 0, 1), ma = -0.9),
                                                         n = 100))
) %>%
  pivot_longer(-x, names_to = "theta", values_to = "y") %>%
  mutate(theta = as.factor(theta),
         theta = fct_inorder(theta))
levels(ma1_sample_paths$theta) <- c(
  TeX("$\\theta = 0.1$"),
  TeX("$\\theta = 0.9$"),
  TeX("$\\theta = -0.9$")
)

ma1_sample_paths %>%
  ggplot() +
  geom_line(aes(x = x, y = y)) +
  facet_wrap(~theta,
             labeller = label_parsed) +
  labs(x = "t", y = expression(x[t]),
       title = TeX("Exemplary MA(1) sample paths")) +
  scale_x_continuous(breaks = seq(0, 100, 50)) +
  theme_light() +
  theme(panel.grid.minor.y = element_blank(),
        panel.grid.minor.x = element_blank(),
        panel.border = element_blank(),
        axis.ticks = element_blank(),
        panel.grid.major = element_line(colour = "grey", size = .2),
        axis.text = element_text(size = 10),
        axis.title = element_text(size = 12),
        strip.background = element_rect(color = "grey"),
        strip.text = element_text(size = 10, color = "black"))


### Assess model quality ts models -----------------------------------

set.seed(222)
sample_tbl <- tibble(
  id = 1:100,
  normal = rnorm(100),
  t = rt(100, df = 2),
  correlated = as.numeric(
    arima.sim(list(ar = c(0.1, 0.3, 0.3, 0.2)), 100)
  ),
  heterogenity = rnorm(100, sd = c(rep(0.5, 50), seq(1, 2, length.out = 50)))
) %>%
  mutate(correlated = (correlated - mean(correlated)) / sd(correlated),
         t = (t - mean(t)) / sd(t)) %>%
  pivot_longer(-id, names_to = "sample", values_to = "value")

sample_tbl <- sample_tbl %>%
  mutate(sample = case_when(
    sample == "normal" ~ "i.i.d. normal",
    sample == "t" ~ "i.i.d. Student's t",
    sample == "heterogenity" ~ "variance heterogeneity",
    TRUE ~ "correlated"
  )) %>%
  mutate(sample = as.ordered(sample),
         sample = fct_inorder(sample))

# simple exploratory
sample_tbl %>%
  ggplot(aes(x = id, y = value)) +
  geom_line() +
  labs(x = "t", y = expression(z[t]),
       title = "") +
  facet_wrap(~sample) +
  scale_x_continuous(breaks = seq(0, 100, 50)) +
  theme_light() +
  theme(panel.grid.minor.y = element_blank(),
        panel.grid.minor.x = element_blank(),
        panel.border = element_blank(),
        axis.ticks = element_blank(),
        panel.grid.major = element_line(colour = "grey", size = .2),
        axis.text = element_text(size = 10),
        axis.title = element_text(size = 12),
        strip.background = element_rect(color = "grey"),
        strip.text = element_text(size = 10, color = "black"))

# ACFs
sample_tbl %>%
  group_by(sample) %>%
  summarize(acf = as.numeric(acf(value, type = "cor", plot = FALSE)$acf),
            lag = 0:20) %>%
  ungroup() %>%
  filter(lag != 0 & lag <= 10) %>%
  ggplot() +
  geom_hline(yintercept = 0, col = "black", size = 0.3) +
  geom_hline(yintercept = qnorm(c(0.025, 0.975)) / sqrt(nrow(sample_tbl)),
             linetype = "longdash", col = my_blue, size = 0.8) +
  geom_segment(aes(x = lag, xend = lag, y = 0, yend = acf)) +
  geom_point(aes(x = lag, y = acf)) +
  scale_x_continuous(breaks = seq(1, 10, 1)) +
  facet_wrap(~sample) +
  labs(x = "h", y = "ACF(h)") +
  theme_light() +
  theme(panel.grid.minor.y = element_blank(),
        panel.grid.minor.x = element_blank(),
        panel.border = element_blank(),
        axis.ticks = element_blank(),
        panel.grid.major = element_line(colour = "grey", size = .2),
        axis.text = element_text(size = 10),
        axis.title = element_text(size = 12),
        strip.background = element_rect(color = "grey"),
        strip.text = element_text(size = 10, color = "black"))

# normal QQ plots
sample_tbl %>%
  ggplot(aes(sample = value)) +
  geom_abline(col = my_blue, size = 0.8) +
  geom_qq(alpha = 0.5) +
  coord_fixed() +
  scale_x_continuous(breaks = seq(-4, 4, 2)) +
  scale_y_continuous(breaks = seq(-4, 4, 2)) +
  facet_wrap(~sample, ncol = 4) +
  labs(x = "theoretical i.e. N(0,1)") +
  theme_light() +
  theme(panel.grid.minor.y = element_blank(),
        panel.grid.minor.x = element_blank(),
        panel.border = element_blank(),
        axis.ticks = element_blank(),
        panel.grid.major = element_line(colour = "grey", size = .2),
        axis.text = element_text(size = 10),
        axis.title = element_text(size = 12),
        strip.background = element_rect(color = "grey"),
        strip.text = element_text(size = 10, color = "black"))

# Ljung Box tests
sample_tbl %>%
  group_by(sample) %>%
  summarize(pval = sapply(1:10,
                    function(i) Box.test(value, lag = i, type = "Lju")$p.value),
            lag = 1:10) %>%
  ungroup() %>%
  ggplot() +
  geom_hline(yintercept = 0, col = "black", size = 0.3) +
  geom_hline(yintercept = 0.05,
             linetype = "longdash", col = my_blue, size = 0.8) +
  geom_line(aes(x = lag, y = pval)) +
  geom_point(aes(x = lag, y = pval)) +
  scale_x_continuous(breaks = seq(1, 10, 1)) +
  facet_wrap(~sample) +
  labs(x = "h", y = "p-value of Ljung-Box test at lag h") +
  theme_light() +
  theme(panel.grid.minor.y = element_blank(),
        panel.grid.minor.x = element_blank(),
        panel.border = element_blank(),
        axis.ticks = element_blank(),
        panel.grid.major = element_line(colour = "grey", size = .2),
        axis.text = element_text(size = 10),
        axis.title = element_text(size = 12),
        strip.background = element_rect(color = "grey"),
        strip.text = element_text(size = 10, color = "black"))

### Value at Risk ----------------------------------------------------
ggplot() +
  stat_function(fun = pnorm, size = 1, n = 100001) +
  # geom_segment(aes(x = -3, xend = -0.5, y = pnorm(-0.5), yend = pnorm(-0.5)),
  #              col = "grey") +
  # geom_segment(aes(x = -0.5, xend = -0.5, y = 0, yend = pnorm(-0.5)),
  #              col = "grey") +
  scale_y_continuous(breaks = c(0, pnorm(-0.5), 1),
                     labels = c("0", TeX("$\\alpha$"), "1")) +
  scale_x_continuous(breaks = c(-0.5), limits = c(-3, 4),
                     labels = c(TeX("$Q_{R^P_t}(\\alpha) = VaR_{\\alpha}^{P,t}$"))) +
  labs(x = "", y = "",
       title = TeX("VaR highlighted on the continuous distribution function $F_{R^P_t}$ of $R^P_t$")) +
  theme_light() +
  theme(panel.grid.minor.y = element_blank(),
        panel.grid.minor.x = element_blank(),
        panel.border = element_blank(),
        axis.ticks = element_blank(),
        panel.grid.major = element_line(colour = "grey", size = .9),
        axis.text = element_text(size = 15))

### Bivariate copulas contours

library(rvinecopulib)
old_par <- par()
png(filename = "img/bivar_cop_cont.png", res = 150,
    width = 1600, height = 1600)
par(pty = "s", mfrow = c(4, 4))
for (family in c("indep", "gaussian", "t",
                 "clayton", "gumbel", "frank", "joe")) {
  if (family == "indep") {
    contour(
      bicop_dist(
        family = "indep", rotation = 0,
        parameters = ktau_to_par(family = "indep", tau = 0.2)
      )
    )
    title(main = "Copula family: Independence", sub = "Rotation: 0°")
  } else if (family == "t") {
    contour(
      bicop_dist(
        family = "t", rotation = 0,
        parameters = matrix(c(0.2, 3), byrow = TRUE, ncol = 1)
      )
    )
    title(main = paste("Copula family:", "Student t"),
          sub = "Rotation: 0° and weak dependence")
    contour(
      bicop_dist(
        family = "t", rotation = 0,
        parameters = matrix(c(0.9, 3), byrow = TRUE, ncol = 1)
      )
    )
    title(main = paste("Copula family:", "Student t"),
          sub = "Rotation: 0° and strong dependence")
  } else {
    label <- str_to_title(family)
    contour(
      bicop_dist(
        family = family, rotation = 0,
        parameters = ktau_to_par(family = family, tau = 0.2)
      )
    )
    title(main = paste("Copula family:", label),
          sub = "Rotation: 0° and weak dependence")
    contour(
      bicop_dist(
        family = family, rotation = 0,
        parameters = ktau_to_par(family = family, tau = 0.8)
      )
    )
    title(main = paste("Copula family:", label),
          sub = "Rotation: 0° and strong dependence")
  }
  if (family == "joe") {
    contour(
      bicop_dist(
        family = "joe", rotation = 90,
        parameters = ktau_to_par(family = "joe", tau = 0.8)
      )
    )
    title(main = paste("Copula family:", "Joe"),
          sub = "Rotation: 90° and strong dependence")
  }
}
dev.off()
suppressWarnings(par(old_par))
