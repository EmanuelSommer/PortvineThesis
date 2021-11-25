### Explanatory visualizations for theoretical concepts

library(tidyverse)
library(latex2exp)
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
  geom_line(size = 0.3) +
  geom_smooth(se = FALSE, col = my_blue, size = 1,
              method = "loess", formula = "y ~ x", span = 1000) +
  labs(x = "t", y = expression(x[t]),
       title = "Exemplary weakly stationary time series with loess smoothing line") +
  theme_light() +
  theme(panel.grid.minor.y = element_blank(),
        panel.grid.minor.x = element_blank(),
        panel.border = element_blank(),
        axis.ticks = element_blank(),
        panel.grid.major = element_line(colour = "grey", size = .2),
        axis.text = element_text(size = 10),
        axis.title = element_text(size = 12))

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
