### Explanatory visualizations for theoretical concepts

library(tidyverse)
library(latex2exp)

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
