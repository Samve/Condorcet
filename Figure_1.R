library(ggplot2)

####################################################

k <- 100
x <- seq(0, 1, 0.001)
xi2 <- (pweibull(x, shape = k, scale = 1/1) + 
          pweibull(x, shape = k, scale = 1/5) + pweibull(x, shape = k, scale = 1/9))/3
xi1 <- (pweibull(x, shape = k, scale = 1/3) + 
          pweibull(x, shape = k, scale = 1/4) + pweibull(x, shape = k, scale = 1/8))/3
xi0 <- (pweibull(x, shape = k, scale = 1/2) + 
          pweibull(x, shape = k, scale = 1/6) + pweibull(x, shape = k, scale = 1/7))/3


d <- data.frame(AVAL = c(xi2, xi1, xi0), 
                TRTP = rep(c("xi2", "xi1", "xi0"), each = length(x)), 
                x = c(x, x, x))

g1 <- d |> ggplot(aes(x = x, y = AVAL, colour  = TRTP, linetype = TRTP)) + 
  geom_line(linewidth = 0.85) +
  scale_color_discrete(labels = c('xi2' = expression(xi[2]),
                                  'xi1' = expression(xi[1]),
                                  'xi0' = expression(xi[0]))) + 
  scale_linetype_discrete(labels = c('xi2' = expression(xi[2]),
                                     'xi1' = expression(xi[1]),
                                     'xi0' = expression(xi[0]))) + 
  theme_bw() + 
  labs(x = "Time", y = "Event Probability", 
       color = "Treatment group", 
       linetype = "Treatment group") + 
  theme(legend.position = "bottom") + 
  scale_x_continuous(breaks = seq(0, 1, 0.1)) + 
  ggtitle("Figure 1: Mixture of Weibull distributions based on non-transitive dice") 

ggsave(filename = "Figure_1.png", plot = g1, width = 8, height = 6, dpi = 300)
