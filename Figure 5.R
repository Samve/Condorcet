library(ggplot2)
library(hce)
################################
Rates_A <- c(1, 2, 5)/2
Rates_P <- c(1.5, 2.5, 5)/2
ORDER <- c("Death", 
           "Hospitalization", "MI", "Change in score")
dat <- simHCE(n = 2500, TTE_A = Rates_A, 
              TTE_P = Rates_P,
              CM_A = 2.1, CM_P = 0, CSD_A = 10, 
              fixedfy = 3, shape = 0.55, seed = 1723238609)
dat$GROUP <- as.factor(dat$GROUP)
levels(dat$GROUP) <- ORDER
CALC <- calcWO(dat)
WP <- round(CALC[["WP"]], 3)
WO <- round(CALC[["WO"]], 2)

HCE0 <- dat[dat$TRTP == "P", ]
HCE1 <- dat[dat$TRTP == "A", ]

x <- seq(0, max(dat$AVAL), 0.1)
X <- ecdf(HCE0$AVAL)(x)
Y <- ecdf(HCE1$AVAL)(x)

res <- unique(data.frame(X = X, Y = Y))
temp <- bquote(theta == .(WP) ~ "(" * kappa == .(WO) * ")")
temp <- as.expression(temp)
g4 <- res |> ggplot(aes(x = X, y = Y)) + theme_bw() + 
  geom_ribbon(aes(ymin = 1, ymax = Y), fill = "lightblue") + 
  geom_line(linewidth = 0.75) + 
  geom_abline(slope = 1, intercept = 0, color = 2, linewidth = 0.75) + 
  labs(x = expression(P(xi < x)), y = expression(P(eta < x))) +
  annotate("text", x = 0.25, y = 0.75, label = temp, parse = T) + 
  ggtitle("Figure 5: MWE as the area above the ordinal dominance graph")


ggsave(filename = "Figure 5.png", plot = g4, width = 8, height = 6, dpi = 300)
