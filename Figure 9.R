library(ggplot2)
library(hce)
############################################################
n <- n0 <- 2500
Rates_A <- c(1.72, 1.74, 3, 1.5, 1)
Rates_P <- c(2.47, 2.24, 2.9, 2, 2)
dat <- simHCE(n = n, n0 = n0, TTE_A = Rates_A, TTE_P = Rates_P,
              CM_A = 0, CM_P = 0, CSD_A = 1, fixedfy = 3, seed = 1)

DAT <- dat
ORD <- c("Continuous", "Outcome 1", "Outcome 2", "Outcome 3", "Outcome 4", "Outcome 5")
DAT$GROUP <- factor(DAT$GROUP)
levels(DAT$GROUP) <- ORD

delta <- 0
res <- NULL
for(delta in seq(0, 50, 0.1)){
  cal <- deltaWO(dat, delta = delta, ref = "P")
  WO <- cal$WO
  WP <- cal$WP
  SD <- cal$SD_WP
  
  pow <- powerWO(N = 1000, SD = SD, WO = WO)$power
  res0 <- data.frame(delta = delta, WP = WP, SD = SD, pow = pow)
  res <- rbind(res, res0)
}

D <- NULL
DELTA <- c(0, 1, 5)
for(size in seq(1000, 5000, 10)){
  for(delta in DELTA){
    d <- res[res$delta == delta, ]
    d$N <- size
    WO <- d$WP/ (1 - d$WP)
    d$pow <- powerWO(N = size, SD = d$SD, WO = WO)$power
    D <- rbind(D, d)
  }
}
D$delta <- factor(D$delta, levels = DELTA)
g <- D |> ggplot(aes(x = N, y = pow, color = delta)) + geom_line() + theme_bw() +
  scale_x_continuous(breaks = seq(1000, 5000, 500)) +
  scale_y_continuous(breaks = seq(0.1, 1, 0.05), limits = c(0.1, 1)) +
  labs(x = "Sample size", y = "Power", colour = "Threshold") +
  ggtitle("Figure 9: Power of the success odds test as a function of the threshold based on the simulated dataset with negative effect on the continuous outcome.")

ggsave(filename = "Figure 9.png", plot = g, width = 15, height = 10, dpi = 300)