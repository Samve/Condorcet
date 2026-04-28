library(hce)
library(ggplot2)
#####################################################
dat <- KHCE
dat <- as_hce(dat)

deltaWO(dat, delta = 0, ref = "P")
deltaWO(dat, delta = 5, ref = "P")

delta <- 0
res <- NULL
for(delta in seq(0, 20, 0.1)){
  cal <- deltaWO(dat, delta = delta, ref = "P")
  WO <- cal$WO
  WP <- cal$WP
  SD <- cal$SD_WP
  
  pow <- powerWO(N = 1000, SD = SD, WO = WO)$power
  res0 <- data.frame(delta = delta, WP = WP, WO = WO, SD = SD, pow = pow)
  res <- rbind(res, res0)
}


D <- NULL
DELTA <- c(0, 3, 5, 10)
for(size in seq(100, 1000, 10)){
  for(delta in DELTA){
    d <- res[res$delta == delta, ]
    d$N <- size
    WO <- d$WP/ (1 - d$WP)
    d$pow <- powerWO(N = size, SD = d$SD, WO = WO, alternative = "ordered")$power
    D <- rbind(D, d)
  }
}
D$delta <- factor(D$delta, levels = DELTA)
g <- D |> ggplot(aes(x = N, y = pow, color = delta)) + geom_line() + theme_bw() +
  scale_x_continuous(breaks = seq(100, 1000, 100)) +
  scale_y_continuous(breaks = seq(0.1, 1, 0.05), limits = c(0.1, 1)) +
  labs(x = "Sample size", y = "Power", colour = "Threshold") +
  ggtitle("Figure 8: The impact of the threshold on the success odds and the standard deviation of the MWE using different thresholds based on the kidney HCE example dataset.")


ggsave(filename = "Figure_8.png", plot = g, width = 15, height = 10, dpi = 300)
