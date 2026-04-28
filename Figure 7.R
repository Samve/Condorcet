library(ggplot2)
library(patchwork)
library(hce)
library(maraca)

#################################
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
RES <- calcWO(AVAL ~ TRTP, data = dat)

###### SD ##########
g1 <- res |> ggplot(aes(x = delta, y = SD)) + geom_line() + 
  scale_y_continuous(breaks = seq(0, 1, 0.02), limits = c(0.35, 0.6)) +
  scale_x_continuous(breaks = seq(0, 20, 2)) +
  labs(x = "Threshold",  y = "Mann-Whitney Effect Standard Deviation") + 
  theme_bw()

WO <- as.numeric(RES["WP"])/(1 - as.numeric(RES["WP"]))
############# Win odds ###########
g2 <- res |> ggplot(aes(x = delta, y = WO)) + geom_line() + 
  theme_bw() +  scale_y_continuous(breaks = seq(0.5, 2, 0.02), 
                                   limits = c(1.10, 1.35)) +
  scale_x_continuous(breaks = seq(0, 20, 2)) +
  labs(x = "Threshold", y = "Success Odds")

g <- g1 | g2 

g <- g + plot_annotation(title = "Figure 7: The impact of the threshold on the success odds and the standard deviation of the MWE using different thresholds based on the kidney HCE example dataset.")

ggsave(filename = "Figure 7.png", plot = g, width = 15, height = 10, dpi = 300)