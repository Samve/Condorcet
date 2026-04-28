library(ggplot2)
library(patchwork)
library(hce)
library(maraca)
################################
colorScheme <- c("P" = "lightblue", "A" = "darkgreen")
m <- 8
PADY <- 3
set.seed(2)
D <- simTTE(n = 1000, TTE_A = c(0.1, 0.04), 
            TTE_P = c(.15, 0.045), theta = 4, alpha0 = 2, alpha = -1, shape = 2, 
            fixedfy = PADY, rHR = 1, m = m)

Tab <- table(D$TRTP, D$GROUP)
nA <- Tab["A", "CEN"]
nP <- Tab["P", "CEN"]
D$AVAL0[D$GROUP == "CEN" & D$TRTP == "A"] <- round(rnorm(nA, mean = 1), 2)

D$AVAL0[D$GROUP == "CEN" & D$TRTP == "P"] <- round(rnorm(nP, mean = 0.78), 2)
D$AVAL0[D$GROUP == "CEN"] <- D$AVAL0[D$GROUP == "CEN"] - min(D$AVAL0[D$GROUP == "CEN"])  

levels(D$GROUP) <- c("Death", "Hospitalization", "Change in score")
####################################################
dat <- D
dat$AVAL0[dat$EVENT2 == "DEATH"] <- dat$AVAL0[dat$EVENT2 == "DEATH"] + 
  dat$AVAL1[dat$EVENT2 == "DEATH"]/(m + 2)
dat$AVAL0[dat$EVENT1 == "DEATH"] <- dat$AVAL0[dat$EVENT1 == "DEATH"] + 
  dat$PADY[dat$EVENT1 == "DEATH"]/(m + 2)

dat <- as_hce(dat)
##############
res <- calcWO(D)
res_p <- res$Pvalue
res <- round(res[c("WO", "LCL", "UCL")], 2)
g1 <- plot(D, last_outcome = "Change in score", continuous_grid_spacing_x = 1, 
           compute_win_odds = F)
g1 <- g1 +  theme(
  legend.position = "bottom",
  axis.text.x.bottom = element_text(
    angle = 90,
    vjust = 0.5,
    hjust = 1
  ),
  axis.title.x = element_blank()
) + annotate(
  geom = "label",
  x = 0,
  y = Inf,
  label = paste(
    "Success Odds: ", res[1],
    "\n95% CI: ", res[2], " - ",
    res[3], "\n",
    "p-value: ", format.pval(res_p, digits = 3, eps = 0.001),
    sep = ""
  ),
  hjust = 0, vjust = 1.4, size = 3
) +
  scale_color_manual(values = colorScheme) +
  scale_fill_manual(values = colorScheme) + 
  scale_y_continuous(breaks = seq(0, 100, 5), 
                     limits = c(0, 55)) + ggtitle("Most-important approach")


################
res <- calcWO(dat)
res_p <- res$Pvalue
res <- round(res[c("WO", "LCL", "UCL")], 2)
g2 <- plot(dat, last_outcome = "Change in score", continuous_grid_spacing_x = 1, 
           compute_win_odds = F)
g2 <- g2 +  theme(
  legend.position = "bottom",
  axis.text.x.bottom = element_text(
    angle = 90,
    vjust = 0.5,
    hjust = 1
  ),
  axis.title.x = element_blank()
) + scale_color_manual(values = colorScheme) +
  scale_fill_manual(values = colorScheme) + scale_y_continuous(breaks = seq(0, 100, 5), 
                                                               limits = c(0, 55)) + 
  ggtitle("Move-down approach") +
  annotate(
    geom = "label",
    x = 0,
    y = Inf,
    label = paste(
      "Success Odds: ", res[1],
      "\n95% CI: ", res[2], " - ",
      res[3], "\n",
      "p-value: ", format.pval(res_p, digits = 3, eps = 0.001),
      sep = ""
    ),
    hjust = 0, vjust = 1.4, size = 3
  )

#########################
g <- g1 | g2
g <- g + plot_annotation(title = "Figure 6: Comparison of the 'most-important' and 'move-down' approaches.")



ggsave(filename = "Figure 6.png", plot = g, width = 10, height = 8, dpi = 300)
