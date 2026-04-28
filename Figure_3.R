library(ggplot2)
library(hce)
library(maraca)
################################
Rates_A <- c(2, 2, 10)/2
Rates_P <- c(1, 6, 5)/2
ORDER <- c("Death", "Hospitalization", "MI", "Change in score")
dat <- simHCE(n = 2500, TTE_A = Rates_A, 
              TTE_P = Rates_P,
              CM_A = 6, CM_P = 0, CSD_A = 10, 
              fixedfy = 3, shape = 0.55, seed = 1723238609)
dat$GROUP <- as.factor(dat$GROUP)
levels(dat$GROUP) <- ORDER

hce_plot <- maraca(
  dat,
  step_outcomes = ORDER[-length(ORDER)],
  last_outcome = ORDER[length(ORDER)],
  fixed_followup_days = 3 * 365,
  column_names = c(outcome = "GROUP", arm = "TRTP", value = "AVAL0"),
  arm_levels = c(active = "A", control = "P"),
  compute_win_odds = TRUE
)
colorScheme <- c("P" = "lightblue", "A" = "darkgreen")

g2 <- plot(hce_plot, compute_win_odds = T) + 
  theme(
    legend.position = "right",
    axis.text.x.bottom = element_text(
      angle = 90,
      vjust = 0.5,
      hjust = 1
    ),
    axis.title.x = element_blank()
  ) + scale_color_manual(values = colorScheme) +
  scale_fill_manual(values = colorScheme) + ylab("Cumulative Event Percentage") 

res <- calcWO(dat)[c("WO", "LCL", "UCL")]
res <- round(res, 2)
res_p <- calcWO(dat)$Pvalue
g2 <- g2 + 
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
  ) + theme(legend.position = "bottom") + 
  ggtitle("Figure 3: The maraca plot with crossing probability curves")

ggsave(filename = "Figure_3.png", plot = g2, width = 8, height = 6, dpi = 300)