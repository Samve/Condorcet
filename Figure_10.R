library(hce)
library(ggplot2)
library(dplyr)
library(tidyr)
library(furrr)
############## Global parameters for beta distributions#####################
rm(list = ls())
alpha1 <- 8
beta1 <- 8
alpha0 <- 4
beta0 <- 5
############# parameters for simulations ###################################
m <- 1000
n <- seq(50, 500, 10) 
M <- c(2, 5, 10)
k <- 0.5
values <- expand.grid(n = n, M = M, sim = 1:m)
set.seed(2024) # seed for selecting the seeds
values$seed <- sample(x = 1:10E7, size = nrow(values), replace = FALSE)

#######################################################
Simulations <- function(i){
  val <- values[i, ]
  set.seed(val$seed)
  d <- simORD(n = val$n, M = val$M, alpha1 = alpha1, beta1 = beta1, 
              alpha0 = alpha0, beta0 = beta0)
  SD_WP1 <- calcWO(GROUPN ~ TRTP, data = d)$SD_WP
  coef <- 1 - sum((table(d$GROUPN)/length(d$GROUPN))^3)
  SD_WP2 <- sqrt(coef*1/(12*k*(1-k)))
  res <- data.frame(N = 2*val$n, M = val$M, SD_WP1 = SD_WP1, SD_WP2 = SD_WP2, seed = val$seed)
  res
} 
  
########### Parallel #################
    
plan(multisession, workers = 10)    
t0 <- Sys.time()
RES <- future_map_dfr(1:nrow(values), Simulations, .progress = TRUE, .options = furrr_options(seed = TRUE))
t1 <- Sys.time()
t1 - t0
###################################
OUT <- aggregate(cbind(SD_WP1, SD_WP2) ~ N + M, data = RES, mean)

OUT1 <- OUT |> mutate(NOET = sqrt(1/3)) |> pivot_longer(cols = c("SD_WP1", "SD_WP2", "NOET"), names_to = "TYPE", 
                                                        values_to = "SD")

OUT1$M[OUT1$TYPE == "NOET"] <- "Continuous"
OUT1$M <- factor(OUT1$M, levels = c("2", "5", "10", "Continuous")) 

OUT1$TYPE[OUT1$TYPE == "NOET"] <- "Noether"
OUT1$TYPE[OUT1$TYPE == "SD_WP1"] <- "Data-driven"
OUT1$TYPE[OUT1$TYPE == "SD_WP2"] <- "Ties-corrected"


g <- OUT1 |> ggplot(aes(x = N, y = SD, colour = TYPE, linetype = M)) + 
  geom_line(linewidth = 0.75) + theme_bw() + 
  scale_y_continuous(breaks = seq(0.45, 0.60, 0.01), limits = c(0.47, 0.59)) + 
  scale_x_continuous(breaks = seq(100, 1000, 100)) +
  labs(x = "Total sample size", y = "Standard Deviation",
       colour = "Method", linetype = "Number of categories") + 
  theme(legend.position = "bottom")
g
ggsave(filename = "Figure_10.png", plot = g, width = 8, height = 6)
