library(hce)

x <- seq(1.05, 2, 0.05)
N1 <- sizeWO(WO = x, power = 0.9, alternative = "m")$SampleSize
N2 <- sizeWO(WO = x, power = 0.9, alternative = "o")$SampleSize
N3 <- sizeWO(WO = x, power = 0.9, alternative = "s")$SampleSize
d <- data.frame(WO = x, N_m = N1, N_o = N2, N_s = N3)
write.csv(x = d, file = "Table_1.csv", row.names = FALSE)
