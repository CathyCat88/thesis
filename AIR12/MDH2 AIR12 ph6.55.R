graphics.off()
remove(list = ls())

library(ggplot2)
library(cowplot)
library(reshape2)

valeurs <- data.frame(read.csv("1µl AIR12 1h MDH2 ph6.55 ttes 10s.csv",
                               sep = ';',
                               dec = ".", 
                               header = TRUE))

c <- 1:ncol(valeurs[-1])
df <- valeurs[-1][ , c%%6==1]
df <- cbind(df[1:31], valeurs[1])
colnames(df) <- c(c(1:31), "wavelength")

df320MDH2AIR12 <- data.frame(df[(df$wavelength == 320),])
write.table(x = df320MDH2AIR12, file = "df320MDH2AIR12.csv", sep = ",", col.names = TRUE)

df360MDH2AIR12 <- data.frame(df[(df$wavelength == 360),])
write.table(x = df360MDH2AIR12, file = "df360MDH2AIR12.csv", sep = ",", col.names = TRUE)
