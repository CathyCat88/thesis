graphics.off()
remove(list = ls())

library(ggplot2)
library(cowplot)
library(reshape2)

valeurs <- data.frame(read.csv("30 min MDH2 1µl AIR12 0.5xanth Xox ph6.55 ttes 10s.csv",
                               sep = ';',
                               dec = ".", 
                               header = TRUE))

c <- 1:ncol(valeurs[-1])
df <- valeurs[-1][ , c%%6==1]
df <- cbind(df[1:31], valeurs[1])
colnames(df) <- c(c(1:31), "wavelength")

df320 <- data.frame(df[(df$wavelength == 320),])
#write.table(x = df320, file = "df320.csv", sep = ",", col.names = TRUE)

df360 <- data.frame(df[(df$wavelength == 360),])
#write.table(x = df360, file = "df360.csv", sep = ",", col.names = TRUE)

deltaAbs <- function(reference, evolution) {
  return(abs(reference - evolution))
}

result <- data.frame(integer(0))
for (i in 2:31) {
  a <- data.frame(deltaAbs(df320$X1, df320[i]))
  print(a)
  result <- rbind(result, a)
}
