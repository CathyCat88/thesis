graphics.off()
remove(list = ls())

library(ggplot2)
library(cowplot)
library(reshape2)

valeurs1 <- read.csv("MDH2 ph5.8.csv", sep = ';', dec = ",", header = TRUE)
valeurs1 <- valeurs1[,1:8]

valeurs2 <- read.csv("MDH2 ph5.8 0 + AIR12 1µL.csv", sep = ';', dec = ",", header = TRUE)

df <- rbind(valeurs1,valeurs2)
colnames(df) <- c("wavelength", "0", "300", "600", "900", "1200", "1500", "1800") 
df <- melt(df, id.vars="wavelength")
df$prot <- rep(c("no", "yes"), each = nrow(valeurs1))

abs320 <- df[df$wavelength == 320,]

no <- abs(abs320$value[abs320$prot == "no"] - abs320$value[abs320$prot == "no" & abs320$variable == 0])
yes <- abs(abs320$value[abs320$prot == "yes"] - abs320$value[abs320$prot == "yes" & abs320$variable == 0])

result <- data.frame(rep(seq(0,30, 5),2))
result$abs <- append(no, yes)
result$prot <- rep(c("no", "yes"), each = 7)
colnames(result) <- c("time", "abs", "prot")

g <- ggplot(result, aes(time, abs, group = prot)) +
  geom_line() +
  geom_point(size = 2.5, aes(shape = prot)) +
  theme(legend.position = "none") +
  scale_x_continuous(name = "\nTime (min)", expand = c(0,0), limits = c(0, 31)) +
  scale_y_continuous(name = "Absorbance\n", expand = c(0,0), limits = c(0,0.04))
g
save_plot("MDH2_AIR12_pH5.8.png", g, base_aspect_ratio = 1.3)
             
