graphics.off()
remove(list = ls())

library(ggplot2)
library(cowplot)
library(reshape2)

valeurs1 <- data.frame(read.csv("1 10 min MDH2 ph6.55 ttes 10s.csv", sep = ';', dec = ".", header = TRUE))
valeurs2 <- data.frame(read.csv("2 10 min MDH2 ph6.55 ttes 10s.csv", sep = ';', dec = ".", header = TRUE))
valeurs3 <- data.frame(read.csv("3 10 min MDH2 ph6.55 ttes 10s.csv", sep = ';', dec = ".", header = TRUE))
valeurs <- cbind(valeurs1[-1],valeurs2[-1], valeurs3[-1])
colnames(valeurs) <- c(0:182)

valeurs4 <- data.frame(read.csv("1µl AIR12 1h MDH2 ph6.55 ttes 10s.csv", sep = ';', dec = ".", header = TRUE))
valeurs4 <- valeurs4[-1]
valeurs4 <- valeurs4[,1:183]
colnames(valeurs4) <- c(0:182)

df <- rbind(valeurs,valeurs4)

c <- 1:ncol(df)
df1 <- df[ , c%%6==1]
df1 <- df1[, (1:31)]
df1$wavelength <- valeurs1[,1]
df1 <- melt(df1, id.vars="wavelength")
df1$prot <- rep(c("no", "yes"), each = nrow(valeurs1))

abs320 <- df1[df1$wavelength == 320,]

no <- abs(abs320$value[abs320$prot == "no"] - abs320$value[abs320$prot == "no" & abs320$variable == 0])
yes <- abs(abs320$value[abs320$prot == "yes"] - abs320$value[abs320$prot == "yes" & abs320$variable == 0])

result <- data.frame(rep(seq(0,30, 1),2))
result$abs <- append(no, yes)
result$prot <- rep(c("no", "yes"), each = 31)
colnames(result) <- c("time", "abs", "prot")

g <- ggplot(result, aes(time, abs, group = prot)) +
  geom_line() +
  geom_point(size = 2.5, aes(shape = prot)) +
  theme(legend.position = "none") +
  scale_x_continuous(name = "\nTime (min)", expand = c(0,0), limits = c(0, 31)) +
  scale_y_continuous(name = "Absorbance\n", expand = c(0,0), limits = c(0,0.26))
g
save_plot("MDH2_AIR12_pH6.55.png", g, base_aspect_ratio = 1.3)

