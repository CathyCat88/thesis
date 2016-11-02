graphics.off()
remove(list = ls())

library(ggplot2)
library(cowplot)
library(reshape2)

valeurs1 <- data.frame(read.csv("1 10 min MDH2 ph6.55 ttes 10s.csv", sep = ';', dec = ".", header = TRUE))
valeurs2 <- data.frame(read.csv("2 10 min MDH2 ph6.55 ttes 10s.csv", sep = ';', dec = ".", header = TRUE))
valeurs3 <- data.frame(read.csv("3 10 min MDH2 ph6.55 ttes 10s.csv", sep = ';', dec = ".", header = TRUE))
#valeurs4 <- data.frame(read.csv("4 10 min MDH2 ph6.55 ttes 10s.csv", sep = ';', dec = ".", header = TRUE))

valeurs <- cbind(valeurs1[-1],valeurs2[-1], valeurs3[-1])#, valeurs4[-1])
c <- 1:ncol(valeurs)
df <- valeurs[ , c%%6==1]
df <- cbind(df, valeurs1[1])
colnames(df) <- c(c(1:31), "wavelength")
df1 <- melt(df, id.vars="wavelength")

g <- ggplot (df1, aes(x= wavelength, y= value, group = variable, color = variable))
g <- g + geom_line()
g <- g + theme(legend.position = "none")
g <- g + scale_x_continuous(name = "\nWavelength (nm)")
g <- g + scale_y_continuous(name = "absorbance\n")
g
save_plot('MDH2 ph6.55 1min.png', g, base_aspect_ratio = 1.3)

g <- ggplot (df1, aes(x= wavelength, y= value, group = variable, color = variable))
g <- g + geom_line()
g <- g + theme(legend.position = "none")
g <- g + scale_x_continuous(name = "\nLongueur d'onde (nm)", limits = c(300, 402), expand = c(0,0))
g <- g + scale_y_continuous(name = "Absorbance\n", limits = c(0, 0.48), expand = c(0,0))
g
save_plot('MDH2_ph655_1min_fr.png', g, base_aspect_ratio = 1.3)
