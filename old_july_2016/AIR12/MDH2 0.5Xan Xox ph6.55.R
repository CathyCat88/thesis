graphics.off()
remove(list = ls())

library(ggplot2)
library(cowplot)
library(reshape2)

valeurs <- data.frame(read.csv("MDH2 0.5Xan Xox ph6.55 ttes10s.csv", sep = ';', dec = ".", header = TRUE))

c <- 1:ncol(valeurs[-1])
df <- valeurs[-1][ , c%%6==1]
df <- cbind(df[1:31], valeurs[1])
colnames(df) <- c(c(1:31), "wavelength")
df1 <- melt(df, id.vars="wavelength")

g <- ggplot (df1, aes(x= wavelength, y= value, group = variable, color = variable))
g <- g + geom_line()
g <- g + theme(legend.position = "none")
g <- g + scale_x_continuous(name = "\nWavelength (nm)", limits = c(300,400))
g <- g + scale_y_continuous(name = "absorbance\n")
g
save_plot('MDH2 0.5Xan Xox ph6.55 1min.png', g, base_aspect_ratio = 1.3)
