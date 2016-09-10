graphics.off()
remove(list = ls())

library(ggplot2)
library(cowplot)
library(reshape2)

valeurs <- data.frame(read.csv("MDH2 ph5.8 0 + AIR12 1µL.csv", sep = ';', dec = ",", header = TRUE))

df <- melt(valeurs, id.vars="wavelength")

colnames(df) <- c("wavelength", "group", "absorbance")

g <- ggplot (df, aes(x= wavelength, y= absorbance, group = group, color = group))
g <- g + geom_line()
g <- g + theme(legend.position = "none")
g <- g + scale_x_continuous(name = "\nWavelength (nm)") #, limits = c(480, 600))
g <- g + scale_y_continuous(name = "Absorbance\n")#, limits = c(0, 300))
g
save_plot('MDH2 AIR12 ph5.8.png', g, base_aspect_ratio = 1.3)
