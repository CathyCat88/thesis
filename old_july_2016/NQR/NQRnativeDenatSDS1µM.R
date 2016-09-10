graphics.off()
remove(list = ls())

library(ggplot2)
library(cowplot)

valeurs <- data.frame(read.csv("NQRnativeDenatSDS1µM.csv", sep = ',', header = TRUE))

g <- ggplot (valeurs, aes(x= wavelength, y= fluorescence.intensity..a.u.., group = state))
g <- g + geom_line(aes(linetype = state))
g <- g + theme(legend.position = "none")
g <- g + scale_x_continuous(name = "\nWavelength (nm)", limits = c(480, 600), breaks = c(500, 550, 600))
g <- g + scale_y_continuous(name = "Fluorescence intensity (a.u)\n", limits = c(0, 10))

save_plot('NQRnativeDenatSDS1µM.png', g, base_aspect_ratio = 1.3)