graphics.off()
remove(list = ls())

library(ggplot2)
library(cowplot)

valeurs <- data.frame(read.csv("NQRnatDenatSDS1µM.csv", sep = ',', dec = ",", header = TRUE))

nat <- data.frame(valeurs[which(valeurs$state == "denature"),])
max(nat$OD)

g <- ggplot (valeurs, aes(x= wavelength, y= OD, group = state))
g <- g + geom_line(aes(linetype = state))
g <- g + theme(legend.position = "none")
g <- g + scale_x_continuous(name = "\nWavelength (nm)", limits = c(300, 500))
g <- g + scale_y_continuous(name = "Absorbance\n", limits = c(0, 0.015))

save_plot('.png', g, base_aspect_ratio = 1.3)