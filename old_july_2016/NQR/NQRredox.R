graphics.off()
remove(list = ls())

library(ggplot2)
library(cowplot)

valeurs <- data.frame(read.csv("redox NQRgroup.csv", sep = ',', header = TRUE))

g <- ggplot (valeurs, aes(x= Wavelength, y= DO, group = ox))
g <- g + geom_line(aes(linetype = ox))
g <- g + theme(legend.position = "none")
g <- g + scale_x_continuous(name = "\nWavelength (nm)", limits = c(300, 500))
g <- g + scale_y_continuous(name = "Absorbance\n", limits = c(0, 0.025))

save_plot('redoxNQR.png', g, base_aspect_ratio = 1.3)