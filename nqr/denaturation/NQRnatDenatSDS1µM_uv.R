graphics.off()
remove(list = ls())

library(ggplot2)
library(cowplot)

valeurs <- data.frame(read.csv("NQRnatDenatSDS1µM_uv.csv", sep = ',', dec = ",", header = TRUE))

g <- ggplot (valeurs, aes(x= wavelength, y= OD, group = state))
g <- g + geom_line(aes(linetype = state))
g <- g + theme(legend.position = "none")
g <- g + scale_x_continuous(name = "\nWavelength (nm)", limits = c(300, 500))
g <- g + scale_y_continuous(name = "Absorbance\n", limits = c(0, 0.015))

save_plot('NQRnatDenatSDS1µM_uv.png', g, base_aspect_ratio = 1.3)

g <- ggplot (valeurs, aes(x= wavelength, y= OD, colour = factor(state, 
                                                                labels = c("NQR native", "NQR dénaturée"))))
g <- g + geom_line()
g <- g + scale_color_manual(values = c("red", "blue"))
g <- g +theme(legend.title = element_blank())
g <- g + scale_x_continuous(name = "\nLongueur d'onde (nm)", limits = c(300, 500), expand = c(0,0))
g <- g + scale_y_continuous(name = "Absorbance\n", limits = c(0, 0.015), expand = c(0,0))
g

save_plot('NQRnatDenatSDS1µM_uv_fr.png', g, base_aspect_ratio = 1.3)
