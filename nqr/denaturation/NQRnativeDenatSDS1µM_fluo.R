graphics.off()
remove(list = ls())

library(ggplot2)
library(cowplot)

valeurs <- data.frame(read.csv("NQRnativeDenatSDS1µM_fluo.csv", sep = ',', header = TRUE))
colnames(valeurs) <- c("wavelength", "state", "fluo")

g <- ggplot (valeurs, aes(x= wavelength, y= fluorescence.intensity..a.u.., group = state))
g <- g + geom_line(aes(linetype = state))
g <- g + theme(legend.position = "none")
g <- g + scale_x_continuous(name = "\nWavelength (nm)", limits = c(480, 600), breaks = c(500, 550, 600))
g <- g + scale_y_continuous(name = "Fluorescence intensity (a.u)\n", limits = c(0, 10))

save_plot('NQRnativeDenatSDS1µM_fluo.png', g, base_aspect_ratio = 1.3)

g <- ggplot (valeurs, aes(x= wavelength, y= fluo, 
                          colour = factor(state, 
                                          labels = c("NQR native", "NQR dénaturée"))))
g <- g + geom_line()
g <- g +theme(legend.title = element_blank())
g <- g + scale_color_manual(values = c("red", "blue"))
g <- g + scale_x_continuous(name = "\nLongueur d'onde (nm)", limits = c(480, 600), breaks = c(500, 550, 600))
g <- g + scale_y_continuous(name = "Intensité de fluorescence (a.u)\n", limits = c(0, 10))
g

save_plot('NQRnativeDenatSDS1µM_fluo_fr.png', g, base_aspect_ratio = 1.3)
