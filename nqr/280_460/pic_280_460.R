graphics.off()
remove(list = ls())

library(ggplot2)
library(cowplot)

#valeurs <- data.frame(read.csv("tamp + 5µl NQR ligne base.csv", header = TRUE))
valeurs <- data.frame(read.csv("NQR ox 3.csv", header = TRUE))

colnames(valeurs) <- c("x", "y")

g <- ggplot (valeurs, aes(x, y))
g <- g + geom_line()
g <- g + scale_x_continuous(name = "\nWavelength (nm)", limits = c(250, 500))
g <- g + scale_y_continuous(name = "Absorbance\n", limits = c(0, 0.3))
g
#save_plot('280_460_redoxNQR.png', g, base_aspect_ratio = 1.3)

save_plot('280_460_redoxNQR_3.png', g, base_aspect_ratio = 1.3)
