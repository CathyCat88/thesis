graphics.off()
remove(list = ls())

library(ggplot2)
library(cowplot)
library(reshape2)

valeurs <- data.frame(read.csv("reconstitutionNQRfluo.csv", sep = ',', dec = ".", header = TRUE))

df <- melt(valeurs, id.vars="wavelength")

colnames(df) <- c("wavelength", "group", "fluorescence")

g <- ggplot (df, aes(x= wavelength, y= fluorescence, 
                     group = group, colour = group))
g <- g + geom_line()
g <- g + theme(legend.position = "none")
g <- g + scale_x_continuous(name = "\nWavelength (nm)", limits = c(470, 602), expand = c(0,0))
g <- g + scale_y_continuous(name = "Fluorescence intensity\n (a.u.)\n", limits = c(0, 300), expand = c(0,0))
g
save_plot('reconstitutionNQRfluo.png', g, base_aspect_ratio = 1.3)

g <- ggplot (df, aes(x= wavelength, y= fluorescence, 
                     group = group, colour = group))
g <- g + geom_line()
g <- g + theme(legend.position = "none")
g <- g + scale_x_continuous(name = "\nLongueur d'onde (nm)", 
                            limits = c(475, 602),
                            expand = c(0, 0))
g <- g + scale_y_continuous(name = "Intensité de fluorescence\n (a.u.)\n", 
                            limits = c(0, 300),
                            expand = c(0,0))
g
save_plot('reconstitutionNQRfluo_fr.png', g, base_aspect_ratio = 1.3)
