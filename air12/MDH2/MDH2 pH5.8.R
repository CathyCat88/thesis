graphics.off()
remove(list = ls())

library(ggplot2)
library(cowplot)
library(reshape2)

valeurs <- data.frame(read.csv("MDH2 ph5.8.csv", sep = ';', dec = ",", header = TRUE))
colnames(valeurs) <- c("wavelength", "0", "300", "600", "900", "1200", "1500", "1800", "2100", "2400", "2700")

df <- melt(valeurs, id.vars="wavelength")

colnames(df) <- c("wavelength", "group", "absorbance")

g <- ggplot (df, aes(x= wavelength, y= absorbance, group = group, color = group))
g <- g + geom_line()
g <- g + theme(legend.position = "none")
g <- g + scale_x_continuous(name = "\nWavelength (nm)") #, limits = c(480, 600))
g <- g + scale_y_continuous(name = "Absorbance\n")#, limits = c(0, 300))
g
save_plot('MDH2 ph5.8.png', g, base_aspect_ratio = 1.3)




graphics.off()
remove(list = ls())

library(ggplot2)



g <- ggplot (valeurs, aes( x= valeurs[,1]))
g <- g + geom_line(aes(y= valeurs[,2]), size = .5, colour = "#F0E442")
g <- g + geom_line(aes(y= valeurs[,3]), size = .5, colour = 'green')
g <- g + geom_line(aes(y= valeurs[,4]), size = .5, colour = "#0072B2")
g <- g + geom_line(aes(y= valeurs[,5]), size = .5, colour = "#D55E00")
g <- g + geom_line(aes(y= valeurs[,6]), size = .5, colour =  "#CC79A7")
g <- g + geom_line(aes(y= valeurs[,7]), size = .5, colour = '#009E73')
g <- g + geom_line(aes(y= valeurs[,8]), size = .5, colour ='blue')
g <- g + geom_line(aes(y= valeurs[,9]), size = .5, colour ='red')
g <- g + geom_line(aes(y= valeurs[,10]), size = .5, colour = "#999999")
g <- g + geom_line(aes(y= valeurs[,11]), size = .5, colour ="#E69F00")
g <- g + scale_x_continuous(name = "\nWavelength (nm)", limits = c(300, 400))
g <- g + scale_y_continuous(name = "Absorbance\n", limits = c(0, 0.5))
g <- g + theme(axis.title.x = element_text(size = 20), axis.text.x = element_text(color = "black", size = 18))
g <- g + theme(axis.title.y = element_text(size = 20), axis.text.y = element_text(color = "black", size = 18))
g

ggsave("MDH2 pH5.8.pdf")
ggsave("MDH2 pH5.8.png")
