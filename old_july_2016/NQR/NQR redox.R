graphics.off()
remove(list = ls())

library(ggplot2)

valeurs <- data.frame(read.csv("NQR/redox NQRgroup.csv", sep = ',', header = TRUE))

g <- ggplot (valeurs, aes(x= Wavelength, y= DO, group = ox))
g <- g + geom_line(aes(linetype = ox), size = 0.8)
g <- g + theme(legend.text = element_text(size=17))
g <- g + theme(legend.title=element_blank())
g <- g + theme_bw()
g <- g + scale_x_continuous(name = "\nWavelength (nm)", limits = c(300, 500))
g <- g + scale_y_continuous(name = "Absorbance\n", limits = c(0, 0.025))
g <- g + theme(axis.title.x = element_text(size = 20), axis.text.x = element_text(color = "black", size = 18))
g <- g + theme(axis.title.y = element_text(size = 20), axis.text.y = element_text(color = "black", size = 18))
g

ggsave("redox NQR.pdf")
ggsave("redox NQR.png")