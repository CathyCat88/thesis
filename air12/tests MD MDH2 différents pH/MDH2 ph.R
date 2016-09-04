graphics.off()
remove(list = ls())

library(ggplot2)

valeurs <- data.frame(read.csv("MDH2 ph.csv", sep = ';', dec = ",", header = TRUE))

g <- ggplot (valeurs, aes( x= valeurs[,1]))
g <- g + geom_line(aes(y= valeurs[,2]), size = .5, colour = "#009E73")
g <- g + geom_line(aes(y= valeurs[,3]), size = .5, colour = 'green')
g <- g + geom_line(aes(y= valeurs[,4]), size = .5, colour = "#CC79A7")
g <- g + scale_x_continuous(name = "\nWavelength (nm)", limits = c(300, 400))
g <- g + scale_y_continuous(name = "Absorbance\n", limits = c(0, 0.5))
g <- g + theme(axis.title.x = element_text(size = 20), axis.text.x = element_text(color = "black", size = 18))
g <- g + theme(axis.title.y = element_text(size = 20), axis.text.y = element_text(color = "black", size = 18))
g

ggsave("MDH2 ph.pdf")
ggsave("MDH2 ph.png")