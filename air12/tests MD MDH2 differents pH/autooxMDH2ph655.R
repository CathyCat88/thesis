graphics.off()
remove(list = ls())

library(ggplot2)

valeurs <- data.frame(read.csv("autooxydation MDH2 ph6.55 1H.csv", sep = ';', dec = ",", header = TRUE))

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
g <- g + geom_line(aes(y= valeurs[,12]), size = .5, colour ="#56B4E9")
g <- g + geom_line(aes(y= valeurs[,13]), size = .5, colour = "black")
g <- g + scale_x_continuous(name = "\nWavelength (nm)", limits = c(300, 400))
g <- g + scale_y_continuous(name = "Absorbance\n", limits = c(0, 0.4))
g <- g + theme(axis.title.x = element_text(size = 20), axis.text.x = element_text(color = "black", size = 18))
g <- g + theme(axis.title.y = element_text(size = 20), axis.text.y = element_text(color = "black", size = 18))
g


ggsave("MDH2Ph6.55.pdf")
ggsave("MDH2Ph6.55.png")
