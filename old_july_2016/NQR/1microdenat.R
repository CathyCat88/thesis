graphics.off()
remove(list = ls())

library(ggplot2)

df <- data.frame(read.csv("1microdenat.csv", sep = ',', header = TRUE))

g <- ggplot(df, aes(x= df[,1]))
g <- g + geom_line(aes(y = df[,2]), colour="red", size = 1.5)
g <- g + geom_line(aes(y = df[,3]), colour="blue", size = 1.5)
g <- g + theme_bw()
g <- g + scale_x_continuous(name = "Wavelength (nm)", limits = c(470, 600))
g <- g + scale_y_continuous(name = "fluorescence \n intensity (a.u.)", limits = c(0, 10))
g <- g + theme(axis.title.x = element_text(size = 28), axis.text.x = element_text(color = "black", size = 22))
g <- g + theme(axis.title.y = element_text(size = 28), axis.text.y = element_text(color = "black", size = 22))
g

ggsave("1microdenatNQR.pdf")
ggsave("1microdenatNQR.png")