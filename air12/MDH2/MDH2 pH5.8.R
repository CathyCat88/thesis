graphics.off()
remove(list = ls())

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

g <- ggplot (df, aes(x= wavelength, y= absorbance, group = group, color = group))
g <- g + geom_line()
g <- g + theme(legend.position = "none")
g <- g + scale_x_continuous(name = "\nLongueur d'onde (nm)", limits = c(300, 402), expand = c(0,0))
g <- g + scale_y_continuous(name = "Absorbance\n", limits = c(0, 0.48), expand = c(0,0))
g
save_plot('MDH2_ph5.8_fr.png', g, base_aspect_ratio = 1.3)