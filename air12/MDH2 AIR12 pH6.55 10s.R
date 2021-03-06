graphics.off()
remove(list = ls())

library(ggplot2)
library(cowplot)
library(reshape2)

valeurs <- data.frame(read.csv("1µl AIR12 1h MDH2 ph6.55 ttes 10s.csv", sep = ';', dec = ".", header = TRUE))

c <- 1:ncol(valeurs[-1])
df <- valeurs[-1][ , c%%6==1]
df <- cbind(df[1:31], valeurs[1])
colnames(df) <- c(c(1:31), "wavelength")
df1 <- melt(df, id.vars="wavelength")

g <- ggplot (df1, aes(x= wavelength, y= value, group = variable, color = variable))
g <- g + geom_line()
g <- g + theme(legend.position = "none")
g <- g + scale_x_continuous(name = "\nWavelength (nm)")
g <- g + scale_y_continuous(name = "absorbance\n")
g
save_plot('MDH2 AIR12 ph6.55 1min.png', g, base_aspect_ratio = 1.3)

df2 <- df1[1:3216,]

g <- ggplot (df2, aes(x= wavelength, y= value, group = variable))
g <- g + geom_line()
g <- g + theme(legend.position = "none")
g <- g + scale_x_continuous(name = "\nWavelength (nm)", expand = c(0,0), limits = c(300,401))
g <- g + scale_y_continuous(name = "Absorbance change\n", expand = c(0,0), limits = c(0,0.45))
g
save_plot('MDH2_AIR12_ph6.55_1min_NOIR.png', g, base_aspect_ratio = 1.3)
