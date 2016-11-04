graphics.off()
remove(list = ls())

library(ggplot2)

valeurs <- data.frame(read.csv("MDH2 ph.csv", sep = ";", dec = ",", header = TRUE))
valeurs <- cbind(valeurs[,1], valeurs[,3:4])
colnames(valeurs) <- c("wavelength", "ph58", "ph8")

df <- melt(valeurs, id.vars="wavelength")

g <- ggplot(df, aes(x =  wavelength, 
                    y = value, 
                    group = variable, 
                    colour = variable, 
                    linetype = variable)) +
  geom_line() +
  theme(legend.position = "none") +
  scale_x_continuous(name = "\nLongueur d'onde (nm)", 
                     expand = c(0,0),
                     limits = c(300, 402)) +
  scale_y_continuous(name = "Absorbance\n", 
                     expand = c(0,0))
g

save_plot("MDH2_pH_fr.png", g, base_aspect_ratio = 1.3)

g <- ggplot(df, aes(x =  wavelength, 
                    y = value, 
                    group = variable, 
                    linetype = variable)) +
  geom_line(size = 1) +
  theme(legend.position = "none") +
  scale_x_continuous(name = "\nWavelength (nm)", 
                     expand = c(0,0),
                     limits = c(300, 402)) +
  scale_y_continuous(name = "Absorbance\n", 
                     expand = c(0,0))
g

save_plot("MDH2_pH.png", g, base_aspect_ratio = 1.3)

