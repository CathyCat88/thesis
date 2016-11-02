graphics.off()
remove(list = ls())

library(ggplot2)
library(cowplot)
library(reshape2)

data <- read.csv("ControleTitrationNQR.csv", header = TRUE)
df.600 <- data[data$wavelength == 600,]

result <- data.frame(abs = numeric(0))

for (i in (2:5)) {
  x <- data[,i] - data[,i][data[,1] == 600]
  result <- rbind(result, data.frame(abs = x))
}

data2 <- melt(data, id = "wavelength")

result <- cbind(result, data2[,1:2])

result$abs[result$variable == "FMN_libre"] <- result$abs[result$variable == "FMN_libre"]/2.05

g <- ggplot(data = result,
            aes(x = wavelength, 
                y = abs, 
                color = factor(variable, 
                               labels = c("FMN libre", "NQR oxydé", "NQR réduit", "NQR fin de titration"))))+
  geom_line() +
  theme(legend.position = "none") +
  theme(legend.title = element_blank()) +
  scale_x_continuous(name = "\nLongueur d'onde (nm)",
                     expand = c(0,0),
                     limits = c(400, 602)) +
  scale_y_continuous(name = "Absorbance\n",
                     expand = c(0,0),
                     limits = c(0,0.33))
g

save_plot("control_titration_NQR_fr.png", g, base_aspect_ratio = 1.3)

g <- ggplot(data = result,
            aes(x = wavelength, 
                y = abs, 
                linetype = variable))+
  geom_line(size = 1) +
  scale_linetype_manual(values = c("solid", "dashed", "twodash", "dotted")) +
  theme(legend.position = "none") +
  scale_x_continuous(name = "\nWavelength (nm)",
                     expand = c(0,0),
                     limits = c(400, 605)) +
  scale_y_continuous(name = "Absorbance\n",
                     expand = c(0,0),
                     limits = c(0,0.33))
g
save_plot("control_titration_NQR.png", g, base_aspect_ratio = 1.3)

