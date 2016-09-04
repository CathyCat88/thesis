remove(list = ls())
graphics.off()

library(ggplot2)
library(reshape2)
library(cowplot)

data <- read.csv("gammeFMN120.csv", header = TRUE)
data <- data[,-c(2,3,4)]
colnames(data) <- c("wavelength", "1.25", "0.6", "0.3", "0.16", "0.08", "0.04", "0")

data2 <- melt(data, id = "wavelength")
colnames(data2) <- c("wavelength", "FMN", "fluo")

g1 <- ggplot(data2, aes(x = wavelength, y = fluo, 
                        colour = FMN)) +
  geom_line() +
  scale_x_continuous(name = "\nLongueur d'onde (nm)", 
                     expand = c(0,0)) +
  scale_y_continuous(name = "Intensité de fluorescence (a.u.) \n",
                      expand = c(0,0))
save_plot("gammeFMN120_fr_tout.png", g1, base_aspect_ratio = 1.3)

data3 <- data2[data2$wavelength > 525 & data2$wavelength < 530,]
result <- aggregate(data3$fluo, list(data3$FMN), max)
colnames(result) <- c("FMN", "fluo")
result$FMN <- as.numeric(as.character(result$FMN))

g2 <- ggplot(result, aes(x = FMN, y = fluo)) +
  geom_point() +
  scale_x_continuous(name = "\nFMN (µM)",
                     expand = c(0,0.01),
                     limits = c(0,1.3)) +
  scale_y_continuous("Intensité de fluorescence (a.u.) \n",
                     expand = c(0,0),
                     limits = c(0,16)) +
  geom_smooth(method = "lm", se = FALSE) +
  geom_text(data = data.frame(), aes(4.5, 30, label = "y = 10.98 x + 2.14, R**2 = 0.98"))
save_plot("gammeFMN120_fr.png", g2, base_aspect_ratio = 1.3)

model = lm(data = result, fluo ~ FMN)
summary(model)
with(result, cor(fluo, FMN, use = "everything", method = "pearson"))

