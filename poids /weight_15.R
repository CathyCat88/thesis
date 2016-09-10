graphics.off()
remove(list = ls())

library(ggplot2)
library(cowplot)

data <- read.csv("masseLer15°C4semaines.csv", dec = ",", header = TRUE)
data <- data[data$ecotype == "Ler",]

g1 <- ggplot(data = data[data$genotype == "WT",], 
             aes(weightg)) + 
  geom_histogram(breaks=seq(0, 0.3, by = 0.025), 
                 colour = "black", fill = "white") +
  scale_x_continuous(name = "\n Weight (g)", 
                     expand = c(0,0),
                     breaks = seq(0,0.3, by = 0.05)) +
  scale_y_continuous(name = "Number of plants \n", 
                     expand = c(0,0), limits = c(0, 9), 
                     breaks = seq(0,9, by = 2))

g2 <- ggplot(data = data[data$genotype == "nqr",], 
             aes(weightg)) + 
  geom_histogram(breaks=seq(0, 0.3, by = 0.025), 
                 colour = "black", fill = "white") +
  scale_x_continuous(name = "\n Weight (g)", 
                     expand = c(0,0),
                     breaks = seq(0,0.3, by = 0.05)) +
  scale_y_continuous(name = "Number of plants \n", 
                     expand = c(0,0), limits = c(0, 9), 
                     breaks = seq(0,9, by = 2))

g3 <- ggplot(data = data[data$genotype == "air12",], 
             aes(weightg)) + 
  geom_histogram(breaks=seq(0, 0.3, by = 0.025), 
                 colour = "black", fill = "white") +
  scale_x_continuous(name = "\n Weight (g)", 
                     expand = c(0,0),
                     breaks = seq(0,0.3, by = 0.05)) +
  scale_y_continuous(name = "Number of plants \n", 
                     expand = c(0,0), limits = c(0, 9), 
                     breaks = seq(0,9, by = 2))

g4 <- ggplot(data = data[data$genotype == "nqrair12",], 
             aes(weightg)) + 
  geom_histogram(breaks=seq(0, 0.3, by = 0.025), 
                 colour = "black", fill = "white") +
  scale_x_continuous(name = "\n Weight (g)", 
                     expand = c(0,0),
                     breaks = seq(0,0.3, by = 0.05)) +
  scale_y_continuous(name = "Number of plants \n", 
                     expand = c(0,0), limits = c(0, 9), 
                     breaks = seq(0,9, by = 2))

save_plot("WT_15_weigth.png", g1, base_aspect_ratio = 1.3)
save_plot("nqr_15_weigth.png", g2, base_aspect_ratio = 1.3)
save_plot("air12_15_weigth.png", g3, base_aspect_ratio = 1.3)
save_plot("nqrair12_15_weigth.png", g4, base_aspect_ratio = 1.3)