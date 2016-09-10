remove(list = ls())
library("ggplot2")
library("cowplot")

data <- read.csv("masseGrainesPlantes.csv", header = TRUE)

# data[data$genotype == "WT",] # doc lookup: dataframe subscript, filter

for (g in unique(data$genotype)) {
  d <- data[data$genotype == g,]
  p <- ggplot(d, aes(x = masse)) +
    geom_histogram(aes(y = ..density..),
                   binwidth = 0.02, 
                   colour = "black", 
                   fill = "white") +
    geom_density(aes(y = ..density..),
                 alpha= 0.2, 
                 fill="#FF6666")
  save_plot(sprintf("%s.png", g), p)
}

?geom_histogram()

