remove(list = ls())
library("ggplot2")
library("cowplot")

data <- read.csv("masseGrainesPlantes.csv", header = TRUE)

data$genotype <- factor(data$genotype, levels = c("WT", "nqr", "fqr1", "nqrfqr1", "fqr2", "fqr1fqr2", "nqrfqr1fqr2"))

gen1 <- c("WT", "nqr", "fqr1", "nqrfqr1")
df1 <- data[data$genotype == "WT" | 
              data$genotype == "nqr" | 
              data$genotype == "fqr1" | 
              data$genotype == "nqrfqr1",]

for (g in unique(df1$genotype)) {
  d <- df1[df1$genotype == g,]
  p <- boxplot(masse ~ genotype, data = df1, ylab = "masse (g)", las = 2)
  save_plot("boxplot1.png", p)
}

for (g in unique(df1$genotype)) {
  d <- df1[df1$genotype == g,]
  p <- ggplot(d, aes(genotype, masse)) +
    geom_boxplot()
  save_plot("boxplot1.png", p)
}

