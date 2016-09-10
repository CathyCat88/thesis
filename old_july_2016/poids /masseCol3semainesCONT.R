rm(list = ls())
graphics.off()

library(ggplot2)
library(cowplot)

data <- read.csv("masseCol3semainesCONT.csv", header = TRUE)

mean <- aggregate(data$masse, list(data$genotype), mean)

StandErr <- function(x){
  return(sd(x) / sqrt(length(x)))
}
se <- aggregate(data$masse, list(data$genotype), StandErr)

result <- cbind(mean, se[,2])
colnames(result) <- c("genotype", "moyenne", "se")
result$genotype <- factor(result$genotype, levels = c("WT", "nqr", "fqr1","nqrfqr1"))

g <- ggplot(result, aes(genotype, moyenne)) +
  geom_bar(stat = "identity", colour = "black", width = 0.6, position = position_dodge(0.8)) +
  xlab(" ") +
  ylab("masse (g)\n") +
  geom_errorbar(aes(ymin = result$moyenne, ymax = result$moyenne + result$se), width = 0.05)
save_plot("masseCol3semainesCONT.png", g, base_aspect_ratio = 1.3)