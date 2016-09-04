rm(list = ls())
graphics.off()

library(ggplot2)
library(cowplot)
library(reshape)

data <- read.csv("croissance_radicule_Col.csv", header = TRUE)
data <- melt(data, id = c("genotype", "repetition"))

mean <- aggregate(data$value, list(data$genotype, data$variable), mean)

StandErr <- function(x){
  return(sd(x) / sqrt(length(x)))
}
se <- aggregate(data$value, list(data$genotype, data$variable), StandErr)

result <- cbind(mean, se[,3])
result[,2] <- NULL
result$temps <- c(rep(0,4), rep(3,4), rep(4,4), rep(5,4), rep(6,4), rep(7,4))
colnames(result) <- c("genotype", "moyenne", "se", "temps")
result$genotype <- factor(result$genotype, levels = (c("WT", "nqr", "fqr1","nqrfqr1")))

g <- ggplot(result, aes(temps, moyenne, 
                        group = genotype)) +
  geom_line(aes(colour = genotype, 
                group = genotype)) +
  xlab("\nTemps (jours)") +
  ylab("Longueur des radicules (cm)\n") +
  geom_errorbar(aes(ymin = result$moyenne - result$se, 
                    ymax = result$moyenne + result$se,
                    colour = genotype,
                    group = genotype), 
                width = 0.2)
save_plot("croissance_radicule_Col.png", g, base_aspect_ratio = 1.3)
