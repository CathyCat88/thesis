remove(list = ls())
graphics.off()

library("ggplot2")
library("cowplot")

#data <- read.csv("germination_lum25°C_Col.csv", header = TRUE)
data <- read.csv("germination_lum35°C_Col.csv", header = TRUE)

mean <- aggregate(data$germination, list(data$genotype, data$temps), mean)

StandErr <- function(x){
  return(sd(x) / sqrt(length(x)))
}
se <- aggregate(data$germination, list(data$genotype, data$temps), StandErr)

result <- cbind(mean, se[,3])
colnames(result) <- c("genotype", "temps", "moyenne", "se")
result$genotype <- factor(result$genotype, levels = c("WT", "nqr", "fqr1","nqrfqr1"))

g <- ggplot(result, aes(temps, moyenne, group = genotype)) +
  geom_line(aes(colour = genotype, group = genotype), size = 1) +
  xlab("\n Temps (heures)") +
  ylab("Germination (%) \n") +
  ylim(0,100) +
  geom_errorbar(aes(ymin = result$moyenne - result$se, 
                    ymax = result$moyenne + result$se, 
                    colour = genotype, 
                    group = genotype), width = 0.2)
#save_plot("germination_lum25°C.png", g, base_aspect_ratio = 1.3)
save_plot("germination_lum35°C.png", g, base_aspect_ratio = 1.3)
