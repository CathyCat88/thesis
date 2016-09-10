remove(list = ls())
graphics.off()

library("ggplot2")
library("cowplot")

data <- read.csv("lumière5C3jstrat.csv", header = TRUE)

pr <- (data[, c("X0", "X1", "X2", "X3", "X4", "X7")] / data$total)*100
data1 <- cbind(data[,1:2], pr)

data2 <- melt(data1, id = c("genotype", "ecotype"))
mean <- aggregate(data2$value, list(data2$genotype, data2$ecotype, data2$variable), mean)

StandErr <- function(x){
  return(sd(x) / sqrt(length(x)))
}
se <- aggregate(data2$value, list(data2$genotype, data2$ecotype, data2$variable), StandErr)

result <- cbind(mean, se[,4])
result$temps <- c(rep(0,8), rep(1,8), rep(2,8), rep(3,8), rep(4,8), rep(7,8))
result[,3] <- NULL
colnames(result) <- c("genotype", "ecotype", "moyenne", "se", "temps")

result.Col <- result[result$ecotype == "Col",]
result.Col$genotype <- factor(result.Col$genotype, levels = c("WT", "nqr", "fqr1","nqrfqr1"))

g1 <- ggplot(result.Col, 
             aes(temps, moyenne,
                 group = genotype)) +
  geom_line(aes(colour = genotype,
                group = genotype), size = 1) +
  xlab("\nTemps (jours)") +
  ylab("Germination (%) \n") +
  geom_errorbar(aes(ymin = result.Col$moyenne - result.Col$se,
                    ymax = result.Col$moyenne + result.Col$se,
                    colour = genotype,
                    group = genotype), 
                width = 0.2)
save_plot("germination_lum5°C_Col.png", g1, base_aspect_ratio = 1.3)

result.Ler <- result[result$ecotype == "Ler",]
result.Ler$genotype <- factor(result.Ler$genotype, levels = c("WT", "nqr", "air12","nqrair12"))

g2 <- ggplot(result.Ler, 
             aes(temps, moyenne,
                 group = genotype)) +
  geom_line(aes(colour = genotype,
                group = genotype), size = 1) +
  xlab("\nTemps (jours)") +
  ylab("Germination (%) \n") +
  geom_errorbar(aes(ymin = result.Ler$moyenne - result.Ler$se,
                    ymax = result.Ler$moyenne + result.Ler$se,
                    colour = genotype,
                    group = genotype), 
                width = 0.2)
save_plot("germination_lum5°C_Ler.png", g2, base_aspect_ratio = 1.3)
