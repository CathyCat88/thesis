remove(list = ls())
graphics.off()

library(agricolae)

data <- read.csv('masseGrainesPlantes_Col.csv', header = TRUE)

data$repetition <- factor(data$repetition)

stats <- lm(data$masse ~ data$genotype + data$repetition)
summary(stats)
anova(stats)

a1 <- aov(data$masse ~ data$genotype + data$repetition)
posthoc <- TukeyHSD(x=a1, 'data$genotype', conf.level=0.95)

out <- HSD.test(stats, 'data$genotype')

aggregate(data$masse, list(data$genotype), mean)

StandErr <- function(x){
  return(sd(x) / sqrt(length(x)))
}

aggregate(data$masse, list(data$genotype), StandErr)
