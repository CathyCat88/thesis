remove(list = ls())
graphics.off()

data <- read.csv("germination_tousQR.csv", header = TRUE)

mean <- aggregate(data$germination, list(data$genotype, data$traitement), mean)

StandErr <- function(x){
  return(sd(x) / sqrt(length(x)))
}
se <- aggregate(data$germination, list(data$genotype, data$traitement), StandErr)

result <- cbind(mean, se[,3])
