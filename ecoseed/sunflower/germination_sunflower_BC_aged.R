remove(list = ls())

data <- read.csv("germination_sunflower_BC_aged.csv", header = TRUE)

data$pourcentage <- (data$germinated / data$total)*100

mean <- aggregate(data$pourcentage, list(data$genotype, data$time), mean)

StandErr <- function(x){
  return(sd(x) / sqrt(length(x)))
}
se <- aggregate(data$pourcentage, list(data$genotype, data$time), StandErr)

genotype <- mean[,1]
treatment <- rep("yes", times = nrow(mean))
temps <- mean[,2]
germination <- mean[,3]
se <- se[,3]
result <- data.frame(genotype, treatment, temps, germination, se)

write.table(result, file = "germination_sunflower_BC_aged_2.csv", sep = ",", dec = ".")
