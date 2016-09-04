remove(list = ls())
graphics.off()

library("ggplot2")
library("cowplot")

#data <- read.csv("Col_sansStrat_obs_temp.csv", header = TRUE)
data <- read.csv("Col_strat_obs_temp.csv", header = TRUE)

#data <- read.csv("Ler_sansStrat_obs_temp.csv", header = TRUE)
#data <- read.csv("Ler_strat_obs_temp.csv", header = TRUE)

pr <- (data[, c("X3days", "X7days")] / data$total)*100
pr$genotype <- data$genotype
pr$temperature <- data$temperature

mn3 <- aggregate(pr$X3days, list(pr$genotype, pr$temperature), mean)
mn7 <- aggregate(pr$X7days, list(pr$genotype, pr$temperature), mean)

StandErr <- function(x){
  return(sd(x) / sqrt(length(x)))
}
se3 <- aggregate(pr$X3days, list(pr$genotype, pr$temperature), StandErr)
se7 <- aggregate(pr$X7days, list(pr$genotype, pr$temperature), StandErr)
res7 <- cbind(mn7$x, se7$x)

result <- cbind(mn3, se3$x)
result <- cbind(result, res7)
colnames(result) <- c("genotype", "temperature", 
                      "moyenne_3j", "standardError_3j", 
                      "moyenne_7j", "standardError_7j")
result$genotype <- factor (result$genotype, levels = c("WT", "nqr", "fqr1","nqrfqr1"))

g3 <- ggplot(data = result, 
       aes(x = temperature, 
           y = moyenne_3j)) +
  geom_line(data = result, 
            aes(colour = genotype, 
                group = genotype)) +
  ylim(c(0,100)) +
  xlab("\n Temperature (°C)") +
  ylab("Germination (%) \n") +
  geom_errorbar(aes(ymin = result$moyenne_3j - result$standardError_3j, 
                    ymax = result$moyenne_3j + result$standardError_3j,
                    colour = genotype,
                    group = genotype),
                width = 0.2)

g7 <- ggplot(data = result, 
            aes(x = temperature, 
                y = moyenne_7j)) +
  geom_line(data = result, 
            aes(colour = genotype, 
                group = genotype)) +
  ylim(c(0,100)) +
  xlab("\n Temperature (°C)") +
  ylab("Germination (%) \n") +
  geom_errorbar(aes(ymin = result$moyenne_7j - result$standardError_7j, 
                    ymax = result$moyenne_7j + result$standardError_7j,
                    colour = genotype,
                    group = genotype),
                width = 0.2)

#save_plot('Col_sansStrat_obs_temp3j.png', g3, base_aspect_ratio = 1.3)
#save_plot('Col_sansStrat_obs_temp7j.png', g7, base_aspect_ratio = 1.3)

save_plot('Col_strat_obs_temp3j.png', g3, base_aspect_ratio = 1.3)
save_plot('Col_strat_obs_temp7j.png', g7, base_aspect_ratio = 1.3)

#save_plot('Ler_sansStrat_obs_temp3j.png', g3, base_aspect_ratio = 1.3)
#save_plot('Ler_sansStrat_obs_temp7j.png', g7, base_aspect_ratio = 1.3)

#save_plot('Ler_strat_obs_temp3j.png', g3, base_aspect_ratio = 1.3)
#save_plot('Ler_strat_obs_temp7j.png', g7, base_aspect_ratio = 1.3)


# data[data$genotype == 'nqr', c("X3days", "X7days")] / data$total


