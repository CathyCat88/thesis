remove(list = ls())
graphics.off()

library("ggplot2")
library("cowplot")

#data <- read.csv("Col_hypoxie_sansStrat.csv", header = TRUE)
data <- read.csv("Col_hypoxie_strat.csv", header = TRUE)

pr <- (data$germination / data$total)*100
data$pr <- pr

mn <- aggregate(data$pr, list(data$genotype, data$oxygene), mean)

StandErr <- function(x){
  return(sd(x) / sqrt(length(x)))
}
se <- aggregate(data$pr, list(data$genotype, data$oxygene), StandErr)

result <- cbind(mn, se$x)
colnames(result) <- c("genotype", "oxygene", "moyenne", "se")
result$genotype <- factor(result$genotype, 
                          levels = c("WT", "nqr", "fqr1","nqrfqr1"))

g <- ggplot(data = result, 
            aes(x = oxygene, 
                y = moyenne)) +
  geom_line(data = result,
            aes(colour = genotype, 
                group = genotype)) +
  xlab("\nOxygene (%)") +
  ylab("Germination (%) \n") +
  geom_errorbar(data = result, 
                aes(ymin = result$moyenne - result$se, 
                    ymax = result$moyenne + result$se, 
                    colour = genotype, 
                    group = genotype), 
                width = 0.2)

#save_plot('Col_hypoxie_sansStrat.png', g, base_aspect_ratio = 1.3)
save_plot('Col_hypoxie_strat.png', g, base_aspect_ratio = 1.3)
