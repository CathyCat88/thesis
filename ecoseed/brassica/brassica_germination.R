remove(list = ls())
graphics.off()

#data <- read.csv("Brassica_germination_aged.csv", header = TRUE)
data <- read.csv("Brassica_germination_aged.csv", header = TRUE)

data$pourcentage <- (data$germination / data$total)*100

moy <- aggregate(data$pourcentage, list(data$genotype, data$traitement, data$temps), mean)

StandErr <- function(x){
  return(sd(x) / sqrt(length(x)))
}
se <- aggregate(data$pourcentage, list(data$genotype, data$traitement, data$temps), StandErr)

result <- cbind(moy, se[,4])
colnames(result) <- c("genotype", "traitement", "temps", "moyenne", "se")
result$traitement <- factor(result$traitement, 
                            levels = c("D", "C", "H"),
                            labels = c("Dessication", "Contrôle", "T haute"))

result.A12 <- result[result$genotype == "A12",]

g1 <- ggplot(data = result.A12, aes(x = temps, 
                               y = moyenne,
                               colour = traitement)) +
  geom_line(size = 1) +
  theme(legend.title = element_blank()) +
  scale_x_continuous(name = "\nTemps(heures)", expand = c(0,0)) +
  scale_y_continuous(name = "Germination (%) \n", expand = c(0,0), limits = c(0,100))
  
result.SL101 <- result[result$genotype == "SL101",]

g2 <- ggplot(data = result.SL101, aes(x = temps, 
                                    y = moyenne,
                                    colour = traitement)) +
  geom_line(size = 1) +
  theme(legend.title = element_blank()) +
  scale_x_continuous(name = "\nTemps(heures)", expand = c(0,0)) +
  scale_y_continuous(name = "Germination (%) \n", expand = c(0,0), limits = c(0,100)) +
  geom_errorbar(aes(ymin = result.SL101$moyenne - result.SL101$se, 
                    ymax = result.SL101$moyenne + result.SL101$se),
                width = 4)

save_plot("brassica_germination_aged_A12.png", g1, base_aspect_ratio = 1.3)
save_plot("brassica_germination_aged_SL101.png", g2, base_aspect_ratio = 1.3)

