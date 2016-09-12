remove(list = ls())
graphics.off()

data <- read.csv("hordeum_germination_nonaged.csv", header = TRUE)
#data <- read.csv("hordeum_germination_aged.csv", header = TRUE)


data$pourcentage <- (data$germination / data$total)*100

moy <- aggregate(data$pourcentage, list(data$genotype, data$traitement, data$temps), mean)

StandErr <- function(x){
  return(sd(x) / sqrt(length(x)))
}
se <- aggregate(data$pourcentage, list(data$genotype, data$traitement, data$temps), StandErr)

result <- cbind(moy, se[,4])
colnames(result) <- c("genotype", "traitement", "temps", "moyenne", "se")
result$traitement <- factor(result$traitement, 
                            levels = c("C", "D", "H"),
                            labels = c("Contrôle", "Sécheresse", "T haute"))

result.2110 <- result[result$genotype == "2110",]

g1 <- ggplot(data = result.2110, aes(x = temps, 
                                    y = moyenne,
                                    colour = traitement)) +
  geom_line(size = 1) +
  geom_point(size = 2.5) +
  scale_color_hue(l = 40, c = 100) +
  theme(legend.title = element_blank()) +
  scale_x_continuous(name = "\nTemps(heures)", expand = c(0,0)) +
  scale_y_continuous(name = "Germination (%) \n", expand = c(0,0), limits = c(0,100)) +
  geom_errorbar(aes(ymin = result.2110$moyenne - result.2110$se, 
                    ymax = result.2110$moyenne + result.2110$se),
                width = 4)

result.4710 <- result[result$genotype == "4710",]

g2 <- ggplot(data = result.4710, aes(x = temps, 
                                     y = moyenne,
                                     colour = traitement)) +
  geom_line(size = 1) +
  geom_point(size = 2.5) +
  scale_color_hue(l = 40, c = 100) +
  theme(legend.title = element_blank()) +
  scale_x_continuous(name = "\nTemps(heures)", expand = c(0,0)) +
  scale_y_continuous(name = "Germination (%) \n", expand = c(0,0), limits = c(0,100)) +
  geom_errorbar(aes(ymin = result.4710$moyenne - result.4710$se, 
                    ymax = result.4710$moyenne + result.4710$se),
                width = 4)

#save_plot("hordeum_germination_nonaged_2110.png", g1, base_aspect_ratio = 1.3)
#save_plot("hordeum_germination_nonaged_4710.png", g2, base_aspect_ratio = 1.3)

#save_plot("hordeum_germination_aged_2110.png", g1, base_aspect_ratio = 1.3)
#save_plot("hordeum_germination_aged_4710.png", g2, base_aspect_ratio = 1.3)
