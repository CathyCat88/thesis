graphics.off()
remove(list = ls())

library(ggplot2)
library(cowplot)

germ15 <- data.frame(read.csv("germination15°Csopalin.csv", sep = ',', header = TRUE))

#############
###CALCULS###
#############

pourcentages <- data.frame(germ15$nbreGerme / germ15$total *100)

#regroupement des données avec références
df <- cbind(germ15[, 1:3], pourcentages)
colnames(df) <- c("genotype", "repetitionBio", "repetitionTech", "pourcentages")

mean <- aggregate(df$pourcentages, list(df$genotype), mean)

StandErr <- function(x){
  return(sd(x) / sqrt(length(x)))
}
se <- aggregate(df$pourcentages, list(df$genotype), StandErr)

#tableau final
result <- cbind(mean, se[,2])
colnames(result) <- c("genotype", "moyenne", "se")
result$genotype <- factor(result$genotype, levels = c("WT", "nqr", "air12", "nqrair12"))

#############
###BARPLOT###
#############

g <- ggplot(data = result, 
            aes(genotype, moyenne, 
                group = genotype, 
                fill = genotype)) +
  geom_bar(stat = "identity", 
           color = "black", 
           width = 0.6) +
  theme(legend.position = "none") +
  scale_fill_hue(l = 40, c = 100) +
  xlab("") +
  scale_y_continuous(name = "Germination (%) à 48 heures\n ", 
                     expand = c(0,0), limits = c(0,76)) +
  geom_errorbar(ymin =result$moyenne, 
                ymax = result$moyenne+result$se, width = 0.05)

save_plot('germination15sopLer2.png', g, base_aspect_ratio = 1.3)

###########
###STATS###
###########

#en théorie nécessité de calculer la normalite des données pour calculer se
#H0 loi normale p > 0.05
#H1 loi pas normale  p < 0.05
shapiroTest <- aggregate(df$pourcentages, list(df$genotype), function(x) shapiro.test(x)$p.value)
#tout normal great

df1 <- data.frame(reference = character(0),
                  genotype = character(0),
                  bartlett.value = integer(0),
                  bartlett.pass = logical(0),
                  student.value = integer(0),
                  student.pass = logical(0))

for (params in list(c("WT", "nqr"),
                    c("WT", "air12"),
                    c("WT", "nqrair12"))) {
  reference <- params[1]
  genotype <- params[2]
  
  bartlett <- bartlett.test(list(df$pourcentages[df$genotype == reference],
                                 df$pourcentages[df$genotype == genotype]))
  
  student <- t.test(df$pourcentages[df$genotype == reference],
                    df$pourcentages[df$genotype == genotype])
  
  df1 <- rbind(df1, data.frame(reference = reference,
                               genotype = genotype,
                               bartlett.value = bartlett$p.value,
                               bartlett.pass = (bartlett$p.value > 0.05),
                               student.value = student$p.value,
                               student.pass = (student$p.value > 0.05)))
}