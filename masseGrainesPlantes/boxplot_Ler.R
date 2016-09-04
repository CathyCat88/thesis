remove(list = ls())
graphics.off()

library("ggplot2")
library("cowplot")

data <- read.csv("masseGrainesPlantes_Ler.csv", header = TRUE)
data$masse <- (data$masse)*1000
data2 <- data[data$repetition != 1,]

##############
###BOXPLOTS###
##############

data2$genotype <- factor(data2$genotype, levels = c("WT", "nqr", "air12", "nqrair12"))

g <- ggplot(data2, aes(genotype, masse, group = genotype, fill = genotype)) + 
  geom_boxplot(width = 0.8) +
  scale_fill_hue(l = 40, c = 100) +
  xlab("") +
  ylab("masse de graines par plante (mg) \n") +
  theme(legend.position = "none")
save_plot("masse_boxplot_Ler.png", g, base_aspect_ratio = 1.3)

aggregate(data2$masse, list(data2$genotype), mean)
StandErr <- function(x) {
  se <- sd(x)/sqrt(length(x))
}
aggregate(data2$masse, list(data2$genotype), StandErr)

###########
###STATS###
###########

shapiroTest <- aggregate(masse ~ genotype, data = data2, 
                         function (x) shapiro.test(x)$p.value)
#toutes les distributions sont normales

result <- data.frame(reference=character(0),
                           genotype=character(0),
                           bartlett=numeric(0),
                           bartlett.pass=logical(0),
                           wilcoxon=numeric(0),
                           wilcoxon.pass=logical(0))


for (params in list(c("WT", "nqr"),
                    c("WT", "air12"),
                    c("WT", "nqrair12"))) {
  
  reference <- params[1]
  genotype <- params[2]
  
  bartlettResult <- bartlett.test(list(data2$masse[data2$genotype == reference],
                                       data2$masse[data2$genotype == genotype]))
  
  wilcoxonResult <- wilcox.test(data2$masse[data2$genotype == reference],
                                data2$masse[data2$genotype == genotype])
  
  result <- rbind(result, data.frame(reference=reference,
                                                 genotype=genotype,
                                                 bartlett=bartlettResult$p.value,
                                                 bartlett.pass=(bartlettResult$p.value > 0.05),
                                                 wilcoxon=wilcoxonResult$p.value,
                                     wilcoxon.pass=(wilcoxonResult$p.value < 0.05)))
}
  

