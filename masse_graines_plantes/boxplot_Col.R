remove(list = ls())
graphics.off()

library("ggplot2")
library("cowplot")

data <- read.csv("masseGrainesPlantes_Col.csv", header = TRUE)
data$masse <- (data$masse)*1000

##############
###BOXPLOTS###
##############

data$genotype <- factor(data$genotype, levels = c("WT", "nqr", "fqr1", "nqrfqr1", "fqr2", "fqr1fqr2", "nqrfqr1fqr2"))

g <- ggplot(data, aes(genotype, masse, fill = genotype)) + 
  geom_boxplot(width = 0.8) +
  xlab("") +
  ylab("masse de graines par plante (mg) \n") +
  theme(axis.text.x = element_text(angle = 60, vjust = 0.5),
        legend.position = "none")
save_plot("masse_tous_genotypes_boxplot.png", g, base_aspect_ratio = 1.3)

data2 <- data[data$genotype == "WT" | 
                data$genotype == "nqr" | 
                data$genotype == "fqr1" | 
                data$genotype == "nqrfqr1",]

data2$genotype <- factor(data2$genotype, levels = c("WT", "nqr", "fqr1", "nqrfqr1"))

aggregate(data2$masse, list(data2$genotype), mean)
StandErr <- function(x) {
  se <- sd(x)/sqrt(length(x))
}
aggregate(data2$masse, list(data2$genotype), StandErr)

g1 <- ggplot(data2, aes(genotype, masse, fill = genotype)) + 
  geom_boxplot(width = 0.8) +
  xlab("") +
  ylab("masse de graines par plante (mg) \n") +
  theme(legend.position = "none")
save_plot("masse_4genotypes_boxplot.png", g1, base_aspect_ratio = 1.3)

###########
###STATS###
###########

shapiroTest <- aggregate(masse ~ genotype, data = data2, 
                         function (x) shapiro.test(x)$p.value)
#toutes les distributions sont normales sauf WT

result <- data.frame(reference=character(0),
                           genotype=character(0),
                           bartlett=numeric(0),
                           bartlett.pass=logical(0),
                           wilcoxon=numeric(0),
                           wilcoxon.pass=logical(0))


for (params in list(c("WT", "nqr"),
                    c("WT", "fqr1"),
                    c("WT", "nqrfqr1"))) {
  
  reference <- params[1]
  genotype <- params[2]
  
  bartlettResult <- bartlett.test(list(data2$masse[data2$genotype == reference],
                                       data2$masse[data2$genotype == genotype]))
  
  wilcoxonResult <- wilcox.test(data2$masse[data2$genotype == reference],
                                data2$masse[data2$genotype == genotype], exact = TRUE)
  
  result <- rbind(result, data.frame(reference=reference,
                                                 genotype=genotype,
                                                 bartlett=bartlettResult$p.value,
                                                 bartlett.pass=(bartlettResult$p.value > 0.05),
                                                 wilcoxon=wilcoxonResult$p.value,
                                     wilcoxon.pass=(wilcoxonResult$p.value > 0.05)))
}
  

