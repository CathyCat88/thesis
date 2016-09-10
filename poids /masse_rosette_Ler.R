remove(list = ls())
graphics.off()

library("ggplot2")
library("cowplot")


control <- read.csv("Lermasseplantes3semainesCONT.csv", header  = TRUE)

stress <- read.csv("masseLer15°C4semaines.csv", header  = TRUE, dec = ",")
stress <- stress[stress$ecotype == "Ler",]

##############
###BOXPLOTS###
##############

control$genotype <- factor(control$genotype, levels = c("WT", "nqr", "air12", "nqrair12"))

g1 <- ggplot(control, aes(genotype, masse, group = genotype, fill = genotype)) + 
  geom_boxplot(width = 0.8) +
  scale_fill_hue(l = 40, c = 100) +
  xlab("") +
  ylab("Masse des rosettes (g) \n") +
  theme(legend.position = "none")
save_plot("masse_rosettes_Ler_cont.png", g1, base_aspect_ratio = 1.3)

stress$genotype <- factor(stress$genotype, levels = c("WT", "nqr", "air12", "nqrair12"))

g2 <- ggplot(stress, aes(genotype, weightg, group = genotype, fill = genotype)) + 
  geom_boxplot(width = 0.8) +
  scale_fill_hue(l = 40, c = 100) +
  xlab("") +
  ylab("Masse des rosettes (g) \n") +
  theme(legend.position = "none")
save_plot("masse_rosettes_Ler_stress.png", g2, base_aspect_ratio = 1.3)

#############
###CALCULS###
#############

StandErr <- function(x) {
  se <- sd(x)/sqrt(length(x))
}

mean_cont <- aggregate(control$masse, list(control$genotype), mean)
se_cont <- aggregate(control$masse, list(control$genotype), StandErr)

mean_stress <- aggregate(stress$weightg, list(stress$genotype), mean)
se_stress <- aggregate(stress$weightg, list(stress$genotype), StandErr)

###########
###STATS###
###########

shapiroTest_cont <- aggregate(masse ~ genotype, data = control, 
                         function (x) shapiro.test(x)$p.value)
shapiroTest_stress <- aggregate(weightg ~ genotype, data = stress, 
                              function (x) shapiro.test(x)$p.value)
#toutes les distributions sont normales

result_cont <- data.frame(reference=character(0),
                     genotype=character(0),
                     bartlett=numeric(0),
                     bartlett.pass=logical(0),
                     student=numeric(0),
                     student.pass=logical(0))


for (params in list(c("WT", "nqr"),
                    c("WT", "air12"),
                    c("WT", "nqrair12"))) {
  
  reference <- params[1]
  genotype <- params[2]
  
  bartlettResult <- bartlett.test(list(control$masse[control$genotype == reference],
                                       control$masse[control$genotype == genotype]))
  
  studentResult <- t.test(control$masse[control$genotype == reference],
                                control$masse[control$genotype == genotype], var.equal = TRUE)
  
  result_cont <- rbind(result_cont, data.frame(reference=reference,
                                     genotype=genotype,
                                     bartlett=bartlettResult$p.value,
                                     bartlett.pass=(bartlettResult$p.value > 0.05),
                                     student=studentResult$p.value,
                                     student.pass=(studentResult$p.value > 0.05)))
}

result_stress <- data.frame(reference=character(0),
                          genotype=character(0),
                          bartlett=numeric(0),
                          bartlett.pass=logical(0),
                          student=numeric(0),
                          student.pass=logical(0))


for (params in list(c("WT", "nqr"),
                    c("WT", "air12"),
                    c("WT", "nqrair12"))) {
  
  reference <- params[1]
  genotype <- params[2]
  
  bartlettResult <- bartlett.test(list(stress$weightg[stress$genotype == reference],
                                       stress$weightg[stress$genotype == genotype]))
  
  studentResult <- t.test(stress$weightg[stress$genotype == reference],
                          stress$weightg[stress$genotype == genotype], var.equal = TRUE)
  
  result_stress <- rbind(result_stress, data.frame(reference=reference,
                                               genotype=genotype,
                                               bartlett=bartlettResult$p.value,
                                               bartlett.pass=(bartlettResult$p.value > 0.05),
                                               student=studentResult$p.value,
                                               student.pass=(studentResult$p.value > 0.05)))
}

wilcox.test(stress$weightg[stress$genotype == "WT"],
                        stress$weightg[stress$genotype == "nqr"])
