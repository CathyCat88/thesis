rm(list = ls())
graphics.off()

library(ggplot2)
library(cowplot)

data <- read.csv("catalase_Col.csv", header = TRUE)

#############
###CALCULS###
#############

# calcul de la concentration en oxygène en µmol/mg/min
Concentration <- function(rep_bio, pente, protein) {
  coeff <- c(0.0126, 0.0125)
  return((pente * coeff[rep_bio]) / protein)
}

data$result <- Concentration(data$rep_bio, data$pente, data$protein)

# Normalisation selon WT
Normalisation <- function(genotype, part, rep_bio, rep_tech){
  return(data$result[data$genotype == genotype &
                       data$part == part &
                       data$rep_bio == rep_bio &
                       data$rep_tech == rep_tech] / data$result[data$genotype == "WT" &
                                                                  data$part == part &
                                                                  data$rep_bio == rep_bio &
                                                                  data$rep_tech == rep_tech])
}
data2 <- data.frame(norm = integer(0))

for (j in 1:2) {
  for (i in 1:2) {
    x <- Normalisation(data$genotype, "shoot", j, i)
    y <- Normalisation(data$genotype, "root", j, i)
    print(x)
    print(y)
    data2 <- rbind(data2, data.frame(norm = c(x,y)))
  }
}

data2$genotype <- rep(c("WT", "nqr", "fqr1", "nqrfqr1"), 8)
data2$part <- rep(c("shoot", "root"), each = 4)
data2$rep_bio <- rep(1:2, each = 16)
data2$rep_tech <- rep(1:2, each = 8)

mean <- aggregate(data2$norm, list(data2$genotype, data2$part), mean)

StandErr <- function(x){
  return(sd(x) / sqrt(length(x)))
}
se <- aggregate(data2$norm, list(data2$genotype, data2$part), StandErr)

result <- cbind(mean, se[,3])
colnames(result) <- c("genotype", "part", "moyenne", "se")
result$genotype <- factor(result$genotype, levels = c("WT", "nqr", "fqr1","nqrfqr1"))

result_root <- result[result$part == "root",]
result_shoot <- result[result$part == "shoot",]

#############
###BARPLOT###
#############

#root
g1 <- ggplot(result_root, aes(genotype, moyenne)) +
  geom_bar(stat = "identity", color = "black", 
           width = 0.6, position = position_dodge(0.8)) +
  xlab("") +
  ylab("activité catalase \n(µmol/mg de protéines/min) \n") +
  geom_errorbar(aes(ymin = result_root$moyenne - result_root$se, 
                    ymax = result_root$moyenne + result_root$se), 
                width = 0.05)
save_plot("catalase_norm_Col_root.png", g1, base_aspect_ratio = 1.3)

#shoots
g2 <- ggplot(result_shoot, aes(genotype, moyenne)) +
  geom_bar(stat = "identity", color = "black", 
           width = 0.6, position = position_dodge(0.8)) +
  xlab("") +
  ylab("activité catalase \n(µmol/mg de protéines/min) \n") +
  geom_errorbar(aes(ymin = result_shoot$moyenne - result_shoot$se, 
                    ymax = result_shoot$moyenne + result_shoot$se), 
                width = 0.05)
save_plot("catalase_norm_Col_shoot.png", g2, base_aspect_ratio = 1.3)


###########
###STATS###
###########

data.shoot <- data[data$part == "shoot",]
data.root <- data[data$part == "root",]

#Normalité
shapiroTest_shoot <- aggregate(result ~ genotype, data = data.shoot, 
                         function (x) shapiro.test(x)$p.value) #normal
shapiroTest_root <- aggregate(result ~ genotype, data = data.root, 
                         function (x) shapiro.test(x)$p.value) #normal

# Egalité des variances
for (params in list(c("WT", "nqr"),
                   c("WT", "fqr1"),
                   c("WT", "nqrfqr1"))) {
  
  reference <- params[1]
  genotype <- params[2]
  
  bartlettResult_shoot <- bartlett.test(list(data.shoot$result[data.shoot$genotype == reference],
                                       data.shoot$result[data.shoot$genotype == genotype]))
  
  bartlettResult_root <- bartlett.test(list(data.root$result[data.root$genotype == reference],
                                             data.root$result[data.root$genotype == genotype]))
  
  print(bartlettResult_shoot$p.value)
  print(bartlettResult_root$p.value)
  
  studentResult_shoot <- t.test(data.shoot$result[data.shoot$genotype == reference],
                                data.shoot$result[data.shoot$genotype == genotype])
  studentResult_root <- t.test(data.root$result[data.root$genotype == reference],
                               data.root$result[data.root$genotype == genotype])
}
  


#roots
t.test(data2$norm[data2$genotype == "WT" & data2$part == "root"], 
       data2$norm[data2$genotype == "nqr" & data2$part == "root"])
t.test(data2$norm[data2$genotype == "WT" & data2$part == "root"], 
       data2$norm[data2$genotype == "fqr1" & data2$part == "root"])
t.test(data2$norm[data2$genotype == "WT" & data2$part == "root"], 
       data2$norm[data2$genotype == "nqrfqr1" & data2$part == "root"])

#shoots
t.test(data2$norm[data2$genotype == "WT" & data2$part == "shoot"], 
       data2$norm[data2$genotype == "nqr" & data2$part == "shoot"])
t.test(data2$norm[data2$genotype == "WT" & data2$part == "shoot"], 
       data2$norm[data2$genotype == "fqr1" & data2$part == "shoot"])
t.test(data2$norm[data2$genotype == "WT" & data2$part == "shoot"], 
       data2$norm[data2$genotype == "nqrfqr1" & data2$part == "shoot"])