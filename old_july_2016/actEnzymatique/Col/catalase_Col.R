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

moyenne <- aggregate(data$result, list(data$genotype, data$part), mean)

StandErr <- function(x){
  return(sd(x) / sqrt(length(x)))
}

se <- aggregate(data$result, list(data$genotype, data$part), StandErr)

df <- cbind(moyenne, se[,3])
colnames(df) <- c("genotype", "part", "moyenne", "se")
df$genotype <- factor(df$genotype, levels = c("WT", "nqr", "fqr1","nqrfqr1"))

#############
###BARPLOT###
#############

df.shoot <- df[df$part == "shoot",]
df.root <- df[df$part == "root",]

g1 <- ggplot(df.shoot, aes(genotype, moyenne)) +
  geom_bar(stat = "identity", color = "black", 
           width = 0.6, position = position_dodge(0.8)) + 
  xlab("") +
  ylab("activité catalase \n(µmol/mg de protéines/min) \n") +
  geom_errorbar(data = df.shoot, 
                aes(ymin = moyenne - se,
                    ymax = moyenne + se), 
                width = 0.05)
save_plot("catalase_Col_shoot.png", g1, base_aspect_ratio = 1.3)

g2 <- ggplot(df.root, aes(genotype, moyenne)) +
  geom_bar(stat = "identity", color = "black", 
           width = 0.6, position = position_dodge(0.8)) + 
  xlab("") +
  ylab("activité catalase \n(µmol/mg de protéines/min) \n") +
  geom_errorbar(data = df.root, 
                aes(ymin = moyenne - se,
                    ymax = moyenne + se), 
                width = 0.05)
save_plot("catalase_Col_root.png", g2, base_aspect_ratio = 1.3)

#Normalité

data.shoot <- data[data$part == "shoot",]
data.root <- data[data$part == "root",]

shapiroTest_shoot <- aggregate(result ~ genotype, data = data.shoot, 
                               function (x) shapiro.test(x)$p.value) #normal
shapiroTest_root <- aggregate(result ~ genotype, data = data.root, 
                              function (x) shapiro.test(x)$p.value) #normal

# tests
result_shoot <- data.frame(reference=character(0),
                 genotype=character(0),
                 bartlett=numeric(0),
                 bartlett.pass=logical(0),
                 student=numeric(0),
                 student.pass=logical(0))

for (params in list(c("WT", "nqr"),
                    c("WT", "fqr1"),
                    c("WT", "nqrfqr1"))) {
  
  reference <- params[1]
  genotype <- params[2]
  
  bartlettResult_shoot <- bartlett.test(list(data.shoot$result[data.shoot$genotype == reference],
                                             data.shoot$result[data.shoot$genotype == genotype]))
  #égalité des variances

  studentResult_shoot <- t.test(data.shoot$result[data.shoot$genotype == reference],
                                data.shoot$result[data.shoot$genotype == genotype])
  
  result_shoot <- rbind(result_shoot, data.frame(reference=reference,
                             genotype=genotype,
                             bartlett=bartlettResult_shoot$p.value,
                             bartlett.pass=(bartlettResult_shoot$p.value < 0.05),
                             student=studentResult_shoot$p.value,
                             student.pass=(studentResult_shoot$p.value > 0.05)))
}

result_root <- data.frame(reference=character(0),
                           genotype=character(0),
                           bartlett=numeric(0),
                           bartlett.pass=logical(0),
                           student=numeric(0),
                           student.pass=logical(0))

for (params in list(c("WT", "nqr"),
                    c("WT", "fqr1"),
                    c("WT", "nqrfqr1"))) {
  
  reference <- params[1]
  genotype <- params[2]
  
  bartlettResult_root <- bartlett.test(list(data.root$result[data.root$genotype == reference],
                                            data.root$result[data.root$genotype == genotype]))
  #pas d'égalité des variances
  
  studentResult_root <- t.test(data.root$result[data.root$genotype == reference],
                               data.root$result[data.root$genotype == genotype])
  
  result_root <- rbind(result_root, data.frame(reference=reference,
                             genotype=genotype,
                             bartlett=bartlettResult_root$p.value,
                             bartlett.pass=(bartlettResult_root$p.value < 0.05),
                             student=studentResult_root$p.value,
                             student.pass=(studentResult_root$p.value > 0.05)))
}

  