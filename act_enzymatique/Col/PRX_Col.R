rm(list = ls())
graphics.off()

library(ggplot2)
library(cowplot)

data <- read.csv("PRX_Col.csv", header = TRUE)

#############
###CALCULS###
#############

# calcul de la concentration en nmol/s/mg
Concentration <- function(pente2, pente1, protein) {
  return(((pente2 - pente1)*10^9) / (26600 * protein * 60))
}

data$result <- Concentration(data$pente2, data$pente1, data$protein)

moyenne <- aggregate(data$result, list(data$genotype, data$part), mean)

StandErr <- function(x){
  return(sd(x) / sqrt(length(x)))
}
se <- aggregate(data$result, list(data$genotype, data$part), StandErr)

df <- cbind(moyenne, se[,3])
colnames(df) <- c("genotype", "part", "moyenne", "se")
df$genotype <- factor(df$genotype, 
                      levels = c("WT", "nqr", "fqr1","nqrfqr1"))

df.shoot <- df[df$part == "shoot",]
df.root <- df[df$part == "root",]

##############
###BARPLOTS###
##############

g1 <- ggplot(df.shoot, 
             aes(genotype, moyenne, fill = genotype)) +
  geom_bar(stat = "identity", 
           colour = "black", 
           width = 0.6) +
  theme(legend.position = "none") +
  xlab("") +
  scale_y_continuous(name = "activité guaiacol péroxydase \n (nmol/mg de protéines/s) \n",
                     expand = c(0,0)) +
  geom_errorbar(aes(ymin = df.shoot$moyenne, 
                    ymax = df.shoot$moyenne + df.shoot$se), 
                width = 0.05)
save_plot("PRX_Col_shoot.png", g1, base_aspect_ratio = 1.3)

g2 <- ggplot(df.root, 
             aes(genotype, moyenne, fill = genotype)) +
  geom_bar(stat = "identity", 
           colour = "black", 
           width = 0.6) +
  theme(legend.position = "none") +
  xlab("") +
  scale_y_continuous(name = "activité guaiacol péroxydase \n (nmol/mg de protéines/s) \n",
                     expand = c(0,0)) +
  geom_errorbar(aes(ymin = df.root$moyenne, 
                    ymax = df.root$moyenne + df.root$se), 
                width = 0.05)
save_plot("PRX_Col_root.png", g2, base_aspect_ratio = 1.3)

###########
###STATS###
###########

data.root <- data[data$part == "root",]

shapiroTest_root <- aggregate(result ~ genotype, data = data.root, 
                              function (x) shapiro.test(x)$p.value) #normal

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

data.shoot <- data[data$part == "shoot",]

shapiroTest_shoot <- aggregate(result ~ genotype, data = data.shoot, 
                              function (x) shapiro.test(x)$p.value) #normal

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
  #pas d'égalité des variances
  
  studentResult_shoot <- t.test(data.shoot$result[data.shoot$genotype == reference],
                               data.shoot$result[data.shoot$genotype == genotype])
  
  result_shoot <- rbind(result_shoot, data.frame(reference=reference,
                                               genotype=genotype,
                                               bartlett=bartlettResult_shoot$p.value,
                                               bartlett.pass=(bartlettResult_shoot$p.value < 0.05),
                                               student=studentResult_shoot$p.value,
                                               student.pass=(studentResult_shoot$p.value > 0.05)))
}

wilcox.test(data.shoot$result[data.shoot$genotype == "WT"],
            data.shoot$result[data.shoot$genotype == "fqr1"])
