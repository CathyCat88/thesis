remove(list = ls())

library("cowplot")

# importation des données
cat <- read.csv("CATt.csv", sep = ",", dec = ".", header = TRUE)

# calcul de la concentration en oxygène en nmol/mg/s
Concentration <- function(rep, pente, protein) {
  coeff <- c(0.01315, 0.0146, 0.014)
  return((pente * coeff[rep] *10^3) / (protein * 60))
}

cat$result <- Concentration(cat$repBIO, cat$pente, cat$protein)

aggregate(cat$result, list(cat$genotype, cat$ecotype, cat$repBIO), mean)

# Normalisation des moyennes selon WT
Normalisation <- function(genotype, repBio, repTech) {
  return((cat$result[cat$genotype == genotype
                     & cat$ecotype == "Ler"
                     & cat$repBIO == repBio
                     & cat$repTech == repTech]) / (cat$result[cat$genotype == "WT"
                                                              & cat$ecotype == "Ler"
                                                              & cat$repBIO == repBio
                                                              & cat$repTech == repTech]))
}
rep <- data.frame(norm = integer(0))

for (j in 1:3) {
  for (i in 1:2) {
    x <- Normalisation(cat$genotype, j, i)
    rep <- rbind(rep, data.frame(norm = x))
  }
}

rep$genotype <- rep(c("WT", "nqr", "air12", "nqrair12"), 6)
rep$repBio <- rep(c(1, 2, 3), each = 8)
rep$repTech <- rep(c(1,2), each = 4)

# calcul de la moyenne des [O2]/genotype
df <- aggregate(rep$norm, list(rep$genotype), mean)
colnames(df) <- c("genotype", "moyenne")

# calcul de l'erreur standard
#StandErr <- function(x) {
#  se <- sd(x)/sqrt(length(x))
#}

#SE <- aggregate(rep$norm, list(rep$genotype), StandErr)
#df$SE <- SE$x
#df$genotype <- factor(df$genotype, levels = c("WT", "nqr", "air12", "nqrair12"))
df$SE <- c(0.07, 0.25, 0.07, 0.13)
df$genotype <- factor(df$genotype, levels = c("WT", "nqr", "air12", "nqrair12"))


###########
# BARPLOT #
###########

g <- ggplot(data = df, aes(x=genotype, 
                           y= moyenne)) 
g <- g + geom_bar(stat = "identity", 
                  color = "black", 
                  width = 0.6, 
                  position = position_dodge(0.8)) 
g <- g + xlab("") 
g <- g + scale_y_continuous(name = "catalase activity \n", 
                            expand = c(0,0))
g <- g + geom_errorbar(aes(ymin = df$moyenne, 
                           ymax = df$moyenne+SE), 
                       width = 0.05)
g
save_plot('CATnorm_Ler.png', 
          g, 
          base_aspect_ratio = 1.3)

g <- ggplot(data = df, aes(x=genotype, 
                           y= moyenne,
                           group = genotype,
                           fill = genotype)) 
g <- g + geom_bar(stat = "identity", 
                  color = "black", 
                  width = 0.6) 
g <- g + theme(legend.position = "none") 
g <- g + scale_fill_hue(l = 40, c = 100) 
g <- g + xlab("") 
g <- g + scale_y_continuous(name = "Activité catalase\n", 
                            expand = c(0,0))
g <- g + geom_errorbar(aes(ymin = df$moyenne, 
                           ymax = df$moyenne+SE), 
                       width = 0.05)
g
save_plot('CATnorm_Ler_fr.png', 
          g, 
          base_aspect_ratio = 1.3)


#########
# TESTS #
#########

#shapiroTest <- aggregate(norm ~ genotype, data = rep, 
                         #function (x) shapiro.test(x)$p.value)

#shapiroTest$normality <- shapiroTest$result > 0.05
#shapiroTest

# création d'un dataframe vide pour pouvoir
# stocker les résultats des tests

df <- data.frame(reference=character(0),
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
  
  bartlettResult <- bartlett.test(list(rep$norm[rep$genotype == reference],
                                       rep$norm[rep$genotype == genotype]))
  
  print(bartlettResult$p.value)
  
  studentResult <- t.test(rep$norm[rep$genotype == reference],
                          rep$norm[rep$genotype == genotype])
  
  print(studentResult$p.value)
  
  df <- rbind(df, data.frame(reference=reference,
                             genotype=genotype,
                             bartlett=bartlettResult$p.value,
                             bartlett.pass=(bartlettResult$p.value > 0.05),
                             student=studentResult$p.value,
                             student.pass=(studentResult$p.value > 0.05)))
}

stats <- lm(rep$norm ~ rep$genotype + rep$repBio)
summary(stats)
anova(stats)

out <- HSD.test(stats, 'rep$genotype')
