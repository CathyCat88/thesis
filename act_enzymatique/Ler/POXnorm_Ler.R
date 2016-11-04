remove(list = ls())

library("cowplot")

# importation des données
pox <- read.csv("POXx.csv", header = TRUE)

# calcul de la concentration en nmol/mg/s
Concentration <- function(pente2, pente1, protein) {
  return((pente2 - pente1) *10^9/ (26600 *60* protein))
}

pox$result <- Concentration(pox$pente2, pox$pente1, pox$protein)

# Normalisation selon WT
Normalisation <- function(genotype,repBio, repTech) {
  return((pox$result[pox$genotype == genotype
                     & pox$ecotype == "Ler"
                     & pox$repBIO == repBio
                     & pox$repTech == repTech]) / (pox$result[pox$genotype == "WT"
                                                              & pox$ecotype == "Ler"
                                                              & pox$repBIO == repBio
                                                              & pox$repTech == repTech]))
}
rep <- data.frame(norm = integer(0))

for (j in 1:3) {
  for (i in 1:2) {
    x <- Normalisation(pox$genotype, j, i)
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
df$SE <- c(0.11, 0.04, 0.02, 0.1)
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
g <- g + scale_y_continuous(name = "guaiacol peroxidase activity \n", 
                            expand = c(0,0))
g <- g + geom_errorbar(aes(ymin = df$moyenne, 
                           ymax = df$moyenne+SE), 
                       width = 0.05)
g
save_plot('poxnorm_Ler.png', 
          g, 
          base_aspect_ratio = 1.3)

g <- ggplot(data = df, aes(x=genotype, 
                           y= moyenne,
                           group = genotype,
                           fill = genotype)) 
g <- g + geom_bar(stat = "identity", 
                  color = "black", 
                  width = 0.6, 
                  position = position_dodge(0.8)) 
g <- g + theme(legend.position = "none") 
g <- g + scale_fill_hue(l = 40, c = 100) 
g <- g + xlab("")
g <- g + scale_y_continuous(name = "Activité guaïacol péroxydase \n", 
                            expand = c(0,0))
g <- g + geom_errorbar(aes(ymin = df$moyenne, 
                           ymax = df$moyenne+SE), 
                       width = 0.05)
g
save_plot('poxnorm_Ler_fr.png', 
          g, 
          base_aspect_ratio = 1.3)

#########
# TESTS #
#########

shapiroTest <- aggregate(norm ~ genotype, data = rep, 
                         function (x) shapiro.test(x)$p.value)
#tout normal

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
                          rep$norm[rep$genotype == genotype], var.equal = TRUE)
  
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
