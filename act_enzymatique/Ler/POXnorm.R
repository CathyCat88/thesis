remove(list = ls())

library("cowplot")

# importation des données
pox <- read.csv("POXx.csv", sep = ",", dec = ".", header = TRUE)

# calcul de la concentration en nkat/mg
Concentration <- function(pente2, pente1, protein) {
  return((pente2 - pente1) * 10^9 / (26600 * protein * 60))
}

pox$result <- Concentration(pox$pente2, pox$pente1, pox$protein)

# Normalisation selon WT
Normalisation <- function(genotype, ecotype, repBio, repTech) {
  return((pox$result[pox$genotype == genotype
                     & pox$ecotype == ecotype
                     & pox$repBIO == repBio
                     & pox$repTech == repTech]) / (pox$result[pox$genotype == "WT"
                                                              & pox$ecotype == ecotype
                                                              & pox$repBIO == repBio
                                                              & pox$repTech == repTech]))
}
rep <- data.frame(norm = integer(0))

for (j in 1:3) {
  for (i in 1:2) {
    x <- Normalisation(pox$genotype, "Ler", j, i)
    y <- Normalisation(pox$genotype, "Col", j, i)
    print(x)
    print(y)
    rep <- rbind(rep, data.frame(norm = c(x,y)))
  }
}

rep$genotype <- rep(c("WTLer", "nqrLer", "air12Ler", "nqrair12Ler", "WTCol", "nqrCol"), 6)
rep$repBio <- rep(c(1, 2, 3), each = 12)
rep$repTech <- rep(c(1,2), each = 6)

# calcul de la moyenne des [O2]/genotype
df <- aggregate(rep$norm, list(rep$genotype), mean)
colnames(df) <- c("genotype", "moyenne")

# calcul de l'erreur standard
StandErr <- function(x) {
  se <- sd(x)/sqrt(length(x))
}

SE <- aggregate(rep$norm, list(rep$genotype), StandErr)
df$SE <- SE$x

###########
# BARPLOT #
###########

df$ordre <- c(3, 4, 6, 2, 5, 1)
df <- df[order(df$ordre), ]
df$gen <- factor(c("WT\nLer", "nqr\nLer", "air12\nLer", "nqrair12\nLer","WT\nCol", "nqr\nCol"), 
                 levels = c("WT\nLer", "nqr\nLer", "air12\nLer", "nqrair12\nLer", "WT\nCol", "nqr\nCol"))

g <- ggplot(data = df, aes(x=df$gen, 
                           y= df$moyenne)) 
g <- g + geom_bar(stat = "identity", 
                  color = "black", 
                  width = 0.6, 
                  position = position_dodge(0.8)) 
g <- g +labs(x = "", 
             y="guaiacol peroxidase activity \n(nkat/mg of protein\n")
g <- g + geom_errorbar(aes(ymin = df$moyenne, 
                           ymax = df$moyenne+SE), 
                       width = 0.05)
g
save_plot('poxnorm.png', 
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

for (params in list(c("WTLer", "nqrLer"),
                    c("WTLer", "air12Ler"),
                    c("WTLer", "nqrair12Ler"),
                    c("WTCol", "nqrCol"))) {
  
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