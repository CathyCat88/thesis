remove(list = ls())

library("cowplot")

# importation des données
cat <- read.csv("CAT.csv", sep = ",", dec = ".", header = TRUE)

# calcul de la concentration en oxygène en µmol/ml
Concentration <- function(rep, pente, protein) {
  coeff <- c(0.01315, 0.0146, 0.014)
  return((pente * coeff[rep] * 10^3) / protein)
}

# calcul de la moyenne des [O2]/genotype
cat$result <- Concentration(cat$repBIO, cat$pente, cat$protein)
df <- aggregate(cat$result, list(cat$genotype), mean)
colnames(df) <- c("genotype", "moyenne")

# calcul de l'erreur standard
StandErr <- function(x) {
  se <- sd(x)/sqrt(length(x))
}

SE <- aggregate(cat$result, list(cat$genotype), StandErr)
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
             y="oxygen production \n(nmol/mg of protein/min)\n")
g <- g + geom_errorbar(aes(ymin = df$moyenne, 
                           ymax = df$moyenne+SE), 
                       width = 0.05)
g
save_plot('CAT.png', 
          g, 
          base_aspect_ratio = 1.3)


#########
# TESTS #
#########

shapiroTest <- aggregate(result ~ genotype, data = cat, 
                         function (x) shapiro.test(x)$p.value)

shapiroTest$normality <- shapiroTest$result > 0.05
shapiroTest

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
  
  bartlettResult <- bartlett.test(list(cat$result[cat$genotype == reference],
                                      cat$result[cat$genotype == genotype]))
  
  print(bartlettResult$p.value)
  
  studentResult <- t.test(var.equal = TRUE,
                          cat$result[cat$genotype == reference],
                          cat$result[cat$genotype == genotype])
  
  print(studentResult$p.value)
  
  df <- rbind(df, data.frame(reference=reference,
                             genotype=genotype,
                             bartlett=bartlettResult$p.value,
                             bartlett.pass=(bartlettResult$p.value > 0.05),
                             student=studentResult$p.value,
                             student.pass=(studentResult$p.value > 0.05)))
}