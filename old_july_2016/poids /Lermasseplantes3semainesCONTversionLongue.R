rm(list = ls())
graphics.off()

require(ggplot2) #librairy pour graphiques

Ler <-  data.frame((read.csv("~/Documents/R/Lermasseplantes.csv", header = TRUE)), stringsAsFactors = FALSE)

#1 tracer le boxplot pour voir s'il y a des outliers
boxplot(Ler, ylim = c(0, 0.2),
        whisklty = 1,
        boxwex = 0.5,
        staplelty = 0,
        main = "Plantes de 3 semaines", 
        ylab = "masse des plantes (g)", 
        xlab = "genotype", 
        cex.lab = 1.3,
        cex.main = 1.5,
        cex.axis = 1.2) # aucun outliers ok

#2 vérification de la normalité des données
# tracé de la densité pour chaque échantillon à tester
zones <- matrix(c(1,2,3,4), ncol = 2) 
layout(zones)

hist.WT <- hist(Ler$WT, freq = FALSE, xlim = c(0, 0.2), breaks = 5,
                main = 'WT Ler', xlab = 'masse (g)', ylab = 'densite de la population',
                cex.lab = 1.5, cex.axis = 1.5)

hist.nqr <- hist(Ler$nqr, freq = FALSE, xlim = c(0, 0.2),
                 main = 'nqr Ler', xlab = 'masse (g)', ylab = 'densite de la population', 
                 cex.lab = 1.5, cex.axis = 1.5)

hist.air12 <- hist(Ler$air12, freq = FALSE, xlim = c(0, 0.2),
                 main = 'air12 Ler', xlab = 'masse (g)', ylab = 'densite de la population', 
                 cex.lab = 1.5, cex.axis = 1.5)

hist.nqrair12 <- hist(Ler$nqrair12, freq = FALSE, xlim = c(0, 0.2),
                 main = 'nqrair12 Ler', xlab = 'masse (g)', ylab = 'densite de la population', 
                 cex.lab = 1.5, cex.axis = 1.5)

# qqplot un peu mieux pour tester la normalité que hist()
qqnorm(Ler[,1])
qqline(Ler[,1])
qqnorm(Ler[,2])
qqline(Ler[,2])
qqnorm(Ler[,3])
qqline(Ler[,3])
qqnorm(Ler[,4])
qqline(Ler[,4])

# encore mieux, test de Shapiro-Wilk
# H0 =  la variable suit une loi normale avec une erreur de 5%
# H1 = la variable ne suit pas une loi normale avec une erreur de 5%
# p-value : probabilité que la différence observée entre les 2 distributions soit due au hasard
# si p-value > 0.05, on accepte H0 : normalité
# si 0 < p-value < 0.05 rejet de H0, on accepte H1 : pas de normalité
shapiro.test(Ler$WT)
shapiro.test(Ler$nqr)
shapiro.test(Ler$air12)
shapiro.test(Ler$nqrair12)
# ici on accepte H0 ok

#3 test de l'homogénéité des variances
# Test de Bartlett
# H0 :  variances égales
# H1 :  variances différentes
# si p-value > 0.05, on accepte H0 : homogénéité des variances
# si 0 < p-value < 0.05 rejet de H0, on accepte H1 : pas d'homogénéité
bartlett.test(list(Ler$WT, Ler$nqr, Ler$air12, Ler$nqrair12))
# on accepte H0, homogénéité variances ok

# calcul des variances peut être pas nécessaire
var.WT <- var(Ler$WT)
var.nqr <- var(Ler$nqr, na.rm = TRUE)
var.air12 <- var(Ler$air12, na.rm = TRUE)
var.nqrair12 <- var(Ler$nqrair12)
df.var <- data.frame(var.WT, var.nqr, var.air12, var.nqrair12)
df.var

#4 tracé de l'histogramme des données avec barres d'erreur
genotype <- factor(c("WT", "nqr", "air12", "nqrair12"), levels = c("WT", "nqr", "air12", "nqrair12")) # classement 

means <- colMeans(Ler, na.rm = TRUE)*10^3 

sds <- sapply(Ler, na.rm = TRUE, FUN = sd)*10^3

df <- data.frame(genotype, means, sds)

ggplot(df, aes(x = genotype, y = means)) +
  geom_bar(stat="identity", fill="dark grey") + ylim(c(0,140)) +
  labs(x = "", y = "masse par plante (g)") +
  theme(axis.text.y = element_text(size = 14)) +
  theme(axis.title.y = element_text(size = 16)) +
  theme(axis.text.x = element_text(size = 15)) +
  ggtitle("Masse de plantes Ler de 3 semaines") + 
  theme(plot.title = element_text(size=16, face = "bold", margin = margin(10,0,10,0))) +
  geom_errorbar(aes(ymin=means - sds, ymax=means + sds), width=0.2, colour = "dark grey")

#5 tests statistiques

# test de student des variables WT et des autres, tests 2 à 2
# pas idéal car on veut comparer les 3 au WT
# H0 :il n'y a pas de différence entre les moyennes des masses de plantes mutantes et du sauvage
# H1 : il y a une différence à 5%
# si p-value > 0.05 : on accepte H0 : aucune différence
# si 0 < p-value < 0.05 rejet de H0, on accepte H1 : des différences statistiques
Ler.corrige <- na.omit(Ler)
# var.equal = TRUE veut dire qu'on considère les variances comme égale, pas de correction
t.test(Ler.corrige[,1], Ler.corrige[,2], var.equal = TRUE) # WT différent de nqr
t.test(Ler.corrige[,1], Ler.corrige[,3], var.equal = TRUE) # WT différent de air12
t.test(Ler.corrige[,1], Ler.corrige[,4], var.equal = TRUE) # WT = nqrair12

# ne rien mettre revient par défaut à var.equal = FALSE : correction de Welch
t.test(Ler.corrige[,1], Ler.corrige[,2]) # WT différent de nqr
t.test(Ler.corrige[,1], Ler.corrige[,3]) # WT différent de air12
t.test(Ler.corrige[,1], Ler.corrige[,4]) # WT = nqrair12

# mieux: one-way ANOVA 
# même chose on vérifie que la p-value > 0.05 : égalité des moyennes
Anova <-  aov(Ler$WT ~ Ler$nqr + Ler$air12 + Ler$nqrair12)
summary(Anova)
# on accepte H0, les 4 moyennes sont égales

oneway.test(masse ~ genotype, data = Ler2, var.equal = TRUE)
anova(lm(masse ~ genotype, data = Ler2))

# ici on compare directement les 4 genotypes ensemble. Attention c'est R qui choisit le seuil d'erreur alpha
# alpha  = 10 % c'est trop pour moi
Ler2 <-  data.frame((read.csv("~/Documents/R/Lermasseplantesmodifanova.csv", header = TRUE)), stringsAsFactors = FALSE)
Anova2 <-  aov(masse ~ genotype, data = Ler2)
# néanmoins, différences non significatives
summary(Anova2)
