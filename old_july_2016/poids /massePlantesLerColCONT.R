graphics.off()
remove(list = ls())

library(ggplot2)
library(matrixStats)

contLer <- data.frame(read.csv("Lermasseplantes3semainesCONT.csv", sep = ',', dec = '.', header = TRUE))
contCol <- data.frame(read.csv("masseCol3semainesCONT.csv", sep = ',', dec = '.', header = TRUE))

p <- ggplot(contLer, aes(contLer$genotype, contLer$masse))
p + geom_boxplot()

p <- ggplot(contCol, aes(contCol$genotype, contCol$masse))
p + geom_boxplot()

WTLer <- data.frame(contLer[which(contLer$genotype == "WT"), ])
moy.WT.Ler <- mean((WTLer$masse))
shapiro.test(as.matrix(WTLer$masse))
qqnorm(as.matrix(WTLer$masse))
qqline(as.matrix(WTLer$masse))
se.WT.Ler <- colSds(as.matrix(WTLer$masse))/(sqrt(length(as.matrix(WTLer$masse))))

nqrLer <- data.frame(contLer[which(contLer$genotype == "nqr"), ])
moy.nqr.Ler <- mean((nqrLer$masse))
shapiro.test(as.matrix(nqrLer$masse))
qqnorm(as.matrix(nqrLer$masse))
qqline(as.matrix(nqrLer$masse))
se.nqr.Ler <- colSds(as.matrix(nqrLer$masse))/(sqrt(length(as.matrix(nqrLer$masse))))

air12Ler <- data.frame(contLer[which(contLer$genotype == "air12"), ])
moy.air12.Ler <- mean((air12Ler$masse))
shapiro.test(as.matrix(air12Ler$masse))
qqnorm(as.matrix(air12Ler$masse))
qqline(as.matrix(air12Ler$masse))
se.air12.Ler <- colSds(as.matrix(air12Ler$masse))/(sqrt(length(as.matrix(air12Ler$masse)))) 

nqrair12Ler <- data.frame(contLer[which(contLer$genotype == "nqrair12"), ])
moy.nqrair12.Ler <- mean((nqrair12Ler$masse))
shapiro.test(as.matrix(nqrair12Ler$masse))
qqnorm(as.matrix(nqrair12Ler$masse))
qqline(as.matrix(nqrair12Ler$masse))
se.nqrair12.Ler <- colSds(as.matrix(nqrair12Ler$masse))/(sqrt(length(as.matrix(nqrair12Ler$masse)))) 

WTCol <- data.frame(contCol[which(contCol$genotype == "WT"), ])
moy.WT.Col <- mean((WTCol$masse))
shapiro.test(as.matrix(WTCol$masse))
qqnorm(as.matrix(WTCol$masse))
qqline(as.matrix(WTCol$masse))
se.WT.Col <- colSds(as.matrix(WTCol$masse))/(sqrt(length(as.matrix(WTCol$masse))))

nqrCol <- data.frame(contCol[which(contCol$genotype == "nqr"), ])
moy.nqr.Col <- mean((nqrCol$masse))
shapiro.test(as.matrix(nqrCol$masse))
qqnorm(as.matrix(nqrCol$masse))
qqline(as.matrix(nqrCol$masse))
se.nqr.Col <- colSds(as.matrix(nqrCol$masse))/(sqrt(length(as.matrix(nqrCol$masse))))

moy <- data.frame(c(moy.WT.Ler, moy.nqr.Ler, moy.air12.Ler, moy.nqrair12.Ler, moy.WT.Col, moy.nqr.Col))
genotype <- c("WT\nLer", "nqr\nLer", "air12\nLer", "nqrair12\nLer","WT\nCol", "nqr\nCol")
moy$genotype <- factor(genotype, levels = c("WT\nLer", "nqr\nLer", "air12\nLer", "nqrair12\nLer","WT\nCol", "nqr\nCol"))
colnames(moy) <- c("moyenne", "genotype")

se <- data.frame(c(se.WT.Ler, se.nqr.Ler, se.air12.Ler, se.nqrair12.Ler, se.WT.Col, se.nqr.Col))
se$genotype <- factor(se, levels = c("WT\nLer", "nqr\nLer", "air12\nLer", "nqrair12\nLer","WT\nCol", "nqr\nCol"))
colnames(se) <- c("stand.error", "genotype")

#3 test de l'homogénéité des variances
# Test de Bartlett
# H0 :  variances égales
# H1 :  variances différentes
# si p-value > 0.05, on accepte H0 : homogénéité des variances
# si 0 < p-value < 0.05 rejet de H0, on accepte H1 : pas d'homogénéité
bartlett.test(list(WTLer$masse, nqrLer$masse, air12Ler$masse, nqrair12Ler$masse))
# on accepte H0, homogénéité variances ok

bartlett.test(list(WTCol$masse, nqrCol$masse))
# on accepte H0, homogénéité variances ok

t.test(WTLer$masse, nqrLer$masse, var.equal = TRUE) # non égalité des moyennes
t.test(WTLer$masse, air12Ler$masse, var.equal = TRUE) # non égalité des moyennes
t.test(WTLer$masse, nqrair12Ler$masse, var.equal = TRUE) # non égalité des moyennes
t.test(WTCol$masse, nqrCol$masse, var.equal = TRUE) # égalité des moyennes

cont2 <-  data.frame((read.csv("Lermasseplantes3semainesCONT.csv", sep = ',', dec = '.', header = TRUE)), stringsAsFactors = FALSE)
Anova2 <-  aov(masse ~ genotype, data = cont2)
# néanmoins, différences non significatives
summary(Anova2)

cont3 <-  data.frame((read.csv("masseCol3semainesCONT.csv", sep = ',', dec = '.', header = TRUE)), stringsAsFactors = FALSE)
Anova3 <-  aov(masse ~ genotype, data = cont3)
# néanmoins, différences non significatives
summary(Anova3)

g <- ggplot(data = moy, aes(x=moy$genotype, y= moy$moyenne))
g <- g + geom_bar(stat = "identity", color = "black", width = 0.6, position = position_dodge(0.8))
g <- g + labs(#title = "Germination of seeds at 15°C under continuous light", 
  x = "", 
  y="weight of plants (g)\n")
g <- g + geom_errorbar(aes(ymin =moy$moyenne, ymax = moy$moyenne+se$stand.error), width = 0.05)

save_plot('massePlantesLerColCONT.png', g, base_aspect_ratio = 1.3)


