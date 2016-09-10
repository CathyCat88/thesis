graphics.off()
remove(list = ls())

library(ggplot2)
library(matrixStats)
library(cowplot)

froid <- data.frame(read.csv("masseLer15°C4semaines.csv", sep = ',', dec = ',', header = TRUE))
Ler <- froid[which(froid$ecotype == 'Ler'),]
Col <- froid[which(froid$ecotype == 'Col'),]

pLer <- ggplot(Ler, aes(Ler$genotype, Ler$weightg))
pLer + geom_boxplot()

pCol <- ggplot(Col, aes(Col$genotype, Col$weightg))
pCol + geom_boxplot()

WTLer <- data.frame(froid[which(froid$genotype == 'WT' & froid$ecotype == 'Ler'), ])
moy.WTLer <- mean((WTLer$weightg))
shapiro.test(as.matrix(WTLer$weightg))
qqnorm(as.matrix(WTLer$weightg))
qqline(as.matrix(WTLer$weightg))
se.WTLer <- colSds(as.matrix(WTLer$weightg))/(sqrt(length(as.matrix(WTLer$weightg))))

nqrLer <- data.frame(froid[which(froid$genotype == 'nqr' & froid$ecotype == 'Ler'), ])
moy.nqrLer <- mean((nqrLer$weightg))
shapiro.test(as.matrix(nqrLer$weightg))
qqnorm(as.matrix(nqrLer$weightg))
qqline(as.matrix(nqrLer$weightg))
se.nqrLer <- colSds(as.matrix(nqrLer$weightg))/(sqrt(length(as.matrix(nqrLer$weightg))))

air12Ler <- data.frame(froid[which(froid$genotype == 'air12' & froid$ecotype == 'Ler'), ])
moy.air12Ler <- mean((air12Ler$weightg))
shapiro.test(as.matrix(air12Ler$weightg))
qqnorm(as.matrix(air12Ler$weightg))
qqline(as.matrix(air12Ler$weightg))
se.air12Ler<- colSds(as.matrix(air12Ler$weightg))/(sqrt(length(as.matrix(air12Ler$weightg)))) 

nqrair12Ler <- data.frame(froid[which(froid$genotype == 'nqrair12' & froid$ecotype == 'Ler'), ])
moy.nqrair12Ler <- mean((nqrair12Ler$weightg))
shapiro.test(as.matrix(nqrair12Ler$weightg))
qqnorm(as.matrix(nqrair12Ler$weightg))
qqline(as.matrix(nqrair12Ler$weightg))
se.nqrair12Ler <- colSds(as.matrix(nqrair12Ler$weightg))/(sqrt(length(as.matrix(nqrair12Ler$weightg)))) 

WTCol <- data.frame(froid[which(froid$genotype == 'WT' & froid$ecotype == 'Col'), ])
moy.WTCol <- mean((WTCol$weightg))
shapiro.test(as.matrix(WTCol$weightg))
qqnorm(as.matrix(WTCol$weightg))
qqline(as.matrix(WTCol$weightg))
se.WTCol <- colSds(as.matrix(WTCol$weightg))/(sqrt(length(as.matrix(WTCol$weightg))))

nqrCol <- data.frame(froid[which(froid$genotype == 'nqr' & froid$ecotype == 'Col'), ])
moy.nqrCol <- mean((nqrCol$weightg))
shapiro.test(as.matrix(nqrCol$weightg))
qqnorm(as.matrix(nqrCol$weightg))
qqline(as.matrix(nqrCol$weightg))
se.nqrCol <- colSds(as.matrix(nqrCol$weightg))/(sqrt(length(as.matrix(nqrCol$weightg))))

moyLer <- data.frame(c(moy.WTLer, moy.nqrLer, moy.air12Ler, moy.nqrair12Ler))
genotype <- c("WT", "nqr", "air12", "nqrair12")
moyLer$genotype <- factor(genotype, levels = c("WT", "nqr", "air12", "nqrair12"))
colnames(moyLer) <- c("moyenne", "genotype")

seLer <- data.frame(c(se.WTLer, se.nqrLer, se.air12Ler, se.nqrair12Ler))
seLer$genotype <- factor(genotype, levels = c("WT", "nqr", "air12", "nqrair12"))
colnames(seLer) <- c("stand.error", "genotype")

moyCol <- data.frame(c(moy.WTCol, moy.nqrCol))
genotype <- c("WT", "nqr")
moyCol$genotype <- factor(genotype, levels = c("WT", "nqr"))
colnames(moyCol) <- c("moyenne", "genotype")

seCol <- data.frame(c(se.WTCol, se.nqrCol))
seCol$genotype <- factor(genotype, levels = c("WT", "nqr"))
colnames(seCol) <- c("stand.error", "genotype")

#3 test de l'homogénéité des variances
# Test de Bartlett
# H0 :  variances égales
# H1 :  variances différentes
# si p-value > 0.05, on accepte H0 : homogénéité des variances
# si 0 < p-value < 0.05 rejet de H0, on accepte H1 : pas d'homogénéité
bartlett.test(list(WTLer$weightg, nqrLer$weightg, air12Ler$weightg, nqrair12Ler$weightg))
# on accepte H0, homogénéité variances ok
bartlett.test(list(WTCol$weightg, nqrCol$weightg))
# on accepte H0, homogénéité variances ok

t.test(WTLer$weightg, nqrLer$weightg, var.equal = TRUE) # égalité des moyennes
t.test(WTLer$weightg, air12Ler$weightg, var.equal = TRUE) # égalité des moyennes
t.test(WTLer$weightg, nqrair12Ler$weightg, var.equal = TRUE) # égalité des moyennes
t.test(WTCol$weightg, nqrCol$weightg, var.equal = TRUE) # égalité des moyennes

froid2 <-  data.frame((read.csv("masseLer15°C4semaines.csv", sep = ',', dec = ',', header = TRUE)), stringsAsFactors = FALSE)
Anova2 <-  aov(weightg ~ genotype, data = froid2)
# néanmoins, différences non significatives
summary(Anova2)

g1 <- ggplot(data = moyLer, aes(x=moyLer$genotype, y= moyLer$moyenne))
g1 <- g1 + geom_bar(stat = "identity", color = "black", width = 0.6, position = position_dodge(0.8))
g1 <- g1 + labs(x = "",
                y="weight of plants (g)\n")
g1 <- g1 + geom_errorbar(aes(ymin =moyLer$moyenne, ymax = moyLer$moyenne+seLer$stand.error), width = 0.05)
g1
save_plot('masseLer15°C4semaines.png', g1, base_aspect_ratio = 1.3)

g2 <- ggplot(data = moyCol, aes(x=moyCol$genotype, y= moyCol$moyenne))
g2 <- g2 + geom_bar(stat = "identity", color = "black", width = 0.6, position = position_dodge(0.8))
g2 <- g2 + labs(x = "", 
                y="weight of plants (g)\n")
g2 <- g2 + geom_errorbar(aes(ymin =moyCol$moyenne, ymax = moyCol$moyenne+seCol$stand.error), width = 0.05)
g2 
save_plot('masseCol15°C4semaines.png', g2, base_aspect_ratio = 1.3)
