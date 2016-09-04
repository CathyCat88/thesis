graphics.off()
remove(list = ls())

library(ggplot2)
library(matrixStats)
library(cowplot)

catalase <- data.frame(read.csv("CAT.csv", sep = ',', dec = '.', header = TRUE))

rep1 <- data.frame(catalase[which(catalase$repBIO == "1"),])
prod1 <- data.frame((rep1$pente*0.01315*10^3)/rep1$protein)
colnames(prod1) <- "ProdOx"

rep2 <- data.frame(catalase[which(catalase$repBIO == "2"),])
prod2 <- data.frame((rep2$pente*0.0146*10^3)/rep2$protein)
colnames(prod2) <- "ProdOx"

rep3 <- data.frame(catalase[which(catalase$repBIO == "3"),])
prod3 <- data.frame((rep3$pente*0.014*10^3)/rep3$protein)
colnames(prod3) <- "ProdOx"

prod <- rbind(prod1,prod2)
prod <- rbind(prod, prod3)

ProdFinal <- catalase[1:4]
ProdFinal$prodox <- prod

WTLer <- data.frame(ProdFinal[which(ProdFinal$genotype == "WT" & ProdFinal$ecotype == "Ler"),])
nqrLer <- data.frame(ProdFinal[which(ProdFinal$genotype == "nqr" & ProdFinal$ecotype == "Ler"),])
air12Ler <- data.frame(ProdFinal[which(ProdFinal$genotype == "air12"),])
nqrair12Ler <- data.frame(ProdFinal[which(ProdFinal$genotype == "nqrair12"),])
WTCol <- data.frame(ProdFinal[which(ProdFinal$genotype == "WT" & ProdFinal$ecotype == "Col"),])
nqrCol <- data.frame(ProdFinal[which(ProdFinal$genotype == "nqr" & ProdFinal$ecotype == "Col"),])
shapiro.test(as.matrix(WTLer$prodox)) #distribution normale 
shapiro.test(as.matrix(nqrLer$prodox)) #distribution normale
shapiro.test(as.matrix(air12Ler$prodox)) #distribution normale
shapiro.test(as.matrix(nqrair12Ler$prodox)) #distribution normale
shapiro.test(as.matrix(WTCol$prodox)) #distribution normale
shapiro.test(as.matrix(nqrCol$prodox)) #distribution normale
bartlett.test(list(WTLer$prodox$ProdOx, nqrLer$prodox$ProdOx, air12Ler$prodox$ProdOx,nqrair12Ler$prodox$ProdOx)) #non homogeneite variances
bartlett.test(list(WTCol$prodox$ProdOx, nqrCol$prodox$ProdOx))#homogeneite des variances


#REP 1
WTLer1 <- data.frame(ProdFinal[which(ProdFinal$genotype == "WT" & ProdFinal$ecotype == "Ler" & ProdFinal$repBIO == "1"),])
moy.WTLer1 <- colMeans(WTLer1$prodox)
se.WTLer1 <- colSds(as.matrix(WTLer1$prodox))/(sqrt(length(as.matrix(WTLer1$prodox))))

nqrLer1 <- data.frame(ProdFinal[which(ProdFinal$genotype == "nqr" & ProdFinal$ecotype == "Ler" & ProdFinal$repBIO == "1"),])
moy.nqrLer1 <- colMeans(nqrLer1$prodox)
se.nqrLer1 <- colSds(as.matrix(nqrLer1$prodox))/(sqrt(length(as.matrix(nqrLer1$prodox))))

air12Ler1 <- data.frame(ProdFinal[which(ProdFinal$genotype == "air12" & ProdFinal$ecotype == "Ler"& ProdFinal$repBIO == "1"),])
moy.air12Ler1 <- colMeans(air12Ler1$prodox)
se.air12Ler1 <- colSds(as.matrix(air12Ler1$prodox))/(sqrt(length(as.matrix(air12Ler1$prodox))))

nqrair12Ler1 <- data.frame(ProdFinal[which(ProdFinal$genotype == "nqrair12" & ProdFinal$ecotype == "Ler"& ProdFinal$repBIO == "1"),])
moy.nqrair12Ler1 <- colMeans(nqrair12Ler1$prodox)
se.nqrair12Ler1 <- colSds(as.matrix(nqrair12Ler1$prodox))/(sqrt(length(as.matrix(nqrair12Ler1$prodox))))

WTCol1 <- data.frame(ProdFinal[which(ProdFinal$genotype == "WT" & ProdFinal$ecotype == "Col"& ProdFinal$repBIO == "1"),])
moy.WTCol1 <- colMeans(WTCol1$prodox)
se.WTCol1 <- colSds(as.matrix(WTCol1$prodox))/(sqrt(length(as.matrix(WTCol1$prodox))))

nqrCol1 <- data.frame(ProdFinal[which(ProdFinal$genotype == "nqr" & ProdFinal$ecotype == "Col"& ProdFinal$repBIO == "1"),])
moy.nqrCol1 <- colMeans(nqrCol1$prodox)
se.nqrCol1 <- colSds(as.matrix(nqrCol1$prodox))/(sqrt(length(as.matrix(nqrCol1$prodox))))

#normalisation rep1
moy.Ler.norm1 <- data.frame(c((moy.WTLer1/moy.WTLer1), (moy.nqrLer1/moy.WTLer1), (moy.air12Ler1/moy.WTLer1), (moy.nqrair12Ler1/moy.WTLer1)))
se.Ler.norm1 <- data.frame(c((se.WTLer1/se.WTLer1), (se.nqrLer1/se.WTLer1), (se.air12Ler1/se.WTLer1), (se.nqrair12Ler1/se.WTLer1)))

moy.Col.norm1 <- data.frame(c((moy.WTCol1/moy.WTCol1), (moy.nqrCol1/moy.WTCol1)))
se.Col.norm1 <- data.frame(c((se.WTCol1/se.WTCol1), (se.nqrCol1/se.WTCol1)))

#REP 2
WTLer2 <- data.frame(ProdFinal[which(ProdFinal$genotype == "WT" & ProdFinal$ecotype == "Ler" & ProdFinal$repBIO == "2"),])
moy.WTLer2 <- colMeans(WTLer2$prodox)
se.WTLer2 <- colSds(as.matrix(WTLer2$prodox))/(sqrt(length(as.matrix(WTLer2$prodox))))

nqrLer2 <- data.frame(ProdFinal[which(ProdFinal$genotype == "nqr" & ProdFinal$ecotype == "Ler" & ProdFinal$repBIO == "2"),])
moy.nqrLer2 <- colMeans(nqrLer2$prodox)
se.nqrLer2 <- colSds(as.matrix(nqrLer2$prodox))/(sqrt(length(as.matrix(nqrLer2$prodox))))

air12Ler2 <- data.frame(ProdFinal[which(ProdFinal$genotype == "air12" & ProdFinal$ecotype == "Ler"& ProdFinal$repBIO == "2"),])
moy.air12Ler2 <- colMeans(air12Ler2$prodox)
se.air12Ler2 <- colSds(as.matrix(air12Ler2$prodox))/(sqrt(length(as.matrix(air12Ler2$prodox))))

nqrair12Ler2 <- data.frame(ProdFinal[which(ProdFinal$genotype == "nqrair12" & ProdFinal$ecotype == "Ler"& ProdFinal$repBIO == "2"),])
moy.nqrair12Ler2 <- colMeans(nqrair12Ler2$prodox)
se.nqrair12Ler2 <- colSds(as.matrix(nqrair12Ler2$prodox))/(sqrt(length(as.matrix(nqrair12Ler2$prodox))))

WTCol2 <- data.frame(ProdFinal[which(ProdFinal$genotype == "WT" & ProdFinal$ecotype == "Col"& ProdFinal$repBIO == "2"),])
moy.WTCol2 <- colMeans(WTCol2$prodox)
se.WTCol2 <- colSds(as.matrix(WTCol2$prodox))/(sqrt(length(as.matrix(WTCol2$prodox))))

nqrCol2 <- data.frame(ProdFinal[which(ProdFinal$genotype == "nqr" & ProdFinal$ecotype == "Col"& ProdFinal$repBIO == "2"),])
moy.nqrCol2 <- colMeans(nqrCol2$prodox)
se.nqrCol2 <- colSds(as.matrix(nqrCol2$prodox))/(sqrt(length(as.matrix(nqrCol2$prodox))))

#normalisation 2

moy.Ler.norm2 <- data.frame(c((moy.WTLer2/moy.WTLer2), (moy.nqrLer2/moy.WTLer2), (moy.air12Ler2/moy.WTLer2), (moy.nqrair12Ler2/moy.WTLer2)))
se.Ler.norm2 <- data.frame(c((se.WTLer2/se.WTLer2), (se.nqrLer2/se.WTLer2), (se.air12Ler2/se.WTLer2), (se.nqrair12Ler2/se.WTLer2)))

moy.Col.norm2 <- data.frame(c((moy.WTCol2/moy.WTCol2), (moy.nqrCol2/moy.WTCol2)))
se.Col.norm2 <- data.frame(c((se.WTCol2/se.WTCol2), (se.nqrCol2/se.WTCol2)))

#REP 3
WTLer3 <- data.frame(ProdFinal[which(ProdFinal$genotype == "WT" & ProdFinal$ecotype == "Ler" & ProdFinal$repBIO == "3"),])
moy.WTLer3 <- colMeans(WTLer3$prodox)
se.WTLer3 <- colSds(as.matrix(WTLer3$prodox))/(sqrt(length(as.matrix(WTLer3$prodox))))

nqrLer3 <- data.frame(ProdFinal[which(ProdFinal$genotype == "nqr" & ProdFinal$ecotype == "Ler" & ProdFinal$repBIO == "3"),])
moy.nqrLer3 <- colMeans(nqrLer3$prodox)
se.nqrLer3 <- colSds(as.matrix(nqrLer3$prodox))/(sqrt(length(as.matrix(nqrLer3$prodox))))

air12Ler3 <- data.frame(ProdFinal[which(ProdFinal$genotype == "air12" & ProdFinal$ecotype == "Ler"& ProdFinal$repBIO == "3"),])
moy.air12Ler3 <- colMeans(air12Ler3$prodox)
se.air12Ler3 <- colSds(as.matrix(air12Ler3$prodox))/(sqrt(length(as.matrix(air12Ler3$prodox))))

nqrair12Ler3 <- data.frame(ProdFinal[which(ProdFinal$genotype == "nqrair12" & ProdFinal$ecotype == "Ler"& ProdFinal$repBIO == "3"),])
moy.nqrair12Ler3 <- colMeans(nqrair12Ler3$prodox)
se.nqrair12Ler3 <- colSds(as.matrix(nqrair12Ler3$prodox))/(sqrt(length(as.matrix(nqrair12Ler3$prodox))))

WTCol3 <- data.frame(ProdFinal[which(ProdFinal$genotype == "WT" & ProdFinal$ecotype == "Col"& ProdFinal$repBIO == "3"),])
moy.WTCol3 <- colMeans(WTCol3$prodox)
se.WTCol3 <- colSds(as.matrix(WTCol3$prodox))/(sqrt(length(as.matrix(WTCol3$prodox))))

nqrCol3 <- data.frame(ProdFinal[which(ProdFinal$genotype == "nqr" & ProdFinal$ecotype == "Col"& ProdFinal$repBIO == "3"),])
moy.nqrCol3 <- colMeans(nqrCol3$prodox)
se.nqrCol3 <- colSds(as.matrix(nqrCol3$prodox))/(sqrt(length(as.matrix(nqrCol3$prodox))))

#normalisation 3
str(moy.Ler.norm3)
moy.Ler.norm3 <- data.frame(c((moy.WTLer3/moy.WTLer3), (moy.nqrLer3/moy.WTLer3), (moy.air12Ler3/moy.WTLer3), (moy.nqrair12Ler3/moy.WTLer3)))
se.Ler.norm3 <- data.frame(c((se.WTLer3/se.WTLer3), (se.nqrLer3/se.WTLer3), (se.air12Ler3/se.WTLer3), (se.nqrair12Ler3/se.WTLer3)))

moy.Col.norm3 <- data.frame(c((moy.WTCol3/moy.WTCol3), (moy.nqrCol3/moy.WTCol3)))
se.Col.norm3 <- data.frame(c((se.WTCol3/se.WTCol3), (se.nqrCol3/se.WTCol3)))

#Regroupement des 3 rep normalisées

genotypeLer <- c("WT", "nqr", "air12", "nqrair12")

moy.Ler.norm1$genotype <- factor(genotypeLer, levels = c("WT", "nqr", "air12", "nqrair12"))
colnames(moy.Ler.norm1) <- c("moyenne", "genotype")
moy.Ler.norm2$genotype <- factor(genotypeLer, levels = c("WT", "nqr", "air12", "nqrair12"))
colnames(moy.Ler.norm2) <- c("moyenne", "genotype")
moy.Ler.norm3$genotype <- factor(genotypeLer, levels = c("WT", "nqr", "air12", "nqrair12"))
colnames(moy.Ler.norm3) <- c("moyenne", "genotype")

moy.Ler.norm <- rbind(moy.Ler.norm1, moy.Ler.norm2)
moy.Ler.norm <- rbind(moy.Ler.norm, moy.Ler.norm3)

se.Ler.norm1$genotype <- factor(genotypeLer, levels = c("WT", "nqr", "air12", "nqrair12"))
colnames(se.Ler.norm1) <- c("stand.error", "genotype")
se.Ler.norm2$genotype <- factor(genotypeLer, levels = c("WT", "nqr", "air12", "nqrair12"))
colnames(se.Ler.norm2) <- c("stand.error", "genotype")
se.Ler.norm3$genotype <- factor(genotypeLer, levels = c("WT", "nqr", "air12", "nqrair12"))
colnames(se.Ler.norm3) <- c("stand.error", "genotype")

se.Ler.norm <- rbind(se.Ler.norm1, se.Ler.norm2)
se.Ler.norm <- rbind(se.Ler.norm, se.Ler.norm3)

genotypeCol <- c("WT", "nqr")
moy.Col.norm1$genotype <- factor(genotypeCol, levels = c("WT", "nqr"))
colnames(moy.Col.norm1) <- c("moyenne", "genotype")
moy.Col.norm2$genotype <- factor(genotypeCol, levels = c("WT", "nqr"))
colnames(moy.Col.norm2) <- c("moyenne", "genotype")
moy.Col.norm3$genotype <- factor(genotypeCol, levels = c("WT", "nqr"))
colnames(moy.Col.norm3) <- c("moyenne", "genotype")

moy.Col.norm <- rbind(moy.Col.norm1, moy.Col.norm2)
moy.Col.norm <- rbind(moy.Col.norm, moy.Col.norm3)

se.Col.norm1$genotype <- factor(genotypeCol, levels = c("WT", "nqr"))
colnames(se.Col.norm1) <- c("stand.error", "genotype")
se.Col.norm2$genotype <- factor(genotypeCol, levels = c("WT", "nqr"))
colnames(se.Col.norm2) <- c("stand.error", "genotype")
se.Col.norm3$genotype <- factor(genotypeCol, levels = c("WT", "nqr"))
colnames(se.Col.norm3) <- c("stand.error", "genotype")

se.Col.norm <- rbind(se.Col.norm1, se.Col.norm2)
se.Col.norm <- rbind(se.Col.norm, se.Col.norm3)

WT.Ler.final <- data.frame(moy.Ler.norm[which(moy.Ler.norm$genotype == "WT" & ProdFinal$ecotype == "Ler"),])
moy.WT.Ler.final <- colMeans(WT.Ler.final$moyenne)

t.test(WTLer$prodox, nqrLer$prodox) #egalité
t.test(WTLer$prodox, air12Ler$prodox) # non egalité
t.test(WTLer$prodox, nqrair12Ler$prodox) # non egalité
t.test(WTCol$prodox, nqrCol$prodox) #egalité

g <- ggplot(data = moy.Ler.norm.final, aes(x=moy.Ler.norm.final$genotype, y= moy.Ler.norm.final$moyenne)) 
g <- g + geom_bar(stat = "identity", color = "black", width = 0.6, position = position_dodge(0.8)) 
g <- g +labs(x = "", 
             y="oxygen production \n(nmol/mg of protein/min)\n")
g <- g + geom_errorbar(aes(ymin =moy.Ler.norm.final$moyenne, ymax = moy.Ler.norm.final$moyenne+se.Ler.norm.final$stand.error), width = 0.05)
g
save_plot('superoxideProdLer.png', g, base_aspect_ratio = 1.3)

