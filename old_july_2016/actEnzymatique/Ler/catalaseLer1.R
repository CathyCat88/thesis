graphics.off()
remove(list = ls())

library(ggplot2)
library(matrixStats)
library(cowplot)

catalase <- data.frame(read.csv("CAT.csv", sep = ',', dec = '.', header = TRUE))

# pour chaque répétition biologique, [O2] en nmol/ml
funToOxygen <- function(n, i) { # n = [O2] en µmol/min, i = repBio
    rep <- data.frame(catalase[which(catalase$repBIO == toString(i)),])
    prod <- data.frame((rep$pente*n*10^3)/rep$protein)
  return(prod)
}

for (x in c(c(0.01315, 1), # boucle pour utiliser funToOxygen 
          c(0.0146, 2), # avec les différentes pentes de chaque répétition
          c(0.014, 3)) ) {
  rep <- funToOxygen(x[1], x[2])
}

df.gen.eco.rep <- function(genotype = "", ecotype = "", repbio = 1) {
  result <- data.frame(prod.final[which(prod.final$genotype == genotype
                                       & prod.final$ecotype == ecotype
                                       & prod.final$repBIO == toString(repbio)),])
  return(prod)
}

prod.final <- data.frame(catalase[1:4])
prod.final$prod <- prod

df.gen.eco.rep("WT", "Col", 1)

se <- colSds(as.matrix(gen$prodox)) / (sqrt(length(as.matrix(gen$prodox))))



prod <- rbind(prod1,prod2)
prod <- rbind(prod, prod3)

ProdFinal <- catalase[1:4]
ProdFinal$prodox <- prod

#WT Ler
WTLer1 <- data.frame(ProdFinal[which(ProdFinal$genotype == "WT" & ProdFinal$ecotype == "Ler" & ProdFinal$repBIO == "1"),])
moy.WTLer1 <- colMeans(WTLer1$prodox)
se.WTLer1 <- colSds(as.matrix(WTLer1$prodox))/(sqrt(length(as.matrix(WTLer1$prodox))))

WTLer2 <- data.frame(ProdFinal[which(ProdFinal$genotype == "WT" & ProdFinal$ecotype == "Ler" & ProdFinal$repBIO == "2"),])
moy.WTLer2 <- colMeans(WTLer2$prodox)
se.WTLer2 <- colSds(as.matrix(WTLer2$prodox))/(sqrt(length(as.matrix(WTLer2$prodox))))

WTLer3 <- data.frame(ProdFinal[which(ProdFinal$genotype == "WT" & ProdFinal$ecotype == "Ler" & ProdFinal$repBIO == "3"),])
moy.WTLer3 <- colMeans(WTLer3$prodox)
se.WTLer3 <- colSds(as.matrix(WTLer3$prodox))/(sqrt(length(as.matrix(WTLer3$prodox))))

#WT Ler: normalisation et moyennes
moy.WTLer <- mean(c(moy.WTLer1/moy.WTLer1), (moy.WTLer2/moy.WTLer2), (moy.WTLer3/moy.WTLer3))
se.WTLer <- 0

#nqr Ler
nqrLer1 <- data.frame(ProdFinal[which(ProdFinal$genotype == "nqr" & ProdFinal$ecotype == "Ler" & ProdFinal$repBIO == "1"),])
moy.nqrLer1 <- colMeans(nqrLer1$prodox)
se.nqrLer1 <- colSds(as.matrix(nqrLer1$prodox))/(sqrt(length(as.matrix(nqrLer1$prodox))))

nqrLer2 <- data.frame(ProdFinal[which(ProdFinal$genotype == "nqr" & ProdFinal$ecotype == "Ler" & ProdFinal$repBIO == "2"),])
moy.nqrLer2 <- colMeans(nqrLer2$prodox)
se.nqrLer2 <- colSds(as.matrix(nqrLer2$prodox))/(sqrt(length(as.matrix(nqrLer2$prodox))))

nqrLer3 <- data.frame(ProdFinal[which(ProdFinal$genotype == "nqr" & ProdFinal$ecotype == "Ler" & ProdFinal$repBIO == "3"),])
moy.nqrLer3 <- colMeans(nqrLer3$prodox)
se.nqrLer3 <- colSds(as.matrix(nqrLer3$prodox))/(sqrt(length(as.matrix(nqrLer3$prodox))))

#nqr Ler: normalisation et moyennes
moy.nqrLer <- mean(c(moy.nqrLer1/moy.WTLer1), (moy.nqrLer2/moy.WTLer2), (moy.nqrLer3/moy.WTLer3))
se.nqrLer <- mean(c(se.nqrLer3/se.WTLer3))

#air12 
air12Ler1 <- data.frame(ProdFinal[which(ProdFinal$genotype == "air12" & ProdFinal$ecotype == "Ler"& ProdFinal$repBIO == "1"),])
moy.air12Ler1 <- colMeans(air12Ler1$prodox)
se.air12Ler1 <- colSds(as.matrix(air12Ler1$prodox))/(sqrt(length(as.matrix(air12Ler1$prodox))))

air12Ler2 <- data.frame(ProdFinal[which(ProdFinal$genotype == "air12" & ProdFinal$ecotype == "Ler"& ProdFinal$repBIO == "2"),])
moy.air12Ler2 <- colMeans(air12Ler2$prodox)
se.air12Ler2 <- colSds(as.matrix(air12Ler2$prodox))/(sqrt(length(as.matrix(air12Ler2$prodox))))

air12Ler3 <- data.frame(ProdFinal[which(ProdFinal$genotype == "air12" & ProdFinal$ecotype == "Ler"& ProdFinal$repBIO == "3"),])
moy.air12Ler3 <- colMeans(air12Ler3$prodox)
se.air12Ler3 <- colSds(as.matrix(air12Ler3$prodox))/(sqrt(length(as.matrix(air12Ler3$prodox))))

#air12 Ler: normalisation et moyennes
moy.air12Ler <- mean(c(moy.air12Ler1/moy.WTLer1), (moy.air12Ler2/moy.WTLer2), (moy.air12Ler3/moy.WTLer3))
se.air12Ler <- mean(c(0, 0, (se.air12Ler3/se.WTLer3)))

#nqrair12 Ler
nqrair12Ler1 <- data.frame(ProdFinal[which(ProdFinal$genotype == "nqrair12" & ProdFinal$ecotype == "Ler"& ProdFinal$repBIO == "1"),])
moy.nqrair12Ler1 <- colMeans(nqrair12Ler1$prodox)
se.nqrair12Ler1 <- colSds(as.matrix(nqrair12Ler1$prodox))/(sqrt(length(as.matrix(nqrair12Ler1$prodox))))

nqrair12Ler2 <- data.frame(ProdFinal[which(ProdFinal$genotype == "nqrair12" & ProdFinal$ecotype == "Ler"& ProdFinal$repBIO == "2"),])
moy.nqrair12Ler2 <- colMeans(nqrair12Ler2$prodox)
se.nqrair12Ler2 <- colSds(as.matrix(nqrair12Ler2$prodox))/(sqrt(length(as.matrix(nqrair12Ler2$prodox))))

nqrair12Ler3 <- data.frame(ProdFinal[which(ProdFinal$genotype == "nqrair12" & ProdFinal$ecotype == "Ler"& ProdFinal$repBIO == "3"),])
moy.nqrair12Ler3 <- colMeans(nqrair12Ler3$prodox)
se.nqrair12Ler3 <- colSds(as.matrix(nqrair12Ler3$prodox))/(sqrt(length(as.matrix(nqrair12Ler3$prodox))))

#nqrair12 Ler: normalisation et moyennes
moy.nqrair12Ler <- mean(c(moy.nqrair12Ler1/moy.WTLer1), (moy.nqrair12Ler2/moy.WTLer2), (moy.nqrair12Ler3/moy.WTLer3))
se.nqrair12Ler <- 0

moyLer <- data.frame(c(moy.WTLer, moy.nqrLer, moy.air12Ler, moy.nqrair12Ler))
seLer <- data.frame(c(se.WTLer, se.nqrLer, se.air12Ler, se.nqrair12Ler))
genotypeLer <- c("WT", "nqr", "air12", "nqrair12")

moyLer$seLer <- seLer
moyLer$genotype <- factor(genotypeLer, levels = c("WT", "nqr", "air12", "nqrair12"))
colnames(moyLer) <- c("moyenne", "se", "genotype")

g <- ggplot(data = moyLer, aes(x=moyLer$genotype, y= moyLer$moyenne)) 
g <- g + geom_bar(stat = "identity", color = "black", width = 0.6, position = position_dodge(0.8)) 
g <- g +labs(x = "", 
             y="oxygen production \n(nmol/mg of protein/min)\n")
g <- g + geom_errorbar(aes(ymin =moyLer$moyenne, ymax = moyLer$moyenne+seLer), width = 0.05)
g
save_plot('superoxideProdLer.png', g, base_aspect_ratio = 1.3)

#WT <- c((moy.WTLer1/moy.WTLer1), (moy.WTLer2/moy.WTLer2), (moy.WTLer3/moy.WTLer3))
#nqr <- c((moy.nqrLer1/moy.WTLer1), (moy.nqrLer2/moy.WTLer2), (moy.nqrLer3/moy.WTLer3))
#air12 <- c((moy.air12Ler1/moy.WTLer1), (moy.air12Ler2/moy.WTLer2), (moy.air12Ler3/moy.WTLer3))
#nqrair12 <- c((moy.nqrair12Ler1/moy.WTLer1), (moy.nqrair12Ler2/moy.WTLer2), (moy.nqrair12Ler3/moy.WTLer3))

#t.test(WT, nqr) # egalite
#t.test(WT, air12) # egalite
#t.test(WT, nqrair12) # egalite

#shapiro.test(as.matrix(air12)) #distribution normale pour tous 
