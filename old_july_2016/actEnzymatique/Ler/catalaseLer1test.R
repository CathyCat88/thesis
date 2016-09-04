graphics.off()
remove(list = ls())

library(ggplot2)
library(matrixStats)
library(cowplot)

catalase <- data.frame(read.csv("CAT.csv",
                                sep = ',',
                                dec = '.',
                                header = TRUE))

rep1 <- data.frame(catalase[ which(catalase$repBIO == "1"), ])
prod1 <- data.frame((rep1$pente * 0.01315 * 10^3) / rep1$protein)
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

#WT Ler
WTLer1 <- data.frame(ProdFinal[ which(ProdFinal$genotype == "WT" &
          ProdFinal$ecotype == "Ler" &
          ProdFinal$repBIO == "1"), ])
moy.WTLer1 <- colMeans(WTLer1$prodox)
se.WTLer1 <- colSds(as.matrix(WTLer1$prodox)) / (sqrt(length(as.matrix(WTLer1$prodox))))

WTLer2 <- data.frame(ProdFinal[ which(ProdFinal$genotype == "WT"
                                       & ProdFinal$ecotype == "Ler"
                                       & ProdFinal$repBIO == "2"), ])
moy.WTLer2 <- colMeans(WTLer2$prodox)
se.WTLer2 <- colSds(as.matrix(WTLer2$prodox))/(sqrt(length(as.matrix(WTLer2$prodox))))

WTLer3 <- data.frame(ProdFinal[which(ProdFinal$genotype == "WT" & ProdFinal$ecotype == "Ler" & ProdFinal$repBIO == "3"),])
moy.WTLer3 <- colMeans(WTLer3$prodox)
se.WTLer3 <- colSds(as.matrix(WTLer3$prodox))/(sqrt(length(as.matrix(WTLer3$prodox))))

#WT Ler: normalisation et moyennes
moy.WTLer <- mean((moy.WTLer1 / moy.WTLer1),
                  (moy.WTLer2 / moy.WTLer2),
                  (moy.WTLer3 / moy.WTLer3))
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
myFunction <- function(n) {
  gen <- data.frame(ProdFinal[which(ProdFinal$genotype == "air12"
                                    & ProdFinal$ecotype == "Ler"
                                    & ProdFinal$repBIO == toString(n)),])
  moy <- colMeans(gen$prodox)
  se <- colSds(as.matrix(gen$prodox)) / (sqrt(length(as.matrix(gen$prodox))))
  return(list(moy=moy, se=se))
}

air12Ler1  <- myFunction(1)
air12Ler2  <- myFunction(2)
air12Ler3  <- myFunction(3)



#air12 Ler: normalisation et moyennes
moy.air12Ler <- mean(c(air12Ler1$moy/moy.WTLer1), (air12Ler2$moy/moy.WTLer2), (air12Ler3$moy/moy.WTLer3))
se.air12Ler <- mean(c(0, 0, (air12Ler3$se/se.WTLer3)))

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
#air12 <- c((air12Ler1$moy/moy.WTLer1), (air12Ler2$moy/moy.WTLer2), (air12Ler3$moy/moy.WTLer3))
#nqrair12 <- c((moy.nqrair12Ler1/moy.WTLer1), (moy.nqrair12Ler2/moy.WTLer2), (moy.nqrair12Ler3/moy.WTLer3))

#t.test(WT, nqr) # egalite
#t.test(WT, air12) # egalite
#t.test(WT, nqrair12) # egalite

#shapiro.test(as.matrix(air12)) #distribution normale pour tous 
