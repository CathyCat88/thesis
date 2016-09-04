graphics.off()
remove(list = ls())

library(ggplot2)
library(matrixStats)

cont <- data.frame(read.csv("Lermasseplantes3semainesCONT.csv", sep = ',', dec = '.', header = TRUE))

p <- ggplot(cont, aes(cont$genotype, cont$masse))
p + geom_boxplot()

WT <- data.frame(cont[which(cont$genotype == "WT"), ])
moy.WT <- mean((WT$masse))
shapiro.test(as.matrix(WT$masse))
qqnorm(as.matrix(WT$masse))
qqline(as.matrix(WT$masse))
se.WT <- colSds(as.matrix(WT$masse))/(sqrt(length(as.matrix(WT$masse))))

nqr <- data.frame(cont[which(cont$genotype == "nqr"), ])
moy.nqr <- mean((nqr$masse))
shapiro.test(as.matrix(nqr$masse))
qqnorm(as.matrix(nqr$masse))
qqline(as.matrix(nqr$masse))
se.nqr <- colSds(as.matrix(nqr$masse))/(sqrt(length(as.matrix(nqr$masse))))

air12 <- data.frame(cont[which(cont$genotype == "air12"), ])
moy.air12 <- mean((air12$masse))
shapiro.test(as.matrix(air12$masse))
qqnorm(as.matrix(air12$masse))
qqline(as.matrix(air12$masse))
se.air12<- colSds(as.matrix(air12$masse))/(sqrt(length(as.matrix(air12$masse)))) 

nqrair12 <- data.frame(cont[which(cont$genotype == "nqrair12"), ])
moy.nqrair12 <- mean((nqrair12$masse))
shapiro.test(as.matrix(nqrair12$masse))
qqnorm(as.matrix(nqrair12$masse))
qqline(as.matrix(nqrair12$masse))
se.nqrair12 <- colSds(as.matrix(nqrair12$masse))/(sqrt(length(as.matrix(nqrair12$masse)))) 

moy <- data.frame(c(moy.WT, moy.nqr, moy.air12, moy.nqrair12))
genotype <- c("WT", "nqr", "air12", "nqrair12")
moy$genotype <- factor(genotype, levels = c("WT", "nqr", "air12", "nqrair12"))
colnames(moy) <- c("moyenne", "genotype")

se <- data.frame(c(se.WT, se.nqr, se.air12, se.nqrair12))
se$genotype <- factor(se, levels = c("WT", "nqr", "air12", "nqrair12"))
colnames(se) <- c("stand.error", "genotype")

#3 test de l'homogénéité des variances
# Test de Bartlett
# H0 :  variances égales
# H1 :  variances différentes
# si p-value > 0.05, on accepte H0 : homogénéité des variances
# si 0 < p-value < 0.05 rejet de H0, on accepte H1 : pas d'homogénéité
bartlett.test(list(WT$masse, nqr$masse, air12$masse, nqrair12$masse))
# on accepte H0, homogénéité variances ok

t.test(WT$masse, nqr$masse, var.equal = TRUE) # non égalité des moyennes
t.test(WT$masse, air12$masse, var.equal = TRUE) # non égalité des moyennes
t.test(WT$masse, nqrair12$masse, var.equal = TRUE) # non égalité des moyennes

cont2 <-  data.frame((read.csv("Lermasseplantes3semainesCONT.csv", sep = ',', dec = '.', header = TRUE)), stringsAsFactors = FALSE)
Anova2 <-  aov(masse ~ genotype, data = cont2)
# néanmoins, différences non significatives
summary(Anova2)

g <- ggplot(data = moy, aes(x=moy$genotype, y= moy$moyenne))
g <- g + geom_bar(stat = "identity", color = "black", width = 0.6, position = position_dodge(0.8))
g <- g + theme_bw()
g <- g + labs(#title = "Germination of seeds at 15°C under continuous light", 
  x = "", 
  y="weight of plants (g)\n")
g <- g + theme(axis.text = element_text(size = 20), 
               #axis.title.x = element_text(size = 12), 
               axis.title.y = element_text(size = 20))
g <- g + geom_errorbar(aes(ymin =moy$moyenne, ymax = moy$moyenne+se$stand.error), width = 0.05)
g 

ggsave("Lermasseplantes3semainesCONT.pdf")
ggsave("Lermasseplantes3semainesCONT.png")

