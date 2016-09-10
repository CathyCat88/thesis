graphics.off()
remove(list = ls())

library(ggplot2)
library(matrixStats)
library(cowplot)

df1 <- data.frame(read.csv("XTT48Himbibition3JstratLer220316.csv", sep = ',', dec = ',', header = TRUE))

df.blank <- data.frame(df1[which(df1$genotype == "blank"),])
blank <- as.data.frame(df.blank$DO470)
moy.blank <- colMeans(blank)

df2 <- data.frame(df1[c(-(25:27), -1, -9),])
superoxprod <- (((df2$DO470) - moy.blank)/df2$masse)/24200*10^6
superoxprod <- as.data.frame(superoxprod)

df3 <- df2[,0:3]
df3$superoxprod <- superoxprod

df.WT <- data.frame(df3[which(df3$genotype == "WT"),])
moy.WT <- colMeans(df.WT$superoxprod)

#en théorie nécessité de calculer la normalite des données pour calculer se
#H0 loi normale p > 0.05
#H1 loi pas normale  p < 0.05
shapiro.test(as.matrix(df.WT$superoxprod))#distribution normale 
#vérif qqplot
qqnorm(as.matrix(df.WT$superoxprod))
qqline(as.matrix(df.WT$superoxprod))
se.WT <- colSds(as.matrix(df.WT$superoxprod))/(sqrt(length(as.matrix(df.WT$superoxprod))))

df.nqr <- data.frame(df3[which(df3$genotype == "nqr"),])
moy.nqr <- colMeans(df.nqr$superoxprod)

shapiro.test(as.matrix(df.nqr$superoxprod))#distribution normale 
qqnorm(as.matrix(df.nqr$superoxprod))
qqline(as.matrix(df.nqr$superoxprod))

se.nqr <- colSds(as.matrix(df.nqr$superoxprod))/(sqrt(length(as.matrix(df.nqr$superoxprod))))

df.air12 <- data.frame(df3[which(df3$genotype == "air12"),])
moy.air12 <- colMeans(df.air12$superoxprod)

shapiro.test(as.matrix(df.air12$superoxprod))#distribution normale 
qqnorm(as.matrix(df.air12$superoxprod))
qqline(as.matrix(df.air12$superoxprod))

se.air12 <- colSds(as.matrix(df.air12$superoxprod))/(sqrt(length(as.matrix(df.air12$superoxprod))))

df.nqrair12 <- data.frame(df3[which(df3$genotype == "nqrair12"),])
moy.nqrair12 <- colMeans(df.nqrair12$superoxprod)

shapiro.test(as.matrix(df.nqrair12$superoxprod))#distribution normale 
qqnorm(as.matrix(df.nqrair12$superoxprod))
qqline(as.matrix(df.nqrair12$superoxprod))

se.nqrair12 <- colSds(as.matrix(df.nqrair12$superoxprod))/(sqrt(length(as.matrix(df.nqrair12$superoxprod))))

#regroupement des moyennes dans un dataframe
genotype <- c("WT", "nqr", "air12", "nqrair12")

moy <- data.frame(c(moy.WT, moy.nqr, moy.air12, moy.nqrair12))
moy$genotype <- factor(genotype, levels = c("WT", "nqr", "air12", "nqrair12"))
colnames(moy) <- c("moyenne", "genotype")

se <- data.frame(c(se.WT, se.nqr, se.air12, se.nqrair12))
se$genotype <- factor(se, levels = c("WT", "nqr", "air12", "nqrair12"))
colnames(se) <- c("stand.error", "genotype")

# test de l'homogénéité des variances
# Test de Bartlett
# H0 :  variances égales
# H1 :  variances différentes
# si p-value > 0.05, on accepte H0 : homogénéité des variances
# si 0 < p-value < 0.05 rejet de H0, on accepte H1 : pas d'homogénéité
bartlett.test(list(df.WT$superoxprod$superoxprod, df.nqr$superoxprod$superoxprod, df.air12$superoxprod$superoxprod, df.nqrair12$superoxprod$superoxprod))
# homogénéité des variances

t.test(df.WT$superoxprod, df.nqr$superoxprod) # égalité des moyennes
t.test(df.WT$superoxprod, df.air12$superoxprod) # égalité des moyennes
t.test(df.WT$superoxprod, df.nqrair12$superoxprod) # non égalité des moyennes

g <- ggplot(data = moy, aes(x=moy$genotype, y= moy$moyenne)) 
g <- g + geom_bar(stat = "identity", color = "black", width = 0.6, position = position_dodge(0.8)) 
g <- g +labs(#title = "superoxide production in seeds after 48h imbibition", 
    x = "", 
    y="superoxide production \n(µM/g/h)\n")
g <- g + geom_errorbar(aes(ymin =moy$moyenne, ymax = moy$moyenne+se$stand.error), width = 0.05)
g
save_plot('superoxideProdLer.png', g, base_aspect_ratio = 1.3)

