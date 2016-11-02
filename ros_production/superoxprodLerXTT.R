graphics.off()
remove(list = ls())

library(ggplot2)
library(cowplot)

df <- data.frame(read.csv("XTT48Himbibition3JstratLer220316.csv",
                          sep = ',', dec = ',', header = TRUE))

#############
###CALCULS###
#############

mean.blank <- mean(df$DO470[df$genotype == "blank"])

df.cor <- data.frame(df[c(-(25:27), -1, -9),])
superoxprod <- (((df.cor$DO470) - mean.blank)/df.cor$masse)*500* 10^-6/24200*10^9

df2 <- cbind(df.cor[,0:3], superoxprod)

mean <- aggregate(df2$superoxprod, list(df2$genotype), mean)

StandErr <- function(x){
  return(sd(x) / sqrt(length(x)))
}
se <- aggregate(df2$superoxprod, list(df2$genotype), StandErr)

result <- cbind(mean, se[,2])
colnames(result) <- c("genotype", "moyenne", "se")
result$genotype <- factor(result$genotype, 
                          levels = c("WT", "nqr", "air12", "nqrair12"))

#############
###BARPLOT###
#############

g <- ggplot(data = result, aes(genotype, moyenne)) + 
  geom_bar(stat = "identity", 
           color = "black",
           width = 0.6) +
  xlab("") +
  scale_y_continuous(name = "Superoxide production \n(nmol/g/h)\n", 
                     limits = c(0, 15),
                     expand = c(0,0)) +
  geom_errorbar(ymin = result$moyenne, 
                ymax = result$moyenne+result$se, 
                width = 0.05)
g
save_plot('superoxideProdLer.png', g, base_aspect_ratio = 1.3)

g <- ggplot(data = result, aes(genotype, moyenne, 
                               group = genotype, 
                               fill = genotype)) + 
  geom_bar(stat = "identity", 
           color = "black",
           width = 0.6) +
  theme(legend.position = "none") +
  scale_fill_hue(l = 40, c = 100) +
  xlab("") +
  scale_y_continuous(name = "Superoxyde (nmol/g/h)\n", 
                     limits = c(0, 15),
                     expand = c(0,0)) +
  geom_errorbar(ymin = result$moyenne, 
                ymax = result$moyenne+result$se, 
                width = 0.05)
g
save_plot('superoxyde_Ler.png', g, base_aspect_ratio = 1.3)

###########
###STATS###
###########

shapiroTest <- aggregate(df2$superoxprod, list(df2$genotype), function(x) shapiro.test(x)$p.value)

# H0 :  variances/moyennes égales
# H1 :  variances/moyennes différentes
# si p-value > 0.05, on accepte H0 : homogénéité des variances, moyennes égales
# si 0 < p-value < 0.05 rejet de H0, on accepte H1 : différences variances/ moyennes

df3 <- data.frame(reference=character(0),
                     genotype=character(0),
                     bartlett=numeric(0),
                     bartlett.pass=logical(0),
                     student=numeric(0),
                     student.pass=logical(0))

for (params in list(c("WT", "nqr"),
                    c("WT", "air12"),
                    c("WT", "nqrair12"))) {
  
  reference <- params[1]
  genotype <- params[2]
  
  bartlettResult <- bartlett.test(list(df2$superoxprod[df2$genotype == reference],
                                       df2$superoxprod[df2$genotype == genotype]))
  #égalité des variances
  
  student <- t.test(df2$superoxprod[df2$genotype == reference],
                              df2$superoxprod[df2$genotype == genotype], var.equal = TRUE)
  
  df3 <- rbind(df3, data.frame(reference=reference,
                                     genotype=genotype,
                                     bartlett=bartlettResult$p.value,
                                     bartlett.pass=(bartlettResult$p.value > 0.05),
                               student=student$p.value,
                               student.pass=(student$p.value > 0.05)))
}

stats <- lm(df2$superoxprod ~ df2$genotype + df2$repbio)
summary(stats)
anova(stats)

a1 <- aov(df2$superoxprod ~ df2$genotype + df2$repbio)
posthoc <- TukeyHSD(x=a1, 'df2$genotype', conf.level=0.95)

out <- HSD.test(stats, 'df2$genotype')
