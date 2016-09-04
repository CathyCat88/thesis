rm(list = ls())
graphics.off()

library(ggplot2)
library(cowplot)

data1 <- read.csv("100415 24himb 24h strat Col.csv", header = TRUE)
data2 <- read.csv("220316 24himb 24h strat Col.csv", header = TRUE)

bk1 <- data1[data1$genotype == "blank",] # sélection des blancs
bk2 <- data2[data2$genotype == "blank",]
data1 <- data1[-(25:29),] # redéfinition des dataframes sans blancs
data2 <- data2[-(25:27),]

norm1 <- data1$DO470 - mean(bk1$DO470) # données normalisées /blancs
data1$norm <- norm1
norm2 <- data2$DO470 - mean(bk2$DO470)
data2$norm <- norm2

data3 <- rbind(data1, data2) # jonction 2 tableaux

conc <- (data3$norm/data3$masse)/24200 *10^6 # calcul des [superoxyde] en µM
data3$conc <- conc
data3 <- data3[data3$conc < 90,] # enlève les données très hautes

mean <- aggregate(data3$conc, list(data3$genotype), mean) # calcul moyenne /génotype

StandErr <- function(x){
  return(sd(x) / sqrt(length(x)))
}
se <- aggregate(data3$conc, list(data3$genotype), StandErr)

data <- cbind(mean, se[,2])
colnames(data) <- c("genotype", "moyenne", "se")

data$genotype <- factor (data$genotype, levels = c("WT", "nqr", "fqr1","nqrfqr1"))

g <- ggplot(data = data, 
            aes(genotype, moyenne)) +
  geom_bar(stat = "identity", colour = "black", 
           width = 0.6, position = position_dodge(0.8)) +
  xlab(" ") +
  ylab("superoxyde (µM/g/h)\n") +
  geom_errorbar(aes(ymin = data$moyenne, 
                    ymax = data$moyenne + data$se), width = .05)
save_plot("superoxyde_Col.png", g, base_aspect_ratio = 1.3)
