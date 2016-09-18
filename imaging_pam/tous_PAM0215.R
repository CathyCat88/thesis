remove(list = ls())
graphics.off()

npq <- read.csv("npq10_0215.csv", header = TRUE)
qP <- read.csv("qP10_0215.csv", header = TRUE)
FvFm <- read.csv("FvFm10_0215.csv", header = TRUE)

all <- npq[,-3]
all <- cbind(all, npq[,3])
all <- cbind(all, qP[,3])
all <- cbind(all, FvFm[,3])
colnames(all) <- c("temps", "genotype", "stress", "npq", "qP", "FvFm")

data <- data.frame(all[all$temps == 346 | all$temps == 347 | all$temps == 352,])
dataF <- data.frame(all[all$temps == 534 | all$temps == 565 | all$temps == 554 | all$temps == 564,])

data2F <- melt(dataF, 
               id.vars = c("temps", "genotype", "stress"),
               variable_name = "param")

data2 <- melt(data, 
              id.vars = c("temps", "genotype", "stress"),
              variable_name = "param")



#############
###CALCULS###
#############

mean <- aggregate(data2$value, list(data2$genotype, data2$stress, data2$param), mean)
meanF <- aggregate(data2F$value, list(data2F$genotype, data2F$stress, data2F$param), mean)


StandErr <- function(x) {
  se <- sd(x)/sqrt(length(x))
}
se <- aggregate(data2$value, list(data2$genotype, data2$stress, data2$param), StandErr)
seF <- aggregate(data2F$value, list(data2F$genotype, data2F$stress, data2F$param), StandErr)

results <- cbind(mean, se[,4])
colnames(results) <- c("genotype", "stress", "parametre", "moyenne", "se")

resultsF <- cbind(meanF, seF[,4])
colnames(resultsF) <- c("genotype", "stress", "parametre", "moyenne", "se")


results$moyenne <- round(x = results$moyenne, digits = 3)
results$se <- round(x = results$se, digits = 3)

write.csv(results, file = "QR_photosynthese.csv")
