rm(list = ls())
graphics.off()

library(ggplot2)
library(cowplot)

data <- read.csv("masseCol3semainesCONT.csv", header = TRUE)

mean <- aggregate(data$masse, list(data$genotype), mean)

StandErr <- function(x){
  return(sd(x) / sqrt(length(x)))
}
se <- aggregate(data$masse, list(data$genotype), StandErr)

result <- cbind(mean, se[,2])
colnames(result) <- c("genotype", "moyenne", "se")
result$genotype <- factor(result$genotype, levels = c("WT", "nqr", "fqr1","nqrfqr1"))

g <- ggplot(result, aes(genotype, moyenne, fill = genotype)) +
  geom_bar(stat = "identity", colour = "black", width = 0.6, position = position_dodge(0.8)) +
  theme(legend.position = "none") +
  xlab(" ") +
  scale_y_continuous(name = "masse (g)\n",
                     expand = c(0,0)) +
  geom_errorbar(aes(ymin = result$moyenne, ymax = result$moyenne + result$se), width = 0.05)
save_plot("masseCol3semainesCONT.png", g, base_aspect_ratio = 1.3)

#######
#TESTS#
#######

shapiroTest <- aggregate(masse ~ genotype, data = data, 
                         function (x) shapiro.test(x)$p.value)
#toutes les distributions sont normales

result <- data.frame(reference=character(0),
                     genotype=character(0),
                     bartlett=numeric(0),
                     bartlett.pass=logical(0),
                     student=numeric(0),
                     student.pass=logical(0))

for (params in list(c("WT", "nqr"),
                    c("WT", "fqr1"),
                    c("WT", "nqrfqr1"))) {
  
  reference <- params[1]
  genotype <- params[2]
  
  bartlettResult <- bartlett.test(list(data$masse[data$genotype == reference],
                                       data$masse[data$genotype == genotype]))
  
  #égalité des variances
  
  studentResult<- t.test(data$masse[data$genotype == reference],
                         data$masse[data$genotype == genotype], var.equal = TRUE)
  
  result <- rbind(result, data.frame(reference=reference,
                                     genotype=genotype,
                                     bartlett=bartlettResult$p.value,
                                     bartlett.pass=(bartlettResult$p.value > 0.05),
                                     student=studentResult$p.value,
                                     student.pass=(studentResult$p.value > 0.05)))
}
