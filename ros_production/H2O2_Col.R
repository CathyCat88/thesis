remove(list = ls())
graphics.off()

library(ggplot2)
library(cowplot)

data <- read.csv("H2O2_Col.csv", header = TRUE)

#############
###CALCULS###
#############

mean <- aggregate(data$conc, list(data$genotype), mean)

StandErr <- function(x) {
  return(sd(x)/sqrt(length(x)))
}
se <- aggregate(data$conc, list(data$genotype), StandErr)

result <- cbind(mean, se[,2])
colnames(result) <- c("genotype", "moyenne", "se")
result$genotype <- factor(result$genotype, levels = c("WT", "nqr", "fqr1","nqrfqr1"))

#############
###BARPLOT###
#############

g <- ggplot(result, 
            aes(genotype, 
                moyenne, 
                fill = genotype)) +
  geom_bar(stat = "identity", 
           colour = "black", 
           width = 0.6) +
  xlab("") +
  scale_y_continuous(name = "Péroxyde d'hydrogène \n(nmol/g/h)\n", 
                     expand = c(0,0),
                     limits = c(0,15.5)) +
  theme(legend.position = "none") +
  geom_errorbar(ymin = result$moyenne, 
                ymax = result$moyenne + result$se, 
                width = 0.05)
save_plot("H2O2_Col.png", g, base_aspect_ratio = 1.3)

###########
###STATS###
###########

shapirotest <- aggregate(data$conc, 
                         list(data$genotype), 
                         function(x) shapiro.test(x)$p.value)
# tout normal!! yeah!!

df <- data.frame(reference = character(0),
                 genotype = character(0),
                 bartlettResult = integer(0),
                 bartlett.pass = logical(0),
                 studentResult = integer(0),
                 student.pass = logical(0))

for (params in list(c("WT", "nqr"),
                     c("WT", "fqr1"),
                     c("WT", "nqrfqr1"))) {
  
  reference <- params[1]
  genotype <- params[2]
  
  bartlettResult <- bartlett.test(list(data$conc[data$genotype == reference], 
                                  data$conc[data$genotype == genotype]))
  
  studentResult <- t.test(data$conc[data$genotype == reference], 
                          data$conc[data$genotype == genotype])
  
  df <- rbind(df, data.frame(reference = reference,
                             genotype = genotype,
                             bartlettResult = bartlettResult$p.value,
                             bartlett.pass = (bartlettResult$p.value < 0.05),
                             studentResult = studentResult$p.value,
                             student.pass = (studentResult$p.value > 0.05)))
}


