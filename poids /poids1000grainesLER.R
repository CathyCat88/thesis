graphics.off()
remove(list = ls())

library(ggplot2)
library(cowplot)

data <- data.frame(read.csv("Ler1000.csv", sep = ',', dec = '.', header = TRUE))

#########
#CALCULS#
#########

mean <- aggregate(data$masse, list(data$genotype), mean)

StandErr <- function(x) {
  se <- sd(x)/sqrt(length(x)) # pas besoin de détailler x, aggregate se charge de tout compléter
}

standard.error <- aggregate(data$masse, list(data$genotype), StandErr)

summary <- cbind(mean, standard.error[,2]) # rassembler 2 tableaux
colnames(summary) <- c("genotype", "weight", "standard_error")

summary$genotype <- factor(summary$genotype, levels = c("WT", "nqr", "air12","nqrair12"))

#########
#BARPLOT#
#########

g <- ggplot(data = summary, 
            aes(x = genotype, 
                y = weight,
                fill = genotype,
                group = genotype))
g <- g + theme(legend.position = "none")
g <- g + geom_bar(stat = "identity", 
                  color = "black", width = 0.6)
g <- g + xlab("")
g <- g + scale_fill_hue(l = 40, c = 100)
g <- g + scale_y_continuous(name = "masse \nde 1000 graines (g)\n",                            
                            expand = c(0,0), 
                            limits = c(0,17))
g <- g + geom_errorbar(aes(ymin = summary$weight, ymax = summary$weight + summary$standard_error), width = 0.05)
g
save_plot('masse1000grainesLER.png', g, base_aspect_ratio = 1.3)

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
                    c("WT", "air12"),
                    c("WT", "nqrair12"))) {
  
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

stats <- lm(data$masse ~ data$genotype + data$repetition)
summary(stats)
anova(stats)

a1 <- aov(data$masse ~ data$genotype + data$repetition)
posthoc <- TukeyHSD(x=a1, 'data$genotype', conf.level=0.95)

out <- HSD.test(stats, 'data$genotype')
