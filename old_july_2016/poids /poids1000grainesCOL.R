remove(list = ls())

library("ggplot2")
library("cowplot")

data <- read.csv("Col1000.csv", sep = ",", header = TRUE)

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

summary$genotype <- factor(summary$genotype, levels = c("WT", "nqr", "fqr1","nqrfqr1"))


#########
#BARPLOT#
#########

g <- ggplot(data = summary, 
            aes(x = summary$genotype, 
                y = summary$weight))
g <- g + geom_bar(stat = "identity", color = "black", width = 0.6, position = position_dodge(0.8))
g <- g + labs(x = "", y="masse \nde 1000 graines (g)\n")
g <- g + geom_errorbar(aes(ymin = summary$weight, ymax = summary$weight + summary$standard_error), width = 0.05)
g
save_plot('masse1000grainesCol.png', g, base_aspect_ratio = 1.3)

#######
#TESTS#
#######

shapiroTest <- aggregate(masse ~ genotype, data = data, 
                         function (x) shapiro.test(x)$p.value)
#toutes les distributions sont normales sauf nqr
