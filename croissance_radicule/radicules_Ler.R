rm(list = ls())
graphics.off()

library(ggplot2)
library(cowplot)
library(reshape)

data <- read.csv("radicules_Ler.csv", header = TRUE)
data <- melt(data, id = c("genotype"))

mean <- aggregate(data$value, list(data$genotype, data$variable), mean)

StandErr <- function(x){
  return(sd(x) / sqrt(length(x)))
}
se <- aggregate(data$value, list(data$genotype, data$variable), StandErr)

result <- cbind(mean, se[,3])
result[,2] <- NULL
result$temps <- c(rep(0,4), rep(3,4), rep(4,4), rep(5,4), rep(6,4), rep(7,4))
colnames(result) <- c("genotype", "moyenne", "se", "temps")
result$genotype <- factor(result$genotype, levels = (c("WT", "nqr", "air12","nqrair12")))

g <- ggplot(result, aes(temps, moyenne, 
                        group = genotype,
                        fill = genotype)) +
  geom_line(aes(colour = genotype, 
                group = genotype), size = 1) +
  geom_point(data = result, 
             aes(colour = genotype, 
                 group = genotype,
                 shape = genotype), size = 3) +
  scale_colour_hue(l = 40, c = 100) +
  scale_x_continuous(name = "\nTemps (jours)",
                     expand = c(0,0)) +
  scale_y_continuous(name = "Longueur des radicules (cm)\n",
                     expand = c(0,0)) +
  theme(legend.title = element_blank())+
  geom_errorbar(aes(ymin = result$moyenne - result$se, 
                    ymax = result$moyenne + result$se,
                    colour = genotype,
                    group = genotype), 
                width = 0.2)
save_plot("radicules_Ler.png", g, base_aspect_ratio = 1.3)

jour7 <- data[data$variable == "X7",]

stats <- lm(jour7$value ~ jour7$genotype)
summary(stats)
anova(stats)

a1 <- aov(jour7$value ~ jour7$genotype)
posthoc <- TukeyHSD(x=a1, 'jour7$genotype', conf.level=0.95)

out <- HSD.test(stats, 'jour7$genotype')
