graphics.off()
remove(list = ls())

data <- read.csv("RPE_hydroxyl_Ler.csv", header = TRUE)

#############
###CALCULS###
#############

mean <- aggregate(data$signal, list(data$genotype), mean)

StandErr <- function(x) {
  return(sd(x)/sqrt(length(x)))
}
se <- aggregate(data$signal, list(data$genotype), StandErr)

result <- cbind(mean, se[,2])
colnames(result) <- c("genotype", "moyenne", "se")
result$genotype <- factor(result$genotype, levels = c("WT", "nqr", "air12", "nqrair12"))

#############
###BARPLOT###
#############

g <- ggplot(result, 
            aes(genotype, moyenne, 
                fill = genotype)) +
  geom_bar(stat = "identity", 
           color = "black", 
           width = 0.6) +
  theme(legend.position = "none") +
  scale_fill_hue(l = 40, c = 100) +
  xlab("") +
  scale_y_continuous(name = "Intensité du signal\n", expand = c(0,0)) +
  geom_errorbar(aes(ymin = result$moyenne, 
                    ymax = result$moyenne + result$se), 
                width = 0.05)
save_plot("RPE_hydroxyl_Ler.png", g, base_aspect_ratio = 1.3)

###########
###TESTS###
###########

shapiroTest <- aggregate(signal ~ genotype, data = data, 
                         function (x) shapiro.test(x)$p.value)
shapiroTest
#tout normal

df <- data.frame(reference=character(0),
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
  
  bartlettResult <- bartlett.test(list(data$signal[data$genotype == reference],
                                       data$signal[data$genotype == genotype]))

  studentResult <- t.test(data$signal[data$genotype == reference],
                          data$signal[data$genotype == genotype], var.equal = TRUE)
  
  df <- rbind(df, data.frame(reference=reference,
                             genotype=genotype,
                             bartlett=bartlettResult$p.value,
                             bartlett.pass=(bartlettResult$p.value > 0.05),
                             student=studentResult$p.value,
                             student.pass=(studentResult$p.value > 0.05)))
}

stats <- lm(data$signal ~ data$genotype)
summary(stats)
anova(stats)

out <- HSD.test(stats, 'data$genotype')

