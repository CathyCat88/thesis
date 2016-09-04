remove(list = ls())
graphics.off()

library("ggplot2")
library("cowplot")

data <- read.csv("lumière5C3jstrat.csv", header = TRUE)
data <- data[data$ecotype == "Ler",]

#########
#CALCULS#
#########

pr <- (data[, c("X0", "X1", "X2", "X3", "X4", "X7")] / data$total)*100
data1 <- cbind(data[,1:2], pr)

data2 <- melt(data1, id = c("genotype", "ecotype"))
data2 <- data2[data2$variable == "X3", ]
mean <- aggregate(data2$value, list(data2$genotype), mean)

StandErr <- function(x){
  return(sd(x) / sqrt(length(x)))
}
se <- aggregate(data2$value, list(data2$genotype), StandErr)

result <- cbind(mean, se[,2])
colnames(result) <- c("genotype", "moyenne", "se")
result$genotype <- factor(result$genotype, levels = c("WT", "nqr", "air12","nqrair12"))

#########
#BARPLOT#
#########

g <- ggplot(result, aes(genotype, moyenne, 
            group = genotype, fill = genotype)) +
  theme(legend.position = "none") +
  geom_bar(stat = "identity", 
           color = "black", width = 0.6) +
  scale_fill_hue(l = 40, c = 100) +
  xlab("") +
  scale_y_continuous(name = "Germination (%) à 72 heures \n",                            
                     expand = c(0,0), limits = c(0,76)) +
  geom_errorbar(aes(ymin = result$moyenne, ymax = result$moyenne + result$se), width = 0.05)
g
save_plot("5C_Ler_diagramme.png", g, base_aspect_ratio = 1.3)


#######
#STATS#
#######

shapiroTest <- aggregate(value ~ genotype, data = data2, 
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
  
  bartlettResult <- bartlett.test(list(data2$value[data2$genotype == reference],
                                       data2$value[data2$genotype == genotype]))
  
  #égalité des variances sauf pour nqrair12
  
  studentResult<- t.test(data2$value[data2$genotype == reference],
                         data2$value[data2$genotype == genotype], var.equal = TRUE)
  
  result <- rbind(result, data.frame(reference=reference,
                                     genotype=genotype,
                                     bartlett=bartlettResult$p.value,
                                     bartlett.pass=(bartlettResult$p.value > 0.05),
                                     student=studentResult$p.value,
                                     student.pass=(studentResult$p.value > 0.05)))
}

