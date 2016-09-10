remove(list = ls())
graphics.off()

data <- read.csv("mbp_NQR_AIR12_XTT.csv", header = TRUE)

#############
###CALCULS###
#############

data$result <- (data$pente * 60 * 10^6)/ (24200 * 43.6)

mean <- aggregate(data$result, list(data$ajout), mean)

StandErr <- function(x){
  return(sd(x) / sqrt(length(x)))
}
se <- aggregate(data$result, list(data$ajout),StandErr)

result <- cbind(mean, se[,2])
colnames(result) <- c("ajout", "moyenne", "se")
result$ajout <- factor(result$ajout, 
                       levels = c("mbp", "NQR", "AIR12"),
                       labels = c("mb", "mb + NQR", "mb + NQR + AIR12"))

#############
###BARPLOT###
#############

g <- ggplot(data = result, 
            aes(x = ajout, 
                y = moyenne)) +
  geom_bar(stat = "identity", 
           position = "dodge",
           color = "black",
           width = 0.6) +
  scale_x_discrete(name = "") +
  scale_y_continuous(name = "Superoxide production (µmol/mg/h)\n",
                     expand = c(0,0)) +
  geom_errorbar(data = result,
                position = position_dodge(0.75),
                aes(x = ajout,
                    ymin = result$moyenne, 
                    ymax = result$moyenne + result$se),
                width = 0.05)

save_plot("mbp_NQR_AIR12_XTT.png", g, base_aspect_ratio = 1.3)

###########
###STATS###
###########

#Normalité

shapiroTest <- aggregate(result ~ ajout, data = data, 
                         function (x) shapiro.test(x)$p.value) 
#tout normal

# tests

bartlett.test(list(data$result[data$ajout == "NQR"],
                   data$result[data$ajout == "AIR12"]))
  #égalité des variances entre tous

data2 <- data[,-2]

stats <- aov(result ~ ajout, data = data2)
summary(stats)

TukeyHSD(stats)
# 3 jeux de données 3 groupes significativement différents

