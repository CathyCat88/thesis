remove(list = ls())
graphics.off()

data <- read.csv("sunflower_BC.csv", header = TRUE)

#############
###CALCULS###
#############

# soustraction des blancs
data$mesure2 <- 0

last.rep <- 0
last.mes <- 0

for (i in (1:nrow(data))) {
  rep <- data[i,]$repetition
  mes <- data[i,]$mesure
  if (rep != last.rep) {
    last.rep <- rep
    last.mes <- mes
  }
  data[i,]$mesure2 <- mes - last.mes
}

data2 <- data[data$genotype != "blank",]

data2$result <- (data2$mesure2 / data2$masse)*10^6 /(24200*2)

mean <- aggregate(data2$result, list(data2$genotype, data2$imbibition), mean)

StandErr <- function(x){
  return(sd(x) / sqrt(length(x)))
}
sd <- aggregate(data2$result, list(data2$genotype, data2$imbibition), StandErr)

result <- cbind(mean, sd[,3])
colnames(result) <- c("genotype", "imbibition", "moyenne", "se")
result$imbibition <- factor(result$imbibition, levels = c("no", "early", "late"))
result$genotype <- factor(result$genotype, 
                          levels = c("B", "B_aged", "Bs", "Bs_aged", "C", "C_aged", "Cs", "Cs_aged"),
                          labels = c("B", "B âgé", "Bs", "Bs âgé", "C", "C âgé", "Cs", "Cs âgé"))

#############
###BARLOTS###
#############

result.B <-result[result$genotype == list("B", "B âgé", "Bs", "Bs âgé"),]

g1 <- ggplot(data = result.B, 
       aes(x = imbibition,
           y = moyenne,
           group = genotype,
           fill = genotype))+
  geom_bar(stat = "identity",
           position = "dodge",
           color = "black",
           width = 0.75) +
  labs(fill = "Génotype") +
  scale_x_discrete(name = "\n Imbibition", 
                     labels = c("no" = "aucune", "early" = "précoce", "late" = "tardive")) +
  scale_y_continuous(name = "Production de superoxyde \n (µM/g/h) \n",
                     expand = c(0,0)) +
  geom_errorbar(data = result.B,
                position = position_dodge(0.75),
                aes(x = imbibition,
                    ymin = result.B$moyenne, 
                    ymax = result.B$moyenne + result.B$se),
                width = 0.1)

save_plot("sunflower_B.png", g1, base_aspect_ratio = 1.3)

result.C <-result[result$genotype == list("C", "C âgé", "Cs", "Cs âgé"),]

g2 <- ggplot(data = result.C, 
            aes(x = imbibition,
                y = moyenne,
                group = genotype,
                fill = genotype))+
  geom_bar(stat = "identity",
           position = "dodge",
           color = "black",
           width = 0.75) +
  labs(fill = "Génotype") +
  scale_x_discrete(name = "\n Imbibition", 
                   labels = c("no" = "aucune", "early" = "précoce", "late" = "tardive")) +
  scale_y_continuous(name = "Production de superoxyde \n (µM/g/h) \n",
                     expand = c(0,0)) +
  geom_errorbar(data = result.C,
                position = position_dodge(0.75),
                aes(x = imbibition,
                    ymin = result.C$moyenne, 
                    ymax = result.C$moyenne + result.C$se),
                width = 0.1)

save_plot("sunflower_C.png", g2, base_aspect_ratio = 1.3)

###########
###STATS###
###########

shapiroTest <- tapply(data2$result, interaction(data2$genotype, data2$imbibition, drop = TRUE), shapiro.test)
# tous normaux sauf 2 C aged no et Bs aged early

bartlett.test(list(data2$result[data2$genotype == "B" & data2$imbibition == "no"],
                   data2$result[data2$genotype == "B_aged" & data2$imbibition == "no"]))

t.test(data2$result[data2$genotype == "C_aged" & data2$imbibition == "no"],
       data2$result[data2$genotype == "C" & data2$imbibition == "no"])
wilcox.test(data2$result[data2$genotype == "C_aged" & data2$imbibition == "no"],
            data2$result[data2$genotype == "C" & data2$imbibition == "no"])
