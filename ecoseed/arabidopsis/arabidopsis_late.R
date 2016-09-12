graphics.off()
remove(list = ls())

data <- read.csv("arabidopsis_late.csv", header = TRUE)

blank <- data[data$temperature == "blank",]
mean.blank <- mean(blank$OD)

data2 <- data[data$temperature != "blank",]

Concentration <- function(OD, weight) {
  return((OD - mean.blank)/ (weight*24200*2*10^-6))
}

data2$result <- Concentration(data2$OD, data2$weight)

mean <- aggregate(data2$result, list(data2$temperature, data2$treatment), mean)

StandErr <- function(x) {
  se <- sd(x)/sqrt(length(x))
}
se <- aggregate(data2$result, list(data2$temperature, data2$treatment), StandErr)

result <- cbind(mean, se[,3])
colnames(result) <- c("conditions", "treatment", "mean", "se")
result$treatment <- factor(result$treatment, 
                           levels = c("no", "aged"))
result$conditions <- factor(result$conditions,
                            levels = c("CTR", "LT", "HT"),
                            labels = c("contrôle", "T basse", "T haute"))

g <- ggplot(result, 
            aes(x = treatment,
                y = mean, 
                group = conditions,
                fill = conditions)) +
  geom_bar(stat = "identity",
            position = "dodge",
            color = "black",
            width = 0.75) +
  labs(fill = "conditions \nde culture") +
  scale_x_discrete(name = "", 
                   labels = c("no" = "non traité", "aged" ="âgé")) +
  scale_y_continuous(name = "Production de superoxyde \n(µM/g/h) \n") +
  geom_errorbar(data = result,
                position = position_dodge(0.75),
                aes(x = treatment,
                    ymin = result$mean, 
                    ymax = result$mean + result$se),
                width = 0.1)

save_plot("arabidopsis_late.png", g, base_aspect_ratio = 1.3)

###########
###STATS###
###########

shapiroTest <- tapply(data2$result, interaction(data2$temperature, data2$treatment, drop = TRUE), shapiro.test)
#tout normal

bartlett.test(list(data2$result[data2$temperature == "CTR" & data2$treatment == "aged"],
                   data2$result[data2$temperature == "LT" & data2$treatment == "aged"]))

t.test(data2$result[data2$temperature == "HT" & data2$treatment == "no"],
       data2$result[data2$temperature == "CTR" & data2$treatment == "no"], var.equal = TRUE)

t.test(data2$result[data2$temperature == "HT" & data2$treatment == "aged"],
       data2$result[data2$temperature == "HT" & data2$treatment == "no"], var.equal = TRUE)