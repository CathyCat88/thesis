remove(list = ls())

library("ggplot2")
library("cowplot")

# importation des données
data <- read.csv("actNQRreconstFMN.csv", sep = ",", dec = ",", header = TRUE)

# calcul de l'activité µmol/mg/min
Activity <- function(penteI, penteF) {
  return(abs(((penteF - penteI) * 10^6 / (6230 * 0.1))))
}

data$result <- Activity(data$penteI, data$penteF)
data <- data[!(data$FMN) > 500, ]

# calcul de la moyenne des [O2]/genotype
df <- aggregate(data$result, list(data$FMN), mean)
colnames(df) <- c("FMN", "moyenne")

# calcul de l'erreur standard
StandErr <- function(x) {
  se <- sd(x)/sqrt(length(x))
}

SE <- aggregate(data$result, list(data$FMN), StandErr)
df$SE <- SE$x


########
##PLOT##
########

g <- ggplot(data = df, aes(x=df$FMN, 
                           y= df$moyenne)) 
g <- g + geom_point(size = 4)
g <- g + scale_x_continuous(name = "\nFMN concentration (nM)", 
                            limits = c(0, 250), breaks = c(0,50,100,250))
g <- g + scale_y_continuous(name = "NQR activity (µmol/mg/min)\n", 
                            limits = c(200, 370))
g <- g + geom_errorbar(aes(ymin = df$moyenne-SE, 
                           ymax = df$moyenne+SE), 
                       width = 0.02)
g
save_plot('actNQRreconstFMN.png', 
          g, 
          base_aspect_ratio = 1.3)