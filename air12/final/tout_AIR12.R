graphics.off()
remove(list = ls())

valeurs1 <- read.csv("MDH2 ph5.8.csv", sep = ';', dec = ",", header = TRUE)
valeurs1 <- valeurs1[,1:8]

valeurs2 <- read.csv("MDH2 ph5.8 0 + AIR12 1µL.csv", sep = ';', dec = ",", header = TRUE)

pH5.8 <- rbind(valeurs1,valeurs2)
colnames(pH5.8) <- c("wavelength", "0", "300", "600", "900", "1200", "1500", "1800") 
pH5.8 <- melt(pH5.8, id.vars="wavelength")
pH5.8$prot <- rep(c("no", "yes"), each = nrow(valeurs1))

abs320.5.8 <- pH5.8[pH5.8$wavelength == 320,]

no <- abs(abs320.5.8$value[abs320.5.8$prot == "no"] - abs320.5.8$value[abs320.5.8$prot == "no" & abs320.5.8$variable == 0])
yes <- abs(abs320.5.8$value[abs320.5.8$prot == "yes"] - abs320.5.8$value[abs320.5.8$prot == "yes" & abs320.5.8$variable == 0])

result.pH5.8 <- data.frame(rep(seq(0,30, 5),2))
result.pH5.8$abs <- append(no, yes)
result.pH5.8$prot <- rep(c("no", "yes"), each = 7)
result.pH5.8$pH <- rep("5.8", 14)
colnames(result.pH5.8) <- c("time", "abs", "prot", "pH")

val1 <- data.frame(read.csv("1 10 min MDH2 ph6.55 ttes 10s.csv", sep = ';', dec = ".", header = TRUE))
val2 <- data.frame(read.csv("2 10 min MDH2 ph6.55 ttes 10s.csv", sep = ';', dec = ".", header = TRUE))
val3 <- data.frame(read.csv("3 10 min MDH2 ph6.55 ttes 10s.csv", sep = ';', dec = ".", header = TRUE))
val <- cbind(val1[-1],val2[-1], val3[-1])
colnames(val) <- c(0:182)

val4 <- data.frame(read.csv("1µl AIR12 1h MDH2 ph6.55 ttes 10s.csv", sep = ';', dec = ".", header = TRUE))
val4 <- val4[-1]
val4 <- val4[,1:183]
colnames(val4) <- c(0:182)

pH6.55 <- rbind(val,val4)

c <- 1:ncol(pH6.55)
pH6.55.1 <- pH6.55[ , c%%6==1]
pH6.55.1 <- pH6.55.1[, (1:31)]
pH6.55.1$wavelength <- val1[,1]
pH6.55.1 <- melt(pH6.55.1, id.vars="wavelength")
pH6.55.1$prot <- rep(c("no", "yes"), each = nrow(val1))

abs320.6.55 <- pH6.55.1[pH6.55.1$wavelength == 320,]

no.1 <- abs(abs320.6.55$value[abs320.6.55$prot == "no"] - abs320.6.55$value[abs320.6.55$prot == "no" & abs320.6.55$variable == 0])
yes.1 <- abs(abs320.6.55$value[abs320.6.55$prot == "yes"] - abs320.6.55$value[abs320.6.55$prot == "yes" & abs320.6.55$variable == 0])

result.pH6.55 <- data.frame(rep(seq(0,30, 1),2))
result.pH6.55$abs <- append(no.1, yes.1)
result.pH6.55$prot <- rep(c("no", "yes"), each = 31)
result.pH6.55$pH <- rep("6.55", 62)
colnames(result.pH6.55) <- c("time", "abs", "prot", "pH")

final <- rbind(result.pH5.8, result.pH6.55)
final$group <- 0
final[final$prot == "no" & final$pH == "5.8",]["group"] <- "A"
final[final$prot == "yes" & final$pH == "5.8",]["group"] <- "B"
final[final$prot == "no" & final$pH == "6.55",]["group"] <- "C"
final[final$prot == "yes" & final$pH == "6.55",]["group"] <- "D"

group <- interaction(final$prot, final$pH)

g <- ggplot(final, aes(time, abs, shape = group, colour = group, group = group)) +
  geom_line(colour = "black") +
  geom_point(size = 4, fill = "white", colour = "black") +
  scale_shape_manual(values = c(21, 24 , 16,17))+
  theme(legend.position = "none") +
  scale_x_continuous(name = "\nTime (min)", expand = c(0,0), limits = c(0, 31)) +
  scale_y_continuous(name = "Absorbance change\n", expand = c(0,0), limits = c(0, 0.26))
g

save_plot("tout_AIR12.png", g, base_aspect_ratio = 1.3)

g <- ggplot(final, aes(time, abs, shape = group, colour = group, group = group)) +
  geom_line(colour = "black") +
  geom_point(size = 4, fill = "white", colour = "black") +
  scale_shape_manual(values = c(21, 24 , 16,17))+
  theme(legend.position = "none") +
  scale_x_continuous(name = "\nTemps (min)", expand = c(0,0), limits = c(0, 31)) +
  scale_y_continuous(name = "Absorbance\n", expand = c(0,0), limits = c(0, 0.26))
g

save_plot("tout_AIR12_fr.png", g, base_aspect_ratio = 1.3)

