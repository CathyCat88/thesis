remove(list = ls())

data1 <- read.csv("germination_sunflower_BC.csv", header = TRUE)
data2 <- read.csv("germination_sunflower_BC_aged_2.csv", header = TRUE)
data <- rbind(data1, data2)


data.B <- data[data$genotype == "B" | data$genotype == "Bs",]
data.B$treatment <- factor(data.B$treatment, 
                           levels = c("no", "yes"),
                           labels = c("non", "oui"))

g1 <- ggplot(data = data.B, 
             aes(x = temps, 
                 y = germination, 
                 colour = genotype,
                 linetype = treatment)) +
  geom_line(size = 1) +
  labs(colour = "Génotype", linetype = "CDT") +
  scale_x_continuous(name = "\nTemps (heures)", 
                     expand = c(0,0),
                     limits = c(0,150)) +
  scale_y_continuous(name = "Germination (%)\n",
                     expand = c(0,0.3),
                     limits = c(0,100)) +
  geom_errorbar(aes(x = temps, 
                    ymin = data.B$germination - data.B$se,
                    ymax = data.B$germination + data.B$se),
                width = 2.5)

save_plot("germination_sunflower_B.png", g1, 
          base_aspect_ratio = 1.3)


data.C <- data[data$genotype == "C" | data$genotype == "Cs",]
data.C$treatment <- factor(data.C$treatment, 
                           levels = c("no", "yes"),
                           labels = c("non", "oui"))

g2 <- ggplot(data = data.C, 
             aes(x = temps, 
                 y = germination, 
                 colour = genotype,
                 linetype = treatment)) +
  geom_line(size = 1) +
  labs(colour = "Génotype", linetype = "CDT") +
  scale_x_continuous(name = "\nTemps (heures)", 
                     expand = c(0,0),
                     limits = c(0,150)) +
  scale_y_continuous(name = "Germination (%)\n",
                     expand = c(0,0.3),
                     limits = c(0,100)) +
  geom_errorbar(aes(x = temps, 
                    ymin = data.C$germination - data.C$se,
                    ymax = data.C$germination + data.C$se),
                width = 2.5)

save_plot("germination_sunflower_C.png", g2, 
          base_aspect_ratio = 1.3)