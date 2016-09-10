library("cowplot")

df <- data.frame(read.csv("LerH2O2.csv", sep = ',', dec = ',', header = TRUE))

df$genotype <- factor(c("WT", "nqr", "air12", "nqrair12"), levels = c("WT", "nqr", "air12", "nqrair12"))

g <- ggplot(data = df, aes(x=df$genotype, y= df$moyenne)) +
  geom_bar(stat = "identity", color = "black", width = 0.6, position = position_dodge(0.8)) +
  xlab("") +
  scale_y_continuous(name = "Hydrogen peroxide\n(nmol/g/h)\n", expand = c(0,0))
g <- g + geom_errorbar(aes(ymin =df$moyenne, ymax = df$moyenne+df$SE), width = 0.05)
g
save_plot('hydrogenPeroxideLer.png', g, base_aspect_ratio = 1.3)

g <- ggplot(data = df, aes(x=df$genotype, 
                           y= df$moyenne, 
                           group = genotype, 
                           fill = genotype)) +
  geom_bar(stat = "identity", 
           color = "black", 
           width = 0.6, 
           position = position_dodge(0.8)) +
  theme(legend.position = "none") +
  scale_fill_hue(l = 40, c = 100) +
  xlab("") +
  scale_y_continuous(name = "Péroxyde d'hydrogène (nmol/g/h)\n", expand = c(0,0))
g <- g + geom_errorbar(aes(ymin =df$moyenne, ymax = df$moyenne+df$SE), width = 0.05)
g
save_plot('peroxyde_hydrogene_Ler.png', g, base_aspect_ratio = 1.3)
