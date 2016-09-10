graphics.off()
remove(list = ls())

library(ggplot2)
library(cowplot)

data <- read.csv("Kruk.csv", header = TRUE)
colnames(data) <- c("group", "retention", "fluo")

standard <- data[data$group == "K1" | data$group == "MQ4",]
hypocotyl <- data[data$group == "hypocotyl",]
root <- data[data$group == "root",]

g1 <- ggplot(standard, aes(x = retention, y = fluo, 
                           group = group,
                           linetype = factor(group, labels = c("vitamin K1", "vitamin K2")))) +
  geom_line() +
  theme(legend.title = element_blank()) +
  scale_fill_continuous(labels = c("vitamin K1", "vitamin K2")) +
  scale_x_continuous(name = "\nRetention time (min)", expand = c(0,0), limits = c(0, 18)) +
  scale_y_continuous(name = "Fluorescence intensity (a.u.)\n", expand = c(0,0), limits = c(-20, 3300))

g2 <- ggplot(hypocotyl, aes(retention, fluo)) +
  geom_line() +
  scale_x_continuous(name = "\nRetention time (min)", expand = c(0,0), limits = c(0, 18)) +
  scale_y_continuous(name = "Fluorescence intensity (a.u.)\n", expand = c(0,0), limits = c(0,13000))

g3 <- ggplot(root, aes(retention, fluo)) +
  geom_line() +
  scale_x_continuous(name = "\nRetention time (min)", expand = c(0,0), limits = c(0, 18)) +
  scale_y_continuous(name = "Fluorescence intensity (a.u.)\n", expand = c(0,0), limits = c(0,13000))

save_plot("standard.png", g1, base_aspect_ratio = 1.3)
save_plot("hypocotyl.png", g2, base_aspect_ratio = 1.3)
save_plot("root.png", g3, base_aspect_ratio = 1.3)
