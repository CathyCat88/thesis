graphics.off()
remove(list = ls())

library(ggplot2)
library(cowplot)

data <- read.csv("Kruk.csv", header = TRUE)
colnames(data) <- c("group", "retention", "fluo")

data$fluo <- (data$fluo)/100

standard <- data[data$group == "standard",]
hypocotyl <- data[data$group == "hypocotyl",]
root <- data[data$group == "root",]

g1 <- ggplot(standard, aes(retention, fluo)) +
  geom_line() +
  scale_x_continuous(name = "\nRetention time (min)", expand = c(0,0), limits = c(0, 18)) +
  scale_y_continuous(name = "Fluorescence intensity (a.u.)\n", expand = c(0,0), limits = c(-10,3000))

g2 <- ggplot(hypocotyl, aes(retention, fluo)) +
  geom_line() +
  scale_x_continuous(name = "\nRetention time (min)", expand = c(0,0), limits = c(0, 18)) +
  scale_y_continuous(name = "Fluorescence intensity (a.u.)\n", expand = c(0,0), limits = c(0,130))

g3 <- ggplot(root, aes(retention, fluo)) +
  geom_line() +
  scale_x_continuous(name = "\nRetention time (min)", expand = c(0,0), limits = c(0, 18)) +
  scale_y_continuous(name = "Fluorescence intensity (a.u.)\n", expand = c(0,0), limits = c(0,130))

save_plot("vitaminK1.png", g1, base_aspect_ratio = 1.3)
save_plot("hypocotyl.png", g2, base_aspect_ratio = 1.3)
save_plot("root.png", g3, base_aspect_ratio = 1.3)
