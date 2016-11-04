graphics.off()
remove(list = ls())

library(ggplot2)
library(cowplot)
library(reshape2)

data <- data.frame(read.csv("30 min MDH2 1µl AIR12 0.5xanth Xox ph6.55 ttes 10s.csv",
                            sep = ";",
                            header = TRUE))

wavelength <- data$wavelength
data <- data[,-1]
colnames(data) <- c(0:180)

c <- 1:ncol(data)
df <- data[ , c%%6==1]
df <- df[, (1:31)]
df$wavelength <- wavelength
df <- melt(df, id.vars="wavelength")

abs320 <- df[df$wavelength == 320,]

norm <- abs(abs320$value - abs320$value[abs320$variable == 0])

result <- data.frame(seq(0,30, 1))
result$abs <- norm
colnames(result) <- c("time", "abs")

g <- ggplot(result, aes(time, abs)) +
  geom_line() +
  geom_point(size = 2.5) +
  theme(legend.position = "none") +
  scale_x_continuous(name = "\nTime (min)", expand = c(0,0), limits = c(0, 31)) +
  scale_y_continuous(name = "Absorbance\n", expand = c(0,0), limits = c(0,0.21)) 
g
save_plot("MDH2_AIR12_pH6.55_xanthOx.png", g, base_aspect_ratio = 1.3)

g <- ggplot(result, aes(time, abs)) +
  geom_line() +
  geom_point(size = 4,
             shape = 15) +
  theme(legend.position = "none") +
  scale_x_continuous(name = "", expand = c(0,0), limits = c(0, 31)) +
  scale_y_continuous(name = "", expand = c(0,0), limits = c(0,0.26))
g
save_plot("MDH2_AIR12_pH6.55_xanthOx_fr.png", g, base_aspect_ratio = 1.3)


g <- ggplot(result, aes(time, abs)) +
  geom_line() +
  geom_point(shape = 15, size = 4) +
  theme(legend.position = "none") +
  scale_x_continuous(name = "", expand = c(0,0), limits = c(0, 31)) +
  scale_y_continuous(name = "", expand = c(0,0), limits = c(0,0.21)) + 
  theme(panel.border = element_blank(),
        line = element_blank(), 
      legend.key = element_blank(),
      axis.ticks = element_blank(),
      axis.text.y = element_blank(),
      axis.text.x = element_blank(),
      panel.grid = element_blank(),
      panel.grid.minor = element_blank(), 
      panel.grid.major = element_blank(),
      panel.background = element_blank(),
      plot.background = element_rect(fill = "transparent",colour = NA))
g
save_plot("MDH2_AIR12_pH6.55_xanthOx_blanc.png", g, base_aspect_ratio = 1.3)

