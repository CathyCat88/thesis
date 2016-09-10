remove(list = ls())
graphics.off()

data <- read.csv("Arabidopsis_germination_aged_10.csv", 
                 header = TRUE)

#data <- read.csv("Arabidopsis_germination_nonaged_10.csv", 
                 header = TRUE)

data$temperature <- factor(data$temperature, 
                           levels = c("LT", "C", "HT"),
                           labels = c("T basse", "Contrôle", "T haute"))

g <- ggplot(data =  data, aes(x = temps, 
                              y = germination, 
                              group = temperature, 
                              color = temperature)) +
  geom_line(size = 1) +
  theme(legend.title = element_blank()) +
  scale_x_continuous(name = "\nTemps (heures)", expand = c(0,0)) +
  scale_y_continuous(name = "Germination (%) \n", expand = c(0,0), limits = c(0,100)) +
  geom_errorbar(aes(ymin = data$germination - data$se, 
                    ymax = data$germination + data$se),
                width = 2.5)

save_plot("Arabidopsis_germination_aged_10.png", g, base_aspect_ratio = 1.3)
#save_plot("Arabidopsis_germination_nonaged_10.png", g, base_aspect_ratio = 1.3)
