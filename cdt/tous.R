remove(list= ls())
graphics.off()

data <- read.csv("Col_sansStrat_lum_CDT.csv", header = TRUE)
#data <- read.csv("Col_strat_lum_CDT.csv", header = TRUE)

mn3 <- aggregate(data$X3days, list(data$genotype, data$cdt), mean)
mn7 <- aggregate(data$X7days, list(data$genotype, data$cdt), mean)

StandErr <- function(x){
  return(sd(x) / sqrt(length(x)))
}
se3 <- aggregate(data$X3days, list(data$genotype, data$cdt), StandErr)
se7 <- aggregate(data$X7days, list(data$genotype, data$cdt), StandErr)

result <- mn3
result$se3 <- se3[,3]
result$X7 <- mn7[,3]
result$se7 <- se7[,3]
colnames(result) <- c("genotype", "cdt", 
                      "moyenne_3j", "se_3j", 
                      "moyenne_7j", "se_7j")

result$genotype <- factor(result$genotype, 
                          levels = c("WT", "nqr", "fqr1","nqrfqr1"))
g3 <- ggplot(data = result, 
            aes(x = cdt, 
                y = moyenne_3j)) +
  geom_line(data = result,
            aes(colour = genotype, 
                group = genotype), size = 1) +
  geom_point(data = result, 
             aes(colour = genotype, 
                 group = genotype,
                 shape = genotype), size = 3) +
  xlab("\n Durée du traitement (jours)") +
  ylab("Germination (%) \n") +
  theme(legend.title = element_blank()) +
  geom_errorbar(data = result, 
                aes(ymin = result$moyenne_3j - result$se_3j, 
                    ymax = result$moyenne_3j + result$se_3j, 
                    colour = genotype, 
                    group = genotype), 
                width = 0.2)

#save_plot("Col_sansStrat_lum_CDT_3j.png", g3, base_aspect_ratio = 1.3)
save_plot("Col_strat_lum_CDT_3j.png", g3, base_aspect_ratio = 1.3)


g7 <- ggplot(data = result, 
            aes(x = cdt, 
                y = moyenne_7j)) +
  geom_line(data = result,
            aes(colour = genotype, 
                group = genotype), size = 1) +
  geom_point(data = result, 
             aes(colour = genotype, 
                 group = genotype,
                 shape = genotype), size = 3) +
  xlab("\n Durée du traitement (jours)") +
  ylab("Germination (%) \n") +
  theme(legend.title = element_blank()) +
  geom_errorbar(data = result, 
                aes(ymin = result$moyenne_7j - result$se_7j, 
                    ymax = result$moyenne_7j + result$se_7j, 
                    colour = genotype, 
                    group = genotype), 
                width = 0.2)

save_plot("Col_sansStrat_lum_CDT_7j.png", g7, base_aspect_ratio = 1.3)
#save_plot("Col_strat_lum_CDT_7j.png", g7, base_aspect_ratio = 1.3)
