library("cowplot")

df <- data.frame(read.csv("LerH2O2.csv", sep = ',', dec = ',', header = TRUE))

df$genotype <- factor(c("WT", "nqr", "air12", "nqrair12"), levels = c("WT", "nqr", "air12", "nqrair12"))

g <- ggplot(data = df, aes(x=df$genotype, y= df$moyenne)) 
g <- g + geom_bar(stat = "identity", color = "black", width = 0.6, position = position_dodge(0.8)) 
g <- g +labs(x = "", 
             y="hydrogen peroxide\n(nmol/g/h)\n")
g <- g + geom_errorbar(aes(ymin =df$moyenne, ymax = df$moyenne+df$SE), width = 0.05)
g
save_plot('hydrogenPeroxideLer.png', g, base_aspect_ratio = 1.3)
