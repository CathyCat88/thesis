graphics.off()
remove(list = ls())

data <- read.csv("act_NQR_quinones.csv", header = TRUE)

# activité NQR en µmol/mg/min
Activity <- function(pente1, pente2, prot) {
  return((pente2 - pente1) * 10^6 /(prot *6230))
}

data$activity <- Activity(data$pente1, data$pente2, data$prot)

mean <- aggregate(data$activity, list(data$quinone), mean)

StandErr <- function(x) {
  se <- sd(x)/sqrt(length(x))
}
se <- aggregate(data$activity, list(data$quinone), StandErr)

result <- cbind(mean, se[,2])
colnames(result) <- c("quinone", "mean", "se")
result
