remove(list = ls())
graphics.off()

qN <- read.csv("qN10.csv", header = TRUE)
qP <- read.csv("qP10.csv", header = TRUE)
FvFm <- read.csv("FvFm10.csv", header = TRUE)

all <- rbind(qN, qP)
all <- rbind(all, FvFm)

#############
###CALCULS###
#############

mean <- aggregate(all$fluo, list(all$genotype, all$stress, all$parametre), mean)

StandErr <- function(x) {
  se <- sd(x)/sqrt(length(x))
}
se <- aggregate(all$fluo, list(all$genotype, all$stress, all$parametre), StandErr)

results <- cbind(mean, se[,4])
colnames(results) <- c("genotype", "stress", "parametre", "moyenne", "se")

results$moyenne <- round(x = results$moyenne, digits = 3)
results$se <- round(x = results$se, digits = 3)

write.csv(results, file = "QR_photosynthese_nouveau.csv")

###########
###STATS###
###########

shapiroTest <- aggregate(fluo ~ interaction(parametre, genotype, stress), data = all, function (x) shapiro.test(x)$p.value)
#tout normal sauf qP NQR froid

df <- data.frame(reference=character(0),
                 genotype=character(0),
                 stress=character(0),
                 parametre = character(0),
                 bartlett=numeric(0),
                 bartlett.pass=logical(0),
                 student=numeric(0),
                 student.pass=logical(0))

for (params in list(c("WT", "nqr"),
                    c("WT", "fqr1"),
                    c("WT", "nqrfqr1"))) {
  
  reference <- params[1]
  genotype <- params[2]
  
  for (stress in list("control", "froid", "chaud")) {
    for(parametre in list("qP", "qN", "FvFm")) {
  
  bartlettResult <- bartlett.test(list(all$fluo[all$genotype == reference & all$stress == stress & all$parametre == parametre],
                                       all$fluo[all$genotype == genotype & all$stress == stress & all$parametre == parametre]))
  
  studentResult <- t.test(all$fluo[all$genotype == reference & all$stress == stress & all$parametre == parametre],
                          all$fluo[all$genotype == genotype & all$stress == stress & all$parametre == parametre], var.equal = TRUE)
  
  df <- rbind(df, data.frame(reference=reference,
                             genotype=genotype,
                             stress = stress,
                             parametre = parametre,
                             bartlett=bartlettResult$p.value,
                             bartlett.pass=(bartlettResult$p.value > 0.05),
                             student=studentResult$p.value,
                             student.pass=(studentResult$p.value > 0.1)))
    }
  }
}

