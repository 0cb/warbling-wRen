# ANOVA Table for F2 population
# > Ripped from Biol148 class
# > F-test for multiple samples
# > Detect major genes in F2 ppln
#
# Author: 0cb (02.08.18)    Christian Bowman


# ======================
# ** Input .csv file **
# ======================
INPUT <- "~/Bioinf0cb/Rscripts/smaRtMath_data/F2.csv"

data <- read.csv(INPUT, header=T, sep=",", dec=".")

c1 <- colnames(data)[1]
c2 <- colnames(data)[2]
c3 <- colnames(data)[3]

# ----------------------
# setting up ANOVA
# ----------------------
y <- data[,3]     # yd/ measured data
x <- as.factor(data[,2])     # genotype (eg A1A1)
result <- lm(y~x)
smry <- summary(result)
anovaTable <- anova(result)
cat("\nANOVA Table"); anovaTable
cat("\n")

