# ANOVA Table for intra-class correlation
# > Estimate narrow-sense (h^2) heritability
# > Find m0 of ppln
# > Determine intra-class corrln (rhoFS)
# > Estimated variance components
# >> (h^2) = 2*(rhoFS)
#
# Author: 0cb (02.26.18)    Christian Bowman


# ======================
# ** Input .csv file **
# ======================
INPUT <- "~/Bioinf0cb/Rscripts/smaRtMath_data/UnbalancedFamilyData2.csv"

data <- read.csv(INPUT, header=T, sep=",", dec=".")

# ----------------------
# setting up ANOVA
# ----------------------
sink('work.txt')
# ----------------------
y <- data[,3]
x <- as.factor(data[,1])
result <- lm(y~x)
anovaTable <- anova(result)
cat("\nANOVA Table: \n"); anovaTable

# ------------------------
# finding m0; measurables
# ------------------------
# you can probably use 'count' or 'aggregate' fx
# library(plyr); ?count, ?aggregate
# ------------------------
m <- c(2,3,4,3,4,4,3,4,4,2,4,3,4,2,4)   # (#) rep per group
n <- length(m)                          # (#) groups
sum.m <- sum(m)
sum.m2 <- sum(m^2)
m0 <- (sum.m-sum.m2/sum.m)*(1/(n-1))    # (#) measurements

cat("\n# Groups (n): "); n
cat("Measurements (m0): "); m0

# =========================
#  finding (rhoFS)
# =========================
# intra-class corrln coeff.
# ------------------------
# Estimated variance components
# ------------------------
# ** MS = expected MS when finding est. var components
# ------------------------
cat("Estimated variance components: \n")

s.W.2 <- anovaTable[2,3]
s.B.2 <- (anovaTable[1,3] - s.W.2)/m0
rho.FS <- s.B.2/(s.W.2+s.B.2)

cat("Variance within groups (sigW)^2: "); s.W.2
cat("Variance between groups (sigB)^2: "); s.B.2
cat("Intra-class corrln (rhoFS): "); rho.FS

# ----------------
# (MSB-MSW)/[MSB +(m0-1)MSW] == (sigB)/(sigB+sigW)
# ----------------
Check1 <- (anovaTable[1,3]-anovaTable[2,3])/(anovaTable[1,3]+(m0-1)*anovaTable[2,3])
cat("Check1: Ans='rho.FS'"); Check1

# ----------------------------------
# finding narrow-sense heritability
# ----------------------------------
naHerit <- 2*rho.FS
cat("\nEst. narrow-sense heritability: "); naHerit

sink()
