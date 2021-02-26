# ANOVA table for GxE interaction
# > Ripped from Biol148 class
# > Test GxE interaction
#
# Author: 0cb (02.05.18)    Christian Bowman


# ======================
# ** Input .csv file **
# ======================
INPUT <- "~/Bioinf0cb/Rscripts/smaRtMath_data/Thrips.csv"

data <- read.csv(INPUT, header=T, sep=",", dec=".")

# ------------------
# setting up anova
# ------------------
y <- data[,4]                 # measured data (eg. yield); y
G <- as.factor(data[,2])      # genotype/ line; lines
E <- as.factor(data[,3])      # environment/ env. effect; x
result <- lm(y~G+E+G*E)
summaryTable <- summary(result)
anovaTable <- anova(result)
cat("\nANOVA Table"); anovaTable
cat("\n")

# ----------------------------------
# Determine conditional mean            *****NEED TO SHRINK, BUT TOO PISSED RIGHT NOW
# ----------------------------------
# ref to GxE_HOLD.R since code is mad long
# ----------------------------------
#source("GxE_HOLD.R")
sink('work.txt')
# ----------------------------------
# (Ybar)j.
# ---------------------------
ydE1.1k <- subset(data, env=="E1", select=c(yd)); ydE1.1k        # grab yd(E1); Y1k
pE1.1k <- subset(data, env=="E1", select=c(jointP)); pE1.1k     # grab prob(E1); p1k
pE1.1.s <- sum(pE1.1k)
cat("\nSum prob(E1): p1."); pE1.1.s                 # sum all prob(E1); p1.
pY.1k <- ydE1.1k * pE1.1k; pY.1k                   # pjk * Yjk into vector
sum.pY.1k <- sum(pY.1k); sum.pY.1k                  # sum 'pjk*Yjk' vector

Ybar1. <- (1/pE1.1.s)*sum.pY.1k
cat("Conditional mean of Ej: (Ybar)1. "); Ybar1.

ydE2.2k <- subset(data, env=="E2", select=c(yd)); ydE2.2k        # grab yd(E1); Y1k
pE2.2k <- subset(data, env=="E2", select=c(jointP)); pE2.2k     # grab prob(E1); p1k
pE2.2.s <- sum(pE2.2k)
cat("\nSum prob(E1): p1."); pE2.2.s                 # sum all prob(E1); p1.
pY.2k <- ydE2.2k * pE2.2k; pY.2k                   # pjk * Yjk into vector
sum.pY.2k <- sum(pY.2k); sum.pY.2k                  # sum 'pjk*Yjk' vector

Ybar2. <- (1/pE2.2.s)*sum.pY.2k
cat("Conditional mean of Ej: (Ybar)2. "); Ybar2.

ydE3.3k <- subset(data, env=="E3", select=c(yd)); ydE3.3k        # grab yd(E1); Y1k
pE3.3k <- subset(data, env=="E3", select=c(jointP)); pE3.3k     # grab prob(E1); p1k
pE3.3.s <- sum(pE3.3k)
cat("\nSum prob(E1): p3."); pE3.3.s                 # sum all prob(E1); p1.
pY.3k <- ydE3.3k * pE3.3k; pY.3k                   # pjk * Yjk into vector
sum.pY.3k <- sum(pY.3k); sum.pY.3k                  # sum 'pjk*Yjk' vector

Ybar3. <- (1/pE3.3.s)*sum.pY.3k
cat("Conditional mean of Ej: (Ybar)3. "); Ybar3.

# ---------------------------
# (Ybar).k
# ---------------------------
ydA.j1 <- subset(data, geno=="A", select=c(yd)); ydA.j1
pA.j1 <- subset(data, geno=="A", select=c(jointP)); pA.j1
pA..1s <- sum(pA.j1)
cat("\nSum prob(A): p.1"); pA..1s
pY.j1 <- ydA.j1 * pA.j1; pY.j1
sum.pY.j1 <- sum(pY.j1); sum.pY.j1

Ybar.1 <- (1/pA..1s)*sum.pY.j1
cat("Conditional mean of Ak: (Ybar).1 "); Ybar.1


ydA.j2 <- subset(data, geno=="B", select=c(yd)); ydA.j2
pA.j2 <- subset(data, geno=="B", select=c(jointP)); pA.j2
pA..2s <- sum(pA.j2)
cat("\nSum prob(A): p.2"); pA..2s
pY.j2 <- ydA.j2 * pA.j2; pY.j2
sum.pY.j2 <- sum(pY.j2); sum.pY.j2

Ybar.2 <- (1/pA..2s)*sum.pY.j2
cat("Conditional mean of Bk: (Ybar).2 "); Ybar.2


ydA.j3 <- subset(data, geno=="C", select=c(yd)); ydA.j3
pA.j3 <- subset(data, geno=="C", select=c(jointP)); pA.j3
pA..3s <- sum(pA.j3)
cat("\nSum prob(A): p.3"); pA..3s
pY.j3 <- ydA.j3 * pA.j3; pY.j3
sum.pY.j3 <- sum(pY.j3); sum.pY.j3

Ybar.3 <- (1/pA..3s)*sum.pY.j3
cat("Conditional mean of Ck: (Ybar).3 "); Ybar.3


# ---------------------------
# (Ybar)..
# ---------------------------
cat("\nGrand Mean: (Ybar).. (the following values should match)\n")
sumj. <- sum(sum.pY.1k,sum.pY.2k,sum.pY.3k)
sum.k <- sum(sum.pY.j1,sum.pY.j2,sum.pY.j3)
sumj.; sum.k

sink()
Ybar.. <- sumj.

# ---------------------------
# GxE intrxn deviation terms
# ---------------------------
# solve for ((delta)jk)
# (delta)jk = Yjk - (Ybar)j. - (Ybar).k + (Ybar)..
data$pY <- data[,4] * data[,5]
data

d11 <- data[1,4] - Ybar1. - Ybar.1 + Ybar..
d12 <- data[2,4] - Ybar1. - Ybar.2 + Ybar..
d13 <- data[3,4] - Ybar1. - Ybar.3 + Ybar..

d21 <- data[4,4] - Ybar2. - Ybar.1 + Ybar..
d22 <- data[5,4] - Ybar2. - Ybar.2 + Ybar..
d23 <- data[6,4] - Ybar2. - Ybar.3 + Ybar..

d31 <- data[7,4] - Ybar3. - Ybar.1 + Ybar..
d32 <- data[8,4] - Ybar3. - Ybar.2 + Ybar..
d33 <- data[9,4] - Ybar3. - Ybar.3 + Ybar..

cat("\nGxE Interaction terms: (delta)jk\n")
d11; d12; d13; d21; d22; d23; d31; d32; d33

# ======================
# GxE interaction plot
# ======================
#png("plot.png", height = 800, width = 600)

interaction.plot(E, G, y)   # (x.factor, trace.factor, response)

#dev.off()

# Not sure what these are for...
#
#X <- drop((E=="E1")-(E=="E3"))    #2*(E=="E1")-1 [HOLD]
#Z <- drop((G=="A")-(G=="C"))      # A= A1A1; C= A2A2
#W <- drop((G=="B")*1.0)           # B= A1A2
#X; Z; W
#XZ<-X*Z
#XW<-X*W
#fit<-lm(y~X+Z+W+XZ+XW)
#summary(fit)
#var(cbind(X,Z,W,XZ,XW))
