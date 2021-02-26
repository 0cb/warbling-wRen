# Holding for HW03_Q4

# ======================
# ** Input .csv file **
# ======================
INPUT <- "~/Bioinf0cb/Rscripts/smaRtMath_data/Thrips.csv"

data <- read.csv(INPUT, header=T, sep=",", dec=".")

# ----------------------------------
# Determine conditional mean            *****NEED TO SHRINK, BUT TOO PISSED RIGHT NOW
# ----------------------------------
# (Ybar)j.= (1/pj.)*sum((pjk)*(Yjk))    
# (Ybar).k= (1/p.k)*sum((pjk)*(Yjk))
# ----------------------------------
# here, j&k = n terms for Y[avg yield] and p[jointProb/freq]
# ----------------------------------
ydE1.1k <- subset(data, env=="E1", select=c(yd))#; ydE1.1k        # grab yd(E1); Y1k
pE1.1k <- subset(data, env=="E1", select=c(jointP))#; pE1.1k     # grab prob(E1); p1k
pE1.1.s <- sum(pE1.1k)
#cat("\nSum prob(E1): p1."); pE1.1.s                 # sum all prob(E1); p1.
pY.1k <- ydE1.1k * pE1.1k#; pY.1k                   # pjk * Yjk into vector
sum.pY.1k <- sum(pY.1k)#; sum.pY.1k                  # sum 'pjk*Yjk' vector

Ybar1. <- (1/pE1.1.s)*sum.pY.1k
cat("Conditional mean of Ej: (Ybar)1. "); Ybar1.

ydE2.2k <- subset(data, env=="E2", select=c(yd))#; ydE1.1k        # grab yd(E1); Y1k
pE2.2k <- subset(data, env=="E2", select=c(jointP))#; pE1.1k     # grab prob(E1); p1k
pE2.2.s <- sum(pE2.2k)
#cat("\nSum prob(E1): p1."); pE1.1.s                 # sum all prob(E1); p1.
pY.2k <- ydE2.2k * pE2.2k#; pY.1k                   # pjk * Yjk into vector
sum.pY.2k <- sum(pY.2k)#; sum.pY.1k                  # sum 'pjk*Yjk' vector

Ybar2. <- (1/pE2.2.s)*sum.pY.2k
cat("Conditional mean of Ej: (Ybar)2. "); Ybar2.

ydE3.3k <- subset(data, env=="E3", select=c(yd))#; ydE1.1k        # grab yd(E1); Y1k
pE3.3k <- subset(data, env=="E3", select=c(jointP))#; pE1.1k     # grab prob(E1); p1k
pE3.3.s <- sum(pE3.3k)
#cat("\nSum prob(E1): p1."); pE1.1.s                 # sum all prob(E1); p1.
pY.3k <- ydE3.3k * pE3.3k#; pY.1k                   # pjk * Yjk into vector
sum.pY.3k <- sum(pY.3k)#; sum.pY.1k                  # sum 'pjk*Yjk' vector

Ybar3. <- (1/pE3.3.s)*sum.pY.3k
cat("Conditional mean of Ej: (Ybar)3. "); Ybar3.
