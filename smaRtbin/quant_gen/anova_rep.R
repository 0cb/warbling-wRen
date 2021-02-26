# ANOVA table for Linear Regression
# > Ripped from Biol148 class
# > Used in comparison of groups over different variables
#
# Author: 0cb (02.05.18)    Christian Bowman


# ======================
# ** Input .csv file **
# ======================
INPUT <- "~/Bioinf0cb/Rscripts/smaRtMath_data/Cows.csv"

data <- read.csv(INPUT, header=T, sep=",", dec=".")

c1 <- colnames(data)[1]
c2 <- colnames(data)[2]
c3 <- colnames(data)[3]

# -------------------
# setting up anova
# -------------------
y <- data[,3]
x <- as.factor(data[,1])            # Between (B)
result <- lm(y~x)
anovaTable <- anova(result)
cat("\nANOVA Table"); anovaTable
cat("\n")

# -------------------
# grabbing (rho)
# -------------------
mm<-table(x)
#cat("table(x)"); mm
t1<-sum(mm)
t2<-sum(mm^2)
n<-length(mm)
m0<-(t1-t2/t1)/(n-1)
MSB<-anovaTable[1,3]    # mean sq. (MS) = SS(n)/df(n)
MSE<-anovaTable[2,3]    # " 
                        # (sigE)^2 = MS(E); (sigB)^2 = (MS(B) - MS(E))/m

# estimated repeatability (intra-class correlation coeff)
# can be found w/ (sigB)^2/((sigB)^2 + (sigE)^2); E = RNG error
rho<-(MSB-MSE)/(MSB+(m0-1)*MSE)

cat("\nNumber repeated measurements: m0"); m0
cat("Variance ratio, repeatability: rho"); rho
cat("\n")
