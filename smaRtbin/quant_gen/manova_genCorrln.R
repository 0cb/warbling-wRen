# MANOVA table for genetic correlation
# ;(Multivariate)ANOVA
# > Ripped from BIol148
# > Determine w.in/btw fam var-cov matrix @2traits
# > Determine trait herit & gen corrln
#
# Author: 0cb (03.20.18)    Christian Bowman


# ======================
# ** Input .csv file **
# ======================
INPUT   <- "~/Bioinf0cb/Rscripts/smaRtMath_data/sheepDataHW08.csv"
data    <- read.csv(INPUT, header=T, sep=",", dec=".")

family  <- as.factor(data[,1])

m       <- table(family)
n       <- length(m)
m0      <- (sum(m)-sum(m^2)/sum(m))/(n-1)

cat("\nMeasures: m0"); m0

# -------------
# setup manova
# -------------
y       <- cbind(data[,3],data[,4])
fit     <- manova(y~family)
ss      <- summary.aov(fit)

cat("\nSummary of responses: "); ss

dfB     <- ss[[1]][1,1]
dfW     <- ss[[1]][2,1]

cat("\nDegrees of freedom: ")
cat("\ndfBetween family: "); dfB
cat("\ndfWithin family: "); dfW

# sum of squares cross products (sscp)
sscp    <- summary.manova(fit)
sscpB   <- sscp$SS$family       #use to setup ANCOVA
sscpW   <- sscp$SS$Residuals    

# mean of squares cross products (mscp)
mscpB   <- sscpB/dfB            
mscpW   <- sscpW/dfW            # = (sigW)xy
sigB    <- (mscpB-mscpW)/m0

cat("\nWithin family var-cov: \n"); mscpW
cat("\nBetween family var-cov: \n"); sigB

# -------------------
# trait heritability
# -------------------
h2x     <- 4*sigB[1,1]/(sigB[1,1]+mscpW[1,1])   # 1st coeff change dep sib
h2y     <- 4*sigB[2,2]/(sigB[2,2]+mscpW[2,2])   # ie 2=full; 4=half
rA      <- sigB[1,2]/sqrt(sigB[1,1]*sigB[2,2])

cat("\nHeritability: ")
cat("\nTrait X: "); h2x
cat("\nTrait Y: "); h2y
cat("\nGen. corrln: "); rA


# end
