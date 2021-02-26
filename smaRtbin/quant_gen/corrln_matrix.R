# Correlation Matrix
# > Ripped from BIol148
# > Produce corrln matrix via 'cor()'
# > Determine:
#   + trait herit & gen corrln
#
# Author: 0cb (03.20.18)    Christian Bowman


# ======================
# ** Input .csv file **
# ======================
INPUT   <- "~/Bioinf0cb/Rscripts/smaRtMath_data/simulDataHW08.csv"
data    <- read.csv(INPUT, header=T, sep=",", dec=".")

# -----------------------
# generate corrln matrix
# -----------------------
xy      <- data[,-1]
rMatr   <- cor(xy)
cat("\nCorrelation matrix: \n"); rMatr

# ----------------------------
# x-gen corrln coeff @2traits
# ----------------------------
# we're looking at x.x, y.y, x.y, y.x
# ----------------------------
rx0.x   <- cor(data[,2],data[,4])
ry0.y   <- cor(data[,3],data[,5])
rx0.y   <- cor(data[,2],data[,5])
ry0.x   <- cor(data[,3],data[,4])

# ----------------------------------------------
# heritability per trait; gen corrln btw traits
# ----------------------------------------------
h2x     <- 2*rx0.x
h2y     <- 2*ry0.y
rA      <- 0.5*(rx0.y+ry0.x)/sqrt(rx0.x*ry0.y)

cat("\nHeritability: ")
cat("\nTrait X: "); h2x
cat("\nTrait Y: "); h2y
cat("\nGen. corrln: "); rA

# end
