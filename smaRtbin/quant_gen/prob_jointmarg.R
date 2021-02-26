# Marginal Probabilities
# > Multiplying 2 columns...
#
# Author: 0cb (02.05.18)    Christian Bowman


# ======================
# ** Input .csv file **
# ======================
INPUT <- "~/Bioinf0cb/Rscripts/smaRtMath_data/MT_Q03.csv"

data <- read.csv(INPUT, header=T, sep=",", dec=".")

#c1 <- colnames(data)[1]
#c2 <- colnames(data)[2]
#c3 <- colnames(data)[3]
#c4 <- colnames(data)[4]
#c5 <- colnames(data)[5]

# ---------------------------------
# Find joint probabilities
# ---------------------------------
# conditional prob * marginal prob
# ---------------------------------
data$c2 <- data[,2] * data[,4]
data$c3 <- data[,3] * data[,4]
#data$c4 <- data[,4] * data[,5]

# ---------------------------------
# Pheno. marginal probabilities
# ---------------------------------
# sum of joint probabilities 
# ---------------------------------
m2 <- sum(data$c2)
m3 <- sum(data$c3)
#m4 <- sum(data$c4)

cat("\nJoint probabilities: c2 c3 #c4\n"); data
cat("\nMarginal probabilities\n"); m2; m3; #m4
