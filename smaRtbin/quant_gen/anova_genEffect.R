# Test gene and epistatic effects
# > Determine epistatic (intrxn) effects btw genes
# > Determine add./dom. effects
#
# Author: 0cb (02.08.18)    Christian Bowman


# ======================
# ** Input .csv file **
# ======================
INPUT <- "~/Bioinf0cb/Rscripts/smaRtMath_data/wheat.csv"

data <- read.csv(INPUT, header=T, sep=",", dec=".")

# =============================================
# Test for epistatic effect and add/dom effect
# =============================================
y <- data[,3]           # col2 is a particular trait; y= arcsin(sqrt(col2))
x1 <- data[,4]          # gene1, where A=homo.d, B=het, C=homo.r
x2 <- data[,5]          # gene2; "
z1 <- (x1=="A") - (x1=="B")     # intrxn @gene1_add
w1 <- (x1=="H") - 0             # intrxn @gene1_dom
z2 <- (x2=="A") - (x2=="B")     # intrxn @gene2_add
w2 <- (x2=="H") - 0             # intrxn @gene2_dom
zz <- z1*z2                 # z1:z2
zw <- z1*w2                 # z1:w2
wz <- w1*z2                 # w1:z2
# wz <- w2*z1               # groups zw-wz into one entry
# zw.2 <- (zw+wz)/2
ww <- w1*w2                 # w1:w2

# test gene1-2 intrxn
result1 <- lm(y~x1+x2+x1*x2)
# test add/dom effect
result2 <- lm(y~z1+w1+z2+w2+zz+zw+wz+ww)

cat("\nGene1-Gene2 epistatic (intrxn) effect\n"); anova(result1)
cat("\nAdd/Dom effect on Gene1-2\n"); summary(result2)
