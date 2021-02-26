# t-/F-Test and Regression analysis
# > Perform t-test on ppl w/ 2 alt. geno
# > Perform regr.anal for marker effect signif
# > F-test applicable to multiple samples
# > Applicable to BC, DH, & RIL
#
# Author: 0cb (02.08.18)    Christian Bowman


# ======================
# ** Input .csv file **
# ======================
INPUT <- "~/Bioinf0cb/Rscripts/smaRtMath_data/RIL(1).csv"

data <- read.csv(INPUT, header=T, sep=",", dec=".")

# ---------------------------------
# setting up 2 sample ppln; t-test
# ---------------------------------
AA11 <- subset(data, Genotype=="A1A1", select=c(y)) # y = pheno. val
AA22 <- subset(data, Genotype=="A2A2", select=c(y))
# 'alternative' = spec Ha (alt hypoth); can be "two.sided, greater, less"
# 'var.equal' forces std t-test as desc. in text; overwrites default opt
tTest <- t.test(x=AA11, y=AA22, alternative="two.sided", var.equal=TRUE)
cat("\n*[ if pval<(alpha), then major gene signif ]*"); tTest

# ====================================
# Regression analysis
# ====================================
# signif of marker effect on pheno
# ------------------------------------
# y=X0b0 + X1b1 + (E) [regr.err]
# ------------------------------------
y <- data[,2]                   # measured data
G <- as.factor(data[,3])        # genotype
x1 <- data[,5]                  # additive effect; X0 = eq.holder
result <- lm(y~x1)              # set single linear model
summaryTable <- summary(result)
anovaTable <- anova(result)     # F-test output
cat("\nSummary: "); summaryTable
cat("\nANOVA Table: "); anovaTable

