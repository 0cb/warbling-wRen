# ANOVA table for broad-sense heritability
# > Determine variance components
# > Use brSense herit (H^2) for (ttl genetic var/ ttl pheno var)
# > if 0 corrln of gen effects; cov=0
#
# Author: 0cb (02.20.18)    Christian Bowman


# ======================
# ** Input .csv file **
# ======================
INPUT <- "~/Bioinf0cb/Rscripts/smaRtMath_data/simulData.csv"

data <- read.csv(INPUT, header=T, sep=",", dec=".")

# ------------------
# setting up anova
# ------------------
sink('work.txt')
y <- data[,1]                 # measured data (eg. yield); y
G <- as.factor(data[,2])      # genotype/ line; lines
E <- as.factor(data[,3])      # environment/ env. effect; x
result <- lm(y~G+E+G*E)
summaryTable <- summary(result)
anovaTable <- anova(result)
cat("\nANOVA Table\n"); anovaTable
cat("\n")

# ------------------
# Estimated variance components
# ------------------
# ** MS = expected MS when finding est. var components
# ------------------
cat("Estimated variance components: \n")

s.2 <- anovaTable[4,3]
s.GxE.2 <- (anovaTable[3,3] - s.2)/3
s.G.2 <- (anovaTable[1,3] - anovaTable[3,3])/3/5
brHerit <- (s.G.2)/(s.G.2 + s.GxE.2 + s.2)          # denom = pheno var

cat("Variance (sig)^2: "); s.2
cat("GxE var (sigGxE)^2: "); s.GxE.2
cat("G var (sigG)^2): "); s.G.2
cat("Est. broad-sense heritability: "); brHerit

sink()
