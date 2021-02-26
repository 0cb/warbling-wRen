# ANOVA table for narrow-sense heritability
# > Determine p-off regr. coeff (byx)
# > Use naSense herit (h^2) for (add genetic var/ ttl pheno var)
# > if corrln of gen effects; cov=/=o
#
# Author: 0cb (02.22.18)    Christian Bowman


# ======================
# ** Input .csv file **
# ======================
INPUT <- "~/Bioinf0cb/Rscripts/smaRtMath_data/FamilyData.csv"

data <- read.csv(INPUT, header=T, sep=",", dec=".")

# ------------------
# setting up anova
# ------------------
sink('work.txt')

sire <- data[,2]    # father/paternal
dam <- data[,3]     # mother/maternal
sib1 <- data[,4]
sib2 <- data[,5]
sib3 <- data[,6]
sib4 <- data[,7]
sib.1t4 <- data[,4:7]
sib.1t3 <- data[,4:6]

midP <- 0.5*(sire + dam)    # midparent; 1/2(P1+P2)
sibM.1t4 <- rowMeans(sib.1t4)
sibM.1t3 <- rowMeans(sib.1t3)

data$midP <- midP
data$sibM.1t4 <- sibM.1t4
data$sibM.1t3 <- sibM.1t3

data

    # lm(y~., data) would perform lin.regr of all var w/ y
    #--------------------------------------------------------

    #anovaOut <- function(x,y) {
    #
    #    result <- lm(y~x)
    #    anova(result)

result1 <- lm(sib2~sire)
result2 <- lm(sib2~midP)
result3 <- lm(sibM.1t4~dam)
result4 <- lm(sibM.1t3~midP)
#resultA <- c(result1,result2,result3,result4)

summary1 <- summary(result1)
summary2 <- summary(result2)
summary3 <- summary(result3)
summary4 <- summary(result4)
#summaryA <- c(summary1,summary2,summary3,summary4)

# b(op) coefficient; [p-off regr. coeff]
byx1 <- summary1$coefficient[2] 
byx2 <- summary2$coefficient[2]
byx3 <- summary3$coefficient[2]
byx4 <- summary4$coefficient[2]
byxA <- c(byx1,byx2,byx3,byx4)

cat("\nParent-Offspring regr. coeff: "); byxA
cat("\nNarrow-sense heritability: "); 2*byxA    # (h^2) = 2*b(yx) or 2*b(op)
                                                # **midP:mean off --(h^2)=byx
sink()
