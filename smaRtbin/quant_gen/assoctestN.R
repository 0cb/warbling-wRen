# 2x2 Contingency table association test
# > Ripped from BIOL148 class
# > Yates' continuity correction can be performed as well
#
# Author: 0cb (01.18.18)    Christian Bowman


# ============================
# ** Input population data **
# ============================
N11 <- 78
N12 <- 23
N22 <- 1

N <- N11 + N12 + N22
cat("\nSum of Population: N "); N
cat("\n)

table <- matrix(c(N11,0.5*N12,0.5*N12,N22),2,2)
table

chisq.test(x=table,correct=FALSE)   #correct=TRUE for Yates' cont. correction

