# Coancestry and Inbreeding
# > Determine inbreeding coeff (fj)
#
# Dependencies:
#   + kinship2
#
# Author: 0cb (03.03.18)    Christian Bowman

# ==================
# ---Dependencies---

library(kinship2)           # cannot handle inbred founders

# ==================

id          <- c("A", "B", "C", "Q", "P", "X")  # setting tabular
paternal    <- c(0, 0, "B", "B", 0, "Q")        # sire
maternal    <- c(0, 0, "A", "A", "C", "P")      # dam

coanMatrix  <- kinship(id, paternal, maternal)
cat("\nCoancestry matrix:\n"); coanMatrix

fX          <- (2* coanMatrix[6,6]) -1
cat("\nInbreeding coefficient: fX"); fX


