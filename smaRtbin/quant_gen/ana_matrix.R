# matrix analysis
# > Using matrix
# > Ripped from Biol148
#
# Author: 0cb (01.23.18)    Christian Bowman

# ======================
# ** Input .csv file **
# ======================
INPUT1 <- "~/Bioinf0cb/Rscripts/smaRtMath_data/matrixHW07.3a.csv"
INPUT2 <- "~/Bioinf0cb/Rscripts/smaRtMath_data/matrixHW07.3b.csv"

data1 <- read.csv(INPUT1, header=F, sep=",", dec=".")
data2 <- read.csv(INPUT2, header=F, sep=",", dec=".")

A <- as.matrix(data1[,-1]) # sets data as matrix; removes first col
B <- as.matrix(data2[,-1])

# using 'sink', we can write output to a new file

#sink('matrix_ana.txt')

Sys.time()                  # get a time for when we last used
cat("\n")

X1 <- 2*A                 # == A + 0.5*A
cat("Matrix '2A'\n"); X1 
X2 <- t(A)
cat("\nTransposed matrix: A^T\n"); X2
X3 <- A%*%t(A)
cat("\nMatrix: AA^T\n"); X3
X4 <- det(X3)
cat("\nDeterminant of AA^T\n"); X4
X5 <- solve(X3)             # I = identity matrix; 'solve' finds the inverse of matrix 
cat("\nInverse of transposed matrix: (AA^T)^-1\n"); X5
X6 <- t(A)%*%A
cat("\nMatrix (A^T)A\n"); X6

#sink()

sink('matrix_ana.txt')

ba <- solve(A)
cat("\nInverse matrix: A^-1\n"); ba

bb <- ba%*%B
cat("\nMatrix: (A^-1)B\n"); bb

# -----------
# slxn idx
# -----------

X1 <- c(10.25,12.05,11.33,15.43,9.38)
X2 <- c(14.51,10.36,12.08,13.75,11.32)

slxn.idx <- function(x,y) {
    (0.2307692*X1)+(0.4120879*X2)
}

slxn.idx(x=X1,y=X2)


sink()

