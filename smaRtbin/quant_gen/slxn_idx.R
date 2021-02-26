# Index selection
# > Ripped from BIol148
# > Determine:
#   + index weights     (b)
#   + slxn objective    (H=w1A1 + w2A2)
#   + genetic gains     (delt(A); dA)
#   + slnx criterion    (I=b1X1 + b2X2)
#   + idx score         (I)
#
#Author: 0cb (03.20.18)     Christian Bowman


# ------------------------
# variable & matrix entry
# ------------------------
i       <- 1.2711
w       <- matrix(c(0.95,0.75),2,1)           # econ weight
P       <- matrix(c(52.3,33.5,33.5,189),2,2) # pheno var-cov matr
G       <- matrix(c(25.7,19.9,19.9,119),2,2)   # gen var-cov matr
b       <- solve(P,G%*%w)           # idx weights; >> order of op
# does ^^ solve as [P]^-1 * [G][w] ??

# -------------
# idx analysis
# -------------
vI      <- t(b)%*%P%*%b             # variance of idx
sigI    <- sqrt(vI)                 # std dev idx
dH      <- i*sigI         # slxn response for aggregate breed val; (deltaH)
# 'drop' removes redundant info
dA      <- drop(i/sigI)*G%*%b       # genetic change (response); (deltaA)

# >> slxn for idx weight based on corrln of traits
cat("\nIndex weights: [b1 b2]"); b
vI
cat("\nSlxn response:aggregate breed val.: dH\n"); dH
cat("\nSlxn response:@2traits: dA: [dA1 dA2]\n"); dA

Xi      <- matrix(c(14.5,15.2),2,1)
Xj      <- matrix(c(15.6,18.5),2,1)
Ii      <- t(Xi)%*%b
Ij      <- t(Xj)%*%b

cat("\nIdx scores: I=b1X1 + b2X2")
cat("\nIndiv i: \n"); Ii
cat("\nIndiv j: \n"); Ij


# end
