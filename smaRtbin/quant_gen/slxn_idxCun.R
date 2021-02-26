# Multistage selection (Cunningham)
# > Ripped from BIol148
# > Determine:
#   + slxn idx weights @stage 'k'   (b)k
#   + multistage selection scheme (based (b)k val)
#       where (I)n = (bnXn)+...+(b(n+1)X(n+1))
#
# Author: 0cb (03.20.18)    Christian Bowman


# ------------------------
# variable & matrix entry
# ------------------------
r1      <- c(11.558,1.103)
r2      <- c(1.103,2.002)
#r3      <- c(0.136,1.103,0.202,0.104)
#r4      <- c(0.564,-1.231,0.104,2.874)

c1      <- c(5.558,0.755)
c2      <- c(0.755,1.002)
#c3      <- c(-0.109,0.103,0.089,0.023)
#c4      <- c(1.233,-2.574,0.023,1.225)

P       <- as.matrix(rbind(r1,r2))
G       <- as.matrix(rbind(c1,c2))
w       <- as.matrix(c(1,0.85),2,1)

# w(c(#)) should match w/ P(max)[,x] & G[,x]

cat("\nMatrices: ")
cat("\npheno var-cov: P\n"); P  # <
cat("\ngen var-cov: G\n"); G    # <
cat("\necon weight: w\n"); w    # <

# ----------------
# (b)k @kth stage
# ----------------
P11     <- P[c(1),c(1)]     # remember to add in your row/col c(1,2)...
G1      <- G[c(1),]
b1      <- solve(P11,G1%*%w)

cat("\n1st stage values: ")
cat("\nP11: \n"); P11   # see above cat lines for definitions
cat("\nG1: \n"); G1     #
cat("\nb1: \n"); b1     #

P22     <- P[c(1,2),c(1,2)]
G2      <- G[c(1,2),]
b2      <- solve(P22,G2%*%w)

cat("\n2nd stage values: ")
cat("\nP22: \n"); P22   # see above cat lines for definitions
cat("\nG2: \n"); G2     #
cat("\nb2: \n"); b2     #


# end
