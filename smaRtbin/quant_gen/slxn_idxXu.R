# Multistage selection (Xu-Muir)
# > Shares characteristics w/ partial least sq met.
# > Establish (I)n are indep.
# > Determine: 
#   + slxn idx weights @stage 'k'   (b)k
#   + multistage selection scheme (based (b)k val)
#       where (I)n = (bnXn)+...+(b(n+1)X(n+1))
##
# Author: 0cb (03.20.18)    Christian Bowman


# ------------------------
# variable & matrix entry
# ------------------------
r1      <- c(137.178,-90.957,0.136,0.564)
r2      <- c(-90.957,201.558,1.103,-1.231)
r3      <- c(0.136,1.103,0.202,0.104)
r4      <- c(0.564,-1.231,0.104,2.874)

c1      <- c(14.634,-18.356,-0.109,1.233)
c2      <- c(-18.356,32.029,0.103,-2.574)
c3      <- c(-0.109,0.103,0.089,0.023)
c4      <- c(1.233,-2.574,0.023,1.225)

P       <- as.matrix(rbind(r1,r2,r3,r4))
G       <- as.matrix(rbind(c1,c2,c3,c4))
w       <- as.matrix(c(-3.555,19.536,-113.746,48.307),4,1) 

# w(c(#)) should match w/ P(max)[,x] & G[,x]

cat("\nMatrices: ")
cat("\npheno var-cov: P\n"); P  # <
cat("\ngen var-cov: G\n"); G    # <
cat("\necon weight: w\n"); w    # <


# ----------------
# (b)k @kth stage
# ----------------
P11     <- P[c(1,2),c(1,2)]
G1      <- G[c(1,2),]
b1      <- solve(P11,G1%*%w)

cat("\n1st stage values: ")
cat("\nP11: \n"); P11   # see above cat lines for definitions
cat("\nG1: \n"); G1     #
cat("\nb1: \n"); b1     #

P22.c   <- P[c(1,2,3),c(1,2,3)]
G2.c    <- G[c(1,2,3),]
b2.c    <- solve(P22.c,G2.c%*%w)

P21     <- P[c(1,2,3),c(1,2)]
R21     <- P21%*%b1
PR.2    <- solve(P22.c,R21)
RPR.2   <- t(R21)%*%PR.2
RPRi.2  <- solve(RPR.2)
b2.a    <- diag(3)-PR.2%*%RPRi.2%*%t(R21)
b2      <- b2.a%*%b2.c

cat("\n2nd stage values: ")
cat("\nP22: \n"); P22.c
cat("\nG2: \n"); G2.c
cat("\nR21=P21b1: \n"); R21
cat("\nb2: \n"); b2

P33.c   <- P[c(1,2,3,4),c(1,2,3,4)]
G3.c    <- G[c(1,2,3,4),]
b3.c    <- solve(P33.c,G3.c%*%w)

b1      <- as.vector(b1)
b1.0    <- c(b1,0)          # need to append a 0 for =b1r x b2r
b2.0    <- as.vector(b2)
B2      <- as.matrix(cbind(b1.0,b2.0))  

P32     <- P[c(1,2,3,4),c(1,2,3)]
R32     <- P32%*%B2
PR.3    <- solve(P33.c,R32)
RPR.3   <- t(R32)%*%PR.3
RPRi.3  <- solve(RPR.3)
b3.a    <- diag(4)-PR.3%*%RPRi.3%*%t(R32)
b3      <- b3.a%*%b3.c

cat("\n3rd stage values: ")
cat("\nP33: \n"); P33.c
cat("\nG3: \n"); G3.c
cat("\nR32=P32b2: \n"); R32
cat("\nb3: \n"); b3

cat("\nSlxn scheme: I=(bnXn)+...+(b(n+1)X(n+1))\n")
cat("\nb1: "); as.vector(b1)
cat("\nb2: "); as.vector(b2)
cat("\nb3: "); as.vector(b3)


# end
