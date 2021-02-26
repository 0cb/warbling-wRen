# Quantitative Analysis
# > Includes:
# >> Genotypic and allelic frequncies
# >> chi-square test for 3x1 @HWE
# >> Genetic effects of quant. traits
#
#Author: 0cb (01.30.18)    Christian Bowman


# ============================
# ** Input population data **
# ============================
N11 <- 40                 #Count for AA 
N12 <- 320                 #count for Aa
N22 <- 640                 #Count for aa
N3 <-c(N11,N12,N22)         #Combo all as list

# ============================
# ** Input scale2 data **
# ============================
G11 <- 84.626                           # A1A1
G12 <- 58.747                           # A1A2
G22 <- 0.010                            # A2A2

# -----------------
# Geno. freq; + HWE
# -----------------
N <- N11 + N12 + N22        #Sum of population
cat("\nSum of population"); N
cat("\n")
P <- N11/N                  #Freq. AA
cat("Frequency of AA: P"); P
H <- N12/N                  #Freq. Aa
cat("Frequency of Aa: H"); H
Q <- N22/N                  #Freq. aa
cat("Frequency of aa: Q"); Q

# -----------------
# SUM(geno. freq) should = 1.00 / 100%
# -----------------
Check1 <- P + H + Q
cat("Check1: Ans=1"); Check1
cat("\n")

# -----------------
# Allele freq based on geno. freq; +- HWE
# -----------------
p <- P+0.5*H                #Freq. A1
cat("Gene freq. based on geno. freq - A1 (p)"); p
q <- Q+0.5*H                #Freq. A2
cat("Gene freq. based on geno. freq - A2 (q)"); q
cat("\n")

# -----------------
# SUM(alle. freq) should = 1.00 / 100%
# -----------------
Check2 <- p + q
cat("Check2: Ans=1"); Check2
cat("\n")

# -----------------
# Expected geno. freq via Hardy-Weinberg Equation (HWE)
# -----------------
cat("Expected geno. freq - AA"); p^2
cat("Expected geno. freq - Aa"); 2*p*q
cat("Expected geno. freq - aa"); q^2
cat("\n")
cat("Carrier freq. - f"); (2*q)/(1+q)   # carrier = het; het =/= carrier
cat("\n")

# =================
# Chi-square test
# =================
# -----------------
# Determine X^2 (chisq. test stat); B = # RNG replicates
# -----------------
# p.value given is the simulant since degrees of freedom (df) are unk
# -----------------
test.1 <- chisq.test(x=N3,p=c(p^2,2*p*q,q^2),simulate.p.value=TRUE,B=5000)
test.1
cat("\n")
x <- test.1$statistic

# -----------------
# Variation in p value due to RNG replicates (B)
# -----------------
# p.value here is with established df= (m-1)(n-1)?
# -----------------
p.value <- 1-pchisq(x,df=1)
cat("p.value.adj"); p.value
cat("\n")


# =================
# Genetic rln 
# =================

# -----------------
# (scale2 -> scale1); 3x1
# -----------------
# u is a scale2 value; a, d, -a are scale1 values
# -----------------
u.S2a <- G11 + G22
u.S2b <- 0.5 * u.S2a                    # (actual)mid-point value
# following are scale1
cat("Mid-point value: u"); u.S2b
a.S1 <- G11 - u.S2b                     # addititive effect
cat("Additive effect: a"); a.S1
d.S1a <- G12 - u.S2b                    # dominance effect
cat("Dominance effect: d"); d.S1a
cat("Genotypic value: -a"); -1 * a.S1
d.S1b <- d.S1a/a.S1                     # degree of dominance
cat("Degree of dom."); d.S1b

# -----------------
# Population
# -----------------
pminq <- p - q                          # (p-q)
apq <- a.S1*pminq                       # a(p-q)
twopqd <- 2*p*q*d.S1a
M.S1a <- apq + twopqd                   # population mean
# can be found also by: ap^2 + d(2pq) + (-a)p^2
cat("\nPopulation mean: M"); M.S1a
M.S1b <- M.S1a + u.S2b                  # actual population mean
cat("Actual ppln mean: M + u"); M.S1b

# -----------------
# M can be found also by: ap^2 + d(2pq) + (-a)q^2
# -----------------

p.sq <- p^2
ap.sq <- a.S1*p.sq
q.sq <- q^2
minaq.sq <- -1*a.S1*q.sq
Check3 <- ap.sq + twopqd + minaq.sq
cat("\nCheck3: Ans=M"); Check3


# -----------------
# Avg effect of gene and gene subst; a1 = ap+dq | a2 = pd-aq
# -----------------
# population mean w/ A(n) (fixed) where prop(p) = AA; prop(q) = aa [ MA(n) ] 
# -----------------
ap <- a.S1 * p      
dq <- d.S1a * q     
MA1 <- ap + dq

pd <- p * d.S1a     
qa <- q * a.S1
MA2 <- pd - qa     

A1.S1 <- MA1 - M.S1a    # avg effect gene A1
A2.S1 <- MA2 - M.S1a    # avg effect gene A2
cat("\nAverage effect gene: A1"); A1.S1
cat("Average effect gene: A2"); A2.S1

Alpha.S1 <- A1.S1 - A2.S1   # avg effect gene subst (alph) = a1 - a2
cat("Average effect of gene subst"); Alpha.S1

# (alpha) = a1 - a2 = a + d(q-p); all values should be same
qminp <- q - p
dqp <- d.S1a * qminp
Check4 <- a.S1 + dqp
cat("\nCheck4: Ans='Avg effect gene subst'"); Check4
cat("\n")


#end
