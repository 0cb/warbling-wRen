# Selection components
# > Use with 'slxn_comp'
# > Determine:
#   + slxn inten        (i)
#   + std. trunc pt     (t)
#   + slxn prop         (p)
#   + origin trunc pt   (T)
#   + slxn differential (S)
#   + slxn response     (R)
#
# Author: 0cb (03.09.18)    Christian Bowman


# ======================
# ** Input .csv file **
# ======================
INPUT <- "~/Bioinf0cb/Rscripts/smaRtMath_formulae/work.csv"

data <- read.csv(INPUT, header=F, sep=",", dec=".")

# ======================
# Functions
# ======================

# given slxn proportioni (p)
given.p <- function(x) {
    t <- qnorm(1-p)
    z <- dnorm(t)
    i <- z/p

    given.p(x=p)
}

# given std. trunc pt (t)
given.t <- function(x) {
    p <- 1-pnorm(t)
    z <- dnorm(t)
    i <- z/p

    given.t(x=t)

# ============================
# ** Input for components  **
# ============================

#    if substr(x,1,1)=p, do   # where first char is p

#source(abc.R)
#if abc.R has abc <-.. and abcd <-..
#can call abc vs abcd
# Rscript ___.R [cmd]
#given.p(x=p)

p <- grepl("p",data[,1])
q <- grepl("p",data[,1])
t <- grepl("p",data[,1])
p <- grepl("p",data[,1])

# if ([,3] true, then set [x,2] -> <y>
