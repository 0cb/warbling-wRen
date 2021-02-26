# Variance of avg. phenotype
# > Determine V(pbar) given (rho) and m
# > Using multiple values for (rho) and m; fixed Vp
#
# Author: 0cb (02.05.18)    Christian Bowman


# =====================================
# ** Input repeatability (rho) data **
# =====================================
Vp <- 10                         # z
rho <- c(0.00, 0.25, 1.00)        # x
meas <- c(1, 2, 3, 4, 5, 500)      # y

#my.list <- list(rho, meas, Vp)
#my.list

sink('pheno_var.txt')

#------------------------------------------------------
# Inf = infinity; 'find the limit' req. if meas = Inf
# need to factor the eq. ((1+(rho)(m-1))/m)*Vp
#
# > ((rho)-(rho/m)+(1/m))*(Vp/m)
# >> lim = 0
#------------------------------------------------------

phenoVar <- function(x,y) {

    10*((1+x*(y-1))/y)

}

# for ea. arg in 'meas', apply function(phenoVar) using 'rho'
sapply(meas, phenoVar, x=rho)   # sapply gives us a vector; lapply gives list ([1] xyz vs [1] x [1] y ...)

sink()

sapply(meas, phenoVar, x=rho)

