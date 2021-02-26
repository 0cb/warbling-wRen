#===========================================================================#
#=									   =#
#   Filename:	    gene_detect.R
#   Version:	    1.0
#=									   =#
#   Description:    Major gene detection
#
#=  Author:	    0cb - Christian Bowman				   =#
#   Creation:	    2020-12-03
#   Updated:	    
#=									   =#
#===========================================================================#

#--------------- BC design ---------------#
# ripped 234
linmod.BC   <- function(b0,b1){
		x <- seq(from = -1, to = 1, by = (1 - (-1))/(100 - 1))
		#b0<-6 b1<-3
		e <- rnorm(100)
		y <- b0 + x*b1 + e
		#plot(x,y)
		LM.BC <- lm(y~x) #LSE for b0 and b1
		anova.BC <- anova(LM.BC)

		message("Least squares estimation for b0 and b1: ", LM.BC)
		message("ANOVA for LSE: ", anova.BC)
}

#--------------- F2 design ---------------#
#ripped 234
linmod.F2   <- function(mu,a,d){
		#mu<-6 a<-3 d<-1
		e <- rnorm(100)
		y <- mu + z*a + w*d + e
		
		g <- c(rep(1, 25), rep(2, 50), rep(3, 25))
		z <- c(rep(1, 25), rep(0, 50), rep(-1, 25))
		w <- c(rep(0, 25), rep(1, 50), rep(0, 25))
		df.F2 <- data.frame(g,z,w,y)
		#plot(z,y)
		#plot(w,y)
		
		LM.F2 <- lm(y~z+w)
		anova.F2 <- anova(LM.F2)
		message("Least squares estimation for b0, b1, and b2: ", LM.F2)
		message("ANOVA for LSE: ", anova.F2)
}

#--------------- statistical power ---------------#
statpow.chi <- function(a,n,alpha,var){
	    	
		ts <- qchisq(alpha, 1, ncp=0, lower.tail=FALSE, log.p=FALSE)
		ncp <- n*0.5*((a^2)/var)
		#beta <- 
		omega <- pchisq(ts, 1, ncp, lower.tail=FALSE, log.p=FALSE)
		
		message("chi-square ts: ", ts)
		message("non-centrality parameter: ", ncp)
		message("statistical power of major gene in F2 design: ", omega)
}


minN.chi    <- function(H2,w,alpha){
		beta <- 1-w

		ts <- qchisq(alpha, 1, ncp=0, lower.tail=FALSE, log.p=FALSE)
		z1 <- qnorm(1-alpha/2, mean=0, sd=1, lower.tail=TRUE, log.p=FALSE)
		z2 <- qnorm(1-beta, mean=0, sd=1, lower.tail=TRUE, log.p=FALSE)
		ncp <- (z1+z2)^2

		n <- (ncp*(1-H2))/H2

		message("chi-square ts: ", ts)
		message("non-centrality parameter: ", ncp)
		message("minimum sample size to detect major gene: ", n)
}




#### Calculate probability density of chi-square
#
#dchisq(3.84, 1, ncp = 0, log = FALSE)
#dchisq(3.84, 1, ncp = 10.505, log = FALSE)
#
#### Calculate P values of chi-square
#
#pchisq(3.84, 1, ncp = 0, lower.tail = FALSE, log.p = FALSE)
#pchisq(3.84, 1, ncp = 10.505, lower.tail = FALSE, log.p = FALSE)
#
#### Calculate quantiles of chi-square
#
#qchisq(0.05, 1, ncp = 0, lower.tail = FALSE, log.p = FALSE)
#qchisq(0.05, 1, ncp = 10.505, lower.tail = FALSE, log.p = FALSE)
#
#
#### Generate chi-square random variables
#
#rchisq(1, 1, ncp = 0)
#rchisq(1, 1, ncp = 10.505)
#
#### Calculate ncp for chisq (using standard normal distribution)
#
#z1 <- qnorm(1-0.05/2, mean = 0, sd = 1, lower.tail = TRUE, log.p = FALSE)
#
#z2 <- qnorm(1-0.1, mean = 0, sd = 1, lower.tail = TRUE, log.p = FALSE)
#
#(z1 + z2)^2
