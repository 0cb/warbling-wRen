#===========================================================================#
#=									   =#
#   Filename:	    rcmb_freq.R
#   Version:	    1.0
#=									   =#
#   Description:    Recombination frequency
#
#=  Author:	    0cb - Christian Bowman				   =#
#   Creation:	    2020-11-24
#   Updated:	    
#=									   =#
#===========================================================================#

#--------------- Backcross (BC) design ---------------#
MLE.Back    <- function(NP,N){
		MLE.BC <- (NP/N)
		#return(MLE.BC)
		message("estimated recombination fraction (rhat); BC: ", MLE.BC)
}

SE.Back	    <- function(rhat,N){
		stdErr.BC <- sqrt((rhat*(1-rhat))/N)
		#return(stdErr.BC)
		message("estimation(std) error of rhat; report as rhat +- s: ", stdErr.BC)
}

fish.Back   <- function(r,N){
		fish.BC <- (N/(r*(1-r)))
		#return(fish.BC)
		message("measure of certainty of parameter given the data; I(r): ", fish.BC)
}

logLike.Back<- function(NP,r,P){
		LL.BC <- (NP*log(r))+(P*log(1-r))
		#return(LL.BC)
		message("log likelihood fx of rhat (L(r)); L(1/2;null)= -0.6931n:", LL.BC)
}

chi.Back    <- function(LL,N){
		chi.BC <- (-2*((-0.6931*N)-LL))
		#return(chi.BC)
		message("chi test statistic; for use in LOD: ", chi.BC)
}

LOD.Back    <- function(chi){
		LOD.BC <- (0.217*chi)
		#return(LOD.BC)
		message("log of odds where LOD(k)>3 is 10^k more likely than null: ", LOD.BC)
} 

#--------------- F2 design ---------------#
MLE.complF2 <- function(NP,N){
		MLE.cF2 <- (NP/(2*N))
		#return(MLE.cF2)
		message("estimated recombination fraction (rhat); F2: ", MLE.cF2)
}

SE.complF2  <- function(rhat,N){
		stdErr.cF2 <- sqrt((rhat*(1-rhat))/(2*N))
		#return(stdErr.cF2)
		message("estimation(std) error of rhat; report as rhat +- s: ", stdErr.cF2)
}

FishInfo.F2 <- function(r,N){
		fish.F2 <- (2*N/(r*(1-r)))
		#return(fish.F2)
		message("measure of certainty of parameter given the data; I(r): ", fish.F2)
}

logLike.cF2 <- function(m11,m12,m13,m21,m22,m23,m31,m32,m33,r){
		LL.cF2 <- ( (2*(m11+m33)+m12+m21+m23+m32)*log(1-r)
		 + (2*(m13+m31)+m12+m21+m23+m32)*log(r)
		 + m22*log((r^2)+(1-r)^2) )
		#return(LL.cF2)
		message("log likelihood fx of r (L(r)): ", LL.cF2)
}

logLiken.cF2<- function(m11,m12,m13,m21,m22,m23,m31,m32,m33){
		Llognull.cF2 <- ( (2*(m11+m33)+m12+m21+m23+m32)*log(1-0.5)
		 + (2*(m13+m31)+m12+m21+m23+m32)*log(0.5)
		 + m22*log((0.5^2)+(1-0.5)^2) )
		#return(Llognull.cF2)
		message("log likelihood fx of null (L(1/2)): ", Llognull.cF2)
}
