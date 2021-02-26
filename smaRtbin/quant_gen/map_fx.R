#===========================================================================#
#=									   =#
#   Filename:	    Map_fx.R
#   Version:	    1.0
#=									   =#
#   Description:    Mapping Functions
#
#=  Author:	    0cb - Christian Bowman				   =#
#   Creation:	    2020-11-24
#   Updated:	    
#=									   =#
#===========================================================================#

GeneticDist <- function(NP,n){
		gendist <- (NP/n)*100
		#return(gendist)
		message("map units between A & B:  ", gendist)
		message("%recombinants:  ", gendist/100)
}

Haldane.rcmb<- function(rcmb){
		if ( any(rcmb < 0 | rcmb > 0.5) ) warning('rcmb is not between 0 and 0.5')
		    haldane.r <- (-1/2)*log(1-(2*rcmb))
		    #return(haldane.r)
		    message("number of crossovers; c=1: ", haldane.r)
}

Kosambi.rcmb<- function(rcmb){
		if ( any(rcmb < 0 | rcmb > 0.5) ) warning('rcmb is not between 0 and 0.5')
		    kosambi.r <- (1/4)*log((1+(2*rcmb))/(1-(2*rcmb)))
		    #return(kosambi.r)
		    message("number of crossovers; c=0: ", kosambi.r)
}

Haldane.X   <- function(x){
		haldane.x <- (1/2)*(1-exp(-2*x))
		#return(haldane.x)
		message("recombination frequency; c=1: ", haldane.x)
}

Kosambi.X   <- function(x){
		kosambi.x <- (1/2)*((exp(2*x)-exp(-2*x))/(exp(2*x)+exp(-2*x)))
		#return(kosambi.x)
		message("recombination frequency; c=0: ", kosambi.x)
}



