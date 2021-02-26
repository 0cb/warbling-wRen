#===========================================================================#
#=									   =#
#   Filename:	    gen_map.R
#   Version:	    1.0
#=									   =#
#   Description:    Genetic Map Construction
#
#=  Author:	    0cb - Christian Bowman				   =#
#   Creation:	    2020-11-24
#   Updated:	    
#=									   =#
#===========================================================================#

#lsf.str() gives the functions & parameters
#source("map_fx.R")

sar.abc	    <- function(rab,rbc){
		sar <- rab+rbc
		#return(sar)
		message("sar_abc; minimum: ", sar)
}

sar.acb	    <- function(rab,rbc){
		rac <- rab+rbc-(2*rab*rbc)
		sar2 <- rac+rbc
		#return(rac)
		message("r_AC: ", rac)
		#return(sar)
		message("sar_ACB; minimum: ", sar2)
}
		
