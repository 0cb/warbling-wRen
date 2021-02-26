#===========================================================================#
#=									   =#
#   Filename:	    multipt_anal.R
#   Version:	    1.0
#=									   =#
#   Description:    Multipoint analysis of Mendelian Loci
#
#=  Author:	    0cb - Christian Bowman				   =#
#   Creation:	    2020-11-28
#   Updated:	    
#=									   =#
#===========================================================================#

#--------------- BC design ---------------#
PrABC.Back  <- function(A,B,C,rab,rbc){
		NO.ab <- 1-rab
		XO.ab <- rab
		NO.bc <- 1-rbc
		XO.bc <- rbc

		T.ab = matrix(
			     c(NO.ab,XO.ab,
			       XO.ab,NO.ab),
			     nrow=2,
			     ncol=2,
			     byrow=TRUE)

		T.bc = matrix(
			     c(NO.bc,XO.bc,
			       XO.bc,NO.bc),
			     nrow=2,
			     ncol=2,
			     byrow=TRUE)
		#commentblock 
		# S-v > :s/^/#/
		# S-v > :s/^#// to remove
		# NOTE: J(unity matrix) is only needed in full matrix notation)
		# where D(A) and D(B) [diag matrices] are used
		# Also, multiplying matrices uses %*% (FYI)
#		J = matrix(
#			   c(1,1),
#			   nrow=2,
#			   ncol=1)
#		tJ = t(J)

		#pr.abc <- (1/2)*tJ*DA*T[A,B]*DB*T[B,C]*DB*J
		pr.abc <- (1/2)*T.ab[A,B]*T.bc[B,C]
		#return(pr.abc)
		message("Probability of progeny with genotype A,B,C: ", pr.abc)
}

#--------------- F2 design ---------------#
PrABC.F2    <- function(A,B,C,rab,rbc){
		ab.1 <- (1-rab)^2
		ab.2 <- rab*(1-rab)
		ab.3 <- (rab)^2

		bc.1 <- (1-rbc)^2
		bc.2 <- rbc*(1-rbc)
		bc.3 <- (rbc)^2

		T.ab2 = matrix(
			      c(ab.1,ab.2,ab.2,ab.3,
				ab.2,ab.1,ab.3,ab.2,
				ab.2,ab.3,ab.1,ab.2,
				ab.3,ab.2,ab.2,ab.1),
			      nrow=4,
			      ncol=4,
			      byrow=TRUE)

		T.bc2 = matrix(
			      c(bc.1,bc.2,bc.2,bc.3,
				bc.2,bc.1,bc.3,bc.2,
				bc.2,bc.3,bc.1,bc.2,
				bc.3,bc.2,bc.2,bc.1),
			      nrow=4,
			      ncol=4,
			      byrow=TRUE)
		pr.abc2 <- (1/4)*T.ab2[A,B]*T.bc2[B,C]
		#return(pr.abc2)
		message("Probability of progeny with genotype A,B,C: ", pr.abc2)
}

#--------------- Matrix calculation ---------------#
# using matrix notation; can be used in an F2 or FW

PrABC.MN    <- function(A,B,C,rab,rbc,A2=0,B2=0,C2=0){
		D.A = matrix(0,4,4)
		D.B = matrix(0,4,4)
		D.C = matrix(0,4,4)

		D.A[A,A] <- 1
		D.A[A2,A2] <- 1
		D.B[B,B] <- 1
		D.B[B2,B2] <- 1
		D.C[C,C] <- 1
		D.C[C2,C2] <- 1

		J.v = matrix(1,4,1)

		ab.1 <- (1-rab)^2
		ab.2 <- rab*(1-rab)
		ab.3 <- (rab)^2

		bc.1 <- (1-rbc)^2
		bc.2 <- rbc*(1-rbc)
		bc.3 <- (rbc)^2

		T.abM = matrix(
			      c(ab.1,ab.2,ab.2,ab.3,
				ab.2,ab.1,ab.3,ab.2,
				ab.2,ab.3,ab.1,ab.2,
				ab.3,ab.2,ab.2,ab.1),
			      nrow=4,
			      ncol=4,
			      byrow=TRUE)

		T.bcM = matrix(
			      c(bc.1,bc.2,bc.2,bc.3,
				bc.2,bc.1,bc.3,bc.2,
				bc.2,bc.3,bc.1,bc.2,
				bc.3,bc.2,bc.2,bc.1),
			      nrow=4,
			      ncol=4,
			      byrow=TRUE)

		pr.abcM <- (1/4)*t(J.v)%*%D.A%*%T.abM%*%D.B%*%T.bcM%*%D.C%*%J.v
		#return(pr.abcM)
		message("Probability of progeny with genotype A,B,C: ", pr.abcM)
}

#--------------- Conditional P(missing genotype) ---------------#
# using matrix notation; separate from PrABC.MN

PrABC.CD    <- function(A,B,C,rab,rbc,A2=0,B2=0,C2=0,post=NA){
		D.A = matrix(0,4,4)
		D.B = matrix(0,4,4)
		D.C = matrix(0,4,4)

		D.A[A,A] <- 1
		D.A[A2,A2] <- 1
		D.B[B,B] <- 1
		D.B[B2,B2] <- 1
		D.C[C,C] <- 1
		D.C[C2,C2] <- 1

		J.v = matrix(1,4,1)
		I.v = diag(4)
# NOTE: post=A, D.A <- I.v <29-11-20, 0cb> #

		ab.1 <- (1-rab)^2
		ab.2 <- rab*(1-rab)
		ab.3 <- (rab)^2

		bc.1 <- (1-rbc)^2
		bc.2 <- rbc*(1-rbc)
		bc.3 <- (rbc)^2

		T.abCD = matrix(
			      c(ab.1,ab.2,ab.2,ab.3,
				ab.2,ab.1,ab.3,ab.2,
				ab.2,ab.3,ab.1,ab.2,
				ab.3,ab.2,ab.2,ab.1),
			      nrow=4,
			      ncol=4,
			      byrow=TRUE)

		T.bcCD = matrix(
			      c(bc.1,bc.2,bc.2,bc.3,
				bc.2,bc.1,bc.3,bc.2,
				bc.2,bc.3,bc.1,bc.2,
				bc.3,bc.2,bc.2,bc.1),
			      nrow=4,
			      ncol=4,
			      byrow=TRUE)

		pr.abcCD <- (1/4)*t(J.v)%*%D.A%*%T.abM%*%D.B%*%T.bcM%*%D.C%*%J.v
}

