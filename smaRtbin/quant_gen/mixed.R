

mixed<-function(x,y,aa,method="REML"){

loglike<-function(theta){
   lambda<-exp(theta)
   logdt<-sum(log(lambda*delta+1))
   h<-1/(lambda*delta+1)
   yy<-sum(yu*h*yu)
   yx<-matrix(0,s,1)
   xx<-matrix(0,s,s)
   for(i in 1:s){
      yx[i]<-sum(yu*h*xu[,i])
      for(j in 1:s){
         xx[i,j]<-sum(xu[,i]*h*xu[,j])
      }
   }
   if(method=="REML"){
   loglike<- -0.5*logdt-0.5*(n-s)*log(yy-t(yx)%*%solve(xx)%*%yx)-0.5*log(det(xx))
   } else {
   loglike<- -0.5*logdt-0.5*n*log(yy-t(yx)%*%solve(xx)%*%yx)
   }
   return(-loglike)
}


fixed<-function(lambda){
   h<-1/(lambda*delta+1)
   yy<-sum(yu*h*yu)
   yx<-matrix(0,s,1)
   xx<-matrix(0,s,s)
   for(i in 1:s){
      yx[i]<-sum(yu*h*xu[,i])
      for(j in 1:s){
         xx[i,j]<-sum(xu[,i]*h*xu[,j])
      }
   } 
   beta<-solve(xx,yx)
   if(method=="REML"){
     sigma2<-(yy-t(yx)%*%solve(xx)%*%yx)/(n-s)
   } else {
     sigma2<-(yy-t(yx)%*%solve(xx)%*%yx)/n
   }
   var<-diag(solve(xx)*drop(sigma2))
   stderr<-sqrt(var)
   return(c(beta,stderr,sigma2))
}

    n<-length(y)
    aa<-aa[,-c(1,2)]
    qq<-eigen(aa)
    delta<-qq[[1]]
    uu<-qq[[2]]
    s<-ncol(x)
    yu<-t(uu)%*%y
    xu<-t(uu)%*%x
    theta<-0
    parm<-optim(par=theta,fn=loglike,NULL,hessian = TRUE, method="L-BFGS-B",lower=-20,upper=20)
    lambda<-exp(parm$par)
    conv<-parm$convergence
    fn1<-parm$value
    fn0<-loglike(-Inf)
    lrt<-2*(fn0-fn1)
    hess<-parm$hessian
    parmfix<-fixed(lambda)
    beta<-parmfix[1:s]
    stderr<-parmfix[(s+1):(2*s)]
    ve<-parmfix[2*s+1]
    lod<-lrt/4.61
    p_value<-1-pchisq(lrt,1)
    va<-lambda*ve
    h2<-va/(va+ve)
    par<-data.frame(method,beta,stderr,va,ve,lambda,h2,conv,fn1,fn0,lrt,lod,p_value)
    return(par)
}
