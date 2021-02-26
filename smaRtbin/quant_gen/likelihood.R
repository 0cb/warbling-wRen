#dir<-"C:\\Users\\SHXU\\Dropbox\\My UCR Teaching\\BPSC148\\BPSC148-2017\\Homework\\HW7"
#setwd(dir)

phe<-read.csv(file="~/Bioinf0cb/Rscripts/smaRtMath_data/phe.csv",header=TRUE)
aa<-read.csv(file="~/Bioinf0cb/Rscripts/smaRtMath_data/aa.csv",header=TRUE)
y<-as.matrix(phe$y)
x<-as.matrix(phe$x)

#theta<-c(48.47172141,36.32155703,1.923134783)
#theta<-c(48.44369521,25.05889188,6.655594983)

likelihood<-function(theta,x,y,aa,method="REML"){
  n<-length(y)
  beta<-theta[1]
  va<-theta[2]
  ve<-theta[3]
  aa<-as.matrix(aa[,-c(1,2)])
  v<-aa*va+diag(n)*ve
  fn<--0.5*log(det(v))-0.5*t(y-x%*%beta)%*%solve(v)%*%(y-x%*%beta)
  if(method=="REML"){
     fn<-fn-0.5*log(det(t(x)%*%solve(v)%*%x))
  } 
  return(fn)
}

#likelihood(theta,x,y,aa,method="REML")


#ML method:

theta<-c(48.4717,36.3215,1.9231) #48.47172141,36.32155703,1.923134783
likelihood(theta,x,y,aa,method="ML")

#REML method:

theta<-c(48.4437,25.0590,6.6555) #48.44369521,25.05889188,6.655594983
likelihood(theta,x,y,aa,method="REML")
