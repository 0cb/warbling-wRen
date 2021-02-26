
#dir<-"C:\\Users\\SHXU\\Dropbox\\My UCR Teaching\\BPSC148\\BPSC148-2016\\Homework\\HW7"
#setwd(dir)

phe<-read.csv(file="~/Bioinf0cb/Rscripts/smaRtMath_data/pheBull.csv",header=TRUE)
aa<-read.csv(file="~/Bioinf0cb/Rscripts/smaRtMath_data/aaBull.csv",header=TRUE)
source("mixed.R")

y<-as.matrix(phe$y)
x<-as.matrix(cbind(phe$x,phe$sex))
par<-mixed(x=x,y=y,aa=aa,method="REML")
par
write.csv(x=par,file="mixed-parms-bull.csv",row.names=F)
par$beta
par$va
par$ve

h2<-par$va/(par$va+par$ve)
h2

