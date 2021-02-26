
dir<-"C:\\Users\\shxu\\Dropbox\\My UCR Teaching\\BPSC148\\BPSC148-2016\\textbook\\Chapter 8"
setwd(dir)

epi<-read.csv(file="data\\bin729bin1064.csv",header=TRUE)
A<-epi$Bin729
B<-epi$Bin1064
kgw<-epi$kgw98
result<-lm(kgw~A+B+A*B)
summary(result)
anova(result)

epi<-read.csv(file="data\\bin729bin1064.csv",header=TRUE)
A<-epi$Bin729
B<-epi$Bin1064
kgw<-epi$kgw98

z1<-(A=="A")-(A=="B")
w1<-(A=="H")
z2<-(B=="A")-(B=="B")
w2<-(B=="H")
zz<-z1*z2
zw<-z1*w2
wz<-w2*z1
zw<-(zw+wz)/2
ww<-w1*w2

fit<-lm(kgw~z1+w1+z2+w2+zz+zw+ww)
summary(fit)
anova(fit)


xx<-data.frame(z1,w1,z2,w2,zz,zw,ww)
var(xx)

