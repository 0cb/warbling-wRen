
dir<-"C:\\Users\\shxu\\Dropbox\\My UCR Teaching\\BPSC148\\BPSC148-2016\\textbook\\Chapter 8"
setwd(dir)
f2<-read.csv(file="data\\F2.csv",header=TRUE)

fit<-lm(y~X1+X2,data=f2)
summary(fit)
anova(fit)

y<-f2$y
X0<-f2$X0
X1<-f2$X1
X2<-f2$X2
fit<-lm(y~X1+X2)
summary(fit)
anova(fit)

