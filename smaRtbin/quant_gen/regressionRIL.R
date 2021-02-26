
dir<-"C:\\Users\\shxu\\Dropbox\\My UCR Teaching\\BPSC148\\BPSC148-2016\\textbook\\Chapter 8"
setwd(dir)
ril<-read.csv(file="data\\RIL.csv",header=TRUE)
y<-ril$y
X0<-ril$X0
X1<-ril$X1

fit<-lm(y~X1)
summary(fit)
anova(fit)


