
#dir<-"C:\\Users\\shxu\\Dropbox\\My UCR Teaching\\BPSC148\\BPSC148-2018\\Textbook\\Chapter 7\\data"
#setwd(dir)

GxE<-read.table(file="~/Bioinf0cb/Rscripts/smaRtMath_data/bin1005GxE.csv",header=T,sep=",")
y<-GxE$yd
G<-as.factor(GxE$bin1005)
E<-as.factor(GxE$year)
result<-lm(y~G+E+G*E)
summary(result)
anova(result)


X<-2*(E=="98")-1
Z<-drop((G=="A")-(G=="B"))
W<-drop((G=="H")*1.0)
X
Z
W
XZ<-X*Z
XW<-X*W
fit<-lm(y~X+Z+W+XZ+XW)
summary(fit)
var(cbind(X,Z,W,XZ,XW))
