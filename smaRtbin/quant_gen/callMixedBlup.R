#dir<-"C:\\Users\\shxu\\Dropbox\\My UCR Teaching\\BPSC148\\BPSC148-2017\\Homework\\HW8"
#setwd(dir)
source(file="~/Bioinf0cb/Rscripts/smaRtMath_formulae/mixedBlup.R")
phe<-read.csv(file="~/Bioinf0cb/Rscripts/smaRtMath_data/phe20.csv",header=TRUE)
aa<-read.csv(file="~/Bioinf0cb/Rscripts/smaRtMath_data/aa20.csv",header=TRUE)
aa<-as.matrix(aa[,-c(1,2)])
y<-as.matrix(phe$y)
n<-length(y)
x<-as.matrix(rep(1,n),n,1)
animal<-phe$animal

par<-mixedBlup(x=x,y=y,aa=aa,method="REML",h2=NULL)
par
write.csv(x=par[[1]],file="mixed-parms.csv",row.names=F)
write.csv(x=cbind(animal,par[[2]]),file="mixed-blup.csv",row.names=F)

beta<-par[[1]]$beta
va<-par[[1]]$va
ve<-par[[1]]$ve
h2<-va/(va+ve)
beta
va
ve
h2

sink('work.txt')

INPUT1 <- "~/Bioinf0cb/Rscripts/smaRtMath_data/mixedblup.csv"

data1 <- read.csv(INPUT1, header=T, sep=",", dec=".")

cat("\nheritability: "); h2

ppred <- data1[,5]
cat("\nppred [,5]: "); ppred

pbar <- mean(data1[,5])
cat("\npbar: "); pbar

breedval <- function(x,y) {
    h2*(x-y)

}

mapply(breedval,x=ppred,y=pbar)

sink()
