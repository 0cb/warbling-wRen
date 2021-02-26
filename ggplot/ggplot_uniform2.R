library(tidyr)
library(ggplot2)
library(reshape2)

#--------------- Variables ---------------#

args <- commandArgs(TRUE)


#--------------- Graphing averages ---------------#
#commentblock 
# S-v > :s/^/#/
# S-v > :s/^#// to remove

#df <- read.table("2020_bDD-top5pc1.csv", header=TRUE, sep=",")
#dfn <- melt(df, value.name = "PC", variable.name = "DAI")
#
#p <- ggplot(dfn, aes(x=DAI, y=PC, group=Code))+
#      geom_line(aes(color=Code))+
#      labs(title="Top 5 Bermudagrass Hybrids (Digital)", x="Days after irrigation (DAI)", y="Percent Coverage (%)")
#p
#
#png("2020_bDD-T5PC-avg.png", height=720, units="px")
#p + coord_cartesian(ylim=c(0.00,1.25))+
#    scale_y_continuous(breaks=seq(0.00,1.25,0.20))+
#    geom_vline(xintercept="D65", linetype="dashed", color="blue")
#dev.off()

#--------------- Graphing points ---------------#

# ./ works, but we're in ~/workspace
#df <- read.table("./ind_2020/17-8-unique.csv", header=FALSE, sep=",")
df <- read.table(args[1], header=FALSE, sep=",")
df$V2 <- as.factor(df$V2)


# 2(accession), 21(LC), 22(PC), 28(nPC), 29(DAI), 32(FLC), 33(FnPC)
#               solid,15                          longdash,18
q <- ggplot(df)+
      geom_point(aes(x=V29, y=V21, group=V2, color=V2), shape=15, alpha=0.15)+    #LC
      geom_line(linetype="solid", aes(x=V29, y=V21, group=V2, color=V2), alpha=0.15)+
      #geom_point(aes(x=V29, y=V28, group=V2, color=V2), alpha=0.2)+    #nPC
      #geom_line(linetype="solid", aes(x=V29, y=V28, group=V2, color=V2), alpha=0.2)+
      geom_point(aes(x=V29, y=V32, group=V2, color=V2), shape=18, size=2, alpha=0.15)+    #FLC
      geom_line(linetype="longdash", aes(x=V29, y=V32, group=V2, color=V2), alpha=0.15)+
      #geom_point(aes(x=V29, y=V33, group=V2, color=V2), shape=18, size=2, alpha=0.2)+    #fnPC
      #geom_line(linetype="longdash", aes(x=V29, y=V33, group=V2, color=V2), alpha=0.2)+
      stat_summary(fun=mean, aes(x=V29, y=V21, group=V6), geom="line", size=2, color="purple")+    #avg on FLC
      stat_summary(fun=mean, aes(x=V29, y=V21, group=V6), geom="point", color="purple")+
      stat_summary(fun.data=mean_se, aes(x=V29, y=V21, group=V6), geom="errorbar", color="purple")+    #SE bars
      labs(title=paste(df$V6[1],"individual data"), x="Days after irrigation (DAI)", y="Percent coverage (%)")
q

rect <- data.frame(xmin=68, xmax=81, ymin=-Inf, ymax=Inf)

png(paste(df$V6[1],"-plot.png", sep=""), height=720, units="px")
q + coord_cartesian(ylim=c(0.00,125))+
  scale_y_continuous(breaks=seq(0.00,125,25))+
  geom_rect(data=rect, aes(xmin=xmin, xmax=xmax, ymin=ymin, ymax=ymax), 
            color="skyblue1", fill="skyblue1", alpha=0.3, inherit.aes=FALSE)
#  geom_vline(xintercept=65, linetype="dotted", color="blue")+
#  geom_vline(xintercept=81, linetype="dotted", color="blue")
dev.off()
