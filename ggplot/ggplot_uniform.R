library(tidyr)
library(ggplot2)
library(reshape2)

#--------------- Variables ---------------#

args <- commandArgs(TRUE)


#--------------- Graphing averages ---------------#

df <- read.table("2020_bDD-top5pc1.csv", header=TRUE, sep=",")
dfn <- melt(df, value.name = "PC", variable.name = "DAI")

p <- ggplot(dfn, aes(x=DAI, y=PC, group=Code))+
      geom_line(aes(color=Code))+
      labs(title="Top 5 Bermudagrass Hybrids (Digital)", x="Days after irrigation (DAI)", y="Percent Coverage (%)")
p

png("2020_bDD-T5PC-avg.png", height=720, units="px")
p + coord_cartesian(ylim=c(0.00,1.25))+
    scale_y_continuous(breaks=seq(0.00,1.25,0.20))+
    geom_vline(xintercept="D65", linetype="dashed", color="blue")
dev.off()

#--------------- Graphing points ---------------#

# ./ works, but we're in ~/workspace
#df <- read.table("./ind_2020/17-8-unique.csv", header=FALSE, sep=",")
df <- read.table(args[1], header=FALSE, sep=",")
df$V2 <- as.factor(df$V2)

q <- ggplot(df, aes(x=V29, y=V27, group=V2))+
      geom_point(aes(color=V2))+
      geom_line(aes(color=V2))+
      labs(title=df$V6[1], x="Days after irrigation (DAI)", y="Percent coverage (%)")
q

png(paste(df$V6[1], ".png"), height=720, units="px")
q + coord_cartesian(ylim=c(0.00,1.25))+
  scale_y_continuous(breaks=seq(0.00,1.25,0.20))+
  geom_vline(xintercept=65, linetype="dashed", color="blue")
dev.off()