library(tidyr)
library(ggplot2)
library(reshape2)
library(ggrepel)
library(ggpubr)

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

#--------------- Graphing t5 vs cc ---------------#

df1 <- read.table("2020_bDrydown-exD1.csv", header=TRUE, sep=",")
df1$No. <- as.factor(df1$No.)

loads <- c("UCRC180040", "UCRC180557", "UCRC180576", "UCRC180038", "UCRC180229",
           "Bandera", "Celebration", "Santa-Ana", "TifTuf", "Tifway-II",
           "UCR17-8", "UCRTP6-3")

bhy <- c("UCRC180040", "UCRC180557", "UCRC180576", "UCRC180038", "UCRC180229", "UCR17-8", "UCRTP6-3")
#bhy <- c("UCRC180040", "UCRC180557", "UCRC180576", "UCRC180038", "UCRC180229")#,"UCR17-8", "UCRTP6-3")

cmc <- c("Bandera", "Celebration", "Santa-Ana", "TifTuf", "Tifway-II")#"UCR17-8", "UCRTP6-3")
#cmc <- c("Bandera", "Celebration", "Santa-Ana", "TifTuf", "Tifway-II", "UCR17-8", "UCRTP6-3")


df2 <- df1[df1$Code %in% loads, ]
df2a <- df2[df2$Code %in% bhy, ]
df2b <- df2[df2$Code %in% cmc, ]

rect <- data.frame(xmin=68, xmax=81, ymin=-Inf, ymax=Inf)



# 2(accession), 21(LC), 22(PC), 28(nPC), 29(DAI), 32(FLC), 33(FnPC)
#               solid,15                          longdash,18

p1 <- ggplot()+
      stat_summary(data=df2a, fun=mean, aes(x=DAI, y=LC, group=Code, color=Code), geom="line", linetype="solid", size=1.5)+#, alpha=0.25)+    #avg on FLC
      #stat_summary(data=df2a, fun=mean, aes(x=DAI, y=LC, group=Code, color=Code), geom="point")+#, alpha=0.25)+
      #stat_summary(data=df2a, fun.data=mean_se, aes(x=DAI, y=LC, group=Code, color=Code), geom="errorbar")+    #SE bars
      #stat_summary(data=df2a, fun.data=mean_se, aes(x=DAI, y=LC, color=Code, label=ifelse(DAI==118, as.character(Code),'')), geom="text", position=position_dodge(width=0, height=4), size=8, hjust=1, vjust=1, show.legend=FALSE)+    #SE bars
      #geom_text_repel(data=df2a, aes(x=DAI, y=LC, label=ifelse(DAI==118, as.character(Code),'')))+
      #
      stat_summary(data=df2b, fun=mean, aes(x=DAI, y=LC, group=Code, color=Code), geom="line", linetype="longdash", size=0.75, alpha=0.500)+    #avg on FLC
      #stat_summary(data=df2b, fun=mean, aes(x=DAI, y=LC, group=Code, color=Code), geom="point", alpha=0.500)+
      #stat_summary(data=df2b, fun.data=mean_se, aes(x=DAI, y=LC, group=Code, color=Code), geom="errorbar", alpha=0.15)+    #SE bars
      #stat_summary(data=df2b, fun.data=mean_se, aes(x=DAI, y=LC, color=Code, label=ifelse(DAI==118, as.character(Code),'')), geom="text", size=8, hjust=1, vjust=1, show.legend=FALSE)+    #SE bars
      #
      labs(title="Bermuda Hybrids vs. Cultivars", x="Days after irrigation (DAI)", y="Percent coverage (%)")+
      #
      theme(
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_rect(fill="transparent", color=NA),
        plot.background = element_rect(fill="transparent", color=NA),
        #plot.margin = unit(c(5.5,5.5,2,5.5), unit="point"),
        #
        axis.text = element_text(size=12),
        axis.title = element_text(size=14),
        axis.line = element_line(size=0.5, color="grey20"),
        plot.title = element_text(size=16),
        #
        #legend.position = "bottom",
        legend.position = "none",
        legend.text = element_text(color="grey20", size=10),
        legend.background = element_rect(linetype="solid", size=0.5, color="grey20"),
      ) +
  coord_cartesian(ylim=c(0.00,130))+
  scale_y_continuous(breaks=seq(0.00,125,25))+
  geom_rect(data=rect, aes(xmin=xmin, xmax=xmax, ymin=ymin, ymax=ymax), 
            color="skyblue1", fill="skyblue1", alpha=0.3, inherit.aes=FALSE)

p2 <- ggplot()+
  stat_summary(data=df2a, fun=mean, aes(x=DAI, y=LC, group=Code, color=Code), geom="line", linetype="solid", size=0.75, alpha=0.500)+    #avg on FLC
  #stat_summary(data=df2a, fun=mean, aes(x=DAI, y=LC, group=Code, color=Code), geom="point", alpha=0.500)+
  #stat_summary(data=df2a, fun.data=mean_se, aes(x=DAI, y=LC, group=Code, color=Code), geom="errorbar")+    #SE bars
  #stat_summary(data=df2a, fun.data=mean_se, aes(x=DAI, y=LC, color=Code, label=ifelse(DAI==118, as.character(Code),'')), geom="text", position=position_dodge(width=0, height=4), size=8, hjust=1, vjust=1, show.legend=FALSE)+    #SE bars
  #geom_text_repel(data=df2a, aes(x=DAI, y=LC, label=ifelse(DAI==118, as.character(Code),'')))+
  #
  stat_summary(data=df2b, fun=mean, aes(x=DAI, y=LC, group=Code, color=Code), geom="line", linetype="longdash", size=1.5)+#, alpha=0.25)+    #avg on FLC
  #stat_summary(data=df2b, fun=mean, aes(x=DAI, y=LC, group=Code, color=Code), geom="point")+#, alpha=0.25)+
  #stat_summary(data=df2b, fun.data=mean_se, aes(x=DAI, y=LC, group=Code, color=Code), geom="errorbar", alpha=0.15)+    #SE bars
  #stat_summary(data=df2b, fun.data=mean_se, aes(x=DAI, y=LC, color=Code, label=ifelse(DAI==118, as.character(Code),'')), geom="text", size=8, hjust=1, vjust=1, show.legend=FALSE)+    #SE bars
  #
  labs(title="Cultivars vs. Bermuda Hybrids", x="Days after irrigation (DAI)", y="Percent coverage (%)")+
  #
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.background = element_rect(fill="transparent", color=NA),
    plot.background = element_rect(fill="transparent", color=NA),
    #plot.margin = unit(c(5.5,5.5,2,5.5), unit="point"),
    #
    axis.text = element_text(size=12),
    axis.title = element_text(size=14),
    axis.line = element_line(size=0.5, color="grey20"),
    plot.title = element_text(size=16),
    #
    legend.position = "none",
    legend.text = element_text(color="grey20", size=10),
    legend.background = element_rect(linetype="solid", size=0.5, color="grey20"),
  )+
  coord_cartesian(ylim=c(0.00,130))+
  scale_y_continuous(breaks=seq(0.00,125,25))+
  geom_rect(data=rect, aes(xmin=xmin, xmax=xmax, ymin=ymin, ymax=ymax), 
            color="skyblue1", fill="skyblue1", alpha=0.3, inherit.aes=FALSE)

q1 <- ggplot()+
  stat_summary(data=df2a, fun=mean, aes(x=DAI, y=nPC2, group=Code, color=Code), geom="line", linetype="solid", size=1.5)+#, alpha=0.25)+    #avg on FLC
  #stat_summary(data=df2a, fun=mean, aes(x=DAI, y=nPC2, group=Code, color=Code), geom="point")+#, alpha=0.25)+
  #stat_summary(data=df2a, fun.data=mean_se, aes(x=DAI, y=LC, group=Code, color=Code), geom="errorbar")+    #SE bars
  #stat_summary(data=df2a, fun.data=mean_se, aes(x=DAI, y=LC, color=Code, label=ifelse(DAI==118, as.character(Code),'')), geom="text", position=position_dodge(width=0, height=4), size=8, hjust=1, vjust=1, show.legend=FALSE)+    #SE bars
  #geom_text_repel(data=df2a, aes(x=DAI, y=LC, label=ifelse(DAI==118, as.character(Code),'')))+
  #
  stat_summary(data=df2b, fun=mean, aes(x=DAI, y=nPC2, group=Code, color=Code), geom="line", linetype="longdash", size=0.75, alpha=0.500)+    #avg on FLC
  #stat_summary(data=df2b, fun=mean, aes(x=DAI, y=nPC2, group=Code, color=Code), geom="point", alpha=0.500)+
  #stat_summary(data=df2b, fun.data=mean_se, aes(x=DAI, y=LC, group=Code, color=Code), geom="errorbar", alpha=0.15)+    #SE bars
  #stat_summary(data=df2b, fun.data=mean_se, aes(x=DAI, y=LC, color=Code, label=ifelse(DAI==118, as.character(Code),'')), geom="text", size=8, hjust=1, vjust=1, show.legend=FALSE)+    #SE bars
  #
  labs(title="Bermuda Hybrids vs. Cultivars (DIA)", x="Days after irrigation (DAI)", y="Percent coverage (%)")+
  #
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.background = element_rect(fill="transparent", color=NA),
    plot.background = element_rect(fill="transparent", color=NA),
    #plot.margin = unit(c(5.5,5.5,2,5.5), unit="point"),
    #
    axis.text = element_text(size=12),
    axis.title = element_text(size=14),
    axis.line = element_line(size=0.5, color="grey20"),
    plot.title = element_text(size=16),
    #
    legend.position = "none",
    legend.text = element_text(color="grey20", size=10),
    legend.background = element_rect(linetype="solid", size=0.5, color="grey20"),
  )+
  coord_cartesian(ylim=c(0.00,130))+
  scale_y_continuous(breaks=seq(0.00,125,25))+
  geom_rect(data=rect, aes(xmin=xmin, xmax=xmax, ymin=ymin, ymax=ymax), 
            color="skyblue1", fill="skyblue1", alpha=0.3, inherit.aes=FALSE)

q2<- ggplot()+
  stat_summary(data=df2a, fun=mean, aes(x=DAI, y=nPC2, group=Code, color=Code), geom="line", linetype="solid", size=0.75, alpha=0.500)+    #avg on FLC
  #stat_summary(data=df2a, fun=mean, aes(x=DAI, y=nPC2, group=Code, color=Code), geom="point", alpha=0.500)+
  #stat_summary(data=df2a, fun.data=mean_se, aes(x=DAI, y=LC, group=Code, color=Code), geom="errorbar")+    #SE bars
  #stat_summary(data=df2a, fun.data=mean_se, aes(x=DAI, y=LC, color=Code, label=ifelse(DAI==118, as.character(Code),'')), geom="text", position=position_dodge(width=0, height=4), size=8, hjust=1, vjust=1, show.legend=FALSE)+    #SE bars
  #geom_text_repel(data=df2a, aes(x=DAI, y=LC, label=ifelse(DAI==118, as.character(Code),'')))+
  #
  stat_summary(data=df2b, fun=mean, aes(x=DAI, y=nPC2, group=Code, color=Code), geom="line", linetype="longdash", size=1.5)+#, alpha=0.25)+    #avg on FLC
  #stat_summary(data=df2b, fun=mean, aes(x=DAI, y=nPC2, group=Code, color=Code), geom="point")+#, alpha=0.25)+
  #stat_summary(data=df2b, fun.data=mean_se, aes(x=DAI, y=LC, group=Code, color=Code), geom="errorbar", alpha=0.15)+    #SE bars
  #stat_summary(data=df2b, fun.data=mean_se, aes(x=DAI, y=LC, color=Code, label=ifelse(DAI==118, as.character(Code),'')), geom="text", size=8, hjust=1, vjust=1, show.legend=FALSE)+    #SE bars
  #
  labs(title="Cultivars vs. Bermuda Hybrids (DIA)", x="Days after irrigation (DAI)", y="Percent coverage (%)")+
  #
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.background = element_rect(fill="transparent", color=NA),
    plot.background = element_rect(fill="transparent", color=NA),
    #plot.margin = unit(c(5.5,5.5,2,5.5), unit="point"),
    #
    axis.text = element_text(size=12),
    axis.title = element_text(size=14),
    axis.line = element_line(size=0.5, color="grey20"),
    plot.title = element_text(size=16),
    #
    legend.position = "none",
    legend.text = element_text(color="grey20", size=10),
    legend.background = element_rect(linetype="solid", size=0.5, color="grey20"),
  )+
  coord_cartesian(ylim=c(0.00,130))+
  scale_y_continuous(breaks=seq(0.00,125,25))+
  geom_rect(data=rect, aes(xmin=xmin, xmax=xmax, ymin=ymin, ymax=ymax), 
            color="skyblue1", fill="skyblue1", alpha=0.3, inherit.aes=FALSE)


png("2020_bFigs.png", height=9.5, width=9.75, units="in", res=300)
#svg("2020_bFigs.svg", height=9.5, width=9.75)
ggarrange(p1, p2, q1, q2,
          labels=c("A","B","C","D"),
          common.legend=TRUE, legend="bottom", font.label=list(size=18),
          ncol=2, nrow=2)

dev.off()
