# ----------Load pacages-------------------------
library(survival)
library(broom)
library(ggplot2)
library(GGally)
library(jtools)
library(likert)
library(cowplot)
#_____________________________________________________________________________________________________________________________
##--- Per period and per tag, all separate models-------------------------------------------------------------

# read data
BeforeFeeding<-read.csv("ForStepSelection_BeforeFeeding_Amp_3_08.csv")
LowFeeding<-read.csv("ForStepSelection_LowFeeding_Amp_3_08.csv")
IntenseFeeding<-read.csv("ForStepSelection_IntenseFeeding_Amp_3_08.csv")

# BEFORE +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# prepare data
BeforeFeedingD<-BeforeFeeding[BeforeFeeding$DaledNonDaled==1,]
BeforeFeedingND<-BeforeFeeding[BeforeFeeding$DaledNonDaled==0,]
# ran model
MBeforeD <- clogit(Random_Realized ~ Harvested + Perennial + Annual +Orchard + Feeding + Other +
                     + strata(Strata), data=BeforeFeedingD)
MBeforeND <- clogit(Random_Realized ~ Harvested + Perennial + Annual +Orchard + Feeding + Other +
                      + strata(Strata), data=BeforeFeedingND)
# create table
summary(MBeforeD)
MBeforeD_table <- tidy(MBeforeD)
MBeforeD_table$sig <- MBeforeD_table$p.value
MBeforeD_table$sig[MBeforeD_table$p.value >= 0.01] <- 0
MBeforeD_table$sig[MBeforeD_table$p.value < 0.05] <- 1
MBeforeD_table$Type<-MBeforeD_table$sig
MBeforeD_table$Type[MBeforeD_table$Type >-1] <- 2
summary(MBeforeND)
MBeforeND_table <- tidy(MBeforeND)
MBeforeND_table$sig <- MBeforeND_table$p.value
MBeforeND_table$sig[MBeforeND_table$p.value >= 0.01] <- 0
MBeforeND_table$sig[MBeforeND_table$p.value < 0.05] <- 1
MBeforeND_table$Type<-MBeforeND_table$sig
MBeforeND_table$Type[MBeforeND_table$Type >-1] <- 1

MBeforeAll <- rbind(MBeforeD_table, MBeforeND_table)
MBeforeAll$Type<-factor(MBeforeAll$Type, labels = c("Avoiding tags","Station tags"))
# order
MBeforeAll$term <- factor(MBeforeAll$term , levels = c("Feeding", "Annual", "Perennial","Harvested","Orchard","Other"))
levels(MBeforeAll$term) <- c("F", "AN", "PRN","LGW","AP","OTH")
# make variable for coloring
MBeforeAll$Fill <- MBeforeAll$sig
MBeforeAll$Fill[MBeforeAll$sig==0 & MBeforeAll$Type=="Station tags"]<-0
MBeforeAll$Fill[MBeforeAll$sig==1 & MBeforeAll$Type=="Station tags"]<-1
MBeforeAll$Fill[MBeforeAll$sig==0 & MBeforeAll$Type=="Avoiding tags"]<-2
MBeforeAll$Fill[MBeforeAll$sig==1 & MBeforeAll$Type=="Avoiding tags"]<-3
MBeforeAll$Fill<-as.factor(MBeforeAll$Fill)
# create the plot
pBefore<-ggplot(data=MBeforeAll, aes(x=term, y=estimate, color=Fill,fill=Fill))+
  geom_hline(yintercept = 0, colour = gray(0.6), lty = "dashed",size=1)+
  geom_errorbar(aes(ymin = estimate-qnorm(0.975)*std.error, ymax = estimate+qnorm(0.975)*std.error)
                ,position=position_dodge(.7), width=.3, size=1.5)+
  geom_point(position=position_dodge(.7), size=5,stroke = 2.2,shape=21) + 
  scale_fill_manual(values=c("#b8b8b8","black","White","White"))+
  scale_color_manual(values=c("#b8b8b8","black","#b8b8b8","black"))+
  #labs(y="Relative selection strength", x="Habitat") + 
  ylim(-1.8, 1)+
  theme(axis.title = element_blank(), axis.text = element_text(size=20), strip.text = element_text(size=30)) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
      panel.background = element_blank(), axis.line = element_line(colour = "black"))+
  theme(legend.position="none") 

# LOW FEEDING +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# prepare data
LowFeedingD<-LowFeeding[LowFeeding$DaledNonDaled==1,]
LowFeedingND<-LowFeeding[LowFeeding$DaledNonDaled==0,]
# ran model
MLowFeedingD <- clogit(Random_Realized ~ Harvested + Perennial + Annual +Orchard + Feeding + Other +
                         + strata(Strata), data=LowFeedingD)
MLowFeedingND <- clogit(Random_Realized ~ Harvested + Perennial + Annual +Orchard+ Feeding + Other +
                          + strata(Strata), data=LowFeedingND)
# create table
summary(MLowFeedingD)
MLowFeedingD_table <- tidy(MLowFeedingD)
MLowFeedingD_table$sig <- MLowFeedingD_table$p.value 
MLowFeedingD_table$sig[MLowFeedingD_table$p.value >= 0.01] <- 0
MLowFeedingD_table$sig[MLowFeedingD_table$p.value < 0.05] <- 1
MLowFeedingD_table$Type<-MLowFeedingD_table$sig
MLowFeedingD_table$Type[MLowFeedingD_table$Type >-1] <- 2
summary(MLowFeedingND)
MLowFeedingND_table <- tidy(MLowFeedingND)
MLowFeedingND_table$sig <- MLowFeedingD_table$p.value 
MLowFeedingND_table$sig[MLowFeedingND_table$p.value >= 0.01] <- 0
MLowFeedingND_table$sig[MLowFeedingND_table$p.value < 0.05] <- 1
MLowFeedingND_table$Type<-MLowFeedingND_table$sig
MLowFeedingND_table$Type[MLowFeedingND_table$Type >-1] <- 1

MLowFeedingAll<- rbind(MLowFeedingD_table, MLowFeedingND_table)
MLowFeedingAll$Type<-factor(MLowFeedingAll$Type, labels = c("Avoiding tags","Station tags"))
# order
MLowFeedingAll$term <- factor(MLowFeedingAll$term, levels = c("Feeding", "Annual", "Perennial","Harvested","Orchard","Other"))
levels(MLowFeedingAll$term) <- c("F", "AN", "PRN","LGW","AP","OTH")
#levels(MLowFeedingAll$term) <- c("feeding", "annual", "peren.","leg./gr.","alm./pec.","other")
# make variable for coloring
MLowFeedingAll$Fill <- MLowFeedingAll$sig
MLowFeedingAll$Fill[MLowFeedingAll$sig==0 & MLowFeedingAll$Type=="Station tags"]<-0
MLowFeedingAll$Fill[MLowFeedingAll$sig==1 & MLowFeedingAll$Type=="Station tags"]<-1
MLowFeedingAll$Fill[MLowFeedingAll$sig==0 & MLowFeedingAll$Type=="Avoiding tags"]<-2
MLowFeedingAll$Fill[MLowFeedingAll$sig==1 & MLowFeedingAll$Type=="Avoiding tags"]<-3
MLowFeedingAll$Fill<-as.factor(MLowFeedingAll$Fill)
# create the plot
pLowFeeding<-ggplot(data=MLowFeedingAll, aes(x=term, y=estimate, color=Fill,fill=Fill))+
  geom_hline(yintercept = 0, colour = gray(0.6), lty = "dashed",size=1)+
  geom_errorbar(aes(ymin = estimate-qnorm(0.975)*std.error, ymax = estimate+qnorm(0.975)*std.error)
                ,position=position_dodge(.7), width=.3, size=1.5)+
  geom_point(position=position_dodge(.7), size=5,stroke = 2.2,shape=21) + 
  scale_fill_manual(values=c("#b8b8b8","black","White","White"))+
  scale_color_manual(values=c("#b8b8b8","black","#b8b8b8","black"))+
  #labs(y="Relative selection strength", x="Habitat") + 
  ylim(-1.8, 1)+
  theme(axis.title = element_blank(), axis.text = element_text(size=20), strip.text = element_text(size=30)) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"))+
  theme(legend.position="none")

# INTENSE FEEDING +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# prepare data
IntenseFeedingD<-IntenseFeeding[IntenseFeeding$DaledNonDaled==1,]
IntenseFeedingND<-IntenseFeeding[IntenseFeeding$DaledNonDaled==0,]
# ran model
MIntenseFeedingD <- clogit(Random_Realized ~ Harvested + Perennial + Annual +Orchard + Feeding + Other +
                             + strata(Strata), data=IntenseFeedingD)
MIntenseFeedingND <- clogit(Random_Realized ~ Harvested + Perennial + Annual +Orchard + Feeding + Other +
                              + strata(Strata), data=IntenseFeedingND)
# create table
summary(MIntenseFeedingD)
MIntenseFeedingD_table <- tidy(MIntenseFeedingD)
MIntenseFeedingD_table$sig <-MIntenseFeedingD_table$p.value
MIntenseFeedingD_table$sig[MIntenseFeedingD_table$p.value >= 0.01] <- 0
MIntenseFeedingD_table$sig[MIntenseFeedingD_table$p.value < 0.05] <- 1
MIntenseFeedingD_table$Type<-MIntenseFeedingD_table$sig
MIntenseFeedingD_table$Type[MIntenseFeedingD_table$Type >-1] <- 2
summary(MIntenseFeedingND)
MIntenseFeedingND_table <- tidy(MIntenseFeedingND)
MIntenseFeedingND_table$sig <-MIntenseFeedingD_table$p.value
MIntenseFeedingND_table$sig[MIntenseFeedingND_table$p.value >= 0.01] <- 0
MIntenseFeedingND_table$sig[MIntenseFeedingND_table$p.value < 0.05] <- 1
MIntenseFeedingND_table$Type<-MIntenseFeedingND_table$sig
MIntenseFeedingND_table$Type[MIntenseFeedingND_table$Type >-1] <- 1

MIntenseFeedingAll<- rbind(MIntenseFeedingD_table, MIntenseFeedingND_table)
MIntenseFeedingAll$Type<-factor(MLowFeedingAll$Type, labels = c("Avoiding tags","Station tags"))
# order
MIntenseFeedingAll$term <- factor(MIntenseFeedingAll$term, levels = c("Feeding", "Annual", "Perennial","Harvested","Orchard","Other"))
levels(MIntenseFeedingAll$term) <- c("F", "AN", "PRN","LGW","AP","OTH")
# make variable for coloring
MIntenseFeedingAll$Fill <- MIntenseFeedingAll$sig
MIntenseFeedingAll$Fill[MIntenseFeedingAll$sig==0 & MIntenseFeedingAll$Type=="Station tags"]<-0
MIntenseFeedingAll$Fill[MIntenseFeedingAll$sig==1 & MIntenseFeedingAll$Type=="Station tags"]<-1
MIntenseFeedingAll$Fill[MIntenseFeedingAll$sig==0 & MIntenseFeedingAll$Type=="Avoiding tags"]<-2
MIntenseFeedingAll$Fill[MIntenseFeedingAll$sig==1 & MIntenseFeedingAll$Type=="Avoiding tags"]<-3
MIntenseFeedingAll$Fill<-as.factor(MIntenseFeedingAll$Fill)
MIntenseFeedingAll$Fill <- factor(MIntenseFeedingAll$Fill, levels = c(levels(MIntenseFeedingAll$Fill), "2"))
MIntenseFeedingAll$Fill <- ordered(MIntenseFeedingAll$Fill, levels = c("0", "1", "2","3"))
# create the plot
pMIntenseFeedingAll<-ggplot(data=MIntenseFeedingAll, aes(x=term, y=estimate, color=Fill,fill=Fill))+
  geom_hline(yintercept = 0, colour = gray(0.6), lty = "dashed",size=1)+
  geom_errorbar(aes(ymin = estimate-qnorm(0.975)*std.error, ymax = estimate+qnorm(0.975)*std.error)
                ,position=position_dodge(.7), width=.3, size=1.5)+
  geom_point(position=position_dodge(.7), size=5,stroke = 2.2,shape=21) + 
  scale_fill_manual(values=c("#b8b8b8","black","White","White"),drop = FALSE)+
  scale_color_manual(values=c("#b8b8b8","black","#b8b8b8","black"),drop = FALSE)+
  #labs(y="Relative selection strength", x="Habitat") +
  ylim(-1.8, 1)+
  theme(axis.title = element_blank(), axis.text = element_text(size=20), strip.text = element_text(size=30)) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"))+
  theme(legend.position="none")

# for insert
pMIntenseFeedingAll_1<-ggplot(data=MIntenseFeedingAll, aes(x=term, y=estimate, color=Fill,fill=Fill))+
  geom_hline(yintercept = 0, colour = gray(0.6), lty = "dashed",size=1)+
  geom_errorbar(aes(ymin = estimate-qnorm(0.975)*std.error, ymax = estimate+qnorm(0.975)*std.error)
                ,position=position_dodge(.7), width=.3, size=1.5)+
  geom_point(position=position_dodge(.7), size=5,stroke = 2.2,shape=21) + 
  scale_fill_manual(values=c("#b8b8b8","black","White","White"),drop = FALSE)+
  scale_color_manual(values=c("#b8b8b8","black","#b8b8b8","black"),drop = FALSE)+
  labs(y="Relative selection strength", x="Habitat") +
  ylim(-5, 2)+
  theme(axis.title = element_text(size=20), axis.text = element_text(size=20), strip.text = element_text(size=30)) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black")) +
  theme(legend.position="none")

# for broken axes
pMIntenseFeedingAll_BAX<-ggplot(data=MIntenseFeedingAll, aes(x=term, y=estimate, color=Fill,fill=Fill))+
  geom_hline(yintercept = 0, colour = gray(0.6), lty = "dashed",size=1)+
  geom_errorbar(aes(ymin = estimate-qnorm(0.975)*std.error, ymax = estimate+qnorm(0.975)*std.error)
                ,position=position_dodge(.7), width=.3, size=1.5)+
  geom_point(position=position_dodge(.7), size=5,stroke = 2.2,shape=21) + 
  scale_fill_manual(values=c("#b8b8b8","black","White","White"),drop = FALSE)+
  scale_color_manual(values=c("#b8b8b8","black","#b8b8b8","black"),drop = FALSE)+
  labs(y="Relative selection strength", x="Habitat") +
  ylim(-5, -2)+
  theme(axis.title = element_text(size=20), axis.text = element_text(size=20), strip.text = element_text(size=30)) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black")) +
  theme(legend.position="none")


#--plot all
#-- plot without the diferent scale and plot it separately (Fig 2)
ggdraw() +
  draw_plot(pBefore, x=0, y=0, width=0.33, height=1) +
  draw_plot(pLowFeeding, x=0.33, y=0, width=0.33, height=1) + 
  draw_plot(pMIntenseFeedingAll, x=0.66, y=0, width=0.33, height=1) + 
  draw_plot_label(c("(a)","(b)","(c)"), c(0.03,0.36,0.69), c(1,1,1), size = 20, vjust=2)

pMIntenseFeedingAll_1
#-- plot for Fig 2.c broken Y-axis
pMIntenseFeedingAll_BAX
