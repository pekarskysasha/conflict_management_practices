##====load packages=====================================================================================================================
library(ARTool) # ARTANOVA (nonparametric ANOVA - Ingo says, its a nonparametric GLMM)
library(emmeans) # for post hoc comparisons 
library(multcomp) # for grups in pot-hoc
library(ggplot2)
library(car)
library(plotly)
library(plyr)
library(reshape2)
##====DTV revists=======================================================================================================================
#--load data------------------------------------------------------------------
CraneData<-read.csv("InfoPerFieldAll.csv")
CraneData$Period<-as.factor(CraneData$Period)
CraneData$crop<-as.factor(CraneData$crop)
CraneData$FieldType[CraneData$crop==5]="Perennial"
CraneData$FieldType[CraneData$crop!=5]="Annual"
CraneData$FieldType=as.factor(CraneData$FieldType)

#-- first we check for Heteroscedasticity - homogenity of variances----------- 
leveneTest (meanGap ~ Period * FieldType, data=CraneData) # df=5 (7 groups), p-value=0.18, GOOD
#-- plot (Appendix S8 Fig S1)---------------------------------------------------------------------
ggplot(CraneData, aes(x=Period, y=meanGap,fill=Period)) + 
  geom_violin(alpha = 0.5)+
  scale_fill_manual(values = c("grey","grey","grey"))+
  geom_boxplot(width = 0.25, fill="white")+  
  theme_bw()

ggplot(CraneData, aes(x=FieldType, y=meanGap,fill=FieldType)) + 
  geom_violin(alpha = 0.5)+
  scale_fill_manual(values = c("grey","grey","grey"))+
  geom_boxplot(width = 0.25, fill="white")+  
  theme_bw()
A#--summerize------------------------------------------------------------------
tapply(CraneData$meanGap, CraneData$Period, summary)
#--ARTool---------------------------------------------------------------------
m = art(meanGap ~ FieldType* Period  + (1|Chaser), data=CraneData)
anova(m) # use the non-capital anova, because this is the one that works with the package

#                       F Df Df.res     Pr(>F)    
#1 FieldType         1.1329  1 4185.6   0.287225    
#2 Period           13.3965  2 4181.5 1.5868e-06 ***
#3 FieldType:Period  3.1790  2 4178.8   0.041727   *

#-- poshoc without interaction  
  #emmeans(artlm(m, "Period"), pairwise ~ Period)
  contrast(emmeans(artlm(m, "Period"), ~ Period), method="pairwise",pbkrtest.limit = 4194)
  #contrast estimate   SE  df z.ratio p.value
  #0 - 1         339 65.5 Inf  5.175  <.0001 
  #0 - 2         117 59.9 Inf  1.958  0.1227 
  #1 - 2        -222 67.6 Inf -3.278  0.0030 
  
#Results are averaged over the levels of: crop 
#P value adjustment: tukey method for comparing a family of 3 estimates 
marginal<-emmeans(artlm(m, "Period"),~ Period,pbkrtest.limit = 5000)
cld(marginal,alpha = 0.05,adjust = "tukey")
#Period emmean  SE   df lower.CL upper.CL .group
#1        1801 111 28.2     1518     2084  1    
#2        2023 109 26.1     1744     2301   2   
#0        2140 106 23.3     1867     2413   2   

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#-------DTV-crane interaction--------------------------------------------------------------------------------------------------------
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#--load data------------------------------------------------------------------
Returned<-read.csv("DWVIneraction_300to1000_aggr.csv")
#How much data we have omchased/not chased?
tapply(Returned$TiemeStayed, Returned$WasChased, length)
tapply(Returned$TiemeStayed, list(Returned$WasChased,Returned$tag), length)

# organize the table
Returned$cropNow=as.factor(Returned$cropNow)
Returned$WasChased=as.factor(Returned$WasChased)
Returned$tag=as.factor(Returned$tag)
Returned$Period=as.factor(Returned$Period)
Returned$TypeOfgTag=as.factor(Returned$TypeOfgTag)

Returned$DidReturned=Returned$daysUntilReturened
Returned$DidReturned[Returned$daysUntilReturened==100]=0
Returned$DidReturned[Returned$daysUntilReturened!=100]=1

Returned$DidReturned10Days=Returned$daysUntilReturened
Returned$DidReturned10Days[Returned$daysUntilReturened>10]=0
Returned$DidReturned10Days[Returned$daysUntilReturened<11]=1

Returned$DidReturnedfirstDay=Returned$daysUntilReturened
Returned$DidReturnedfirstDay[Returned$daysUntilReturened==0]=1
Returned$DidReturnedfirstDay[Returned$daysUntilReturened!=00]=0

# crop type
Returned$FieldType[Returned$cropNow==2]="Perennial"
Returned$FieldType[Returned$cropNow!=2]="Annual"
Returned$FieldTyp=as.factor(Returned$FieldType)

##==== chased vs. not chased: (1) how long cranes stay on fileds (Perrenial/annual) ======================================================================
#--summerize and chseck------------------------------------------------------
leveneTest (TiemeStayed ~ WasChased*FieldTyp, data=Returned) # NOT GOOD: p<0.05
#--agragate by crop and chasing (sutable only to check model with interactions)-
RR <- aggregate(TiemeStayed~FieldTyp+tag+WasChased,data=Returned,FUN=median)
#RR <- aggregate(TiemeStayed~tag+WasChased,data=Returned,FUN=median)
leveneTest (TiemeStayed ~ WasChased*FieldTyp, data=RR) # GOOD: p=0.19
TiemeStayedSumerize <- ddply(RR, c("FieldTyp","WasChased"),summarise,
                             N    = length(TiemeStayed),
                             mean = mean(TiemeStayed),
                             median = median(TiemeStayed),
                             sd   = sd(TiemeStayed),
                             se   = sd / sqrt(N))
TiemeStayedSumerize <- ddply(RR, c("WasChased"),summarise,
                             N    = length(TiemeStayed),
                             median = median(TiemeStayed),
                             mean = mean(TiemeStayed),
                             sd   = sd(TiemeStayed),
                             se   = sd / sqrt(N))
#--plot (Fig 4.a) ---------------------------------------------

ggplot(data = RR, aes(x = WasChased, y = TiemeStayed)) +
  geom_boxplot(linetype = "dashed", outlier.shape = 1,width = 0.9, fatten = 4) +
  stat_boxplot(aes(ymin = ..lower.., ymax = ..upper..), outlier.shape = 16,width = 0.9, fatten = 4) +
  stat_boxplot(geom = "errorbar", aes(ymin = ..ymax..), width = 0.1) +
  stat_boxplot(geom = "errorbar", aes(ymax = ..ymin..), width = 0.1) +
  labs(
       x = "",
       y = "length of stay (minutes)") +
  theme_classic() + # remove panel background and gridlines
  theme(text = element_text(size=20),
        panel.border = element_rect(linetype = "solid",
                                    colour = "black", fill = "NA", size = 0.5))+
  theme(
    # Remove panel border
    panel.border = element_blank(),  
    # Remove panel grid lines
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    # Remove panel background
    panel.background = element_blank(),
    # Add axis line
    axis.line = element_line(colour = "black")
  )+
  theme(text = element_text(size=20))
  

#--analyse-------------------------------------------------------------------
m = art(TiemeStayed ~ WasChased*FieldTyp  + (1|tag), data=RR)
anova(m)


##==== chased vs. not chased: (2) Returned first day==================================================================================
#--summerize and chseck------------------------------------------------------
tapply(Returned$DidReturnedfirstDay, list(Returned$FieldTyp,Returned$WasChased), length) # sample size
leveneTest (DidReturnedfirstDay ~ WasChased*FieldTyp, data=Returned) # NOT GOOD: p<0.001
#--agragate by crop and chasing----------------------------------------------
RR1 <- aggregate(DidReturnedfirstDay~FieldTyp+tag+WasChased,data=Returned,FUN=mean)
leveneTest (DidReturnedfirstDay ~ WasChased*FieldTyp, data=RR1) #GOOD: p=0.16

RR1 <- aggregate(DidReturnedfirstDay~tag+WasChased,data=Returned,FUN=mean)
leveneTest (DidReturnedfirstDay ~ WasChased, data=RR1) # GOOD: p=0.1 BUT NO FIELD TYPE
tapply(RR1$DidReturnedfirstDay, list(RR1$WasChased,RR1$FieldTyp), mean)
------------------------------------------------------
m = art(DidReturnedfirstDay ~ WasChased + (1|tag), data=RR1)
anova(m)

#             F Df Df.res  Pr(>F)  
#WasChased 2.4187  1     16 0.13945  


##==== chased vs. not chased: (3) Returned after 10 days==================================================================================
#--summerize and chseck------------------------------------------------------
tapply(Returned$DidReturned10Days, list(Returned$cropNow,Returned$WasChased), length) # sample size
leveneTest (DidReturned10Days ~ WasChased*cropNow, data=Returned) # NOT GOOD: p<0.001
#--agragate by crop and chasing----------------------------------------------
RR2 <- aggregate(DidReturned10Days~FieldTyp+tag+WasChased,data=Returned,FUN=mean)
leveneTest (DidReturned10Days ~ WasChased, data=RR2) # GOOD: p=0.16
tapply(RR2$DidReturned10Days, list(RR2$FieldTyp,RR2$WasChased), mean)
tapply(RR2$DidReturned10Days, list(RR2$WasChased), length)
#--analyse-------------------------------------------------------------------
m = art(DidReturned10Days ~  WasChased + (1|tag), data=RR2)
anova(m)

#               F Df Df.res   Pr(>F)  
#WasChased 3.2445  1 48.175 0.077923 .
#--plot----------------------------------------------------------------------
ggplot(RR2, aes(x=WasChased, y=DidReturned10Days, fill = as.factor(FieldTyp))) +
  geom_boxplot()+
  scale_fill_manual(values=c("#bfbfbf", "#616160"))

##==== Only chased: How long until returned the first day==================================================================================
#--subset only chased--------------------------------------------------------
ReturnedFirstD=Returned[Returned$daysUntilReturened==0 & Returned$WasChased==1,]
leveneTest (TimeUntilReturened ~ Period * FieldTyp, data=ReturnedFirstD) # df=5 (7 groups), p-value=0.31 - GOOD
FirstDaySumerizeTime <- ddply(ReturnedFirstD, c("FieldTyp","WasChased"),summarise,
                            N    = length(TimeUntilReturened),
                            mean = mean(TimeUntilReturened),
                            sd   = sd(TimeUntilReturened),
                            se   = sd / sqrt(N))
#--analyse-------------------------------------------------------------------
m = art(TimeUntilReturened ~ Period * FieldTyp  + (1|tag), data=ReturnedFirstD)
anova(m)
#                         F Df  Df.res  Pr(>F)  
#1 Period          0.867249  2  98.185 0.42330  
#2 FieldTyp        0.021726  1 154.867 0.88301  
#3 Period:FieldTyp 1.044168  2 125.225 0.35503      

#--Density plot (Appendix S8 Fig S2)--------------------------------------------------------
d <- density(ReturnedFirstD$TimeUntilReturened)
pdx <- which(d$x>=0)
xx<-d$x[pdx]
yy<-d$y[pdx]
df<-data.frame(xx,yy)

peak<-d$x[which(d$y==max(d$y))] # max return time (minutes)

ggplot(df, aes(x=xx, y=yy))+
  geom_line(color='#b3200c', size=2)+
  geom_vline(xintercept = peak,linetype="dashed")+
  labs(x = "Return Time (minutes)",
       y = "Density") +
  theme(
    # Remove panel border
    panel.border = element_blank(),  
    # Remove panel grid lines
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    # Remove panel background
    panel.background = element_blank(),
    # Add axis line
    axis.line = element_line(colour = "black")
  )+
  theme(text = element_text(size=20))

##==== Only chased: what is the chance of return per field==================================================================================
#--subset only chased--------------------------------------------------------
ReturnedChased=Returned[Returned$WasChased==1,]
leveneTest (DidReturnedfirstDay ~ Period *  FieldTyp, data=ReturnedChased) # df=5 (7 groups), p-value<0.001 
#--agragate by crop and chasing----------------------------------------------
RR1 <- aggregate(DidReturnedfirstDay~FieldTyp*tag,data=ReturnedChased,FUN=mean)
leveneTest (DidReturnedfirstDay ~ FieldTyp, data=RR1) # GOOD: p=0.55
tapply(RR1$DidReturnedfirstDay, list(RR1$FieldTyp), median)
tapply(RR1$DidReturnedfirstDay, list(RR1$FieldTyp), length)
#--calculate means and plot (Fig 4.c) -----------------------------------------
RRR <- aggregate(DidReturnedfirstDay~FieldTyp,data=RR1,FUN=mean)
st.err <- function(x) {
  sd(x)/sqrt(length(x))
}
RRR_SE <- aggregate(DidReturnedfirstDay~FieldTyp,data=RR1,FUN=st.err)
ggplot(RRR, aes(x=FieldTyp, y=DidReturnedfirstDay)) + 
  geom_bar(stat="identity", position=position_dodge())+
  geom_errorbar(aes(ymin = DidReturnedfirstDay-RRR_SE$DidReturnedfirstDay, 
                    ymax = DidReturnedfirstDay+RRR_SE$DidReturnedfirstDay)
                , width=.1, size=1.2)+
  theme(
    # Remove panel border
    panel.border = element_blank(),  
    # Remove panel grid lines
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    # Remove panel background
    panel.background = element_blank(),
    # Add axis line
    axis.line = element_line(colour = "grey")
  )+
  theme(text = element_text(size=25, face="bold"))
  
#--analyse-------------------------------------------------------------------
m = art(DidReturnedfirstDay ~ FieldTyp  + (1|tag), data=RR1)
anova(m)
#               F Df Df.res     Pr(>F)    
# FieldTyp 9.8189  1     16 0.0064132 **

contrast(emmeans(artlm(m, "FieldTyp"), ~ FieldTyp), method="pairwise")


marginal<-emmeans(artlm(m, "FieldTyp"),~ FieldTyp)
cld(marginal,alpha = 0.05,adjust = "tukey")
