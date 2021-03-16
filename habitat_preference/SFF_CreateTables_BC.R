# ----------Load pacages-------------------------
library(survival)
library(broom)
library(ggplot2)
library(GGally)
library(jtools)
library(likert)
library(cowplot)
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
##====Separate periods + interaction with tag =====================================================================
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#------------read data---------------------------------------------------------------------------------------------
BeforeFeeding<-read.csv("ForStepSelection_BeforeFeeding_Amp_3_08.csv")
# change order
BeforeFeeding$DaledNonDaled<-as.factor(BeforeFeeding$DaledNonDaled ) 
levels(BeforeFeeding$DaledNonDaled)<- c("DF-independent","DF-dependent")
BeforeFeeding$DaledNonDaled<- factor(BeforeFeeding$DaledNonDaled, levels = c("DF-dependent", "DF-independent"))

LowFeeding<-read.csv("ForStepSelection_LowFeeding_Amp_3_08.csv")
LowFeeding$DaledNonDaled<-as.factor(LowFeeding$DaledNonDaled ) 
levels(LowFeeding$DaledNonDaled)<- c("DF-independent","DF-dependent")
LowFeeding$DaledNonDaled<- factor(LowFeeding$DaledNonDaled, levels = c("DF-dependent", "DF-independent"))

IntenseFeeding<-read.csv("ForStepSelection_IntenseFeeding_Amp_3_08.csv")
IntenseFeeding$DaledNonDaled<-as.factor(IntenseFeeding$DaledNonDaled ) 
levels(IntenseFeeding$DaledNonDaled)<- c("DF-independent","DF-dependent")
IntenseFeeding$DaledNonDaled<- factor(IntenseFeeding$DaledNonDaled, levels = c("DF-dependent", "DF-independent"))
#--------- Run the models----------------------------------------------------------------------------------------
# BEFORE 
# ran model
MBefore <- clogit(Random_Realized ~ (Feeding + Annual + Perennial + Harvested + Orchard + Other) * DaledNonDaled +
                     + strata(Strata), data=BeforeFeeding)
# create table
summary(MBefore)
MBefore_table <- tidy(MBefore)
MBefore_table <- na.omit(MBefore_table)
MBefore_table$period<-"Before"
# LOW FEEDING 
# ran model
MLowFeeding <- clogit(Random_Realized ~ (Feeding + Annual + Perennial + Harvested + Orchard + Other) * DaledNonDaled +
                    + strata(Strata), data=LowFeeding)
# create table
summary(MLowFeeding)
MLowFeeding_table <- tidy(MLowFeeding)
MLowFeeding_table <- na.omit(MLowFeeding_table)
MLowFeeding_table$period<-"LowFeeding"

# INTENSE FEEDING 
# ran model
MIntenseFeeding<- clogit(Random_Realized ~ (Feeding + Annual + Perennial + Harvested + Orchard + Other) * DaledNonDaled +
                        + strata(Strata), data=IntenseFeeding)
# create table
summary(MIntenseFeeding)
MIntenseFeeding_table <- tidy(MIntenseFeeding)
MIntenseFeeding_table <- na.omit(MIntenseFeeding_table)
MIntenseFeeding_table$period <- "IntenseFeeding"
# bind all together
InteractionWithTag<-rbind(MBefore_table,MLowFeeding_table,MIntenseFeeding_table)
# write csv
write.csv(InteractionWithTag,"InteractionWithTag.csv")


#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
##====Separate tag types + interaction perid =====================================================================
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#------------read data---------------------------------------------------------------------------------------------
CraneData_Daled<-read.csv("ForStepSelection_RegularTags_Amp_3_08.csv")
CraneData_Daled$Period <- as.factor(CraneData_Daled$BeforeAfter) 
levels(CraneData_Daled$Period)<- c("Before","LowFeeding","IntenseFeeeding")

CraneData_NonDaled<-read.csv("ForStepSelection_NonDaledTags_Amp_3_08.csv")
CraneData_NonDaled$Period <- as.factor(CraneData_NonDaled$BeforeAfter) 
levels(CraneData_NonDaled$Period)<- c("Before","LowFeeding","IntenseFeeeding")
#--------- Run the models----------------------------------------------------------------------------------------
# DF-dependent TAGS
MDaled <- clogit(Random_Realized ~ (Feeding + Annual + Perennial + Harvested + Orchard + Other) * Period +
                 + strata(Strata), data=CraneData_Daled)
summary(MDaled )
MDaled_table <- tidy(MDaled)
MDaled_table <- na.omit(MDaled_table)
MDaled_table$Type<-"DF-dependent"

# DF-indepenedent TAGS
MNonDaled <- clogit(Random_Realized ~ (Feeding + Annual + Perennial + Harvested + Orchard + Other) * Period +
                 + strata(Strata), data=CraneData_NonDaled)
summary(MNonDaled )
MNonDaled_table <- tidy(MNonDaled)
MNonDaled_table <- na.omit(MNonDaled_table)
MNonDaled_table$Type<-"DF-indepenedent"

# bind all together
InteractionWithPeriod<-rbind(MDaled_table,MNonDaled_table)
# write csv
write.csv(InteractionWithPeriod,"InteractionWithPeriod.csv")

#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
##====Run all models separtely =====================================================================
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#------------read data---------------------------------------------------------------------------------------------
BeforeFeeding<-read.csv("ForStepSelection_BeforeFeeding_Amp_3_08.csv")
LowFeeding<-read.csv("ForStepSelection_LowFeeding_Amp_3_08.csv")
IntenseFeeding<-read.csv("ForStepSelection_IntenseFeeding_Amp_3_08.csv")

#BEFORE
BeforeFeedingD<-BeforeFeeding[BeforeFeeding$DaledNonDaled==1,]
BeforeFeedingND<-BeforeFeeding[BeforeFeeding$DaledNonDaled==0,]
# ran model
MBeforeD <- clogit(Random_Realized ~ Feeding + Annual + Perennial + Harvested + Orchard + Other + 
                     + strata(Strata), data=BeforeFeedingD)
MBeforeND <- clogit(Random_Realized ~ Feeding + Annual + Perennial + Harvested + Orchard + Other + 
                      + strata(Strata), data=BeforeFeedingND)
# create table
summary(MBeforeD)
MBeforeD_table <- tidy(MBeforeD)
MBeforeD_table$model <- "Before_DF-dependent"
summary(MBeforeND)
MBeforeND_table <- tidy(MBeforeND)
MBeforeND_table$model <- "Before_DF-indepenedent"

#LOW FEEDING
# prepare data
LowFeedingD<-LowFeeding[LowFeeding$DaledNonDaled==1,]
LowFeedingND<-LowFeeding[LowFeeding$DaledNonDaled==0,]
# ran model
MLowFeedingD <- clogit(Random_Realized ~ Feeding + Annual + Perennial + Harvested + Orchard + Other + 
                         + strata(Strata), data=LowFeedingD)
MLowFeedingND <- clogit(Random_Realized ~ Feeding + Annual + Perennial + Harvested + Orchard + Other + 
                          + strata(Strata), data=LowFeedingND)
# create table
summary(MLowFeedingD)
MLowFeedingD_table <- tidy(MLowFeedingD)
MLowFeedingD_table$model <- "Low_DF-dependent"
summary(MLowFeedingND)
MLowFeedingND_table <- tidy(MLowFeedingND)
MLowFeedingND_table$model <- "Low_DF-indepenedent"

#INTENSE
IntenseFeedingD<-IntenseFeeding[IntenseFeeding$DaledNonDaled==1,]
IntenseFeedingND<-IntenseFeeding[IntenseFeeding$DaledNonDaled==0,]
# ran model
MIntenseFeedingD <- clogit(Random_Realized ~ Feeding + Annual + Perennial + Harvested + Orchard + Other + 
                             + strata(Strata), data=IntenseFeedingD)
MIntenseFeedingND <- clogit(Random_Realized ~ Feeding + Annual + Perennial + Harvested + Orchard + Other +
                              + strata(Strata), data=IntenseFeedingND)
# create table
summary(MIntenseFeedingD)
MIntenseFeedingD_table <- tidy(MIntenseFeedingD)
MIntenseFeedingD_table$model <- "Intnse_DF-dependent"
summary(MIntenseFeedingND)
MIntenseFeedingND_table <- tidy(MIntenseFeedingND)
MIntenseFeedingND_table$model <- "Intnse_DF-indepenedent"

# bind all together
AllSeparate<-rbind(MBeforeD_table,MBeforeND_table,MLowFeedingD_table,
                             MLowFeedingND_table,MIntenseFeedingD_table,MIntenseFeedingND_table)
# write csv
write.csv(AllSeparate,"AllSeparate.csv")
