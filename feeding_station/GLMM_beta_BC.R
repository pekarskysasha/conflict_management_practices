library(ggplot2)

#glmTMB test
library(glmmTMB)
library(car)
library(lubridate)
library(emmeans)
library(multcomp)

FoodData<-read.csv("TablePresence_feeding.csv")
FoodData <-FoodData[FoodData$Food_amount_kg>0,] # only once the feeding sattation is operational 
colnames(FoodData)<-c('Tag','Date','Feed','P')
FoodData$DateTime_pox<- as.POSIXct(FoodData$Date,tz="","%d-%m-%Y")
FoodData$Stage[FoodData$Date > "2017-12-21"]<-1
FoodData$Stage[FoodData$Date <"2017-12-22"]<-0
zdx <- which(FoodData$P==0)
FoodData$P[zdx]<-0.0001
odx <- which(FoodData$P==1)
FoodData$P[odx]<-0.9999
FoodData$Month <- as.factor(month(FoodData$Date))
FoodData$FeedF<-as.factor(round(FoodData$Feed/1000)*1000)
m1 <- glmmTMB(P ~ Month + FeedF + (1|Tag),family=list(family="beta",link="logit"),
              data=FoodData)
summary(m1)
Anova(m1)
marginal<-emmeans(m1,~ Month)
pairs(marginal,adjust="tukey")
Sum = cld(marginal,alpha = 0.05,adjust = "tukey")
Sum

marginal<-emmeans(m1,~ FeedF)
pairs(marginal,adjust="tukey")
Sum = cld(marginal,alpha = 0.05,adjust = "tukey")
Sum

#----Corelation between the amount of food and the date after December--------------------------------------
SubFoodData$DateNum<-as.numeric(SubFoodData$DateTime_pox)
# we will use the non-parameteric test of correlation (Spearman rank correlation coefficient)
res2 <-cor.test(SubFoodData$Feed, SubFoodData$DateNum,  method = "spearman") # rho=-0.18, p-value< 0.001

