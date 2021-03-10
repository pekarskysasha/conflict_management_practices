library(ggplot2)
#logistic function fit - from Ingo
library(drc)
#--------------explanation what is that thing doing-------------------------------------------------------------
# see explanations here: http://rstats4ag.org/dose-response-curves.html
# see here type of functions: https://www.statforbiology.com/nonlinearregression/usefulequations
# basically:
# d: higher asymptote, c: lower asymptote, e:  X value producing a response half-way between d and c, b: the slope around the inflection point
# Logistic curve: L.4 - all 4 parameters estimated, L.3 - with lower limit fixed at 0, L.2 - with lower limit fixed at 0 and upper to 1
# Gompertz Curve: not symmetric around the inflection point (G.4,G.3 and G.2)
# Log-based sygmoidal curves: functions where the independent variable X is contrained to be positive.(LL.4,LL.3 and LL.2)

#--Read the data and organize it------------------------------------------------------------------------------------------
 # including time before the station operational
FoodData<-read.csv("TablePresence_feeding.csv")
colnames(FoodData)<-c('Tag','Date','Feed','P')
FoodData<-na.omit(FoodData)
FoodData$DateTime_pox<- as.POSIXct(FoodData$Date,tz="","%d-%m-%Y")
FoodData$Stage[FoodData$DateTime_pox> "2017-12-21"]<-2
FoodData$Stage[FoodData$DateTime_pox<"2017-12-22"]<-1
FoodData$Stage[FoodData$DateTime_pox<"2017-12-05"]<-0

#--create the models--------------------------------------------------------------------------------------------------
#--Logistic curve
mL3 <- drm(P ~ Feed, data = FoodData, fct = L.3(), type = "continuous")
mL4 <- drm(P ~ Feed, data = FoodData, fct = L.4(), type = "continuous")
#--Log Logistic curve 
mLL3 <- drm(P ~ Feed, data = FoodData, fct = LL.3(), type = "continuous")
mLL4 <- drm(P ~ Feed, data = FoodData, fct = LL.4(), type = "continuous")
mLL5 <- drm(P ~ Feed, data = FoodData, fct = LL.5(), type = "continuous") 
#--Weibull curve type 1
mW13 <- drm(P ~ Feed, data = FoodData, fct = W1.3(), type = "continuous")
mW14 <- drm(P ~ Feed, data = FoodData, fct = W2.4(), type = "continuous")
#---comapere the models------------------------------------------------------------------------------------------------
AIC(mL3,mL4,mLL3,mLL4,mLL5,mW13,mW14)
cl <- list(L.4(),LL.3(),LL.4(),AR.2(),AR.3(),MM.2()) # AR.2 Asymptotic regression, AR.3 Power curve, MM.2 Michaelis-Menten equation 
#-Model selection by comparison of different models using the following criteria: 
# (1) the log likelihood value, (2) Akaike's information criterion (AIC), 
# (3)the estimated residual standard error and (4) the p-value from a lack-of-fit test.
mselect(mL3,cl,linreg = TRUE,icfct = AIC) # compare also to the linear model

#---Check the L.3, L.4 and mLL.4 (they have relatively good fit )------------------------
#-(1)---L.3
#Residual plot
op <- par(mfrow = c(1, 2), mar=c(3.2,3.2,2,.5), mgp=c(2,.7,0)) #put two graphs together
plot(residuals(mL3) ~ fitted(mL3), main="Residuals vs Fitted")
abline(h=0)
qqnorm(residuals(mL3))
qqline(residuals(mL3))

#summerize and plot 
summary(mL3)
plot(mL3)

# Test for lack of fit
modelFit(mL3)

#-(2)---L.4
#Residual plot
op <- par(mfrow = c(1, 2), mar=c(3.2,3.2,2,.5), mgp=c(2,.7,0)) #put two graphs together
plot(residuals(mL4) ~ fitted(mL4), main="Residuals vs Fitted")
abline(h=0)
qqnorm(residuals(mL4))
qqline(residuals(mL4))

#summerize and plot 
summary(mL4)
plot(mL4)

#Residual plot
modelFit(mL4)

#-(3)---mLL.4
#Residual plot
op <- par(mfrow = c(1, 2), mar=c(3.2,3.2,2,.5), mgp=c(2,.7,0)) #put two graphs together
plot(residuals(mLL4) ~ fitted(mLL4), main="Residuals vs Fitted")
abline(h=0)
qqnorm(residuals(mLL4))
qqline(residuals(mLL4))

#summerize and plot 
summary(mLL4)
plot(mLL4,broken=TRUE)

#Residual plot
modelFit(mLL4)


#--------Claculate Effective Dose (ED) --------------------------------
maED(mL3,cl,c(10,50,90,95,99), "buckland")
# estimates effective doses (ECp/EDp/ICp) for given reponse levels.
ED(mL3, c(10,50,90,95,99), interval = "delta")
#--------Plot--------------------------------------------------------------------------------------------------
#---first get the cooficients

# (2) Logistic curve, with 3 parameters 
CoF <- coef(mL3)
d <- CoF[2] #higher asymptote
c <- 0 #lower asymptote
e <- CoF[3] # X value producing a response half-way
b <- CoF[1] # the slope around the inflection point
lmeqL3 <- function(x) c+(d-c)/(1+exp(b*(x-e))) 

# plot all data -------------------------------------------
ggplot(data=FoodData, aes(x = Feed , y = P, color=factor(Stage))) +
  geom_point(alpha = 0.4,size = 3) +
  theme(text = element_text(size=20),panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "grey50"))+
  scale_colour_manual(values = c("#f29d3d","#bf84d8","#6eb297"))+
  scale_y_continuous(limits=c(0, 1))+
  scale_x_continuous(breaks=seq(2000,14000,2000))+
  labs( x = "Amount of corn fed per day (Kg)",y = "proportion of time at the feeding station")+
  stat_function(fun = lmeqL3, size=2, color="#717272")
 
# plot agregated per day -------------------------------------------

#--- agregate the data for plotting   
RR1 <- aggregate(P~Feed+DateTime_pox,data=FoodData,FUN=mean)
RR1$Stage[RR1$DateTime_pox> "2017-12-21"]<-2
RR1$Stage[RR1$DateTime_pox<"2017-12-22"]<-1
RR1$Stage[RR1$DateTime_pox<"2017-12-05"]<-0 
RR1$Feed<-RR1$Feed+1
#--summerize data per period
tapply(RR1$P, RR1$Stage, mean)
tapply(RR1$P, RR1$Stage, sd)
tapply(RR1$Feed, RR1$Stage, summary)

# plot balck and white
ggplot(data=RR1, aes(x = Feed , y = P, shape=factor(Stage))) +
  geom_point(alpha = 0.4,size = 3.5,stroke=1.5) +
  theme(text = element_text(size=20),panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "grey50"))+
  scale_shape_manual(values = c(23, 22, 21))+
  scale_y_continuous(limits=c(0, 1))+
  scale_x_continuous(breaks=seq(2000,14000,2000))+
  labs( x = "Amount of corn fed per day (kg)",y = "Proportion of time at the feeding station")+
  stat_function(fun = lmeqL3, size=2, color="#717272")+
  theme(legend.position="none")

