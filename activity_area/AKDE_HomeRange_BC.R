library(ctmm)
library(leaflet) # required for interactive map display
library(tidyverse) # remove duplicates
library(sp) # project
library(mapview) # to save interactive maps
library(maptools) # for reading feeding station kml

opar <- par(no.readonly = TRUE) # save the defult par settings
# dev.off() -> if you do not manage to restore the defult parameters, but it erazes all the plots

#--- read polygon of the feeding station for plotting
tkml <- getKMLcoordinates(kmlfile="Deled.kml", ignoreAltitude=T)
#make polygon
Daled = Polygon(tkml)

#--- projections
itm <- "+init=epsg:2039 +proj=tmerc +lat_0=31.73439361111111 +lon_0=35.20451694444445 +k=1.0000067 +x_0=219529.584 +y_0=626907.39 +ellps=GRS80 +towgs84=-48,55,52,0,0,0,0 +units=m +no_defs" 
wgs84<-"+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"

# load data
CraneData<-read.csv("KernelDataInBeforeMiddleAfterFinal_new1.csv")
#-- take only before and after
CraneData <- CraneData[CraneData$Split=="Before" | CraneData$Split=="After",]
CraneData$Date_pox<- as.POSIXct(CraneData$Date_Time,tz="","%d-%m-%Y %H:%M:%S")
#============= convert to telemetry object for ctmm ====================================================
#-(1) remove duplicates (tidyverse)
dup <- !duplicated(CraneData[,c(9,2)])
CraneDataClean <- CraneData[dup,]
CraneDataClean <- unite(CraneDataClean, individual.local.identifier, c(Split, Tag), remove=FALSE)

# -(2) rename columns to MoveBank format
CraneDataClean<-rename(CraneDataClean, location.long = Lon)
CraneDataClean<-rename(CraneDataClean, location.lat = Lat)
CraneDataClean<-rename(CraneDataClean, timestamp = Date_pox)

#-(3) convert to telemetry object
#---- all of models are range resident and so only those portions of the data should be selected
#-- see here: https://cran.r-project.org/web/packages/ctmm/vignettes/variogram.html
#-- see workflow here: https://besjournals.onlinelibrary.wiley.com/doi/10.1111/2041-210X.12559
yourAnimals <- as.telemetry(CraneDataClean,projection=itm,drop=FALSE)

#***********************************************************************************************************
#***********************************************************************************************************
#***********************************************************************************************************
#-- Before
#============= home range calculation ====================================================

yourAnimals_Before=yourAnimals[19:36]
# prepare the dataframe
HomeRange <- as.data.frame(matrix(data=NA,nrow=length(yourAnimals_Before),ncol=9))
colnames(HomeRange)<-c("identifier","tag","UD95","UD50","total_area","bandwidth","x","y","period") 
HomeRange$period<-as.character(HomeRange$period)
HomeRange$identifier<-as.character(HomeRange$identifier)

Model_selection <- list()
BestModel2Save <- list()
ur95 <-  list()
ur50 <-  list()
FITS <- list()
Before.AKDE <- list()
for (i in 1:length(yourAnimals_Before)) {
  Animal <- yourAnimals_Before[[i]]
  SVF <- variogram(Animal)
  GUESS_Auto <- ctmm.guess(Animal,
                           variogram = SVF,
                           interactive = FALSE)
  fitted.mods <- ctmm.select(Animal,CTMM=GUESS_Auto,IC = "AICc",verbose=TRUE,cores=2)
  # save the model selection results to plot viograms
  Model_selection[[i]] <- summary(fitted.mods)
  names(Model_selection)[i] <- yourAnimals_Before[[i]]@info$identity
  BestModel2Save[[i]]  <- fitted.mods [[1]]
  names(BestModel2Save)[i] <- paste0(rownames(summary(fitted.mods))[1],',',yourAnimals_Before[[i]]@info$identity)
  # extract the best model 
  best_model <- fitted.mods [[1]]
  UD <- akde(Animal,best_model)
  # save the model selection results and the AKDE results to use with "meta"
  FITS[[i]] <- ctmm.fit(Animal,best_model,trace=2)
  Before.AKDE[[i]] <- UD
  # save the home range 
  sum50 <- summary(UD,level.UD=0.5,units=FALSE)
  sum95 <- summary(UD,level.UD=0.95,units=FALSE)
  #-- area => how many independent data points we have, because of the aoutocorelation
  HomeRange$total_area[i]<-sum50$DOF[["area"]]
  #--- bandwidth always bigger than in KDE bacause of the uncertainty
  HomeRange$bandwidth[i]<-sum50$DOF[["bandwidth"]]
  HomeRange$UD50[i] <- sum50$CI[2]/1000000
  HomeRange$UD95[i] <- sum95$CI[2]/1000000
  HomeRange$period[i] <- "Before"
  HomeRange$identifier[i] <- yourAnimals_Before[[i]]@info$identity
  pos <- str_locate_all(yourAnimals_Before[[i]]@info$identity, "[_]")[[1]]
  HomeRange$tag[i] <-str_sub(yourAnimals_Before[[i]]@info$identity, pos[[1]]+1, -1)
  
  #Gridsize
  HomeRange$x[i] <- UD[["h"]][["x"]]
  HomeRange$y[i] <- UD[["h"]][["y"]]
  
  # get the plygons for plotting
  #--- level: Confidence level for the magnitude of the above area. I.e., the 95% CI of the core home range area
  #--- level.UD: 	Coverage level of the UD area
  ur95[[i]]<- SpatialPolygonsDataFrame.UD(UD,level.UD=0.95,level=0.95) 
  ur50[[i]]<- SpatialPolygonsDataFrame.UD(UD,level.UD=0.5,level=0.95)
}
#-- save the data-------------------------------------------------------------
HomeRange_Before <- HomeRange
BestModel2Save_Before<-BestModel2Save
ur95_Before<-ur95
ur50_Before<-ur50
FITS_before <- FITS
save(ur95_Before, ur50_Before, HomeRange_Before,
     FITS_before, BestModel2Save_Before,Before.AKDE,file = "ctmm_before.RData")
#====================================================================
# look at viograms for all tags together with best model fit  - before (zoom out)
#====================================================================
par(mfrow=c(6,3))
par(mar=c(2,2,2,2))
for (i in 1:length(yourAnimalsBefore)) {
  Animal <- yourAnimals[[Name[i]]]
  SVF <- variogram(Animal)
  BestModel <- BestModel2Save[[i]]
  level <- c(0.5,0.95) # 50% and 95% CIs
  plot(SVF,CTMM=BestModel,col.CTMM=c("red"),fraction=1,level=0.5)
  title(names(BestModel2Save)[i])
}
par(opar) # restore the defult par settings


#***********************************************************************************************************
#***********************************************************************************************************
#***********************************************************************************************************
#-- After
#============= home range calculation ====================================================

yourAnimals_After=yourAnimals[1:18]
# prepare the dataframe
HomeRange <- as.data.frame(matrix(data=NA,nrow=length(yourAnimals_After),ncol=9))
colnames(HomeRange)<-c("identifier","tag","UD95","UD50","total_area","bandwidth","x","y","period") 
HomeRange$period<-as.character(HomeRange$period)
HomeRange$identifier<-as.character(HomeRange$identifier)

Model_selection <- list()
BestModel2Save <- list()
ur95 <-  list()
ur50 <-  list()
FITS <- list()
After.AKDE <- list()
for (i in 1:length(yourAnimals_After)) {
  Animal <- yourAnimals_After[[i]]
  SVF <- variogram(Animal)
  GUESS_Auto <- ctmm.guess(Animal,
                           variogram = SVF,
                           interactive = FALSE)
  fitted.mods <- ctmm.select(Animal,CTMM=GUESS_Auto,IC = "AICc",verbose=TRUE,cores=2)
  # save the model selection results to plot viograms
  Model_selection[[i]] <- summary(fitted.mods)
  names(Model_selection)[i] <- yourAnimals_After[[i]]@info$identity
  BestModel2Save[[i]]  <- fitted.mods [[1]]
  names(BestModel2Save)[i] <- paste0(rownames(summary(fitted.mods))[1],',',yourAnimals_After[[i]]@info$identity)
  # extract the best model 
  best_model <- fitted.mods [[1]]
  UD <- akde(Animal,best_model)
  # save the model selection results and the AKDE results to use with "meta"
  FITS[[i]] <- ctmm.fit(Animal,best_model,trace=2)
  After.AKDE[[i]] <- UD
  
  # save the home range 
  sum50 <- summary(UD,level.UD=0.5,units=FALSE)
  sum95 <- summary(UD,level.UD=0.95,units=FALSE)
  #-- area => how many independent data points we have, because the aoutocorelation
  HomeRange$total_area[i]<-sum50$DOF[["area"]]
  #--- bandwidth always bigger than in KDE bacause of the uncertenty
  HomeRange$bandwidth[i]<-sum50$DOF[["bandwidth"]]
  HomeRange$UD50[i] <- sum50$CI[2]/1000000
  HomeRange$UD95[i] <- sum95$CI[2]/1000000
  HomeRange$period[i] <- "After"
  HomeRange$identifier[i] <- yourAnimals_After[[i]]@info$identity
  pos <- str_locate_all(yourAnimals_After[[i]]@info$identity, "[_]")[[1]]
  HomeRange$tag[i] <-str_sub(yourAnimals_After[[i]]@info$identity, pos[[1]]+1, -1)
  
  #Gridsize
  HomeRange$x[i] <- UD[["h"]][["x"]]
  HomeRange$y[i] <- UD[["h"]][["y"]]
  
  # get the plygons for plotting
  #--- level: Confidence level for the magnitude of the above area. I.e., the 95% CI of the core home range area
  #--- level.UD: 	Coverage level of the UD area
  ur95[[i]]<- SpatialPolygonsDataFrame.UD(UD,level.UD=0.95,level=0.95) 
  ur50[[i]]<- SpatialPolygonsDataFrame.UD(UD,level.UD=0.5,level=0.95)
}
#-- save the data-------------------------------------------------------------------
HomeRange_After <- HomeRange
BestModel2Save_After<-BestModel2Save
ur95_After<-ur95
ur50_After<-ur50
FITS_After <- FITS
save(ur95_After, ur50_After, HomeRange_After,
     FITS_After, BestModel2Save_After,After.AKDE,file = "ctmm_after.RData")
#====================================================================
# look at viograms for all tags together with best model fit
#before (zoom out)
#====================================================================
par(mfrow=c(6,3))
par(mar=c(2,2,2,2))
for (i in 1:length(Name)) {
  Animal <- yourAnimals[[Name[i]]]
  SVF <- variogram(Animal)
  BestModel <- BestModel2Save[[i]]
  level <- c(0.5,0.95) # 50% and 95% CIs
  plot(SVF,CTMM=BestModel,col.CTMM=c("red"),fraction=1,level=0.5)
  title(names(BestModel2Save)[i])
}
par(opar) # restore the defult par settings

#***********************************************************************************************************
#***********************************************************************************************************
#***********************************************************************************************************
#============= Plot home range examples ====================================================

#++++++++++++++++++++++++++++++++++++++++++++++++++++
#--Plot example of 2 tags [BEFORE] (Fig 1.b)
#++++++++++++++++++++++++++++++++++++++++++++++++++++

Daledtag <- "#83c8fc"
NDaledTag <- "#fc3d03"

ur50<-ur50_Before
ur95<-ur95_Before

Name <- HomeRange_Before$identifier
indev <- as.numeric(HomeRange_Before$tag)

basemap <- leaflet() %>%
  addScaleBar(options = scaleBarOptions( maxWidth = 100,metric = TRUE,imperial = TRUE, updateWhenIdle = TRUE))%>%
  addProviderTiles("Esri.WorldImagery",group = "Esri.WorldImagery")

m=which(indev %in% c(170831,170591))

for (i in m){
  Animal <- yourAnimals[[Name[i]]]
  urll50<-spTransform(ur50[[i]], wgs84)
  urll50@polygons[[3]]<-NULL
  urll50@polygons[[1]]<-NULL
  urll95<-spTransform(ur95[[i]], wgs84)
  urll95@polygons[[3]]<-NULL
  urll95@polygons[[1]]<-NULL
  
  if (Name[i]=="Before_170831" | Name[i]=="Before_17084" |
      Name[i]=="Before_170951" |Name[i]=="Before_170991") {
    colorC <- NDaledTag
  }
  else{
    colorC <- Daledtag
  }
  
  if (i==min(m)) {
    map1 <- basemap %>% addPolygons(data = urll50,
                                    fill = T,fillColor = colorC, weight = 4,opacity = 0.7,color = colorC, fillOpacity = 0.7, stroke = F)
    
    map1 <- map1 %>% addPolygons(data = urll95,
                                 fill = F,fillColor = colorC, weight = 4,opacity = 0.7,color = colorC, stroke = T,dashArray = "3")
  }
  else{
    map1 <- map1 %>% addPolygons(data = urll50,
                                 fill = T,fillColor = colorC, weight = 4,opacity = 0.7,color = colorC, fillOpacity = 0.7, stroke = F)
    
    map1 <- map1 %>% addPolygons(data = urll95,
                                 fill = F,fillColor = colorC, weight = 4,opacity = 0.7,color = colorC, stroke = T,dashArray = "3")
  }
}

map1 <- map1 %>% addPolygons(data = Daled,
                             fill = F,fillOpacity = 1,weight = 4,opacity = 0.7,color = "white")
mapshot(map1, url = paste0(getwd(), "/newCTMM/example_Before_83&59.html"))
#++++++++++++++++++++++++++++++++++++++++++++++++++++
#--Plot example of 2 tags [DURING]  (Fig 1.c) 
#++++++++++++++++++++++++++++++++++++++++++++++++++++++
Daledtag <- "#83c8fc"
NDaledTag <- "#fc3d03"

ur50<-ur50_After
ur95<-ur95_After

Name <- HomeRange_After$identifier
indev <- as.numeric(HomeRange_After$tag)

basemap <- leaflet() %>%
  addScaleBar(options = scaleBarOptions( maxWidth = 100,metric = TRUE,imperial = TRUE, updateWhenIdle = TRUE))%>%
  addProviderTiles("Esri.WorldImagery",group = "Esri.WorldImagery")

m=which(indev %in% c(170831,170591))

for (i in m){
  Animal <- yourAnimals[[Name[i]]]
  urll50<-spTransform(ur50[[i]], wgs84)
  urll50@polygons[[3]]<-NULL
  urll50@polygons[[1]]<-NULL
  urll95<-spTransform(ur95[[i]], wgs84)
  urll95@polygons[[3]]<-NULL
  urll95@polygons[[1]]<-NULL
  
  if (Name[i]=="After_170831" | Name[i]=="After_17084" |
      Name[i]=="After_170951" |Name[i]=="After_170991") {
    colorC <- NDaledTag
  }
  else{
    colorC <- Daledtag
  }
  
  if (i==min(m)) {
    map1 <- basemap %>% addPolygons(data = urll50,
                                    fill = T,fillColor = colorC, weight = 4,opacity = 0.7,color = colorC, fillOpacity = 0.7, stroke = F)
    
    map1 <- map1 %>% addPolygons(data = urll95,
                                 fill = F,fillColor = colorC, weight = 4,opacity = 0.7,color = colorC, stroke = T,dashArray = "3")
  }
  else{
    map1 <- map1 %>% addPolygons(data = urll50,
                                 fill = T,fillColor = colorC, weight = 4,opacity = 0.7,color = colorC, fillOpacity = 0.5, stroke = F)
    
    map1 <- map1 %>% addPolygons(data = urll95,
                                 fill = F,fillColor = colorC, weight = 4,opacity = 0.7,color = colorC, stroke = T,dashArray = "3")
  }
}

map1 <- map1 %>% addPolygons(data = Daled,
                             fill = F,fillOpacity = 1,weight = 4,opacity = 0.7,color = "white")
mapshot(map1, url = paste0(getwd(), "/newCTMM/example_After_83&59.html"))


#***********************************************************************************************************
#***********************************************************************************************************
#***********************************************************************************************************
#----------------------------------------------------------------------------------------------------
  ###################################### analyse home range size ####################################
#----------------------------------------------------------------------------------------------------
load("ctmm_before.RData")
load("ctmm_after.RData")

HomeRangeAll <- rbind(HomeRange_Before,HomeRange_After)
HomeRangeAll$TagType <- 0
HomeRangeAll$TagType[HomeRangeAll$tag==170831 | HomeRangeAll$tag==17084 |
            HomeRangeAll$tag==170951 | HomeRangeAll$tag==170991] <- 1
HomeRangeAll$TagType<-factor(HomeRangeAll$TagType, labels=c("DF-dependent","DF-independent"))
HomeRangeAll$period<-factor(HomeRangeAll$period, levels=c("Before","After"),ordered=TRUE)
levels(HomeRangeAll$period) <- c("before feeding","during feeding")

#---------------plot by tag type (Fig 1.a) ----------------------------------------------------------------
ggplot(HomeRangeAll, aes(x = period, y = UD50, color = TagType,fill = TagType)) +
  geom_boxplot(outlier.shape = NA,alpha = 0.4)+
  geom_point(alpha = 0.6, size=3, position=position_jitterdodge(0.05),aes(color=TagType))+
  scale_color_manual(values = c("#83c8fc","#fc3d03"))+
  scale_fill_manual(values = c("#83c8fc","#fc3d03"))+
  labs(y="Core area (km^2)")+
  theme(text = element_text(size=20))+
  theme(text = element_text(size=20),panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "grey50"))

#--------- Make a paired test for the same indevidual---------------------------------------------------------------------
# compute the difference
d <- with(HomeRangeAll, 
          UD50[period == "Before"] - UD50[period == "After"])
# Shapiro-Wilk normality test for the differences
shapiro.test(d) # => p-value = 0.03169
# need non parametric test: Wilcoxon signed rank test
res <- wilcox.test(UD50 ~ period, data = HomeRangeAll, paired = TRUE)
res
# V = 171, p-value = 7.629e-06
res <- wilcox.test(UD95 ~ period, data = HomeRangeAll, paired = TRUE)
res

# analyse per tag type
iso2<-HomeRangeAll[HomeRangeAll$TagType=="DF-dependent",] # feeding station tags
d2 <- with(iso2, 
           iso2$UD50[period == "before feeding"] - UD50[period == "during feeding"])
shapiro.test(d2) # p-value = 0.01416
wilcox.test(UD50 ~ period, data = iso2, paired = TRUE) # p-value = 0.0001221
wilcox.test(UD95 ~ period, data = iso2, paired = TRUE) # p-value = 0.0001221

iso3<-HomeRangeAll[HomeRangeAll$TagType=="DF-independent",] # Non feeding station tags
d3 <- with(iso3, 
           UD50[period == "before feeding"] - UD50[period == "during feeding"])
shapiro.test(d3) # p-value = 0.7705
wilcox.test(UD50 ~ period, data = iso3, paired = TRUE) # p-value = 0.125
wilcox.test(UD95 ~ period, data = iso3, paired = TRUE) # p-value = 0.125
t.test(UD95 ~ period, data = iso3, paired = TRUE) # p-value = 0.02171

#summeryse

TiemeStayedSumerize <- ddply(HomeRangeAll, c("period"),summarise,
                             medianUD95 = median(UD95),
                             medianUD50 = median(UD50),
                             minUD95   = min(UD95),
                             minUD50   = min(UD50),
                             maxUD95   = max(UD95),
                             maxUD50   = max(UD50))

iemeStayedSumerize <- ddply(HomeRangeAll, c("period","TagType"),summarise,
                            medianUD95 = median(UD95),
                            medianUD50 = median(UD50),
                            minUD95   = min(UD95),
                            minUD50   = min(UD50),
                            maxUD95   = max(UD95),
                            maxUD50   = max(UD50))

################################################################  

