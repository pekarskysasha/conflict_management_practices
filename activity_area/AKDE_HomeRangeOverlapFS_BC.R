# pay attention to the order the packages are loaded, don't change this one, otherwise doesn't work!
library(maptools)
library(raster)
library(ggplot2)

library(sp)
library(rgdal)
library(raster)
library(leaflet) 
library(plyr)
library(ggplot2)

## Chris D=Fkemin advice: 
#--exporting the probability mass function (PMF) raster and calculating the probability 
#--mass in certain areas by adding the PMF values in those polygons. 
#--That sum gives you the predicted fraction of time spent in those areas.
#---PMF: gives the total probability per cell (see here: https://ctmm-initiative.github.io/ctmm/reference/export.html)
##_______________________________________________________________________________
wgs84<-"+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"
itm <- "+init=epsg:2039 +proj=tmerc +lat_0=31.73439361111111 +lon_0=35.20451694444445 +k=1.0000067 +x_0=219529.584 +y_0=626907.39 +ellps=GRS80 +towgs84=-48,55,52,0,0,0,0 +units=m +no_defs" 
#_______________________________________________________________________________

# load data (only "After" all cranes)
CraneData<-read.csv("KernelData4Overlap.csv")
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

#============= home range calculation ====================================================
# prepare the dataframe
HomeRange <- as.data.frame(matrix(data=NA,nrow=length(yourAnimals),ncol=9))
colnames(HomeRange)<-c("identifier","tag","UD95","UD50","total_area","bandwidth","x","y","period") 
HomeRange$period<-as.character(HomeRange$period)
HomeRange$identifier<-as.character(HomeRange$identifier)

Model_selection <- list()
BestModel2Save <- list()
ur95 <-  list()
ur50 <-  list()
FITS <- list()
Before.AKDE <- list()
for (i in 1:length(yourAnimals)) {
  Animal <- yourAnimals[[i]]
  SVF <- variogram(Animal)
  GUESS_Auto <- ctmm.guess(Animal,
                           variogram = SVF,
                           interactive = FALSE)
  fitted.mods <- ctmm.select(Animal,CTMM=GUESS_Auto,IC = "AICc",verbose=TRUE,cores=2)
  # save the model selection results to plot viograms
  Model_selection[[i]] <- summary(fitted.mods)
  names(Model_selection)[i] <- yourAnimals[[i]]@info$identity
  BestModel2Save[[i]]  <- fitted.mods [[1]]
  names(BestModel2Save)[i] <- paste0(rownames(summary(fitted.mods))[1],',',yourAnimals[[i]]@info$identity)
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
  HomeRange$identifier[i] <- yourAnimals[[i]]@info$identity
  pos <- str_locate_all(yourAnimals[[i]]@info$identity, "[_]")[[1]]
  HomeRange$tag[i] <-str_sub(yourAnimals[[i]]@info$identity, pos[[1]]+1, -1)
  
  #Gridsize
  HomeRange$x[i] <- UD[["h"]][["x"]]
  HomeRange$y[i] <- UD[["h"]][["y"]]
  
  # get the plygons for plotting
  #--- level: Confidence level for the magnitude of the above area. I.e., the 95% CI of the core home range area
  #--- level.UD: 	Coverage level of the UD area
  ur95[[i]]<- SpatialPolygonsDataFrame.UD(UD,level.UD=0.95,level=0.95) 
  ur50[[i]]<- SpatialPolygonsDataFrame.UD(UD,level.UD=0.5,level=0.95)
  
  writeRaster(UD,paste0("PMF_raster_ctmm_4Overlap/",yourAnimals[[i]]@info$identity),"GTiff",DF="PMF")
}


save(ur95, ur50, HomeRange,
     FITS, BestModel2Save,Before.AKDE,file = "ctmm_4overlap.RData")

#***********************************************************************************************************
#***********************************************************************************************************
#***********************************************************************************************************
#============= calculte the predicted time in the feeding station ===========================================

# copied from here: https://rstudio-pubs-static.s3.amazonaws.com/254726_caf5ac8c774645d890f97a674f6afa33.html
#--- Load data --------------------------------------------------
#-list files (in this case raster TIFFs)
grids <- list.files("./PMF_raster_ctmm_4Overlap", pattern = "*.tif$")
#-check the number of files in the raster list (grids)
length <- length(grids)
#------Feeding station polygon (shapefile not kml)-------------------
Daled <- readOGR(".","Daled")
Daled_prj <- spTransform(Daled, CRS(itm))
crs(Daled_prj)<-itm

#---extract raster cell count (sum) within each polygon area (poly)-------------------
InDaled <- as.data.frame(matrix(data=NA,nrow=length(grids),ncol=5))
colnames(InDaled)<-c("identifier","tag","period","totalSum","insiede_D")
for (i in 1:length(grids)){
  s <- raster(paste0("./PMF_raster_ctmm_4Overlap/", grids[i]))
  ex <- extract(s, Daled, fun=sum, na.rm=TRUE, df=TRUE)
  if (length(row.names(ex))>0) { # if intercect with Daled
  InDaled$insiede_D[i] <- ex[[2]]
  }
  else {
    InDaled$insiede_D[i] <- 0
  }
  InDaled$totalSum[i] <- cellStats(s, 'sum')
  InDaled$identifier[i] <- str_sub(grids[i],1, -5)
  pos <- str_locate_all(grids[i], "[_]")[[1]]
  InDaled$tag[i] <- str_sub(grids[i], pos[[1]]+1, -5)
  InDaled$period[i] <- str_sub(grids[i],1, pos[[1]]-1)
}


# find tags that stayed less than 1% of the time in Daled
InDaled$tag[InDaled$insiede_D<0.01]

InDaled$TagType <- 0
InDaled$TagType[InDaled$insiede_D<0.01] <- 1
InDaled$TagType<-factor(InDaled$TagType, labels=c("DF-dependent","DF-independent"))
# make boxplot and calculate stats for the rest

ggplot(InDaled, aes(x = TagType, y = insiede_D, color = TagType,fill = TagType)) +
  geom_boxplot(outlier.shape = NA,alpha = 0.2)+
  geom_point(alpha = 0.6, size=3, position=position_jitterdodge(0.05),aes(color=TagType))+
  scale_color_manual(values = c("#83c8fc","#fc3d03"))+
  scale_fill_manual(values = c("#83c8fc","#fc3d03"))+
  labs(y="predicted time inside the feeding station")+
  theme(text = element_text(size=20))+
  theme(text = element_text(size=20),panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "grey50"))

tapply(InDaled$insiede_D ,list(InDaled$TagType),median)
tapply(InDaled$insiede_D ,list(InDaled$TagType),mean)
tapply(InDaled$insiede_D ,list(InDaled$TagType),min)
tapply(InDaled$insiede_D ,list(InDaled$TagType),max)


#***********************************************************************************************************
#***********************************************************************************************************
#***********************************************************************************************************
#============= relate the predicted time to age & sex ===========================================

# load personal information
AgeSex<-read.csv("AgeSex.csv")
InDaled$tag=as.numeric(InDaled$tag)

for (i in 1:nrow(InDaled)) {
InDaled$Age[i] <- AgeSex$age[AgeSex$ï..indevidual==InDaled$tag[i]]
InDaled$Sex[i] <- AgeSex$sex[AgeSex$ï..indevidual==InDaled$tag[i]]
}
InDaled$Age<-factor(InDaled$Age)
InDaled$Sex<-factor(InDaled$Sex)
InDaled$InD <- 0
InDaled$InD[InDaled$insiede_D>0.01] <-1
InDaled$AgeN<-1
InDaled$AgeN[InDaled$Age=="Sub-Ad"] <-2
InDaled$AgeN[InDaled$Age=="Ad"] <-3

model1 <-glm(insiede_D ~ AgeN + Sex, family = quasibinomial,data=InDaled)
summary(model1)
