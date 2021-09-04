#Making estimates of habitat suitability and shelf occupancy for spring habitat models 
#JJS 21-06-30 through 21 08 18


########Data Preparation#############

# read in and grid the EcoMon (zooplankton) data 

setwd("D:/Sand_Lance/SL_Chapter_2/Habitat_Occupancy")
library (dplyr)
#read in and grid the daily eke metric 
VM<-read.csv("Annual_EKE_Properties.csv", header=FALSE)
VM_2<-read.csv("Annual_EKE_Properties_2010_2016.csv", header=FALSE)
VM_Full<-rbind(VM, VM_2)
colnames(VM_Full)<-c("Year","Lon","Lat","EKE")
#Grid in the EKE data 
Gridded_EKE<-VM_Full %>% 
  mutate(binlon = cut(Lon, seq(from = min(-75.5), to = max(-65), by = .1), include.lowest = T, right = F, dig.lab=4),
         binlat = cut(Lat, seq(from = min(36.5), to = max(44.5), by = .1), include.lowest = T, right = F, dig.lab=4)) %>% 
  group_by(Year, binlat= forcats::fct_explicit_na(binlat), binlon= forcats::fct_explicit_na(binlon), .drop=FALSE) %>% 
  summarise(EKE=mean(EKE), .groups="keep")

#read in the TS properties 
setwd("D:/Sand_Lance/SL_Chapter_2/Habitat_Occupancy")
#read in values that were able to be calculated from daily values
TS<-read.csv("Annual_T_S_Properties.csv", header=FALSE)
#read in values that were able to be calculate from hourly values
TS_2<-read.csv("Annual_T_S_Properties_2014_2016.csv", header=FALSE)
TS_Full<-rbind(TS, TS_2)
colnames(TS_Full)<-c("Year","Lon","Lat","SST","SSS","BT","BS")
#Grid the data, do this by 0.05, and then agaoin by 0.25
Gridded_TS<-TS_Full %>% 
  mutate(binlon = cut(Lon, seq(from = min(-75.5), to = max(-65), by = .1), include.lowest = T, right = F, dig.lab=4),
         binlat = cut(Lat, seq(from = min(36.5), to = max(44.5), by = .1), include.lowest = T, right = F, dig.lab=4)) %>% 
  group_by(Year, binlat= forcats::fct_explicit_na(binlat), binlon= forcats::fct_explicit_na(binlon), .drop=FALSE) %>% 
  summarise(SST_Use = mean(SST),SSS_Use = mean(SSS), BS_Use = mean(BS), BT_Use = mean(BT), .groups="keep" )

#clean up missing data 
Gridded_TS<-Gridded_TS[Gridded_TS$binlat!="(Missing)",]
Gridded_TS<-Gridded_TS[Gridded_TS$binlon!="(Missing)",]

Gridded_EKE<-Gridded_EKE[Gridded_EKE$binlat!="(Missing)",]
Gridded_EKE<-Gridded_EKE[Gridded_EKE$binlon!="(Missing)",]

#merge all the dataframes
Grouped_Data<-merge(Gridded_TS,Gridded_EKE, by=c("Year","binlon","binlat"))
library(splitstackshape)
Q<-cSplit(Grouped_Data, "binlon", sep=",")
Grouped_Data_Split<-cSplit(Q, "binlat", sep=",")

#convert the latitude and longitude into numerical values
Grouped_Data_Split$Latitude<-round(as.numeric(substr(Grouped_Data_Split$binlat_1, start=2, stop=6)),2)
Grouped_Data_Split$Longitude<-round(as.numeric(substr(Grouped_Data_Split$binlon_1, start=2, stop=7)),2)

setwd("D:/Sand_Lance/SL_Chapter_2/Trawl_Strata")
library(rgdal)
Strata<-readOGR("strata.shp")
plot(Strata)
K<-which(Strata$STRATA %in% Strata_to_Include_Sal)
Good_Strata<-Strata[K,]

Good_Strata@proj4string<- CRS("+proj=utm +zone=19 +ellps=GRS80 +datum=NAD83") 

coordinates(Grouped_Data_Split)<-c("Longitude","Latitude")

as(Grouped_Data_Split,"SpatialPoints")
proj4string(Grouped_Data_Split)<-CRS("+proj=utm +zone=19 +ellps=GRS80 +datum=NAD83")
Strata_Overlap<-over(Grouped_Data_Split,Good_Strata)
Grouped_Data_Split<-as.data.frame(Grouped_Data_Split)

#edit the below to correspond with the spring dates
Grouped_Data_Split1<-cbind(Grouped_Data_Split, Strata_Overlap)

#remove the bad strata
Grouped_Data_Split<-Grouped_Data_Split1[!is.na(Grouped_Data_Split1$STRATA),]
Grouped_Data_Split<-Grouped_Data_Split[Grouped_Data_Split$Year>=1979,]


#plot maps of properties to ensure they make sense
library(ggplot2)
library(viridis)
library(maps)
library(rnaturalearth)
library(rnaturalearthdata)
library(rgeos)
setwd("D:/Sand_Lance/NOAA_Seminar_25_May_2021")
world<-ne_countries(scale="medium", returnclass = "sf")
png("BT_Gridded.png",height=5, width=6, units="in", res=300)
ggplot(data=world) + geom_raster(data = Grouped_Data_Split, aes(x=Longitude, y = Latitude, fill=BT_Use)) + 
  coord_fixed(ratio = 1) +
  scale_fill_viridis(direction = 1) +
  theme_bw()+ggtitle("Bottom Temp.")+labs(fill =expression(paste(""^"o","C")))+geom_sf()+coord_sf(xlim=c(-75.5,-64), ylim=c(36, 44.5))
dev.off()

#interpolation fill 

library(gstat)
library(sp)
Years=seq(1979,2016,1)
Data_Gaps <- data.frame()


for (i in 1:length(Years)){
#fill bottom temperature
Annual_Data<-Grouped_Data_Split[Grouped_Data_Split$Year==Years[i],]
coordinates(Annual_Data)<-~Longitude+Latitude
valid = !is.na(Annual_Data$BT_Use)
BT_predictions = idw(BT_Use~1, 
                    locations=Annual_Data[valid,,drop=FALSE],
                    newdata=Annual_Data[!valid,,drop=FALSE])
Annual_Data$BT_Use[!valid] = BT_predictions$var1.pred
valid = !is.na(Annual_Data$SST_Use)

#fill SST
SST_predictions = idw(SST_Use~1, 
                     locations=Annual_Data[valid,,drop=FALSE],
                     newdata=Annual_Data[!valid,,drop=FALSE])
Annual_Data$SST_Use[!valid] = SST_predictions$var1.pred

#fill bottom salinity
valid = !is.na(Annual_Data$BS_Use)
BS_predictions = idw(BS_Use~1, 
                     locations=Annual_Data[valid,,drop=FALSE],
                     newdata=Annual_Data[!valid,,drop=FALSE])
Annual_Data$BS_Use[!valid] = BS_predictions$var1.pred

#fill SSS
valid = !is.na(Annual_Data$SSS_Use)
SSS_predictions = idw(SSS_Use~1, 
                     locations=Annual_Data[valid,,drop=FALSE],
                     newdata=Annual_Data[!valid,,drop=FALSE])
Annual_Data$SSS_Use[!valid] = SSS_predictions$var1.pred

#fill EKE
valid = !is.na(Annual_Data$EKE)
EKE_predictions = idw(EKE~1, 
                      locations=Annual_Data[valid,,drop=FALSE],
                      newdata=Annual_Data[!valid,,drop=FALSE])
Annual_Data$EKE[!valid] = EKE_predictions$var1.pred
Annual_Data1<-as.data.frame(Annual_Data)
Data_Gaps<-rbind(Data_Gaps, Annual_Data1)
rm(Annual_Data1)
}

Grouped_Data_Split<-Data_Gaps

#ensure no aggregious issues with the interpolation
ggplot(data=world) + geom_raster(data = Data_Gaps, aes(x=Longitude, y = Latitude, fill=EKE)) + 
  coord_fixed(ratio = 1) +
  scale_fill_viridis(direction = 1) +
  theme_bw()+ggtitle("Daily EKE")+labs(fill=expression(paste("m"^"2","s"^"-2")))+geom_sf()+coord_sf(xlim=c(-75.5,-64), ylim=c(36, 44.5))

ggplot(data=world) + geom_raster(data = Data_Gaps, aes(x=Longitude, y = Latitude, fill=BS_Use)) + 
  coord_fixed(ratio = 1) +
  scale_fill_viridis(direction = 1) +
  theme_bw()+ggtitle("Bottom Salinity")+geom_sf()+coord_sf(xlim=c(-75.5,-64), ylim=c(36, 44.5))

ggplot(data=world) + geom_raster(data = Grouped_Data_Split, aes(x=Longitude, y = Latitude, fill=SSS_Use)) + 
  coord_fixed(ratio = 1) +
  scale_fill_viridis(direction = 1) +
  theme_bw()+ggtitle("SSS")+geom_sf()+coord_sf(xlim=c(-75.5,-64), ylim=c(36, 44.5))

ggplot(data=world) + geom_raster(data = Grouped_Data_Split, aes(x=Longitude, y = Latitude, fill=SST_Use)) + 
  coord_fixed(ratio = 1) +
  scale_fill_viridis(direction = 1) +
  theme_bw()+ggtitle("SST")+geom_sf()+coord_sf(xlim=c(-75.5,-64), ylim=c(36, 44.5))

#assign sediment type (not used), slope, and grain size to the grid
setwd("D:/Sand_Lance/SL_Chapter_2")
library(sf)
library(rgdal)
library(raster)
Sediments<-readOGR("conmapsg.shp")
Bathymetry<-raster("Coastal_Mod_Bathy.tif")

#regrid to 2km in order to calculate 2 km slope 
Regrid_Factor=.02/0.0008333333
Bathy_Coarse<- aggregate(Bathymetry, fact=Regrid_Factor)
Slope_Bath<-terrain(Bathy_Coarse, opt="slope", unit="tangent", neighbors=8)
Sediments@proj4string<- CRS("+proj=utm +zone=19 +ellps=GRS80 +datum=NAD83") 

coordinates(Grouped_Data_Split)<-c("Longitude","Latitude")

as(Grouped_Data_Split,"SpatialPoints")
proj4string(Grouped_Data_Split)<-CRS("+proj=utm +zone=19 +ellps=GRS80 +datum=NAD83")
Sed_Points<-over(Grouped_Data_Split,Sediments)

#now match bathymetry
#get rid of land 
Bathymetry[Bathymetry>=0]<-NA

#take a mean of bathymetry to have spatial mean at the appropriate grid size
Regrid_Bathy<-aggregate(Bathymetry, c(120),fun=mean, expand=TRUE, na.rm=TRUE) 

St_Coords<-Grouped_Data_Split@coords
pts<-SpatialPoints(St_Coords, proj4string = CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0 "))
SDN<-spTransform(pts,crs(Slope_Bath))
Bathy<-spTransform(pts,crs(Regrid_Bathy))
Depth<-extract(Regrid_Bathy,Bathy)

#get depth and slope estimates
Depth1<-extract(Bathymetry,Bathy)
Slope_try<-extract(Slope_Bath,SDN)
Grouped_Data_Split<- as.data.frame(Grouped_Data_Split)




#match all the grain size data
setwd("E:/Hab_Modeling_data/Spring_Survey/ecstdb2005")
Grain_Size<-readOGR("ecstdb2005.shp")
Grain_Size@proj4string<- CRS("+proj=utm +zone=19 +ellps=GRS80 +datum=NAD83") 
Grain_Size_Data<-as.data.frame(Grain_Size)
Grain_Size_Data_Red<-Grain_Size_Data[,c("LONGITUDE","LATITUDE","MEDIAN")]
Grain_Size_Data_Red<-Grain_Size_Data_Red[complete.cases(Grain_Size_Data_Red),]
coordinates(Grain_Size_Data_Red)  <- ~ LONGITUDE + LATITUDE
coordinates(Grouped_Data_Split)<-c("Longitude","Latitude")
Grain_Size_Data_Red@proj4string<- CRS("+proj=utm +zone=19 +ellps=GRS80 +datum=NAD83") 

as(Grouped_Data_Split,"SpatialPoints")
proj4string(Grouped_Data_Split)<-CRS("+proj=utm +zone=19 +ellps=GRS80 +datum=NAD83")

#use IDW for grain size estimates following Morley et al. 2018
library(gstat)
idwmodel = idw(MEDIAN ~1, Grain_Size_Data_Red,Grouped_Data_Split,
               maxdist = Inf, idp = 2)
Estimated_Grain_Size<-idwmodel@data$var1.pred

Grouped_Data_Split<- as.data.frame(Grouped_Data_Split)


#merge all of the extracted data
Grouped_Data_Split2<-cbind(Grouped_Data_Split, Sed_Points, Slope_try, Estimated_Grain_Size, Depth)
Grouped_Data_Split2$Unique_Location<-paste(Grouped_Data_Split2$Latitude, Grouped_Data_Split2$Longitude)

Grouped_Data_Split_Full<-Grouped_Data_Split2

Grouped_Data_Split_Full$Year1<-Grouped_Data_Split_Full$Year
#get rid of 1978 data (not enough FVCOM data for this) and the strata shift
Grouped_Data_Split_Full<-Grouped_Data_Split_Full[Grouped_Data_Split_Full$Year1>1978,]


########sand lance###############
Grouped_Data_Split_SL<-Grouped_Data_Split_Full
Grouped_Data_Split_SL$Year1<-Grouped_Data_Split_SL$Year

#fill in fake years by closest year available in abundance
Grouped_Data_Split_SL$Year[Grouped_Data_Split_SL$Year==2008]<-"1999"
Grouped_Data_Split_SL$Year[Grouped_Data_Split_SL$Year==2007]<-"2005"
Grouped_Data_Split_SL$Year[Grouped_Data_Split_SL$Year==2006]<-"1988"

#get rid of the bigelow data for sand lance 
Grouped_Data_Split_SL<-Grouped_Data_Split_SL[Grouped_Data_Split_SL$Year<2009,]



#predict "inferred habitat suitability" along with CI
SL_Occupancy<-as.data.frame(predict(SL_Multi_Avg_Spring,Grouped_Data_Split_SL,type="link",se.fit=T))
SL_Occupancy$PA<-exp(SL_Occupancy$fit)/(1+exp(SL_Occupancy$fit))
SL_Occupancy$PA_LWR<-exp(SL_Occupancy$fit-(1.96*SL_Occupancy$se.fit))/(1+exp(SL_Occupancy$fit-(1.96*SL_Occupancy$se.fit)))
SL_Occupancy$PA_UPR<-exp(SL_Occupancy$fit+(1.96*SL_Occupancy$se.fit))/(1+exp(SL_Occupancy$fit+(1.96*SL_Occupancy$se.fit)))

#predict shelf occupancy along with CI
SL_Partial_Effects<-as.data.frame(predict(SL_Multi_Avg_Spring,Grouped_Data_Split_SL,exclude="s(Year)",type="link", se.fit=T))
SL_Partial_Effects$PA_Fixed<-exp(SL_Partial_Effects$fit)/(1+exp(SL_Partial_Effects$fit))
SL_Partial_Effects$PA_Fixed_LWR<-exp(SL_Partial_Effects$fit-(1.96*SL_Partial_Effects$se.fit))/(1+exp(SL_Partial_Effects$fit-(1.96*SL_Partial_Effects$se.fit)))
SL_Partial_Effects$PA_Fixed_UPR<-exp(SL_Partial_Effects$fit+(1.96*SL_Partial_Effects$se.fit))/(1+exp(SL_Partial_Effects$fit+(1.96*SL_Partial_Effects$se.fit)))


Grouped_Data_Full_SL<-cbind(Grouped_Data_Split_SL, SL_Occupancy, SL_Partial_Effects)

Grouped_Data_Full_SL<-Grouped_Data_Full_SL[,-which(names(Grouped_Data_Full_SL) %in% c("fit","se.fit"))]
library(plyr)
#set occurrence to mean prevalance
Annual_SL_Prev=ddply(SL_Matched, .(Year), summarize, Prev=sum(PA)/length(PA))
SL_Spring_Prevalence=mean(Annual_SL_Prev$Prev)

AnnualSL_Occupancy<-ddply(Grouped_Data_Full_SL, .(Year1), summarize, Mean_hab_Prop=length(which(PA>SL_Spring_Prevalence ))/length(which(!is.na(PA))), Low_hap_prop=length(which(PA_LWR>SL_Spring_Prevalence ))/length(which(!is.na(PA))),High_hap_prop=length(which(PA_UPR>SL_Spring_Prevalence ))/length(which(!is.na(PA))) )

#habitat suitability
AnnualSL_Partial_Effects<-ddply(Grouped_Data_Full_SL, .(Year1), summarize, Mean_hab_Prop=(length(which(PA_Fixed>SL_Spring_Prevalence )))/(length(which(!is.na(PA_Fixed)))), Low_hap_prop=(length(which(PA_Fixed_LWR>SL_Spring_Prevalence )))/(length(which(!is.na(PA_Fixed)))),High_hap_prop=(length(which(PA_Fixed_UPR>SL_Spring_Prevalence )))/(length(which(!is.na(PA_Fixed)))))


#perform betaregressions on occupancy with time
library(betareg)
AnnualSL_Occupancy$Mean_hab_Prop<-AnnualSL_Occupancy$Mean_hab_Prop+0.00000001
SL_Spring_Full_Hab_Occ<-betareg(AnnualSL_Occupancy$Mean_hab_Prop~AnnualSL_Occupancy$Year1)
summary(SL_Spring_Full_Hab_Occ)

SL_Spring_Fixed_Hab_Occ<-betareg(AnnualSL_Partial_Effects$Mean_hab_Prop~AnnualSL_Partial_Effects$Year1)
summary(SL_Spring_Fixed_Hab_Occ)

setwd("D:/Thesis_Compiled/Chapter_2_Figs")
tiff("Sand_Lance_Occupiable_Habitat_210714.tif", height=5, width=6, res=300, units="in")
par(mar=c(4,6,4,3))
plot(AnnualSL_Occupancy$Year1, AnnualSL_Occupancy$Mean_hab_Prop, type="l", lwd=4, xlab="Year", ylab="Proportion of \nOccupiable Shelf", main="Sand lance",cex.lab=1.2, ylim=c(0,0.6), col="black")
polygon(x=c(AnnualSL_Occupancy$Year1,rev(AnnualSL_Occupancy$Year1)),y=c(AnnualSL_Occupancy$Low_hap_prop,rev(AnnualSL_Occupancy$High_hap_prop)), border=NA, col= adjustcolor("gray", alpha.f=0.5))
par(new=T)
plot(AnnualSL_Occupancy$Year1, AnnualSL_Occupancy$Mean_hab_Prop, type="l", lwd=4, xlab="Year", ylab="Proportion of \nOccupiable Shelf", main="Sand lance",cex.lab=1.2, ylim=c(0,0.6), col="black")
dev.off()


setwd("D:/Thesis_Compiled/Chapter_2_Figs")
tiff("Sand_Lance_Occ_Suitable_Habitat_210714.tif", height=5, width=6, res=300, units="in")
par(mar=c(4,6,4,3))
plot(AnnualSL_Occupancy$Year1, AnnualSL_Occupancy$Mean_hab_Prop, type="l", lwd=4, xlab="Year", ylab="Proportion of \nOccupiable Shelf", main="Sand lance",cex.lab=1.2, ylim=c(0,0.6), col="black")
polygon(x=c(AnnualSL_Occupancy$Year1,rev(AnnualSL_Occupancy$Year1)),y=c(AnnualSL_Occupancy$Low_hap_prop,rev(AnnualSL_Occupancy$High_hap_prop)), border=NA, col= adjustcolor("gray", alpha.f=0.5))
par(new=T)
plot(AnnualSL_Occupancy$Year1, AnnualSL_Occupancy$Mean_hab_Prop, type="l", lwd=4, xlab="Year", ylab="Proportion of \nOccupiable Shelf", main="Sand lance",cex.lab=1.2, ylim=c(0,0.6), col="black")
polygon(x=c(AnnualSL_Partial_Effects$Year1,rev(AnnualSL_Partial_Effects$Year1)),y=c(AnnualSL_Partial_Effects$Low_hap_prop,rev(AnnualSL_Partial_Effects$High_hap_prop)), border=NA, col= adjustcolor("indianred", alpha.f=0.2))
par(new=T)
plot(AnnualSL_Partial_Effects$Year1, AnnualSL_Partial_Effects$Mean_hab_Prop, type="l", lwd=4, xlab="Year", ylab="Proportion of \nOccupiable Shelf", main="Sand lance",cex.lab=1.2, ylim=c(0,0.6), col="red")
dev.off()

#make averages over space for all years to make a habitat suitability map
Annual_Averages_SL_Spring<-ddply(Grouped_Data_Full_SL, .(Longitude, Latitude), summarize, Mean_PA=mean(PA))
library(maps)
library(rnaturalearth)
library(rnaturalearthdata)
library(rgeos)
library(viridis)
world<-ne_countries(scale="medium", returnclass = "sf")
setwd("D:/Sand_Lance/SL_Chapter_2/Ch2_Manuscript/Revisions_Round_One/Updated_Temporary_Figure/Spring")
png("Sand_lance_Model_Pred_210712.png", width=6, height=5, units="in", res=300)
ggplot(data=world) + geom_raster(data = Annual_Averages_SL_Spring, aes(x=Longitude, y = Latitude, fill=Mean_PA),linejoin = "mitre") + 
  coord_fixed(ratio = 1) +
  scale_fill_viridis(direction = 1) +
  theme_bw()+ggtitle("Sand lance")+labs(fill = "Prob. Occurrence")+geom_sf()+coord_sf(xlim=c(-75.5,-64), ylim=c(36, 44.5))
dev.off()

#Calcualte weighted lat and lon

Grouped_Data_Full_SL$Weighted_Lat_Mean<-Grouped_Data_Full_SL$Latitude*Grouped_Data_Full_SL$PA
Grouped_Data_Full_SL$Weighted_Lat_LWR<-Grouped_Data_Full_SL$Latitude*Grouped_Data_Full_SL$PA_LWR
Grouped_Data_Full_SL$Weighted_Lat_UPR<-Grouped_Data_Full_SL$Latitude*Grouped_Data_Full_SL$PA_UPR

Grouped_Data_Full_SL$Weighted_Lat_Mean_Fixed<-Grouped_Data_Full_SL$Latitude*Grouped_Data_Full_SL$PA_Fixed
Grouped_Data_Full_SL$Weighted_Lat_LWR_Fixed<-Grouped_Data_Full_SL$Latitude*Grouped_Data_Full_SL$PA_Fixed_LWR
Grouped_Data_Full_SL$Weighted_Lat_UPR_Fixed<-Grouped_Data_Full_SL$Latitude*Grouped_Data_Full_SL$PA_Fixed_UPR


#divide these by sum of abundance for each year and it will work
library(plyr)
Annual_Centroid_Locations_SL<-ddply(Grouped_Data_Full_SL, .(Year1), summarize, Mean_Weighted_Lat=sum(Weighted_Lat_Mean, na.rm=TRUE)/(sum(PA, na.rm=TRUE)), Mean_Weighted_Lat_LWR=sum(Weighted_Lat_LWR, na.rm=TRUE)/(sum(PA_LWR, na.rm=TRUE)),Mean_Weighted_Lat_UPR=sum(Weighted_Lat_UPR, na.rm=TRUE)/(sum(PA_UPR, na.rm=TRUE)) )
Annual_Centroid_Locations_SL_Fixed<-ddply(Grouped_Data_Full_SL, .(Year1), summarize, Mean_Weighted_Lat=sum(Weighted_Lat_Mean_Fixed, na.rm=TRUE)/(sum(PA_Fixed, na.rm=TRUE)), Mean_Weighted_Lat_LWR=sum(Weighted_Lat_LWR_Fixed, na.rm=TRUE)/(sum(PA_Fixed_LWR, na.rm=TRUE)),Mean_Weighted_Lat_UPR=sum(Weighted_Lat_UPR_Fixed, na.rm=TRUE)/(sum(PA_Fixed_UPR, na.rm=TRUE)))

#Test relationship between lat-lon and year 
Lat_SL_Spring<-lm(Mean_Weighted_Lat~Year1, data=Annual_Centroid_Locations_SL)
summary(Lat_SL_Spring)

Lat_SL_Spring_Fixed<-lm(Mean_Weighted_Lat~Year1, data=Annual_Centroid_Locations_SL_Fixed)
summary(Lat_SL_Spring_Fixed)

setwd("D:/Thesis_Compiled/Chapter_2_Figs")
tiff("Sand_Lance_Occupancy_PA_Comparison_210714.tif", height=5, width=6, res=300, units="in")
par(mar=c(4,6,4,3))
plot(Annual_Centroid_Locations_SL$Year1, Annual_Centroid_Locations_SL$Mean_Weighted_Lat, type="l", lwd=4, xlab="Year", ylab="Mean Weighted \nLatitude", main="Sand lance",cex.lab=1.2, ylim=c(40.3,40.7), col="black")
polygon(x=c(Annual_Centroid_Locations_SL$Year1,rev(Annual_Centroid_Locations_SL$Year1)), y=c(Annual_Centroid_Locations_SL$Mean_Weighted_Lat_UPR,rev(Annual_Centroid_Locations_SL$Mean_Weighted_Lat_LWR)), border=NA, col= adjustcolor("gray", alpha.f=0.5))
par(new=T)
plot(Annual_Centroid_Locations_SL$Year1, Annual_Centroid_Locations_SL$Mean_Weighted_Lat, type="l", lwd=4, xlab="Year", ylab="Mean Weighted \nLatitude", main="Sand lance",cex.lab=1.2, ylim=c(40.3,40.7), col="black")
dev.off()


tiff("Sand_Lance_Mean_Location_PA_Comparison_210714.tif", height=5, width=6, res=300, units="in")
par(mar=c(4,6,4,3))
plot(Annual_Centroid_Locations_SL$Year1, Annual_Centroid_Locations_SL$Mean_Weighted_Lat, type="l", lwd=4, xlab="Year", ylab="Mean Weighted \nLatitude", main="Sand lance",cex.lab=1.2, ylim=c(40.3,40.7), col="black")
polygon(x=c(Annual_Centroid_Locations_SL$Year1,rev(Annual_Centroid_Locations_SL$Year1)), y=c(Annual_Centroid_Locations_SL$Mean_Weighted_Lat_LWR,rev(Annual_Centroid_Locations_SL$Mean_Weighted_Lat_UPR)), border=NA, col= adjustcolor("gray", alpha.f=0.5))
par(new=T)
plot(Annual_Centroid_Locations_SL$Year1, Annual_Centroid_Locations_SL$Mean_Weighted_Lat, type="l", lwd=4, xlab="Year", ylab="Mean Weighted \nLatitude", main="Sand lance",cex.lab=1.2, ylim=c(40.3,40.7), col="black")
polygon(x=c(Annual_Centroid_Locations_SL_Fixed$Year1,rev(Annual_Centroid_Locations_SL_Fixed$Year1)),y=c(Annual_Centroid_Locations_SL_Fixed$Mean_Weighted_Lat_LWR,rev(Annual_Centroid_Locations_SL_Fixed$Mean_Weighted_Lat_UPR)), border=NA, col= adjustcolor("indianred", alpha.f=0.2))
par(new=T)
plot(Annual_Centroid_Locations_SL_Fixed$Year1, Annual_Centroid_Locations_SL_Fixed$Mean_Weighted_Lat, type="l", lwd=4,lty=1, xlab="Year", ylab="Mean Weighted \nLatitude", main="Sand lance",cex.lab=1.2, ylim=c(40.3,40.7), col="red")
dev.off()

#now mean weighted depth
Grouped_Data_Full_SL$Weighted_Depth_Fixed<-Grouped_Data_Full_SL$Depth*Grouped_Data_Full_SL$PA_Fixed
Grouped_Data_Full_SL$Weighted_Depth_Fixed_LWR<-Grouped_Data_Full_SL$Depth*Grouped_Data_Full_SL$PA_Fixed_LWR
Grouped_Data_Full_SL$Weighted_Depth_Fixed_UPR<-Grouped_Data_Full_SL$Depth*Grouped_Data_Full_SL$PA_Fixed_UPR

Grouped_Data_Full_SL$Weighted_Depth<-Grouped_Data_Full_SL$Depth*Grouped_Data_Full_SL$PA
Grouped_Data_Full_SL$Weighted_Depth_LWR<-Grouped_Data_Full_SL$Depth*Grouped_Data_Full_SL$PA_LWR
Grouped_Data_Full_SL$Weighted_Depth_UPR<-Grouped_Data_Full_SL$Depth*Grouped_Data_Full_SL$PA_UPR


library(plyr)
Annual_Centroid_Depth_SL<-ddply(Grouped_Data_Full_SL, .(Year1), summarize, Mean_Weighted_Depth=sum(Weighted_Depth, na.rm=TRUE)/(sum(PA, na.rm=TRUE)),  Mean_Weighted_Depth_LWR=sum(Weighted_Depth_LWR, na.rm=TRUE)/(sum(PA_LWR, na.rm=TRUE)),  Mean_Weighted_Depth_UPR=sum(Weighted_Depth_UPR, na.rm=TRUE)/(sum(PA_UPR, na.rm=TRUE)))
Annual_Centroid_Depth_SL_Fixed<-ddply(Grouped_Data_Full_SL, .(Year1), summarize, Mean_Weighted_Depth=sum(Weighted_Depth_Fixed, na.rm=TRUE)/(sum(PA_Fixed, na.rm=TRUE)),  Mean_Weighted_Depth_LWR=sum(Weighted_Depth_Fixed_LWR, na.rm=TRUE)/(sum(PA_Fixed_LWR, na.rm=TRUE)),  Mean_Weighted_Depth_UPR=sum(Weighted_Depth_Fixed_UPR, na.rm=TRUE)/(sum(PA_Fixed_UPR, na.rm=TRUE)))

#test for correlation between depth and year for suitability and occupancy 
SL_Depth_Occupancy_Spring<-lm(Mean_Weighted_Depth~Year1, data=Annual_Centroid_Depth_SL)
summary(SL_Depth_Occupancy_Spring)

SL_Depth_Suitability_Spring<-lm(Mean_Weighted_Depth~Year1, data=Annual_Centroid_Depth_SL_Fixed)
summary(SL_Depth_Suitability_Spring)

tiff("Sand_Lance_Mean_Depth_PA_Occ_210714.tif", height=5, width=6, res=300, units="in")
par(mar=c(4,6,4,3))
plot(Annual_Centroid_Depth_SL$Year1, (Annual_Centroid_Depth_SL$Mean_Weighted_Depth)*-1, type="l", lwd=4, xlab="Year", ylab="Mean Weighted \nDepth (m)", main="Sand lance",cex.lab=1.2, ylim=rev(c(45,65)), col="black")
polygon(x=c(Annual_Centroid_Depth_SL$Year1,rev(Annual_Centroid_Depth_SL$Year1)), y=c((Annual_Centroid_Depth_SL$Mean_Weighted_Depth_LWR)*-1,rev((Annual_Centroid_Depth_SL$Mean_Weighted_Depth_UPR)*-1)), border=NA, col= adjustcolor("gray", alpha.f=0.5))
par(new=T)
plot(Annual_Centroid_Depth_SL$Year1, (Annual_Centroid_Depth_SL$Mean_Weighted_Depth)*-1, type="l", lwd=4, xlab="Year", ylab="Mean Weighted \nDepth (m)", main="Sand lance",cex.lab=1.2, ylim=rev(c(45,65)), col="black")
dev.off()


tiff("Sand_Lance_Mean_Depth_PA_Comparison_210714_Regridded.tif", height=5, width=6, res=300, units="in")
par(mar=c(4,6,4,3))
plot(Annual_Centroid_Depth_SL$Year1, (Annual_Centroid_Depth_SL$Mean_Weighted_Depth)*-1, type="l", lwd=4, xlab="Year", ylab="Mean Weighted \nDepth (m)", main="Sand lance",cex.lab=1.2, ylim=rev(c(45,65)), col="black")
polygon(x=c(Annual_Centroid_Depth_SL$Year1,rev(Annual_Centroid_Depth_SL$Year1)), y=c((Annual_Centroid_Depth_SL$Mean_Weighted_Depth_LWR)*-1,rev((Annual_Centroid_Depth_SL$Mean_Weighted_Depth_UPR)*-1)), border=NA, col= adjustcolor("gray", alpha.f=0.5))
par(new=T)
plot(Annual_Centroid_Depth_SL$Year1, (Annual_Centroid_Depth_SL$Mean_Weighted_Depth)*-1, type="l", lwd=4, xlab="Year", ylab="Mean Weighted \nDepth (m)", main="Sand lance",cex.lab=1.2, ylim=rev(c(45,65)), col="black")
polygon(x=c(Annual_Centroid_Depth_SL_Fixed$Year1,rev(Annual_Centroid_Depth_SL_Fixed$Year1)), y=c((Annual_Centroid_Depth_SL_Fixed$Mean_Weighted_Depth_LWR)*-1,rev((Annual_Centroid_Depth_SL_Fixed$Mean_Weighted_Depth_UPR)*-1)), border=NA, col= adjustcolor("indianred", alpha.f=0.2))
par(new=T)
plot(Annual_Centroid_Depth_SL_Fixed$Year1, (Annual_Centroid_Depth_SL_Fixed$Mean_Weighted_Depth)*-1, type="l", lwd=4,lty=1, xlab="Year", ylab="Mean Weighted \nDepth (m)", main="Sand lance",cex.lab=1.2, ylim=rev(c(45,65)), col="red")
dev.off()




#plotting response curves for each "important" variable
View(SL_PA_Dredge)
library(mgcv)
library(ggplot2)
library(dplyr)

Best_SL_PA_GAM<-gam(PA~
                       s(Year, bs="re")+te(Longitude, Latitude)+s(EKE, k=6)+s(Estimated_Grain_Size,k=6)
                    +s(BS_Use, k=6)
                    ,family=binomial,
                       data=SL_PA_Fitting, na.action = "na.fail")
setwd("D:/Sand_Lance/SL_Chapter_2/Ch2_Manuscript/Revisions_Round_One/Updated_Temporary_Figure/Spring")
tiff("Sand_Lance_BS_210630.tif", height=5, width=5, units="in", res=300)
plot(Best_SL_PA_GAM,trans=function(x)exp(x)/(1+exp(x)), shade=T,ylab="Probability of Occurrence",xlab="Bottom Salinity", cex.lab=1.5, cex.axis=1.5 )
dev.off()

tiff("Sand_Lance_BS_Supp_210630.tif", height=5, width=5, units="in", res=300)
plot(Best_SL_PA_GAM, shade=T,xlab="Bottom Salinity", cex.lab=1.5, cex.axis=1.5 )
dev.off()

Best_SL_PA_GAM<-gam(PA~
                      s(Year, bs="re")+te(Longitude, Latitude)+s(Estimated_Grain_Size,k=6)
                    +s(BS_Use, k=6)+s(EKE, k=6)
                    ,family=binomial,
                    data=SL_PA_Fitting, na.action = "na.fail")
tiff("Sand_Lance_EKE_210630.tif", height=5, width=5, units="in", res=300)
plot(Best_SL_PA_GAM,trans=function(x)exp(x)/(1+exp(x)), shade=T,ylab="Probability of Occurrence",xlab=expression(paste("Mean kinetic energy (cm"^"-2","s"^"-2",")")), cex.axis=1.5, cex.lab=1.5 )
dev.off()

tiff("Sand_Lance_EKE_Supp_210630.tif", height=5, width=5, units="in", res=300)
plot(Best_SL_PA_GAM, shade=T,xlab=expression(paste("Mean kinetic energy (cm"^"-2","s"^"-2",")")), cex.axis=1.5, cex.lab=1.5 )
dev.off()
library(mgcv)
Best_SL_PA_GAM<-gam(PA~
                      s(Year, bs="re")+te(Longitude, Latitude)
                    +s(BS_Use, k=6)+s(EKE, k=6)+s(Estimated_Grain_Size,k=6)
                    ,family=binomial,
                    data=SL_PA_Fitting, na.action = "na.fail")
tiff("Sand_Lance_Grain_210630.tif", height=5, width=5, units="in", res=300)
plot(Best_SL_PA_GAM,trans=function(x)exp(x)/(1+exp(x)), shade=T,ylab="Probability of Occurrence",xlab=expression(paste("Grain Size (", phi, ")")), cex.lab=1.5, cex.axis=1.5 )
dev.off()
tiff("Sand_Lance_Grain_Supp_210630.tif", height=5, width=5, units="in", res=300)
plot(Best_SL_PA_GAM, shade=T,xlab=expression(paste("Grain Size (", phi, ")")), cex.lab=1.5, cex.axis=1.5 )
dev.off()

Best_SL_PA_GAM_RE<-gam(PA~
                      te(Longitude, Latitude)
                    +s(BS_Use, k=6)+s(EKE, k=6)+s(Estimated_Grain_Size,k=6)+s(Year, bs="re")
                    ,family=binomial,
                    data=SL_PA_Fitting, na.action = "na.fail")
setwd("D:/Sand_Lance/SL_Chapter_2/Ch2_Manuscript/Revisions_Round_One/Updated_Temporary_Figure/Spring/RE")
tiff("Sand_Lance_Spring_RE.tif", height=5, width=5, units="in", res=300)
plot(Best_SL_PA_GAM_RE, cex.lab=1.5, cex.axis=1.5, main="Sand Lance" )
dev.off()


#make a gridded map of trends in habitat suitability

Gridded_Final_Values_SL_Spring<-ddply(Grouped_Data_Full_SL[Grouped_Data_Full_SL$Year1>=2004,], .(Latitude, Longitude), summarize, Final_Probability=mean(PA_Fixed))
Gridded_Initial_Values_SL_Spring<-ddply(Grouped_Data_Full_SL[Grouped_Data_Full_SL$Year1<=1983,], .(Latitude, Longitude), summarize, Initial_Probability=mean(PA_Fixed))

SL_Spring_Space<-cbind(Gridded_Final_Values_SL_Spring, Gridded_Initial_Values_SL_Spring)
SL_Spring_Space<-SL_Spring_Space[,!duplicated(colnames(SL_Spring_Space), fromLast = TRUE)]

SL_Spring_Space$Occurrence_Diff<-SL_Spring_Space$Final_Probability-SL_Spring_Space$Initial_Probability


tiff("Spring_Sand_Lance_Spatial_Difference_Five_Yrs.tif", height=5, width=6, res=300, units="in")
ggplot(data=world) + geom_raster(data = SL_Spring_Space, aes(x=Longitude, y = Latitude, fill=Occurrence_Diff),linejoin = "mitre") + 
  coord_fixed(ratio = 1) +
  scale_fill_gradientn(colours=c("blue","#F1ECE4","red"), limits=c(-0.4,0.4))+theme_bw()+ggtitle("Sand lance")+labs(fill = "Diff. in Occurr.")+geom_sf()+coord_sf(xlim=c(-75.5,-64), ylim=c(36, 44.5))+
  theme(plot.title = element_text(size=18,face="bold"), legend.title = element_text(size=16,face="bold"), legend.text = element_text(size=12, face="bold"))
dev.off()


#####butterfish#########

Grouped_Data_Split_PT<-Grouped_Data_Split_Full
Grouped_Data_Split_PT$Year<-as.factor(as.character(Grouped_Data_Split_PT$Year))
Grouped_Data_Split_PT$Longitude<-as.numeric(as.character(Grouped_Data_Split_PT$Longitude))
Grouped_Data_Split_PT$Latitude<-as.numeric(as.character(Grouped_Data_Split_PT$Latitude))

#set year blocks based on gear changes (pre and post Bigelow)
for (i in 1: nrow(Grouped_Data_Split_PT)){
  Grouped_Data_Split_PT$Gear[i]<-ifelse(Grouped_Data_Split_PT$Year1[i]<2009,  "B", "C")
}

#choose replacement years for butterfish based on abundance
Grouped_Data_Split_PT$Year[Grouped_Data_Split_PT$Year==2016]<-"2012"
Grouped_Data_Split_PT$Year[Grouped_Data_Split_PT$Year==2015]<-"2012"
Grouped_Data_Split_PT$Year[Grouped_Data_Split_PT$Year==2014]<-"2011"



#make predictions of suitability and occupancy
Grouped_Data_Split_PT2<-Grouped_Data_Split_PT
Grouped_Data_Split_PT2$Gear<-"B"
PT_Occupancy<-as.data.frame(predict(Trimmed_PT_PA_GAM,Grouped_Data_Split_PT,type="link",se.fit=T))
PT_Occupancy$PA<-exp(PT_Occupancy$fit)/(1+exp(PT_Occupancy$fit))
PT_Occupancy$PA_LWR<-exp(PT_Occupancy$fit-(1.96*PT_Occupancy$se.fit))/(1+exp(PT_Occupancy$fit-(1.96*PT_Occupancy$se.fit)))
PT_Occupancy$PA_UPR<-exp(PT_Occupancy$fit+(1.96*PT_Occupancy$se.fit))/(1+exp(PT_Occupancy$fit+(1.96*PT_Occupancy$se.fit)))

#predict shelf occupancy 
PT_Partial_Effects<-as.data.frame(predict(Trimmed_PT_PA_GAM,Grouped_Data_Split_PT2,exclude=c("Gear","s(Year)"),type="link", se.fit=T))
PT_Partial_Effects$PA_Fixed<-exp(PT_Partial_Effects$fit)/(1+exp(PT_Partial_Effects$fit))
PT_Partial_Effects$PA_Fixed_LWR<-exp(PT_Partial_Effects$fit-(1.96*PT_Partial_Effects$se.fit))/(1+exp(PT_Partial_Effects$fit-(1.96*PT_Partial_Effects$se.fit)))
PT_Partial_Effects$PA_Fixed_UPR<-exp(PT_Partial_Effects$fit+(1.96*PT_Partial_Effects$se.fit))/(1+exp(PT_Partial_Effects$fit+(1.96*PT_Partial_Effects$se.fit)))

Grouped_Data_Split_PT3<-Grouped_Data_Split_PT
Grouped_Data_Split_PT3$Gear<-"B"
PT_No_Vess_Effects<-as.data.frame(predict(Trimmed_PT_PA_GAM,Grouped_Data_Split_PT3,exclude=c("Gear"),type="link", se.fit=T))
PT_No_Vess_Effects$PA_Vessel<-exp(PT_No_Vess_Effects$fit)/(1+exp(PT_No_Vess_Effects$fit))
PT_No_Vess_Effects$PA_Vessel_LWR<-exp(PT_No_Vess_Effects$fit-(1.96*PT_No_Vess_Effects$se.fit))/(1+exp(PT_No_Vess_Effects$fit-(1.96*PT_No_Vess_Effects$se.fit)))
PT_No_Vess_Effects$PA_Vessel_UPR<-exp(PT_No_Vess_Effects$fit+(1.96*PT_No_Vess_Effects$se.fit))/(1+exp(PT_No_Vess_Effects$fit+(1.96*PT_No_Vess_Effects$se.fit)))

Grouped_Data_Full_PT<-cbind(Grouped_Data_Split_PT, PT_Occupancy, PT_Partial_Effects, PT_No_Vess_Effects)


Grouped_Data_Full_PT<-Grouped_Data_Full_PT[,-which(names(Grouped_Data_Full_PT) %in% c("fit","se.fit"))]

library(plyr)
#set occurrence to mean prevalance
Annual_PT_Prev=ddply(PT_Matched, .(Year), summarize, Prev=sum(PA)/length(PA))
PT_Spring_Prevalence=mean(Annual_PT_Prev$Prev)

#shelf occupancy
AnnualPT_Occupancy<-ddply(Grouped_Data_Full_PT, .(Year1), summarize, Mean_hab_Prop=length(which(PA>PT_Spring_Prevalence ))/length(which(!is.na(PA))), Low_hap_prop=length(which(PA_LWR>PT_Spring_Prevalence ))/length(which(!is.na(PA))),High_hap_prop=length(which(PA_UPR>PT_Spring_Prevalence ))/length(which(!is.na(PA))) )

#habitat suitability
AnnualPT_Partial_Effects<-ddply(Grouped_Data_Full_PT, .(Year1), summarize, Mean_hab_Prop=(length(which(PA_Fixed>PT_Spring_Prevalence )))/(length(which(!is.na(PA_Fixed)))), Low_hap_prop=(length(which(PA_Fixed_LWR>PT_Spring_Prevalence )))/(length(which(!is.na(PA_Fixed)))),High_hap_prop=(length(which(PA_Fixed_UPR>PT_Spring_Prevalence )))/(length(which(!is.na(PA_Fixed)))))

AnnualPT_No_Vess_Effects<-ddply(Grouped_Data_Full_PT, .(Year1), summarize, Mean_hab_Prop=(length(which(PA_Vessel>PT_Spring_Prevalence )))/(length(which(!is.na(PA_Vessel)))), Low_hap_prop=(length(which(PA_Vessel_LWR>PT_Spring_Prevalence )))/(length(which(!is.na(PA_Vessel)))),High_hap_prop=(length(which(PA_Vessel_UPR>PT_Spring_Prevalence )))/(length(which(!is.na(PA_Vessel)))))

#make annual averages for plotting suitability
Annual_Averages_PT<-ddply(Grouped_Data_Full_PT, .(Longitude, Latitude), summarize, Mean_PA=mean(PA))


world<-ne_countries(scale="medium", returnclass = "sf")
setwd("D:/Sand_Lance/SL_Chapter_2/Ch2_Manuscript/Revisions_Round_One/Updated_Temporary_Figure/Spring")
tiff("Butterfish_Model_Pred_210712.tif", width=6, height=5, units="in", res=300)
ggplot(data=world) + geom_raster(data = Annual_Averages_PT, aes(x=Longitude, y = Latitude, fill=Mean_PA),linejoin = "mitre") + 
  coord_fixed(ratio = 1) +
  scale_fill_viridis(direction = 1) +
  theme_bw()+ggtitle("Butterfish")+labs(fill = "Prob. Occurrence")+geom_sf()+coord_sf(xlim=c(-75.5,-64), ylim=c(36, 44.5))
dev.off()


setwd("D:/Thesis_Compiled/Chapter_2_Figs")
tiff("Butterfish_Occupiable_Habitat_210714.tif", height=5, width=6, res=300, units="in")
par(mar=c(4,6,4,3))
plot(AnnualPT_Occupancy$Year1, AnnualPT_Occupancy$Mean_hab_Prop, type="l", lwd=4, xlab="Year", ylab="Proportion of \nOccupiable Shelf", main="Butterfish",cex.lab=1.2, ylim=c(0,1.0), col="black")
polygon(x=c(AnnualPT_Occupancy$Year1,rev(AnnualPT_Occupancy$Year1)),y=c(AnnualPT_Occupancy$Low_hap_prop,rev(AnnualPT_Occupancy$High_hap_prop)), border=NA, col= adjustcolor("gray", alpha.f=0.5))
par(new=T)
plot(AnnualPT_Occupancy$Year1, AnnualPT_Occupancy$Mean_hab_Prop, type="l", lwd=4, xlab="Year", ylab="Proportion of \nOccupiable Shelf", main="Butterfish",cex.lab=1.2, ylim=c(0,1.0), col="black")
dev.off()

tiff("Butterfish_Occupiable_Habitat_210831_NewThreshold_Comparison.tif", height=5, width=6, res=300, units="in")
par(mar=c(4,6,4,3))
plot(AnnualPT_Occupancy$Year1, AnnualPT_Occupancy$Mean_hab_Prop, type="l", lwd=4, xlab="Year", ylab="Proportion of \nOccupiable Shelf", main="Butterfish",cex.lab=1.2, ylim=c(0,1.0), col="black")
polygon(x=c(AnnualPT_Occupancy$Year1,rev(AnnualPT_Occupancy$Year1)),y=c(AnnualPT_Occupancy$Low_hap_prop,rev(AnnualPT_Occupancy$High_hap_prop)), border=NA, col= adjustcolor("gray", alpha.f=0.5))
par(new=T)
plot(AnnualPT_Occupancy$Year1, AnnualPT_Occupancy$Mean_hab_Prop, type="l", lwd=4, xlab="Year", ylab="Proportion of \nOccupiable Shelf", main="Butterfish",cex.lab=1.2, ylim=c(0,1.0), col="black")
polygon(x=c(AnnualPT_Partial_Effects$Year1,rev(AnnualPT_Partial_Effects$Year1)),y=c(AnnualPT_Partial_Effects$Low_hap_prop,rev(AnnualPT_Partial_Effects$High_hap_prop)), border=NA, col= adjustcolor("indianred", alpha.f=0.25))
par(new=T)
plot(AnnualPT_Partial_Effects$Year1, AnnualPT_Partial_Effects$Mean_hab_Prop, type="l", lwd=4,lty=3, xlab="Year", ylab="Proportion of \nOccupiable Shelf", main="Butterfish",cex.lab=1.2, ylim=c(0,1.0), col="red")
dev.off()

#calculate trends in suitability and occupancy using betaregression 
library(betareg)
AnnualPT_Occupancy$Mean_hab_Prop<-AnnualPT_Occupancy$Mean_hab_Prop+0.00000001
PT_Spring_Full_Hab_Occ<-betareg(AnnualPT_Occupancy$Mean_hab_Prop~AnnualPT_Occupancy$Year1)
summary(PT_Spring_Full_Hab_Occ)

PT_Spring_Fixed_Hab_Occ<-betareg(AnnualPT_Partial_Effects$Mean_hab_Prop~AnnualPT_Partial_Effects$Year1)
summary(PT_Spring_Fixed_Hab_Occ)




#look at trends with latitude and depth
#Calculate weighted lat and lon
Grouped_Data_Full_PT$Weighted_Lat_Mean<-Grouped_Data_Full_PT$Latitude*Grouped_Data_Full_PT$PA
Grouped_Data_Full_PT$Weighted_Lat_LWR<-Grouped_Data_Full_PT$Latitude*Grouped_Data_Full_PT$PA_LWR
Grouped_Data_Full_PT$Weighted_Lat_UPR<-Grouped_Data_Full_PT$Latitude*Grouped_Data_Full_PT$PA_UPR

Grouped_Data_Full_PT$Weighted_Lat_Mean_Fixed<-Grouped_Data_Full_PT$Latitude*Grouped_Data_Full_PT$PA_Fixed
Grouped_Data_Full_PT$Weighted_Lat_LWR_Fixed<-Grouped_Data_Full_PT$Latitude*Grouped_Data_Full_PT$PA_Fixed_LWR
Grouped_Data_Full_PT$Weighted_Lat_UPR_Fixed<-Grouped_Data_Full_PT$Latitude*Grouped_Data_Full_PT$PA_Fixed_UPR


#divide these by sum of abundance for each year and it will work
library(plyr)
Annual_Centroid_Locations_PT<-ddply(Grouped_Data_Full_PT, .(Year1), summarize, Mean_Weighted_Lat=sum(Weighted_Lat_Mean, na.rm=TRUE)/(sum(PA, na.rm=TRUE)), Mean_Weighted_Lat_LWR=sum(Weighted_Lat_LWR, na.rm=TRUE)/(sum(PA_LWR, na.rm=TRUE)),Mean_Weighted_Lat_UPR=sum(Weighted_Lat_UPR, na.rm=TRUE)/(sum(PA_UPR, na.rm=TRUE)) )
Annual_Centroid_Locations_PT_Fixed<-ddply(Grouped_Data_Full_PT, .(Year1), summarize, Mean_Weighted_Lat=sum(Weighted_Lat_Mean_Fixed, na.rm=TRUE)/(sum(PA_Fixed, na.rm=TRUE)), Mean_Weighted_Lat_LWR=sum(Weighted_Lat_LWR_Fixed, na.rm=TRUE)/(sum(PA_Fixed_LWR, na.rm=TRUE)),Mean_Weighted_Lat_UPR=sum(Weighted_Lat_UPR_Fixed, na.rm=TRUE)/(sum(PA_Fixed_UPR, na.rm=TRUE)))

#Test relationship between lat-lon and year 
Lat_PT_Spring<-lm(Mean_Weighted_Lat~Year1, data=Annual_Centroid_Locations_PT)
summary(Lat_PT_Spring)

Lat_PT_Spring_Fixed<-lm(Mean_Weighted_Lat~Year1, data=Annual_Centroid_Locations_PT_Fixed)
summary(Lat_PT_Spring_Fixed)

tiff("Butterfish_Mean_Location_PA_Comparison_210831.tif", height=5, width=6, res=300, units="in")
par(mar=c(4,6,4,3))
plot(Annual_Centroid_Locations_PT$Year1, Annual_Centroid_Locations_PT$Mean_Weighted_Lat, type="l", lwd=4, xlab="Year", ylab="Mean Weighted \nLatitude", main="Butterfish",cex.lab=1.2, ylim=c(39,41), col="black")
polygon(x=c(Annual_Centroid_Locations_PT$Year1,rev(Annual_Centroid_Locations_PT$Year1)), y=c(Annual_Centroid_Locations_PT$Mean_Weighted_Lat_LWR,rev(Annual_Centroid_Locations_PT$Mean_Weighted_Lat_UPR)), border=NA, col= adjustcolor("gray", alpha.f=0.5))
par(new=T)
plot(Annual_Centroid_Locations_PT$Year1, Annual_Centroid_Locations_PT$Mean_Weighted_Lat, type="l", lwd=4, xlab="Year", ylab="Mean Weighted \nLatitude", main="Butterfish",cex.lab=1.2, ylim=c(39,41), col="black")
polygon(x=c(Annual_Centroid_Locations_PT_Fixed$Year1,rev(Annual_Centroid_Locations_PT_Fixed$Year1)),y=c(Annual_Centroid_Locations_PT_Fixed$Mean_Weighted_Lat_LWR,rev(Annual_Centroid_Locations_PT_Fixed$Mean_Weighted_Lat_UPR)), border=NA, col= adjustcolor("indianred", alpha.f=0.2))
par(new=T)
plot(Annual_Centroid_Locations_PT_Fixed$Year1, Annual_Centroid_Locations_PT_Fixed$Mean_Weighted_Lat, type="l", lwd=4,lty=1, xlab="Year", ylab="Mean Weighted \nLatitude", main="Butterfish",cex.lab=1.2, ylim=c(39,41), col="red")
dev.off()


Grouped_Data_Full_PT$Weighted_Depth_Fixed<-Grouped_Data_Full_PT$Depth*Grouped_Data_Full_PT$PA_Fixed
Grouped_Data_Full_PT$Weighted_Depth_Fixed_LWR<-Grouped_Data_Full_PT$Depth*Grouped_Data_Full_PT$PA_Fixed_LWR
Grouped_Data_Full_PT$Weighted_Depth_Fixed_UPR<-Grouped_Data_Full_PT$Depth*Grouped_Data_Full_PT$PA_Fixed_UPR

Grouped_Data_Full_PT$Weighted_Depth<-Grouped_Data_Full_PT$Depth*Grouped_Data_Full_PT$PA
Grouped_Data_Full_PT$Weighted_Depth_LWR<-Grouped_Data_Full_PT$Depth*Grouped_Data_Full_PT$PA_LWR
Grouped_Data_Full_PT$Weighted_Depth_UPR<-Grouped_Data_Full_PT$Depth*Grouped_Data_Full_PT$PA_UPR



library(plyr)
Annual_Centroid_Depth_PT<-ddply(Grouped_Data_Full_PT, .(Year1), summarize, Mean_Weighted_Depth=sum(Weighted_Depth, na.rm=TRUE)/(sum(PA, na.rm=TRUE)),  Mean_Weighted_Depth_LWR=sum(Weighted_Depth_LWR, na.rm=TRUE)/(sum(PA_LWR, na.rm=TRUE)),  Mean_Weighted_Depth_UPR=sum(Weighted_Depth_UPR, na.rm=TRUE)/(sum(PA_UPR, na.rm=TRUE)))
Annual_Centroid_Depth_PT_Fixed<-ddply(Grouped_Data_Full_PT, .(Year1), summarize, Mean_Weighted_Depth=sum(Weighted_Depth_Fixed, na.rm=TRUE)/(sum(PA_Fixed, na.rm=TRUE)),  Mean_Weighted_Depth_LWR=sum(Weighted_Depth_Fixed_LWR, na.rm=TRUE)/(sum(PA_Fixed_LWR, na.rm=TRUE)),  Mean_Weighted_Depth_UPR=sum(Weighted_Depth_Fixed_UPR, na.rm=TRUE)/(sum(PA_Fixed_UPR, na.rm=TRUE)))

#test for correlation between depth and year for suitability and occupancy 
PT_Depth_Occupancy_Spring<-lm(Mean_Weighted_Depth~Year1, data=Annual_Centroid_Depth_PT)
summary(PT_Depth_Occupancy_Spring)

PT_Depth_Suitability_Spring<-lm(Mean_Weighted_Depth~Year1, data=Annual_Centroid_Depth_PT_Fixed)
summary(PT_Depth_Suitability_Spring)

tiff("Butterfish_Mean_Depth_PA_Comparison_210831_Regridded.tif", height=5, width=6, res=300, units="in")
par(mar=c(4,6,4,3))
plot(Annual_Centroid_Depth_PT$Year1, (Annual_Centroid_Depth_PT$Mean_Weighted_Depth)*-1, type="l", lwd=4,lty=1, xlab="Year", ylab="Mean Weighted \nDepth (m)", main="Butterfish",cex.lab=1.2, ylim=rev(c(70,115)), col="black")
polygon(x=c(Annual_Centroid_Depth_PT$Year1,rev(Annual_Centroid_Depth_PT$Year1)), y=c((Annual_Centroid_Depth_PT$Mean_Weighted_Depth_LWR)*-1,rev((Annual_Centroid_Depth_PT$Mean_Weighted_Depth_UPR)*-1)), border=NA, col= adjustcolor("gray", alpha.f=0.5))
par(new=T)
plot(Annual_Centroid_Depth_PT$Year1, (Annual_Centroid_Depth_PT$Mean_Weighted_Depth)*-1, type="l", lwd=4,lty=1, xlab="Year", ylab="Mean Weighted \nDepth (m)", main="Butterfish",cex.lab=1.2, ylim=rev(c(70,115)), col="black")
polygon(x=c(Annual_Centroid_Depth_PT_Fixed$Year1,rev(Annual_Centroid_Depth_PT_Fixed$Year1)), y=c((Annual_Centroid_Depth_PT_Fixed$Mean_Weighted_Depth_LWR)*-1,rev((Annual_Centroid_Depth_PT_Fixed$Mean_Weighted_Depth_UPR)*-1)), border=NA, col= adjustcolor("indianred", alpha.f=0.2))
par(new=T)
plot(Annual_Centroid_Depth_PT_Fixed$Year1, (Annual_Centroid_Depth_PT_Fixed$Mean_Weighted_Depth)*-1, type="l", lwd=4,lty=1, xlab="Year", ylab="Mean Weighted \nDepth (m)", main="Butterfish",cex.lab=1.2, ylim=rev(c(70,115)), col="red")
dev.off()



#make response cuves 
Trimmed_PT_PA_GAM<-gam(PA~Gear+s(Year, bs="re")+te(Longitude, Latitude)+s(BT_Use, k=6), family=binomial, data=Complete_PT_PA, na.action = "na.fail")

tiff("Butterfish_Bottom_Temp_210630.tif", height=5, width=5, units="in", res=300)
plot(Trimmed_PT_PA_GAM,trans=function(x)exp(x)/(1+exp(x)), shade=T,ylab="Probability of Occurrence",xlab=expression(paste("Bottom Temperature ("^"o","C)")),cex.lab=1.5,cex.axis=1.5)
dev.off()

tiff("Butterfish_Bottom_Temp_Supp_210630.tif", height=5, width=5, units="in", res=300)
plot(Trimmed_PT_PA_GAM, shade=T,xlab=expression(paste("Bottom Temperature ("^"o","C)")),cex.lab=1.5,cex.axis=1.5)
dev.off()

setwd("D:/Sand_Lance/SL_Chapter_2/Ch2_Manuscript/Revisions_Round_One/Updated_Temporary_Figure/Spring/RE")
Trimmed_PT_PA_GAM<-gam(PA~Gear+te(Longitude, Latitude)+s(BT_Use, k=6)+s(Year, bs="re"), family=binomial, data=Complete_PT_PA, na.action = "na.fail")
tiff("Butterfish_Spring_RE.tif", height=5, width=5, units="in", res=300)
plot(Trimmed_PT_PA_GAM,cex.lab=1.5,cex.axis=1.5, main="Butterfish")
dev.off()


Gridded_Final_Values_PT_Spring<-ddply(Grouped_Data_Full_PT[Grouped_Data_Full_PT$Year1>=2012,], .(Latitude, Longitude), summarize, Final_Probability=mean(PA_Fixed))
Gridded_Initial_Values_PT_Spring<-ddply(Grouped_Data_Full_PT[Grouped_Data_Full_PT$Year1<=1983,], .(Latitude, Longitude), summarize, Initial_Probability=mean(PA_Fixed))

PT_Spring_Space<-cbind(Gridded_Final_Values_PT_Spring, Gridded_Initial_Values_PT_Spring)
PT_Spring_Space<-PT_Spring_Space[,!duplicated(colnames(PT_Spring_Space), fromLast = TRUE)]

PT_Spring_Space$Occurrence_Diff<-PT_Spring_Space$Final_Probability-PT_Spring_Space$Initial_Probability

tiff("Spring_Butterfish_Spatial_Difference_Five_Yrs.tif", height=5, width=6,res=300, units="in")
ggplot(data=world) + geom_raster(data = PT_Spring_Space, aes(x=Longitude, y = Latitude, fill=Occurrence_Diff),linejoin = "mitre") + 
  coord_fixed(ratio = 1) +
  scale_fill_gradientn(colours=c("#0000FFFF","#F1ECE4","#FF0000FF"), limits=c(-0.4,0.4)) +
  theme_bw()+ggtitle("Butterfish")+labs(fill = "Diff. in Occurr.")+geom_sf()+coord_sf(xlim=c(-75.5,-64), ylim=c(36, 44.5))+
  theme(plot.title = element_text(size=18,face="bold"), legend.title = element_text(size=16,face="bold"), legend.text = element_text(size=12, face="bold"))
dev.off()


########atl herring##########
#
Grouped_Data_Split_CH<-Grouped_Data_Split_Full
Grouped_Data_Split_CH$Year<-as.factor(as.character(Grouped_Data_Split_CH$Year))
Grouped_Data_Split_CH$Longitude<-as.numeric(as.character(Grouped_Data_Split_CH$Longitude))
Grouped_Data_Split_CH$Latitude<-as.numeric(as.character(Grouped_Data_Split_CH$Latitude))

#split into gear blocks (A=pre door change, B=post door change, C=Bigelow)
for (i in 1:nrow(Grouped_Data_Split_CH)){
  if (Grouped_Data_Split_CH$Year1[i]<1985){
    Grouped_Data_Split_CH$Gear[i]="A"}
  else if (Grouped_Data_Split_CH$Year1[i]>=1985 & Grouped_Data_Split_CH$Year1[i]<=2008){
    Grouped_Data_Split_CH$Gear[i]="B"}
  else if (Grouped_Data_Split_CH$Year1[i]>2008){
    Grouped_Data_Split_CH$Gear[i]="C"
  }
}
#fake years based on abudance for intercept
Grouped_Data_Split_CH$Year[Grouped_Data_Split_CH$Year==2016]<-"2009"
Grouped_Data_Split_CH$Year[Grouped_Data_Split_CH$Year==2015]<-"2009"
Grouped_Data_Split_CH$Year[Grouped_Data_Split_CH$Year==2014]<-"2009"

#predict occupancy
CH_Occupancy<-as.data.frame(predict(CH_Multi_Avg_Spring,Grouped_Data_Split_CH,type="link",se.fit=T))
CH_Occupancy$PA<-exp(CH_Occupancy$fit)/(1+exp(CH_Occupancy$fit))
CH_Occupancy$PA_LWR<-exp(CH_Occupancy$fit-(1.96*CH_Occupancy$se.fit))/(1+exp(CH_Occupancy$fit-(1.96*CH_Occupancy$se.fit)))
CH_Occupancy$PA_UPR<-exp(CH_Occupancy$fit+(1.96*CH_Occupancy$se.fit))/(1+exp(CH_Occupancy$fit+(1.96*CH_Occupancy$se.fit)))

#predict suitability
Grouped_Data_Split_CH2<-Grouped_Data_Split_CH
Grouped_Data_Split_CH2$Gear<-"B"
CH_Partial_Effects<-as.data.frame(predict(CH_Multi_Avg_Spring,Grouped_Data_Split_CH2,exclude=c("Gear","s(Year)"),type="link", se.fit=T))
CH_Partial_Effects$PA_Fixed<-exp(CH_Partial_Effects$fit)/(1+exp(CH_Partial_Effects$fit))
CH_Partial_Effects$PA_Fixed_LWR<-exp(CH_Partial_Effects$fit-(1.96*CH_Partial_Effects$se.fit))/(1+exp(CH_Partial_Effects$fit-(1.96*CH_Partial_Effects$se.fit)))
CH_Partial_Effects$PA_Fixed_UPR<-exp(CH_Partial_Effects$fit+(1.96*CH_Partial_Effects$se.fit))/(1+exp(CH_Partial_Effects$fit+(1.96*CH_Partial_Effects$se.fit)))

Grouped_Data_Split_CH3<-Grouped_Data_Split_CH
Grouped_Data_Split_CH3$Gear<-"B"
CH_No_Vess_Effects<-as.data.frame(predict(CH_Multi_Avg_Spring,Grouped_Data_Split_CH3,exclude=c("Gear"),type="link", se.fit=T))
CH_No_Vess_Effects$PA_Vessel<-exp(CH_No_Vess_Effects$fit)/(1+exp(CH_No_Vess_Effects$fit))
CH_No_Vess_Effects$PA_Vessel_LWR<-exp(CH_No_Vess_Effects$fit-(1.96*CH_No_Vess_Effects$se.fit))/(1+exp(CH_No_Vess_Effects$fit-(1.96*CH_No_Vess_Effects$se.fit)))
CH_No_Vess_Effects$PA_Vessel_UPR<-exp(CH_No_Vess_Effects$fit+(1.96*CH_No_Vess_Effects$se.fit))/(1+exp(CH_No_Vess_Effects$fit+(1.96*CH_No_Vess_Effects$se.fit)))

Grouped_Data_Full_CH<-cbind(Grouped_Data_Split_CH, CH_Occupancy, CH_Partial_Effects, CH_No_Vess_Effects)

library(plyr)
#set occurrence to mean prevalance
Annual_CH_Prev=ddply(CH_Matched, .(Year), summarize, Prev=sum(PA)/length(PA))
CH_Spring_Prevalence=mean(Annual_CH_Prev$Prev)


#remove duplicate names 
Grouped_Data_Full_CH<-Grouped_Data_Full_CH[,-which(names(Grouped_Data_Full_CH) %in% c("fit","se.fit"))]

#shelf occupancy
AnnualCH_Occupancy<-ddply(Grouped_Data_Full_CH, .(Year1), summarize, Mean_hab_Prop=length(which(PA>CH_Spring_Prevalence ))/length(which(!is.na(PA))), Low_hap_prop=length(which(PA_LWR>CH_Spring_Prevalence ))/length(which(!is.na(PA))),High_hap_prop=length(which(PA_UPR>CH_Spring_Prevalence ))/length(which(!is.na(PA))) )

#habitat suitability
AnnualCH_Partial_Effects<-ddply(Grouped_Data_Full_CH, .(Year1), summarize, Mean_hab_Prop=(length(which(PA_Fixed>CH_Spring_Prevalence )))/(length(which(!is.na(PA_Fixed)))), Low_hap_prop=(length(which(PA_Fixed_LWR>CH_Spring_Prevalence )))/(length(which(!is.na(PA_Fixed)))),High_hap_prop=(length(which(PA_Fixed_UPR>CH_Spring_Prevalence )))/(length(which(!is.na(PA_Fixed)))))

AnnualCH_No_Vess_Effects<-ddply(Grouped_Data_Full_CH, .(Year1), summarize, Mean_hab_Prop=(length(which(PA_Vessel>CH_Spring_Prevalence )))/(length(which(!is.na(PA_Vessel)))), Low_hap_prop=(length(which(PA_Vessel_LWR>CH_Spring_Prevalence )))/(length(which(!is.na(PA_Vessel)))),High_hap_prop=(length(which(PA_Vessel_UPR>CH_Spring_Prevalence )))/(length(which(!is.na(PA_Vessel)))))

#make annual averages for plotting suitability
Annual_Averages_CH<-ddply(Grouped_Data_Full_CH, .(Longitude, Latitude), summarize, Mean_PA=mean(PA))


world<-ne_countries(scale="medium", returnclass = "sf")
tiff("Herring_Model_Pred_210712.tif", width=6, height=5, units="in", res=300)
ggplot(data=world) + geom_raster(data = Annual_Averages_CH, aes(x=Longitude, y = Latitude, fill=Mean_PA),linejoin = "mitre") + 
  coord_fixed(ratio = 1) +
  scale_fill_viridis(direction = 1) +
  theme_bw()+ggtitle("Atl. herring")+labs(fill = "Prob. Occurrence")+geom_sf()+coord_sf(xlim=c(-75.5,-64), ylim=c(36, 44.5))
dev.off()


tiff("Herring_Occupiable_Habitat_210714.tif", height=5, width=6, res=300, units="in")
par(mar=c(4,6,4,3))
plot(AnnualCH_Occupancy$Year1, AnnualCH_Occupancy$Mean_hab_Prop, type="l", lwd=4, xlab="Year", ylab="Proportion of \nOccupiable Shelf", main="Atl. herring",cex.lab=1.2, ylim=c(0,1.0), col="black")
polygon(x=c(AnnualCH_Occupancy$Year1,rev(AnnualCH_Occupancy$Year1)),y=c(AnnualCH_Occupancy$Low_hap_prop,rev(AnnualCH_Occupancy$High_hap_prop)), border=NA, col= adjustcolor("gray", alpha.f=0.5))
par(new=T)
plot(AnnualCH_Occupancy$Year1, AnnualCH_Occupancy$Mean_hab_Prop, type="l", lwd=4, xlab="Year", ylab="Proportion of \nOccupiable Shelf", main="Atl. herring",cex.lab=1.2, ylim=c(0,1.0), col="black")
dev.off()

tiff("Herring_Occupiable_Habitat_210831_Threshold_Comparison.tif", height=5, width=6, res=300, units="in")
par(mar=c(4,6,4,3))
plot(AnnualCH_Occupancy$Year1, AnnualCH_Occupancy$Mean_hab_Prop, type="l", lwd=4, xlab="Year", ylab="Proportion of \nOccupiable Shelf", main="Atl. herring",cex.lab=1.2, ylim=c(0,1.0), col="black")
polygon(x=c(AnnualCH_Occupancy$Year1,rev(AnnualCH_Occupancy$Year1)),y=c(AnnualCH_Occupancy$Low_hap_prop,rev(AnnualCH_Occupancy$High_hap_prop)), border=NA, col= adjustcolor("gray", alpha.f=0.5))
par(new=T)
plot(AnnualCH_Occupancy$Year1, AnnualCH_Occupancy$Mean_hab_Prop, type="l", lwd=4, xlab="Year", ylab="Proportion of \nOccupiable Shelf", main="Atl. herring",cex.lab=1.2, ylim=c(0,1.0), col="black")
polygon(x=c(AnnualCH_No_Vess_Effects$Year1,rev(AnnualCH_No_Vess_Effects$Year1)),y=c(AnnualCH_No_Vess_Effects$Low_hap_prop,rev(AnnualCH_No_Vess_Effects$High_hap_prop)), border=NA, col= adjustcolor("blue", alpha.f=0.25))
par(new=T)
plot(AnnualCH_No_Vess_Effects$Year1, AnnualCH_No_Vess_Effects$Mean_hab_Prop, type="l", lwd=4,lty=1, xlab="Year", ylab="Proportion of \nOccupiable Shelf", main="Atl. herring",cex.lab=1.2, ylim=c(0,1.0), col="blue")
polygon(x=c(AnnualCH_Partial_Effects$Year1,rev(AnnualCH_Partial_Effects$Year1)),y=c(AnnualCH_Partial_Effects$Low_hap_prop,rev(AnnualCH_Partial_Effects$High_hap_prop)), border=NA, col= adjustcolor("indianred", alpha.f=0.25))
par(new=T)
plot(AnnualCH_Partial_Effects$Year1, AnnualCH_Partial_Effects$Mean_hab_Prop, type="l", lwd=4,lty=1, xlab="Year", ylab="Proportion of \nOccupiable Shelf", main="Atl. herring",cex.lab=1.2, ylim=c(0,1.0), col="red")
dev.off()


#calculate trends in suitability and occupancy using betaregression 
library(betareg)
AnnualCH_Occupancy$Mean_hab_Prop<-AnnualCH_Occupancy$Mean_hab_Prop+0.00000001
CH_Spring_Full_Hab_Occ<-betareg(AnnualCH_Occupancy$Mean_hab_Prop~AnnualCH_Occupancy$Year1)
summary(CH_Spring_Full_Hab_Occ)

CH_Spring_Fixed_Hab_Occ<-betareg(AnnualCH_Partial_Effects$Mean_hab_Prop~AnnualCH_Partial_Effects$Year1)
summary(CH_Spring_Fixed_Hab_Occ)

#look at trends with latitude and depth
#Calculate weighted lat and lon
Grouped_Data_Full_CH$Weighted_Lat_Mean<-Grouped_Data_Full_CH$Latitude*Grouped_Data_Full_CH$PA
Grouped_Data_Full_CH$Weighted_Lat_LWR<-Grouped_Data_Full_CH$Latitude*Grouped_Data_Full_CH$PA_LWR
Grouped_Data_Full_CH$Weighted_Lat_UPR<-Grouped_Data_Full_CH$Latitude*Grouped_Data_Full_CH$PA_UPR

Grouped_Data_Full_CH$Weighted_Lat_Mean_Fixed<-Grouped_Data_Full_CH$Latitude*Grouped_Data_Full_CH$PA_Fixed
Grouped_Data_Full_CH$Weighted_Lat_LWR_Fixed<-Grouped_Data_Full_CH$Latitude*Grouped_Data_Full_CH$PA_Fixed_LWR
Grouped_Data_Full_CH$Weighted_Lat_UPR_Fixed<-Grouped_Data_Full_CH$Latitude*Grouped_Data_Full_CH$PA_Fixed_UPR


#divide these by sum of abundance for each year
library(plyr)
Annual_Centroid_Locations_CH<-ddply(Grouped_Data_Full_CH, .(Year1), summarize, Mean_Weighted_Lat=sum(Weighted_Lat_Mean, na.rm=TRUE)/(sum(PA, na.rm=TRUE)), Mean_Weighted_Lat_LWR=sum(Weighted_Lat_LWR, na.rm=TRUE)/(sum(PA_LWR, na.rm=TRUE)),Mean_Weighted_Lat_UPR=sum(Weighted_Lat_UPR, na.rm=TRUE)/(sum(PA_UPR, na.rm=TRUE)) )
Annual_Centroid_Locations_CH_Fixed<-ddply(Grouped_Data_Full_CH, .(Year1), summarize, Mean_Weighted_Lat=sum(Weighted_Lat_Mean_Fixed, na.rm=TRUE)/(sum(PA_Fixed, na.rm=TRUE)), Mean_Weighted_Lat_LWR=sum(Weighted_Lat_LWR_Fixed, na.rm=TRUE)/(sum(PA_Fixed_LWR, na.rm=TRUE)),Mean_Weighted_Lat_UPR=sum(Weighted_Lat_UPR_Fixed, na.rm=TRUE)/(sum(PA_Fixed_UPR, na.rm=TRUE)))



#Test relationship between lat-lon and year 
Lat_CH_Spring<-lm(Mean_Weighted_Lat~Year1, data=Annual_Centroid_Locations_CH)
summary(Lat_CH_Spring)

Lat_CH_Spring_Fixed<-lm(Mean_Weighted_Lat~Year1, data=Annual_Centroid_Locations_CH_Fixed)
summary(Lat_CH_Spring_Fixed)

tiff("Herring_Mean_Location_PA_Comparison_210714.tif", height=5, width=6, res=300, units="in")
par(mar=c(4,6,4,3))
plot(Annual_Centroid_Locations_CH$Year1, Annual_Centroid_Locations_CH$Mean_Weighted_Lat, type="l", lwd=4,lty=1, xlab="Year", ylab="Mean Weighted \nLatitude", main="Atl. herring",cex.lab=1.2, ylim=c(41,41.8), col="black")
polygon(x=c(Annual_Centroid_Locations_CH$Year1,rev(Annual_Centroid_Locations_CH$Year1)), y=c(Annual_Centroid_Locations_CH$Mean_Weighted_Lat_LWR,rev(Annual_Centroid_Locations_CH$Mean_Weighted_Lat_UPR)), border=NA, col= adjustcolor("gray", alpha.f=0.5))
par(new=T)
plot(Annual_Centroid_Locations_CH$Year1, Annual_Centroid_Locations_CH$Mean_Weighted_Lat, type="l", lwd=4,lty=1, xlab="Year", ylab="Mean Weighted \nLatitude", main="Atl. herring",cex.lab=1.2, ylim=c(41,41.8), col="black")
polygon(x=c(Annual_Centroid_Locations_CH_Fixed$Year1,rev(Annual_Centroid_Locations_CH_Fixed$Year1)),y=c(Annual_Centroid_Locations_CH_Fixed$Mean_Weighted_Lat_LWR,rev(Annual_Centroid_Locations_CH_Fixed$Mean_Weighted_Lat_UPR)), border=NA, col= adjustcolor("indianred", alpha.f=0.2))
par(new=T)
plot(Annual_Centroid_Locations_CH_Fixed$Year1, Annual_Centroid_Locations_CH_Fixed$Mean_Weighted_Lat, type="l", lwd=4,lty=1, xlab="Year", ylab="Mean Weighted \nLatitude", main="Atl. herring",cex.lab=1.2, ylim=c(41,41.8), col="red")
dev.off()


Grouped_Data_Full_CH$Weighted_Depth_Fixed<-Grouped_Data_Full_CH$Depth*Grouped_Data_Full_CH$PA_Fixed
Grouped_Data_Full_CH$Weighted_Depth_Fixed_LWR<-Grouped_Data_Full_CH$Depth*Grouped_Data_Full_CH$PA_Fixed_LWR
Grouped_Data_Full_CH$Weighted_Depth_Fixed_UPR<-Grouped_Data_Full_CH$Depth*Grouped_Data_Full_CH$PA_Fixed_UPR

Grouped_Data_Full_CH$Weighted_Depth<-Grouped_Data_Full_CH$Depth*Grouped_Data_Full_CH$PA
Grouped_Data_Full_CH$Weighted_Depth_LWR<-Grouped_Data_Full_CH$Depth*Grouped_Data_Full_CH$PA_LWR
Grouped_Data_Full_CH$Weighted_Depth_UPR<-Grouped_Data_Full_CH$Depth*Grouped_Data_Full_CH$PA_UPR



library(plyr)
Annual_Centroid_Depth_CH<-ddply(Grouped_Data_Full_CH, .(Year1), summarize, Mean_Weighted_Depth=sum(Weighted_Depth, na.rm=TRUE)/(sum(PA, na.rm=TRUE)),  Mean_Weighted_Depth_LWR=sum(Weighted_Depth_LWR, na.rm=TRUE)/(sum(PA_LWR, na.rm=TRUE)),  Mean_Weighted_Depth_UPR=sum(Weighted_Depth_UPR, na.rm=TRUE)/(sum(PA_UPR, na.rm=TRUE)))
Annual_Centroid_Depth_CH_Fixed<-ddply(Grouped_Data_Full_CH, .(Year1), summarize, Mean_Weighted_Depth=sum(Weighted_Depth_Fixed, na.rm=TRUE)/(sum(PA_Fixed, na.rm=TRUE)),  Mean_Weighted_Depth_LWR=sum(Weighted_Depth_Fixed_LWR, na.rm=TRUE)/(sum(PA_Fixed_LWR, na.rm=TRUE)),  Mean_Weighted_Depth_UPR=sum(Weighted_Depth_Fixed_UPR, na.rm=TRUE)/(sum(PA_Fixed_UPR, na.rm=TRUE)))

#test for correlation between depth and year for suitability and occupancy 
CH_Depth_Occupancy_Spring<-lm(Mean_Weighted_Depth~Year1, data=Annual_Centroid_Depth_CH)
summary(CH_Depth_Occupancy_Spring)

CH_Depth_Suitability_Spring<-lm(Mean_Weighted_Depth~Year1, data=Annual_Centroid_Depth_CH_Fixed)
summary(CH_Depth_Suitability_Spring)

tiff("Herring_Mean_Depth_PA_Comparison_210714_Regridded.tif", height=5, width=6, res=300, units="in")
par(mar=c(4,6,4,3))
plot(Annual_Centroid_Depth_CH$Year1, (Annual_Centroid_Depth_CH$Mean_Weighted_Depth)*-1, type="l", lwd=4, xlab="Year", ylab="Mean Weighted \nDepth (m)", main="Atl. herring",cex.lab=1.2, ylim=rev(c(75,105)), col="black")
polygon(x=c(Annual_Centroid_Depth_CH$Year1,rev(Annual_Centroid_Depth_CH$Year1)), y=c((Annual_Centroid_Depth_CH$Mean_Weighted_Depth_LWR)*-1,rev((Annual_Centroid_Depth_CH$Mean_Weighted_Depth_UPR)*-1)), border=NA, col= adjustcolor("gray", alpha.f=0.5))
par(new=T)
plot(Annual_Centroid_Depth_CH$Year1, (Annual_Centroid_Depth_CH$Mean_Weighted_Depth)*-1, type="l", lwd=4, xlab="Year", ylab="Mean Weighted \nDepth (m)", main="Atl. herring",cex.lab=1.2, ylim=rev(c(75,105)), col="black")
polygon(x=c(Annual_Centroid_Depth_CH_Fixed$Year1,rev(Annual_Centroid_Depth_CH_Fixed$Year1)), y=c((Annual_Centroid_Depth_CH_Fixed$Mean_Weighted_Depth_LWR)*-1,rev((Annual_Centroid_Depth_CH_Fixed$Mean_Weighted_Depth_UPR)*-1)), border=NA, col= adjustcolor("indianred", alpha.f=0.2))
par(new=T)
plot(Annual_Centroid_Depth_CH_Fixed$Year1, (Annual_Centroid_Depth_CH_Fixed$Mean_Weighted_Depth)*-1, type="l", lwd=4,lty=1, xlab="Year", ylab="Mean Weighted \nDepth (m)", main="Atl. herring",cex.lab=1.2, ylim=rev(c(75,105)), col="red")
dev.off()


#make response curves
View(CH_PA_Dredge_Spring)
Best_CH_PA_GAM<-gam(PA~s(SST_Use,k=6)+s(EKE,k=6)
                    +s(Estimated_Grain_Size, k=6)+Gear+s(Year, bs="re")+te(Longitude, Latitude)+s(BS_Use, k=6),family=binomial,
                    data=Complete_CH_PA1, na.action = "na.fail")

tiff("Atl_herring_Bottom_Sal_210630.tif", height=5, width=5, units="in", res=300)
plot(Best_CH_PA_GAM,trans=function(x)exp(x)/(1+exp(x)), shade=T,ylab="Probability of Occurrence",xlab="Bottom Salinity", cex.lab=1.5, cex.axis=1.5 )
dev.off()

tiff("Atl_herring_Bottom_Sal_Supp_210630.tif", height=5, width=5, units="in", res=300)
plot(Best_CH_PA_GAM, shade=T,xlab="Bottom Salinity", cex.lab=1.5, cex.axis=1.5 )
dev.off()


Best_CH_PA_GAM<-gam(PA~s(EKE,k=6)
                    +s(Estimated_Grain_Size, k=6)+Gear+s(Year, bs="re")+te(Longitude, Latitude)+s(BS_Use, k=6)+s(SST_Use,k=6),family=binomial,
                    data=Complete_CH_PA1, na.action = "na.fail")




tiff("Atl_herring_SST_210630.tif", height=5, width=5, units="in", res=300)
plot(Best_CH_PA_GAM,trans=function(x)exp(x)/(1+exp(x)), shade=T,xlab=expression(paste("Sea Surface Temperature ("^"o","C)")), ylab="Probability of Occurrence", cex.lab=1.5, cex.axis=1.5 )
dev.off()

tiff("Atl_herring_Bottom_SST_Supp_210630.tif", height=5, width=5, units="in", res=300)
plot(Best_CH_PA_GAM, shade=T,xlab=expression(paste("Sea Surface Temperature ("^"o","C)")), cex.lab=1.5, cex.axis=1.5 )
dev.off()

Best_CH_PA_GAM<-gam(PA~
                    s(Estimated_Grain_Size, k=6)+Gear+s(Year, bs="re")+te(Longitude, Latitude)+s(BS_Use, k=6)+s(SST_Use,k=6)+s(EKE,k=6),family=binomial,
                    data=Complete_CH_PA1, na.action = "na.fail")




tiff("Atl_herring_EKE_210630.tif", height=5, width=5, units="in", res=300)
plot(Best_CH_PA_GAM,trans=function(x)exp(x)/(1+exp(x)), shade=T,xlab=expression(paste("Mean kinetic energy (cm"^"-2","s"^"-2",")")), ylab="Probability of Occurrence", cex.lab=1.5, cex.axis=1.5 )
dev.off()

tiff("Atl_herring_EKE_Supp_210630.tif", height=5, width=5, units="in", res=300)
plot(Best_CH_PA_GAM, shade=T,xlab=expression(paste("Mean kinetic energy (cm"^"-2","s"^"-2",")")), cex.lab=1.5, cex.axis=1.5 )
dev.off()

Best_CH_PA_GAM_RE<-gam(PA~
                      s(Estimated_Grain_Size, k=6)+Gear+te(Longitude, Latitude)+s(BS_Use, k=6)+s(SST_Use,k=6)+s(EKE,k=6)+s(Year, bs="re"),family=binomial,
                    data=Complete_CH_PA1, na.action = "na.fail")

setwd("D:/Sand_Lance/SL_Chapter_2/Ch2_Manuscript/Revisions_Round_One/Updated_Temporary_Figure/Spring/RE")
tiff("Atl_herring_Spring_RE.tif", height=5, width=5, units="in", res=300)
plot(Best_CH_PA_GAM_RE,cex.lab=1.5,cex.axis=1.5, main="Atl. herring")
dev.off()

Gridded_Final_Values_CH_Spring<-ddply(Grouped_Data_Full_CH[Grouped_Data_Full_CH$Year1>=2012,], .(Latitude, Longitude), summarize, Final_Probability=mean(PA_Fixed))
Gridded_Initial_Values_CH_Spring<-ddply(Grouped_Data_Full_CH[Grouped_Data_Full_CH$Year1<=1983,], .(Latitude, Longitude), summarize, Initial_Probability=mean(PA_Fixed))

CH_Spring_Space<-cbind(Gridded_Final_Values_CH_Spring, Gridded_Initial_Values_CH_Spring)
CH_Spring_Space<-CH_Spring_Space[,!duplicated(colnames(CH_Spring_Space), fromLast = TRUE)]

CH_Spring_Space$Occurrence_Diff<-CH_Spring_Space$Final_Probability-CH_Spring_Space$Initial_Probability

tiff("Spring_Herring_Spatial_Difference_Five_Yrs.tif", height=5, width=6, units="in", res=300)
ggplot(data=world) + geom_raster(data = CH_Spring_Space, aes(x=Longitude, y = Latitude, fill=Occurrence_Diff),linejoin = "mitre") + 
  coord_fixed(ratio = 1) +
  scale_fill_gradientn(colours=c("#0000FFFF","#F1ECE4","#FF0000FF"), limits=c(-0.4,0.4)) +
  theme_bw()+ggtitle("Atl. herring")+labs(fill = "Diff. in Occurr.")+geom_sf()+coord_sf(xlim=c(-75.5,-64), ylim=c(36, 44.5))+
  theme(plot.title = element_text(size=18,face="bold"), legend.title = element_text(size=16,face="bold"), legend.text = element_text(size=12, face="bold"))

dev.off()


######### alewife###########
Grouped_Data_Split_AP<-Grouped_Data_Split_Full
Grouped_Data_Split_AP$Year<-as.factor(as.character(Grouped_Data_Split_AP$Year))
Grouped_Data_Split_AP$Longitude<-as.numeric(as.character(Grouped_Data_Split_AP$Longitude))
Grouped_Data_Split_AP$Latitude<-as.numeric(as.character(Grouped_Data_Split_AP$Latitude))

#split into gear blocks (A=pre door change, B=post door change, C=Bigelow)
for (i in 1:nrow(Grouped_Data_Split_AP)){
  if (Grouped_Data_Split_AP$Year1[i]<1985){
    Grouped_Data_Split_AP$Gear[i]="A"}
  else if (Grouped_Data_Split_AP$Year1[i]>=1985 & Grouped_Data_Split_AP$Year1[i]<=2008){
    Grouped_Data_Split_AP$Gear[i]="B"}
  else if (Grouped_Data_Split_AP$Year1[i]>2008){
    Grouped_Data_Split_AP$Gear[i]="C"
  }
}
#fake years based on abudance for intercept
Grouped_Data_Split_AP$Year[Grouped_Data_Split_AP$Year==2016]<-"2011"
Grouped_Data_Split_AP$Year[Grouped_Data_Split_AP$Year==2015]<-"2010"
Grouped_Data_Split_AP$Year[Grouped_Data_Split_AP$Year==2014]<-"2010"

#shelf occupancy
AP_Occupancy<-as.data.frame(predict(AP_Multi_Avg_Spring,Grouped_Data_Split_AP,type="link",se.fit=T))
AP_Occupancy$PA<-exp(AP_Occupancy$fit)/(1+exp(AP_Occupancy$fit))
AP_Occupancy$PA_LWR<-exp(AP_Occupancy$fit-(1.96*AP_Occupancy$se.fit))/(1+exp(AP_Occupancy$fit-(1.96*AP_Occupancy$se.fit)))
AP_Occupancy$PA_UPR<-exp(AP_Occupancy$fit+(1.96*AP_Occupancy$se.fit))/(1+exp(AP_Occupancy$fit+(1.96*AP_Occupancy$se.fit)))

#predict habitat suitability
Grouped_Data_Split_AP2<-Grouped_Data_Split_AP
Grouped_Data_Split_AP2$Gear<-"B"
AP_Partial_Effects<-as.data.frame(predict(AP_Multi_Avg_Spring,Grouped_Data_Split_AP2,exclude=c("Gear","s(Year)"),type="link", se.fit=T))
AP_Partial_Effects$PA_Fixed<-exp(AP_Partial_Effects$fit)/(1+exp(AP_Partial_Effects$fit))
AP_Partial_Effects$PA_Fixed_LWR<-exp(AP_Partial_Effects$fit-(1.96*AP_Partial_Effects$se.fit))/(1+exp(AP_Partial_Effects$fit-(1.96*AP_Partial_Effects$se.fit)))
AP_Partial_Effects$PA_Fixed_UPR<-exp(AP_Partial_Effects$fit+(1.96*AP_Partial_Effects$se.fit))/(1+exp(AP_Partial_Effects$fit+(1.96*AP_Partial_Effects$se.fit)))

Grouped_Data_Split_AP3<-Grouped_Data_Split_AP
Grouped_Data_Split_AP3$Gear<-"B"
AP_No_Vess_Effects<-as.data.frame(predict(AP_Multi_Avg_Spring,Grouped_Data_Split_AP3,exclude=c("Gear"),type="link", se.fit=T))
AP_No_Vess_Effects$PA_Vessel<-exp(AP_No_Vess_Effects$fit)/(1+exp(AP_No_Vess_Effects$fit))
AP_No_Vess_Effects$PA_Vessel_LWR<-exp(AP_No_Vess_Effects$fit-(1.96*AP_No_Vess_Effects$se.fit))/(1+exp(AP_No_Vess_Effects$fit-(1.96*AP_No_Vess_Effects$se.fit)))
AP_No_Vess_Effects$PA_Vessel_UPR<-exp(AP_No_Vess_Effects$fit+(1.96*AP_No_Vess_Effects$se.fit))/(1+exp(AP_No_Vess_Effects$fit+(1.96*AP_No_Vess_Effects$se.fit)))

Grouped_Data_Full_AP<-cbind(Grouped_Data_Split_AP, AP_Occupancy, AP_Partial_Effects, AP_No_Vess_Effects)

library(plyr)
#set occurrence to mean prevalance
Annual_AP_Prev=ddply(AP_Matched, .(Year), summarize, Prev=sum(PA)/length(PA))
AP_Spring_Prevalence=mean(Annual_AP_Prev$Prev)

Grouped_Data_Full_AP<-Grouped_Data_Full_AP[,-which(names(Grouped_Data_Full_AP) %in% c("fit","se.fit"))]

#shelf occupancy
AnnualAP_Occupancy<-ddply(Grouped_Data_Full_AP, .(Year1), summarize, Mean_hab_Prop=length(which(PA>AP_Spring_Prevalence ))/length(which(!is.na(PA))), Low_hap_prop=length(which(PA_LWR>AP_Spring_Prevalence ))/length(which(!is.na(PA))),High_hap_prop=length(which(PA_UPR>AP_Spring_Prevalence ))/length(which(!is.na(PA))) )

#habitat suitability
AnnualAP_Partial_Effects<-ddply(Grouped_Data_Full_AP, .(Year1), summarize, Mean_hab_Prop=(length(which(PA_Fixed>AP_Spring_Prevalence )))/(length(which(!is.na(PA_Fixed)))), Low_hap_prop=(length(which(PA_Fixed_LWR>AP_Spring_Prevalence )))/(length(which(!is.na(PA_Fixed)))),High_hap_prop=(length(which(PA_Fixed_UPR>AP_Spring_Prevalence )))/(length(which(!is.na(PA_Fixed)))))

AnnualAP_No_Vess_Effects<-ddply(Grouped_Data_Full_AP, .(Year1), summarize, Mean_hab_Prop=(length(which(PA_Vessel>AP_Spring_Prevalence )))/(length(which(!is.na(PA_Vessel)))), Low_hap_prop=(length(which(PA_Vessel_LWR>AP_Spring_Prevalence )))/(length(which(!is.na(PA_Vessel)))),High_hap_prop=(length(which(PA_Vessel_UPR>AP_Spring_Prevalence )))/(length(which(!is.na(PA_Vessel)))))

#make annual averages for plotting suitability
Annual_Averages_AP<-ddply(Grouped_Data_Full_AP, .(Longitude, Latitude), summarize, Mean_PA=mean(PA))


world<-ne_countries(scale="medium", returnclass = "sf")
tiff("Alewife_Model_Pred_210712.tif", width=6, height=5, units="in", res=300)
ggplot(data=world) + geom_raster(data = Annual_Averages_AP, aes(x=Longitude, y = Latitude, fill=Mean_PA),linejoin = "mitre") + 
  coord_fixed(ratio = 1) +
  scale_fill_viridis(direction = 1) +
  theme_bw()+ggtitle("Alewife")+labs(fill = "Prob. Occurrence")+geom_sf()+coord_sf(xlim=c(-75.5,-64), ylim=c(36, 44.5))
dev.off()


tiff("Alewife_Occupiable_Habitat_210714.tif", height=5, width=6, res=300, units="in")
par(mar=c(4,6,4,3))
plot(AnnualAP_Occupancy$Year1, AnnualAP_Occupancy$Mean_hab_Prop, type="l", lwd=4,lty=1, xlab="Year", ylab="Proportion of \nOccupiable Shelf", main="Alewife",cex.lab=1.2, ylim=c(0.3,0.8), col="black")
polygon(x=c(AnnualAP_Occupancy$Year1,rev(AnnualAP_Occupancy$Year1)),y=c(AnnualAP_Occupancy$Low_hap_prop,rev(AnnualAP_Occupancy$High_hap_prop)), border=NA, col= adjustcolor("gray", alpha.f=0.5))
par(new=T)
plot(AnnualAP_Occupancy$Year1, AnnualAP_Occupancy$Mean_hab_Prop, type="l", lwd=4,lty=1, xlab="Year", ylab="Proportion of \nOccupiable Shelf", main="Alewife",cex.lab=1.2, ylim=c(0.3,0.8), col="black")
dev.off()

tiff("Alewife_Occupiable_Habitat_210831_Threshold_Comparison.tif", height=5, width=6, res=300, units="in")
par(mar=c(4,6,4,3))
plot(AnnualAP_Occupancy$Year1, AnnualAP_Occupancy$Mean_hab_Prop, type="l", lwd=4,lty=1, xlab="Year", ylab="Proportion of \nOccupiable Shelf", main="Alewife",cex.lab=1.2, ylim=c(0.3,0.8), col="black")
polygon(x=c(AnnualAP_Occupancy$Year1,rev(AnnualAP_Occupancy$Year1)),y=c(AnnualAP_Occupancy$Low_hap_prop,rev(AnnualAP_Occupancy$High_hap_prop)), border=NA, col= adjustcolor("gray", alpha.f=0.5))
par(new=T)
plot(AnnualAP_Occupancy$Year1, AnnualAP_Occupancy$Mean_hab_Prop, type="l", lwd=4,lty=1, xlab="Year", ylab="Proportion of \nOccupiable Shelf", main="Alewife",cex.lab=1.2, ylim=c(0.3,0.8), col="black")
polygon(x=c(AnnualAP_No_Vess_Effects$Year1,rev(AnnualAP_No_Vess_Effects$Year1)),y=c(AnnualAP_No_Vess_Effects$Low_hap_prop,rev(AnnualAP_No_Vess_Effects$High_hap_prop)), border=NA, col= adjustcolor("blue", alpha.f=0.25))
par(new=T)
plot(AnnualAP_No_Vess_Effects$Year1, AnnualAP_No_Vess_Effects$Mean_hab_Prop, type="l", lwd=4,lty=1, xlab="Year", ylab="Proportion of \nOccupiable Shelf", main="Alewife",cex.lab=1.2, ylim=c(0.3,0.8), col="blue")
polygon(x=c(AnnualAP_Partial_Effects$Year1,rev(AnnualAP_Partial_Effects$Year1)),y=c(AnnualAP_Partial_Effects$Low_hap_prop,rev(AnnualAP_Partial_Effects$High_hap_prop)), border=NA, col= adjustcolor("indianred", alpha.f=0.25))
par(new=T)
plot(AnnualAP_Partial_Effects$Year1, AnnualAP_Partial_Effects$Mean_hab_Prop, type="l", lwd=4,lty=1, xlab="Year", ylab="Proportion of \nOccupiable Shelf", main="Alewife",cex.lab=1.2, ylim=c(0.3,0.8), col="red")
dev.off()


#calculate trends in suitability and occupancy using betaregression 
library(betareg)
AnnualAP_Occupancy$Mean_hab_Prop<-AnnualAP_Occupancy$Mean_hab_Prop+0.00000001
AP_Spring_Full_Hab_Occ<-betareg(AnnualAP_Occupancy$Mean_hab_Prop~AnnualAP_Occupancy$Year1)
summary(AP_Spring_Full_Hab_Occ)

AP_Spring_Fixed_Hab_Occ<-betareg(AnnualAP_Partial_Effects$Mean_hab_Prop~AnnualAP_Partial_Effects$Year1)
summary(AP_Spring_Fixed_Hab_Occ)




#look at trends with latitude and depth
#Calculate weighted lat and lon
Grouped_Data_Full_AP$Weighted_Lat_Mean<-Grouped_Data_Full_AP$Latitude*Grouped_Data_Full_AP$PA
Grouped_Data_Full_AP$Weighted_Lat_LWR<-Grouped_Data_Full_AP$Latitude*Grouped_Data_Full_AP$PA_LWR
Grouped_Data_Full_AP$Weighted_Lat_UPR<-Grouped_Data_Full_AP$Latitude*Grouped_Data_Full_AP$PA_UPR

Grouped_Data_Full_AP$Weighted_Lat_Mean_Fixed<-Grouped_Data_Full_AP$Latitude*Grouped_Data_Full_AP$PA_Fixed
Grouped_Data_Full_AP$Weighted_Lat_LWR_Fixed<-Grouped_Data_Full_AP$Latitude*Grouped_Data_Full_AP$PA_Fixed_LWR
Grouped_Data_Full_AP$Weighted_Lat_UPR_Fixed<-Grouped_Data_Full_AP$Latitude*Grouped_Data_Full_AP$PA_Fixed_UPR


#divide these by sum of abundance for each year and it will work
library(plyr)
Annual_Centroid_Locations_AP<-ddply(Grouped_Data_Full_AP, .(Year1), summarize, Mean_Weighted_Lat=sum(Weighted_Lat_Mean, na.rm=TRUE)/(sum(PA, na.rm=TRUE)), Mean_Weighted_Lat_LWR=sum(Weighted_Lat_LWR, na.rm=TRUE)/(sum(PA_LWR, na.rm=TRUE)),Mean_Weighted_Lat_UPR=sum(Weighted_Lat_UPR, na.rm=TRUE)/(sum(PA_UPR, na.rm=TRUE)) )
Annual_Centroid_Locations_AP_Fixed<-ddply(Grouped_Data_Full_AP, .(Year1), summarize, Mean_Weighted_Lat=sum(Weighted_Lat_Mean_Fixed, na.rm=TRUE)/(sum(PA_Fixed, na.rm=TRUE)), Mean_Weighted_Lat_LWR=sum(Weighted_Lat_LWR_Fixed, na.rm=TRUE)/(sum(PA_Fixed_LWR, na.rm=TRUE)),Mean_Weighted_Lat_UPR=sum(Weighted_Lat_UPR_Fixed, na.rm=TRUE)/(sum(PA_Fixed_UPR, na.rm=TRUE)))

#Test relationship between lat-lon and year 
Lat_AP_Spring<-lm(Mean_Weighted_Lat~Year1, data=Annual_Centroid_Locations_AP)
summary(Lat_AP_Spring)

Lat_AP_Spring_Fixed<-lm(Mean_Weighted_Lat~Year1, data=Annual_Centroid_Locations_AP_Fixed)
summary(Lat_AP_Spring_Fixed)

tiff("Alewife_Mean_Location_PA_Comparison_210714.tif", height=5, width=6, res=300, units="in")
par(mar=c(4,6,4,3))
plot(Annual_Centroid_Locations_AP$Year1, Annual_Centroid_Locations_AP$Mean_Weighted_Lat, type="l", lwd=4,lty=1, xlab="Year", ylab="Mean Weighted \nLatitude", main="Alewife",cex.lab=1.2, ylim=c(41.4,42.1), col="black")
polygon(x=c(Annual_Centroid_Locations_AP$Year1,rev(Annual_Centroid_Locations_AP$Year1)), y=c(Annual_Centroid_Locations_AP$Mean_Weighted_Lat_LWR,rev(Annual_Centroid_Locations_AP$Mean_Weighted_Lat_UPR)), border=NA, col= adjustcolor("gray", alpha.f=0.5))
par(new=T)
plot(Annual_Centroid_Locations_AP$Year1, Annual_Centroid_Locations_AP$Mean_Weighted_Lat, type="l", lwd=4,lty=1, xlab="Year", ylab="Mean Weighted \nLatitude", main="Alewife",cex.lab=1.2, ylim=c(41.4, 42.1), col="black")
polygon(x=c(Annual_Centroid_Locations_AP_Fixed$Year1,rev(Annual_Centroid_Locations_AP_Fixed$Year1)),y=c(Annual_Centroid_Locations_AP_Fixed$Mean_Weighted_Lat_LWR,rev(Annual_Centroid_Locations_AP_Fixed$Mean_Weighted_Lat_UPR)), border=NA, col= adjustcolor("indianred", alpha.f=0.2))
par(new=T)
plot(Annual_Centroid_Locations_AP_Fixed$Year1, Annual_Centroid_Locations_AP_Fixed$Mean_Weighted_Lat, type="l", lwd=4,lty=1, xlab="Year", ylab="Mean Weighted \nLatitude", main="Alewife",cex.lab=1.2, ylim=c(41.4,42.1), col="red")
dev.off()


Grouped_Data_Full_AP$Weighted_Depth_Fixed<-Grouped_Data_Full_AP$Depth*Grouped_Data_Full_AP$PA_Fixed
Grouped_Data_Full_AP$Weighted_Depth_Fixed_LWR<-Grouped_Data_Full_AP$Depth*Grouped_Data_Full_AP$PA_Fixed_LWR
Grouped_Data_Full_AP$Weighted_Depth_Fixed_UPR<-Grouped_Data_Full_AP$Depth*Grouped_Data_Full_AP$PA_Fixed_UPR

Grouped_Data_Full_AP$Weighted_Depth<-Grouped_Data_Full_AP$Depth*Grouped_Data_Full_AP$PA
Grouped_Data_Full_AP$Weighted_Depth_LWR<-Grouped_Data_Full_AP$Depth*Grouped_Data_Full_AP$PA_LWR
Grouped_Data_Full_AP$Weighted_Depth_UPR<-Grouped_Data_Full_AP$Depth*Grouped_Data_Full_AP$PA_UPR



library(plyr)
Annual_Centroid_Depth_AP<-ddply(Grouped_Data_Full_AP, .(Year1), summarize, Mean_Weighted_Depth=sum(Weighted_Depth, na.rm=TRUE)/(sum(PA, na.rm=TRUE)),  Mean_Weighted_Depth_LWR=sum(Weighted_Depth_LWR, na.rm=TRUE)/(sum(PA_LWR, na.rm=TRUE)),  Mean_Weighted_Depth_UPR=sum(Weighted_Depth_UPR, na.rm=TRUE)/(sum(PA_UPR, na.rm=TRUE)))
Annual_Centroid_Depth_AP_Fixed<-ddply(Grouped_Data_Full_AP, .(Year1), summarize, Mean_Weighted_Depth=sum(Weighted_Depth_Fixed, na.rm=TRUE)/(sum(PA_Fixed, na.rm=TRUE)),  Mean_Weighted_Depth_LWR=sum(Weighted_Depth_Fixed_LWR, na.rm=TRUE)/(sum(PA_Fixed_LWR, na.rm=TRUE)),  Mean_Weighted_Depth_UPR=sum(Weighted_Depth_Fixed_UPR, na.rm=TRUE)/(sum(PA_Fixed_UPR, na.rm=TRUE)))

#test for correlation between depth and year for suitability and occupancy 
AP_Depth_Occupancy_Spring<-lm(Mean_Weighted_Depth~Year1, data=Annual_Centroid_Depth_AP)
summary(AP_Depth_Occupancy_Spring)

AP_Depth_Suitability_Spring<-lm(Mean_Weighted_Depth~Year1, data=Annual_Centroid_Depth_AP_Fixed)
summary(AP_Depth_Suitability_Spring)

tiff("Alewife_Mean_Depth_PA_Comparison_210714_Regridded.tif", height=5, width=6, res=300, units="in")
par(mar=c(4,6,4,3))
plot(Annual_Centroid_Depth_AP$Year1, (Annual_Centroid_Depth_AP$Mean_Weighted_Depth)*-1, type="l", lwd=4,lty=1, xlab="Year", ylab="Mean Weighted \nDepth (m)", main="Alewife",cex.lab=1.2, ylim=rev(c(105,125)), col="black")
polygon(x=c(Annual_Centroid_Depth_AP$Year1,rev(Annual_Centroid_Depth_AP$Year1)), y=c((Annual_Centroid_Depth_AP$Mean_Weighted_Depth_LWR)*-1,rev((Annual_Centroid_Depth_AP$Mean_Weighted_Depth_UPR)*-1)), border=NA, col= adjustcolor("gray", alpha.f=0.5))
par(new=T)
plot(Annual_Centroid_Depth_AP$Year1, (Annual_Centroid_Depth_AP$Mean_Weighted_Depth)*-1, type="l", lwd=4,lty=1, xlab="Year", ylab="Mean Weighted \nDepth (m)", main="Alewife",cex.lab=1.2, ylim=rev(c(105,125)), col="black")
polygon(x=c(Annual_Centroid_Depth_AP_Fixed$Year1,rev(Annual_Centroid_Depth_AP_Fixed$Year1)), y=c((Annual_Centroid_Depth_AP_Fixed$Mean_Weighted_Depth_LWR)*-1,rev((Annual_Centroid_Depth_AP_Fixed$Mean_Weighted_Depth_UPR)*-1)), border=NA, col= adjustcolor("indianred", alpha.f=0.2))
par(new=T)
plot(Annual_Centroid_Depth_AP_Fixed$Year1, (Annual_Centroid_Depth_AP_Fixed$Mean_Weighted_Depth)*-1, type="l", lwd=4,lty=1, xlab="Year", ylab="Mean Weighted \nDepth (m)", main="Alewife",cex.lab=1.2, ylim=rev(c(105,125)), col="red")
dev.off()

#response curves 

Best_AP_PA_GAM<-gam(PA~s(SSS_Use, k=6)+s(SST_Use,k=6)+s(EKE, k=6)+s(Slope_try, k=6)+Gear+s(Year, bs="re")+te(Longitude, Latitude)+s(BT_Use, k=6),family=binomial,
                       data=Complete_AP_PA, na.action = "na.fail")


#View(AP_PA_Dredge)

tiff("Alewife_bottom_temp_210630.tif", height=5, width=5, units="in", res=300)
plot(Best_AP_PA_GAM,trans=function(x)exp(x)/(1+exp(x)), shade=T,ylab="Probability of Occurrence",xlab=expression(paste("Bottom Temperature ("^"o","C)" )), cex.lab=1.5, cex.axis=1.5)
dev.off()

tiff("Alewife_bottom_temp_Supp_210630.tif", height=5, width=5, units="in", res=300)
plot(Best_AP_PA_GAM, shade=T,xlab=expression(paste("Bottom Temperature ("^"o","C)" )), cex.lab=1.5, cex.axis=1.5)
dev.off()

Best_AP_PA_GAM<-gam(PA~s(SSS_Use, k=6)+s(EKE, k=6)+s(Slope_try, k=6)+Gear+s(Year, bs="re")+te(Longitude, Latitude)+s(BT_Use, k=6)+s(SST_Use,k=6),family=binomial,
                    data=Complete_AP_PA, na.action = "na.fail")

tiff("Alewife_SST_210630.tif", height=5, width=5, units="in", res=300)
plot(Best_AP_PA_GAM,trans=function(x)exp(x)/(1+exp(x)), shade=T,xlab=expression(paste("Sea Surface Temperature ("^"o","C)")), ylab="Probability of Occurrence", cex.lab=1.5, cex.axis=1.5 )
dev.off()

tiff("Alewife_SST_Supp_210630.tif", height=5, width=5, units="in", res=300)
plot(Best_AP_PA_GAM, shade=T,xlab=expression(paste("Sea Surface Temperature ("^"o","C)")), cex.lab=1.5, cex.axis=1.5 )
dev.off()

Best_AP_PA_GAM<-gam(PA~s(SSS_Use, k=6)+s(Slope_try, k=6)+Gear+s(Year, bs="re")+te(Longitude, Latitude)+s(BT_Use, k=6)+s(SST_Use,k=6)+s(EKE, k=6),family=binomial,
                    data=Complete_AP_PA, na.action = "na.fail")
tiff("Alewife_EKE_210630.tif", height=5, width=5, units="in", res=300)
plot(Best_AP_PA_GAM,trans=function(x)exp(x)/(1+exp(x)), shade=T,ylab="Probability of Occurrence",xlab=expression(paste("Mean kinetic energy (cm"^"-2","s"^"-2",")")),cex.lab=1.5, cex.axis=1.5 )
dev.off()

tiff("Alewife_EKE_Supp_210630.tif", height=5, width=5, units="in", res=300)
plot(Best_AP_PA_GAM,shade=T,xlab=expression(paste("Mean kinetic energy (cm"^"-2","s"^"-2",")")),cex.lab=1.5, cex.axis=1.5 )
dev.off()

Best_AP_PA_GAM_RE<-gam(PA~s(SSS_Use, k=6)+s(Slope_try, k=6)+Gear+te(Longitude, Latitude)+s(BT_Use, k=6)+s(SST_Use,k=6)+s(EKE, k=6)+s(Year, bs="re"),family=binomial,
                    data=Complete_AP_PA, na.action = "na.fail")

setwd("D:/Sand_Lance/SL_Chapter_2/Ch2_Manuscript/Revisions_Round_One/Updated_Temporary_Figure/Spring/RE")
tiff("Alewife_Spring_RE.tif", height=5, width=5, units="in", res=300)
plot(Best_AP_PA_GAM_RE,cex.lab=1.5,cex.axis=1.5, main="Alewife")
dev.off()


Gridded_Final_Values_AP_Spring<-ddply(Grouped_Data_Full_AP[Grouped_Data_Full_AP$Year1>=2012,], .(Latitude, Longitude), summarize, Final_Probability=mean(PA_Fixed))
Gridded_Initial_Values_AP_Spring<-ddply(Grouped_Data_Full_AP[Grouped_Data_Full_AP$Year1<=1983,], .(Latitude, Longitude), summarize, Initial_Probability=mean(PA_Fixed))

AP_Spring_Space<-cbind(Gridded_Final_Values_AP_Spring, Gridded_Initial_Values_AP_Spring)
AP_Spring_Space<-AP_Spring_Space[,!duplicated(colnames(AP_Spring_Space), fromLast = TRUE)]

AP_Spring_Space$Occurrence_Diff<-AP_Spring_Space$Final_Probability-AP_Spring_Space$Initial_Probability

tiff("Spring_Alewife_Spatial_Difference_Five_Yrs.tif", height=5, width=6, units="in", res=300)
ggplot(data=world) + geom_raster(data = AP_Spring_Space, aes(x=Longitude, y = Latitude, fill=Occurrence_Diff),linejoin = "mitre") + 
  coord_fixed(ratio = 1) +
  scale_fill_gradientn(colours=c("#0000FFFF","#F1ECE4","#FF0000FF"), limits=c(-.4,0.4)) +
  theme_bw()+ggtitle("Alewife")+labs(fill = "Diff. in Occurr.")+geom_sf()+coord_sf(xlim=c(-75.5,-64), ylim=c(36, 44.5))+
  theme(plot.title = element_text(size=18,face="bold"), legend.title = element_text(size=16,face="bold"), legend.text = element_text(size=12, face="bold"))

dev.off()


######blueback#########
Grouped_Data_Split_AA<-Grouped_Data_Split_Full
Grouped_Data_Split_AA$Year<-as.factor(as.character(Grouped_Data_Split_AA$Year))
Grouped_Data_Split_AA$Longitude<-as.numeric(as.character(Grouped_Data_Split_AA$Longitude))
Grouped_Data_Split_AA$Latitude<-as.numeric(as.character(Grouped_Data_Split_AA$Latitude))

#split into gear blocks (A=pre door change, B=post door change, C=Bigelow)
for (i in 1:nrow(Grouped_Data_Split_AA)){
  if (Grouped_Data_Split_AA$Year1[i]<1985){
    Grouped_Data_Split_AA$Gear[i]="A"}
  else if (Grouped_Data_Split_AA$Year1[i]>=1985 & Grouped_Data_Split_AA$Year1[i]<=2008){
    Grouped_Data_Split_AA$Gear[i]="B"}
  else if (Grouped_Data_Split_AA$Year1[i]>2008){
    Grouped_Data_Split_AA$Gear[i]="C"
  }
}
#fake years based on abudance for intercept
Grouped_Data_Split_AA$Year[Grouped_Data_Split_AA$Year==2016]<-"2012"
Grouped_Data_Split_AA$Year[Grouped_Data_Split_AA$Year==2015]<-"2010"
Grouped_Data_Split_AA$Year[Grouped_Data_Split_AA$Year==2014]<-"2011"

#predict occupancy
AA_Occupancy<-as.data.frame(predict(Trimmed_Aa_Gam,Grouped_Data_Split_AA,type="link",se.fit=T))
AA_Occupancy$PA<-exp(AA_Occupancy$fit)/(1+exp(AA_Occupancy$fit))
AA_Occupancy$PA_LWR<-exp(AA_Occupancy$fit-(1.96*AA_Occupancy$se.fit))/(1+exp(AA_Occupancy$fit-(1.96*AA_Occupancy$se.fit)))
AA_Occupancy$PA_UPR<-exp(AA_Occupancy$fit+(1.96*AA_Occupancy$se.fit))/(1+exp(AA_Occupancy$fit+(1.96*AA_Occupancy$se.fit)))

#predict habitat suitability 
Grouped_Data_Split_AA2<-Grouped_Data_Split_AA
Grouped_Data_Split_AA2$Gear<-"B"
AA_Partial_Effects<-as.data.frame(predict(Trimmed_Aa_Gam,Grouped_Data_Split_AA2,exclude=c("Gear","s(Year)"),type="link", se.fit=T))
AA_Partial_Effects$PA_Fixed<-exp(AA_Partial_Effects$fit)/(1+exp(AA_Partial_Effects$fit))
AA_Partial_Effects$PA_Fixed_LWR<-exp(AA_Partial_Effects$fit-(1.96*AA_Partial_Effects$se.fit))/(1+exp(AA_Partial_Effects$fit-(1.96*AA_Partial_Effects$se.fit)))
AA_Partial_Effects$PA_Fixed_UPR<-exp(AA_Partial_Effects$fit+(1.96*AA_Partial_Effects$se.fit))/(1+exp(AA_Partial_Effects$fit+(1.96*AA_Partial_Effects$se.fit)))

Grouped_Data_Split_AA3<-Grouped_Data_Split_AA
Grouped_Data_Split_AA3$Gear<-"B"
AA_No_Vess_Effects<-as.data.frame(predict(Trimmed_Aa_Gam,Grouped_Data_Split_AA3,exclude=c("Gear"),type="link", se.fit=T))
AA_No_Vess_Effects$PA_Vessel<-exp(AA_No_Vess_Effects$fit)/(1+exp(AA_No_Vess_Effects$fit))
AA_No_Vess_Effects$PA_Vessel_LWR<-exp(AA_No_Vess_Effects$fit-(1.96*AA_No_Vess_Effects$se.fit))/(1+exp(AA_No_Vess_Effects$fit-(1.96*AA_No_Vess_Effects$se.fit)))
AA_No_Vess_Effects$PA_Vessel_UPR<-exp(AA_No_Vess_Effects$fit+(1.96*AA_No_Vess_Effects$se.fit))/(1+exp(AA_No_Vess_Effects$fit+(1.96*AA_No_Vess_Effects$se.fit)))

Grouped_Data_Full_AA<-cbind(Grouped_Data_Split_AA, AA_Occupancy, AA_Partial_Effects, AA_No_Vess_Effects)

library(plyr)
#set occurrence to mean prevalance
Annual_AA_Prev=ddply(Aa_Matched, .(Year), summarize, Prev=sum(PA)/length(PA))
AA_Spring_Prevalence=mean(Annual_AA_Prev$Prev)

Grouped_Data_Full_AA<-Grouped_Data_Full_AA[,-which(names(Grouped_Data_Full_AA) %in% c("fit","se.fit"))]

#shelf occupancy
AnnualAA_Occupancy<-ddply(Grouped_Data_Full_AA, .(Year1), summarize, Mean_hab_Prop=length(which(PA>AA_Spring_Prevalence ))/length(which(!is.na(PA))), Low_hap_prop=length(which(PA_LWR>AA_Spring_Prevalence ))/length(which(!is.na(PA))),High_hap_prop=length(which(PA_UPR>AA_Spring_Prevalence ))/length(which(!is.na(PA))) )

#habitat suitability
AnnualAA_Partial_Effects<-ddply(Grouped_Data_Full_AA, .(Year1), summarize, Mean_hab_Prop=(length(which(PA_Fixed>AA_Spring_Prevalence )))/(length(which(!is.na(PA_Fixed)))), Low_hap_prop=(length(which(PA_Fixed_LWR>AA_Spring_Prevalence )))/(length(which(!is.na(PA_Fixed)))),High_hap_prop=(length(which(PA_Fixed_UPR>AA_Spring_Prevalence )))/(length(which(!is.na(PA_Fixed)))))

AnnualAA_No_Vess_Effects<-ddply(Grouped_Data_Full_AA, .(Year1), summarize, Mean_hab_Prop=(length(which(PA_Vessel>AA_Spring_Prevalence )))/(length(which(!is.na(PA_Vessel)))), Low_hap_prop=(length(which(PA_Vessel_LWR>AA_Spring_Prevalence )))/(length(which(!is.na(PA_Vessel)))),High_hap_prop=(length(which(PA_Vessel_UPR>AA_Spring_Prevalence )))/(length(which(!is.na(PA_Vessel)))))

#make annual averages for plotting suitability
Annual_Averages_AA<-ddply(Grouped_Data_Full_AA, .(Longitude, Latitude), summarize, Mean_PA=mean(PA))


world<-ne_countries(scale="medium", returnclass = "sf")
tiff("Blueback_Model_Pred_210712.tif", width=6, height=5, units="in", res=300)
ggplot(data=world) + geom_raster(data = Annual_Averages_AA, aes(x=Longitude, y = Latitude, fill=Mean_PA),linejoin = "mitre") + 
  coord_fixed(ratio = 1) +
  scale_fill_viridis(direction = 1) +
  theme_bw()+ggtitle("Blueback herring")+labs(fill = "Prob. Occurrence")+geom_sf()+coord_sf(xlim=c(-75.5,-64), ylim=c(36, 44.5))
dev.off()


tiff("Blueback_Occupiable_Habitat_210714.tif", height=5, width=6, res=300, units="in")
par(mar=c(4,6,4,3))
plot(AnnualAA_Occupancy$Year1, AnnualAA_Occupancy$Mean_hab_Prop, type="l", lwd=4, xlab="Year", ylab="Proportion of \nOccupiable Shelf", main="Blueback herring",cex.lab=1.2, ylim=c(0.0,0.7), col="black")
polygon(x=c(AnnualAA_Occupancy$Year1,rev(AnnualAA_Occupancy$Year1)),y=c(AnnualAA_Occupancy$Low_hap_prop,rev(AnnualAA_Occupancy$High_hap_prop)), border=NA, col= adjustcolor("gray", alpha.f=0.5))
par(new=T)
plot(AnnualAA_Occupancy$Year1, AnnualAA_Occupancy$Mean_hab_Prop, type="l", lwd=4, xlab="Year", ylab="Proportion of \nOccupiable Shelf", main="Blueback herring",cex.lab=1.2, ylim=c(0.0,0.7), col="black")
dev.off()

tiff("Blueback_Occupiable_Habitat_210831_Threshold_Comparison.tif", height=5, width=6, res=300, units="in")
par(mar=c(4,6,4,3))
plot(AnnualAA_Occupancy$Year1, AnnualAA_Occupancy$Mean_hab_Prop, type="l", lwd=4, xlab="Year", ylab="Proportion of \nOccupiable Shelf", main="Blueback herring",cex.lab=1.2, ylim=c(0.0,0.7), col="black")
polygon(x=c(AnnualAA_Occupancy$Year1,rev(AnnualAA_Occupancy$Year1)),y=c(AnnualAA_Occupancy$Low_hap_prop,rev(AnnualAA_Occupancy$High_hap_prop)), border=NA, col= adjustcolor("gray", alpha.f=0.5))
par(new=T)
plot(AnnualAA_Occupancy$Year1, AnnualAA_Occupancy$Mean_hab_Prop, type="l", lwd=4, xlab="Year", ylab="Proportion of \nOccupiable Shelf", main="Blueback herring",cex.lab=1.2, ylim=c(0.0,0.7), col="black")
polygon(x=c(AnnualAA_No_Vess_Effects$Year1,rev(AnnualAA_No_Vess_Effects$Year1)),y=c(AnnualAA_No_Vess_Effects$Low_hap_prop,rev(AnnualAA_No_Vess_Effects$High_hap_prop)), border=NA, col= adjustcolor("blue", alpha.f=0.25))
par(new=T)
plot(AnnualAA_No_Vess_Effects$Year1, AnnualAA_No_Vess_Effects$Mean_hab_Prop, type="l", lwd=4,lty=1, xlab="Year", ylab="Proportion of \nOccupiable Shelf", main="Blueback herring",cex.lab=1.2, ylim=c(0.0,0.7), col="blue")
polygon(x=c(AnnualAA_Partial_Effects$Year1,rev(AnnualAA_Partial_Effects$Year1)),y=c(AnnualAA_Partial_Effects$Low_hap_prop,rev(AnnualAA_Partial_Effects$High_hap_prop)), border=NA, col= adjustcolor("indianred", alpha.f=0.25))
par(new=T)
plot(AnnualAA_Partial_Effects$Year1, AnnualAA_Partial_Effects$Mean_hab_Prop, type="l", lwd=4,lty=1, xlab="Year", ylab="Proportion of \nOccupiable Shelf", main="Blueback herring",cex.lab=1.2, ylim=c(0.0,0.7), col="red")
dev.off()

#calculate trends in suitability and occupancy using betaregression 
library(betareg)
AnnualAA_Occupancy$Mean_hab_Prop<-AnnualAA_Occupancy$Mean_hab_Prop+0.00000001
AA_Spring_Full_Hab_Occ<-betareg(AnnualAA_Occupancy$Mean_hab_Prop~AnnualAA_Occupancy$Year1)
summary(AA_Spring_Full_Hab_Occ)

AA_Spring_Fixed_Hab_Occ<-betareg(AnnualAA_Partial_Effects$Mean_hab_Prop~AnnualAA_Partial_Effects$Year1)
summary(AA_Spring_Fixed_Hab_Occ)




#look at trends with latitude and depth
#Calculate weighted lat and lon
Grouped_Data_Full_AA$Weighted_Lat_Mean<-Grouped_Data_Full_AA$Latitude*Grouped_Data_Full_AA$PA
Grouped_Data_Full_AA$Weighted_Lat_LWR<-Grouped_Data_Full_AA$Latitude*Grouped_Data_Full_AA$PA_LWR
Grouped_Data_Full_AA$Weighted_Lat_UPR<-Grouped_Data_Full_AA$Latitude*Grouped_Data_Full_AA$PA_UPR

Grouped_Data_Full_AA$Weighted_Lat_Mean_Fixed<-Grouped_Data_Full_AA$Latitude*Grouped_Data_Full_AA$PA_Fixed
Grouped_Data_Full_AA$Weighted_Lat_LWR_Fixed<-Grouped_Data_Full_AA$Latitude*Grouped_Data_Full_AA$PA_Fixed_LWR
Grouped_Data_Full_AA$Weighted_Lat_UPR_Fixed<-Grouped_Data_Full_AA$Latitude*Grouped_Data_Full_AA$PA_Fixed_UPR


#divide these by sum of abundance for each year and it will work
library(plyr)
Annual_Centroid_Locations_AA<-ddply(Grouped_Data_Full_AA, .(Year1), summarize, Mean_Weighted_Lat=sum(Weighted_Lat_Mean, na.rm=TRUE)/(sum(PA, na.rm=TRUE)), Mean_Weighted_Lat_LWR=sum(Weighted_Lat_LWR, na.rm=TRUE)/(sum(PA_LWR, na.rm=TRUE)),Mean_Weighted_Lat_UPR=sum(Weighted_Lat_UPR, na.rm=TRUE)/(sum(PA_UPR, na.rm=TRUE)) )
Annual_Centroid_Locations_AA_Fixed<-ddply(Grouped_Data_Full_AA, .(Year1), summarize, Mean_Weighted_Lat=sum(Weighted_Lat_Mean_Fixed, na.rm=TRUE)/(sum(PA_Fixed, na.rm=TRUE)), Mean_Weighted_Lat_LWR=sum(Weighted_Lat_LWR_Fixed, na.rm=TRUE)/(sum(PA_Fixed_LWR, na.rm=TRUE)),Mean_Weighted_Lat_UPR=sum(Weighted_Lat_UPR_Fixed, na.rm=TRUE)/(sum(PA_Fixed_UPR, na.rm=TRUE)))



#Test relationship between lat-lon and year 
Lat_AA_Spring<-lm(Mean_Weighted_Lat~Year1, data=Annual_Centroid_Locations_AA)
summary(Lat_AA_Spring)

Lat_AA_Spring_Fixed<-lm(Mean_Weighted_Lat~Year1, data=Annual_Centroid_Locations_AA_Fixed)
summary(Lat_AA_Spring_Fixed)

tiff("Blueback_Mean_Location_PA_Comparison_210714.tif", height=5, width=6, res=300, units="in")
par(mar=c(4,6,4,3))
plot(Annual_Centroid_Locations_AA$Year1, Annual_Centroid_Locations_AA$Mean_Weighted_Lat, type="l", lwd=4,lty=3, xlab="Year", ylab="Mean Weighted \nLatitude", main="Blueback herring",cex.lab=1.2, ylim=c(40.6,41.9), col="black")
polygon(x=c(Annual_Centroid_Locations_AA$Year1,rev(Annual_Centroid_Locations_AA$Year1)), y=c(Annual_Centroid_Locations_AA$Mean_Weighted_Lat_LWR,rev(Annual_Centroid_Locations_AA$Mean_Weighted_Lat_UPR)), border=NA, col= adjustcolor("gray", alpha.f=0.5))
par(new=T)
plot(Annual_Centroid_Locations_AA$Year1, Annual_Centroid_Locations_AA$Mean_Weighted_Lat, type="l", lwd=4,lty=3, xlab="Year", ylab="Mean Weighted \nLatitude", main="Blueback herring",cex.lab=1.2, ylim=c(40.6,41.9), col="black")
polygon(x=c(Annual_Centroid_Locations_AA_Fixed$Year1,rev(Annual_Centroid_Locations_AA_Fixed$Year1)),y=c(Annual_Centroid_Locations_AA_Fixed$Mean_Weighted_Lat_LWR,rev(Annual_Centroid_Locations_AA_Fixed$Mean_Weighted_Lat_UPR)), border=NA, col= adjustcolor("indianred", alpha.f=0.2))
par(new=T)
plot(Annual_Centroid_Locations_AA_Fixed$Year1, Annual_Centroid_Locations_AA_Fixed$Mean_Weighted_Lat, type="l", lwd=4,lty=3, xlab="Year", ylab="Mean Weighted \nLatitude", main="Blueback herring",cex.lab=1.2, ylim=c(40.6,41.9), col="red")
dev.off()


Grouped_Data_Full_AA$Weighted_Depth_Fixed<-Grouped_Data_Full_AA$Depth*Grouped_Data_Full_AA$PA_Fixed
Grouped_Data_Full_AA$Weighted_Depth_Fixed_LWR<-Grouped_Data_Full_AA$Depth*Grouped_Data_Full_AA$PA_Fixed_LWR
Grouped_Data_Full_AA$Weighted_Depth_Fixed_UPR<-Grouped_Data_Full_AA$Depth*Grouped_Data_Full_AA$PA_Fixed_UPR

Grouped_Data_Full_AA$Weighted_Depth<-Grouped_Data_Full_AA$Depth*Grouped_Data_Full_AA$PA
Grouped_Data_Full_AA$Weighted_Depth_LWR<-Grouped_Data_Full_AA$Depth*Grouped_Data_Full_AA$PA_LWR
Grouped_Data_Full_AA$Weighted_Depth_UPR<-Grouped_Data_Full_AA$Depth*Grouped_Data_Full_AA$PA_UPR



library(plyr)
Annual_Centroid_Depth_AA<-ddply(Grouped_Data_Full_AA, .(Year1), summarize, Mean_Weighted_Depth=sum(Weighted_Depth, na.rm=TRUE)/(sum(PA, na.rm=TRUE)),  Mean_Weighted_Depth_LWR=sum(Weighted_Depth_LWR, na.rm=TRUE)/(sum(PA_LWR, na.rm=TRUE)),  Mean_Weighted_Depth_UPR=sum(Weighted_Depth_UPR, na.rm=TRUE)/(sum(PA_UPR, na.rm=TRUE)))
Annual_Centroid_Depth_AA_Fixed<-ddply(Grouped_Data_Full_AA, .(Year1), summarize, Mean_Weighted_Depth=sum(Weighted_Depth_Fixed, na.rm=TRUE)/(sum(PA_Fixed, na.rm=TRUE)),  Mean_Weighted_Depth_LWR=sum(Weighted_Depth_Fixed_LWR, na.rm=TRUE)/(sum(PA_Fixed_LWR, na.rm=TRUE)),  Mean_Weighted_Depth_UPR=sum(Weighted_Depth_Fixed_UPR, na.rm=TRUE)/(sum(PA_Fixed_UPR, na.rm=TRUE)))

#test for correlation between depth and year for suitability and occupancy 
AA_Depth_Occupancy_Spring<-lm(Mean_Weighted_Depth~Year1, data=Annual_Centroid_Depth_AA)
summary(AA_Depth_Occupancy_Spring)

AA_Depth_Suitability_Spring<-lm(Mean_Weighted_Depth~Year1, data=Annual_Centroid_Depth_AA_Fixed)
summary(AA_Depth_Suitability_Spring)

tiff("Blueback_Mean_Depth_PA_Comparison_210714_Regridded.tif", height=5, width=6, res=300, units="in")
par(mar=c(4,6,4,3))
plot(Annual_Centroid_Depth_AA$Year1, (Annual_Centroid_Depth_AA$Mean_Weighted_Depth)*-1, type="l", lwd=4,lty=1, xlab="Year", ylab="Mean Weighted \nDepth (m)", main="Blueback herring",cex.lab=1.2, ylim=rev(c(75,105)), col="black")
polygon(x=c(Annual_Centroid_Depth_AA$Year1,rev(Annual_Centroid_Depth_AA$Year1)), y=c((Annual_Centroid_Depth_AA$Mean_Weighted_Depth_LWR)*-1,rev((Annual_Centroid_Depth_AA$Mean_Weighted_Depth_UPR)*-1)), border=NA, col= adjustcolor("gray", alpha.f=0.5))
par(new=T)
plot(Annual_Centroid_Depth_AA$Year1, (Annual_Centroid_Depth_AA$Mean_Weighted_Depth)*-1, type="l", lwd=4,lty=1, xlab="Year", ylab="Mean Weighted \nDepth (m)", main="Blueback herring",cex.lab=1.2, ylim=rev(c(75,105)), col="black")
polygon(x=c(Annual_Centroid_Depth_AA_Fixed$Year1,rev(Annual_Centroid_Depth_AA_Fixed$Year1)), y=c((Annual_Centroid_Depth_AA_Fixed$Mean_Weighted_Depth_LWR)*-1,rev((Annual_Centroid_Depth_AA_Fixed$Mean_Weighted_Depth_UPR)*-1)), border=NA, col= adjustcolor("indianred", alpha.f=0.2))
par(new=T)
plot(Annual_Centroid_Depth_AA_Fixed$Year1, (Annual_Centroid_Depth_AA_Fixed$Mean_Weighted_Depth)*-1, type="l", lwd=4,lty=1, xlab="Year", ylab="Mean Weighted \nDepth (m)", main="Blueback herring",cex.lab=1.2, ylim=rev(c(75,105)), col="red")
dev.off()

Best_Aa_Gam<-gam(PA~s(SSS_Use, k=6)+s(Estimated_Grain_Size, k=6)+Gear+s(Year, bs="re")+te(Longitude, Latitude)+s(SST_Use, k=6), family=binomial, data=Complete_Aa_PA, na.action = "na.fail")



tiff("Blueback_SST_210630.tif", height=5, width=5, units="in", res=300)
plot(Best_Aa_Gam,trans=function(x)exp(x)/(1+exp(x)), shade=T,xlab=expression(paste("Sea Surface Temperature ("^"o","C)")), ylab="Probability of Occurrence", cex.lab=1.5, cex.axis=1.5)
dev.off()

tiff("Blueback_SST_Supp_210630.tif", height=5, width=5, units="in", res=300)
plot(Best_Aa_Gam, shade=T,xlab=expression(paste("Sea Surface Temperature ("^"o","C)")), cex.lab=1.5, cex.axis=1.5)
dev.off()

Best_Aa_Gam<-gam(PA~s(Estimated_Grain_Size, k=6)+Gear+s(Year, bs="re")+te(Longitude, Latitude)+s(SST_Use, k=6)+s(SSS_Use, k=6), family=binomial, data=Complete_Aa_PA, na.action = "na.fail")



tiff("Blueback_SSS_210630.tif", height=5, width=5, units="in", res=300)
plot(Best_Aa_Gam,trans=function(x)exp(x)/(1+exp(x)), shade=T,xlab="Sea Surface Salinity", ylab="Probability of Occurrence", cex.lab=1.5, cex.axis=1.5)
dev.off()

tiff("Blueback_SSS_Supp_210630.tif", height=5, width=5, units="in", res=300)
plot(Best_Aa_Gam, shade=T,xlab="Sea Surface Salinity", cex.lab=1.5, cex.axis=1.5)
dev.off()

Best_Aa_Gam<-gam(PA~Gear+s(Year, bs="re")+te(Longitude, Latitude)+s(SST_Use, k=6)+s(SSS_Use, k=6)+s(Estimated_Grain_Size, k=6), family=binomial, data=Complete_Aa_PA, na.action = "na.fail")

tiff("Blueback_Grain_210630.tif", height=5, width=5, units="in", res=300)
plot(Best_Aa_Gam,trans=function(x)exp(x)/(1+exp(x)), shade=T,xlab=expression(paste("Grain Size (", phi, ")")), ylab="Probability of Occurrence", cex.lab=1.5, cex.axis=1.5)
dev.off()

tiff("Blueback_Grain_Supp_210630.tif", height=5, width=5, units="in", res=300)
plot(Best_Aa_Gam, shade=T,xlab=expression(paste("Grain Size (", phi, ")")), cex.lab=1.5, cex.axis=1.5)
dev.off()

Best_Aa_GAM_RE<-gam(PA~Gear+te(Longitude, Latitude)+s(SST_Use, k=6)+s(SSS_Use, k=6)+s(Estimated_Grain_Size, k=6)+s(Year, bs="re"), family=binomial, data=Complete_Aa_PA, na.action = "na.fail")

setwd("D:/Sand_Lance/SL_Chapter_2/Ch2_Manuscript/Revisions_Round_One/Updated_Temporary_Figure/Spring/RE")
tiff("Blueback_Spring_RE.tif", height=5, width=5, units="in", res=300)
plot(Best_Aa_GAM_RE,cex.lab=1.5,cex.axis=1.5, main="Blueback herring")
dev.off()

Gridded_Final_Values_AA_Spring<-ddply(Grouped_Data_Full_AA[Grouped_Data_Full_AA$Year1>=2012,], .(Latitude, Longitude), summarize, Final_Probability=mean(PA_Fixed))
Gridded_Initial_Values_AA_Spring<-ddply(Grouped_Data_Full_AA[Grouped_Data_Full_AA$Year1<=1983,], .(Latitude, Longitude), summarize, Initial_Probability=mean(PA_Fixed))

AA_Spring_Space<-cbind(Gridded_Final_Values_AA_Spring, Gridded_Initial_Values_AA_Spring)
AA_Spring_Space<-AA_Spring_Space[,!duplicated(colnames(AA_Spring_Space), fromLast = TRUE)]

AA_Spring_Space$Occurrence_Diff<-AA_Spring_Space$Final_Probability-AA_Spring_Space$Initial_Probability

tiff("Spring_Blueback_Spatial_Difference_Five_Yrs.tif", height=5, width=6, units="in", res=300)
ggplot(data=world) + geom_raster(data = AA_Spring_Space, aes(x=Longitude, y = Latitude, fill=Occurrence_Diff),linejoin = "mitre") + 
  coord_fixed(ratio = 1) +
  scale_fill_gradientn(colours=c("#0000FFFF","#F1ECE4","#FF0000FF"), limits=c(-0.4,0.4)) +
  theme_bw()+ggtitle("Blueback herring")+labs(fill = "Diff. in Occurr.")+geom_sf()+coord_sf(xlim=c(-75.5,-64), ylim=c(36, 44.5))+
  theme(plot.title = element_text(size=18,face="bold"), legend.title = element_text(size=16,face="bold"), legend.text = element_text(size=12, face="bold"))
dev.off()


###########atl mackerel################
Grouped_Data_Split_SS<-Grouped_Data_Split_Full
Grouped_Data_Split_SS$Year<-as.factor(as.character(Grouped_Data_Split_SS$Year))
Grouped_Data_Split_SS$Year1<-as.numeric(as.character(Grouped_Data_Split_SS$Year))

Grouped_Data_Split_SS$Longitude<-as.numeric(as.character(Grouped_Data_Split_SS$Longitude))
Grouped_Data_Split_SS$Latitude<-as.numeric(as.character(Grouped_Data_Split_SS$Latitude))

#split by bigelow
#set year blocks based on gear changes (pre and post Bigelow)
for (i in 1: nrow(Grouped_Data_Split_SS)){
  Grouped_Data_Split_SS$Gear[i]<-ifelse(Grouped_Data_Split_SS$Year1[i]<2009,  "B", "C")
}

#choose replacement years for butterfish based on abundance
Grouped_Data_Split_SS$Year[Grouped_Data_Split_SS$Year==2016]<-"2010"
Grouped_Data_Split_SS$Year[Grouped_Data_Split_SS$Year==2015]<-"2011"
Grouped_Data_Split_SS$Year[Grouped_Data_Split_SS$Year==2014]<-"2010"



#make predictions of suitability and occupancy
SS_Occupancy<-as.data.frame(predict(Trimmed_Ss_PA_GAM,Grouped_Data_Split_SS,type="link",se.fit=T))
SS_Occupancy$PA<-exp(SS_Occupancy$fit)/(1+exp(SS_Occupancy$fit))
SS_Occupancy$PA_LWR<-exp(SS_Occupancy$fit-(1.96*SS_Occupancy$se.fit))/(1+exp(SS_Occupancy$fit-(1.96*SS_Occupancy$se.fit)))
SS_Occupancy$PA_UPR<-exp(SS_Occupancy$fit+(1.96*SS_Occupancy$se.fit))/(1+exp(SS_Occupancy$fit+(1.96*SS_Occupancy$se.fit)))

#predict habitat suitability
Grouped_Data_Split_SS2<-Grouped_Data_Split_SS
Grouped_Data_Split_SS2$Gear<-"B"
SS_Partial_Effects<-as.data.frame(predict(Trimmed_Ss_PA_GAM,Grouped_Data_Split_SS2,exclude=c("Gear","s(Year)"),type="link", se.fit=T))
SS_Partial_Effects$PA_Fixed<-exp(SS_Partial_Effects$fit)/(1+exp(SS_Partial_Effects$fit))
SS_Partial_Effects$PA_Fixed_LWR<-exp(SS_Partial_Effects$fit-(1.96*SS_Partial_Effects$se.fit))/(1+exp(SS_Partial_Effects$fit-(1.96*SS_Partial_Effects$se.fit)))
SS_Partial_Effects$PA_Fixed_UPR<-exp(SS_Partial_Effects$fit+(1.96*SS_Partial_Effects$se.fit))/(1+exp(SS_Partial_Effects$fit+(1.96*SS_Partial_Effects$se.fit)))

Grouped_Data_Split_SS3<-Grouped_Data_Split_SS
Grouped_Data_Split_SS3$Gear<-"B"
SS_No_Vess_Effects<-as.data.frame(predict(Trimmed_Ss_PA_GAM,Grouped_Data_Split_SS3,exclude=c("Gear"),type="link", se.fit=T))
SS_No_Vess_Effects$PA_Vessel<-exp(SS_No_Vess_Effects$fit)/(1+exp(SS_No_Vess_Effects$fit))
SS_No_Vess_Effects$PA_Vessel_LWR<-exp(SS_No_Vess_Effects$fit-(1.96*SS_No_Vess_Effects$se.fit))/(1+exp(SS_No_Vess_Effects$fit-(1.96*SS_No_Vess_Effects$se.fit)))
SS_No_Vess_Effects$PA_Vessel_UPR<-exp(SS_No_Vess_Effects$fit+(1.96*SS_No_Vess_Effects$se.fit))/(1+exp(SS_No_Vess_Effects$fit+(1.96*SS_No_Vess_Effects$se.fit)))

Grouped_Data_Full_SS<-cbind(Grouped_Data_Split_SS, SS_Occupancy, SS_Partial_Effects, SS_No_Vess_Effects)

library(plyr)
#set occurrence to mean prevalance
Annual_SS_Prev=ddply(Ss_Matched, .(Year), summarize, Prev=sum(PA)/length(PA))
SS_Spring_Prevalence=mean(Annual_SS_Prev$Prev)

Grouped_Data_Full_SS<-Grouped_Data_Full_SS[,-which(names(Grouped_Data_Full_SS) %in% c("fit","se.fit"))]

#shelf occupancy
AnnualSS_Occupancy<-ddply(Grouped_Data_Full_SS, .(Year1), summarize, Mean_hab_Prop=length(which(PA>SS_Spring_Prevalence ))/length(which(!is.na(PA))), Low_hap_prop=length(which(PA_LWR>SS_Spring_Prevalence ))/length(which(!is.na(PA))),High_hap_prop=length(which(PA_UPR>SS_Spring_Prevalence ))/length(which(!is.na(PA))) )

#habitat suitability
AnnualSS_Partial_Effects<-ddply(Grouped_Data_Full_SS, .(Year1), summarize, Mean_hab_Prop=(length(which(PA_Fixed>SS_Spring_Prevalence )))/(length(which(!is.na(PA_Fixed)))), Low_hap_prop=(length(which(PA_Fixed_LWR>SS_Spring_Prevalence )))/(length(which(!is.na(PA_Fixed)))),High_hap_prop=(length(which(PA_Fixed_UPR>SS_Spring_Prevalence )))/(length(which(!is.na(PA_Fixed)))))

AnnualSS_No_Vess_Effects<-ddply(Grouped_Data_Full_SS, .(Year1), summarize, Mean_hab_Prop=(length(which(PA_Vessel>SS_Spring_Prevalence )))/(length(which(!is.na(PA_Vessel)))), Low_hap_prop=(length(which(PA_Vessel_LWR>SS_Spring_Prevalence )))/(length(which(!is.na(PA_Vessel)))),High_hap_prop=(length(which(PA_Vessel_UPR>SS_Spring_Prevalence )))/(length(which(!is.na(PA_Vessel)))))

#make annual averages for plotting suitability
Annual_Averages_SS<-ddply(Grouped_Data_Full_SS, .(Longitude, Latitude), summarize, Mean_PA=mean(PA))


world<-ne_countries(scale="medium", returnclass = "sf")
setwd("D:/Sand_Lance/SL_Chapter_2/Ch2_Manuscript/Revisions_Round_One/Updated_Temporary_Figure/Spring")
tiff("Mackerel_Model_Pred_210712.tif", width=6, height=5, units="in", res=300)
ggplot(data=world) + geom_raster(data = Annual_Averages_SS, aes(x=Longitude, y = Latitude, fill=Mean_PA),linejoin = "mitre") + 
  coord_fixed(ratio = 1) +
  scale_fill_viridis(direction = 1) +
  theme_bw()+ggtitle("Atl. mackerel")+labs(fill = "Prob. Occurrence")+geom_sf()+coord_sf(xlim=c(-75.5,-64), ylim=c(36, 44.5))
dev.off()

#make annual averages for quality based on suitability

tiff("Mackerel_Occupiable_Habitat_210714.tif", height=5, width=6, res=300, units="in")
par(mar=c(4,6,4,3))
plot(AnnualSS_Occupancy$Year1, AnnualSS_Occupancy$Mean_hab_Prop, type="l", lwd=4, xlab="Year", ylab="Proportion of \nOccupiable Shelf", main="Atl. mackerel",cex.lab=1.2, ylim=c(0,1.0), col="black")
polygon(x=c(AnnualSS_Occupancy$Year1,rev(AnnualSS_Occupancy$Year1)),y=c(AnnualSS_Occupancy$Low_hap_prop,rev(AnnualSS_Occupancy$High_hap_prop)), border=NA, col= adjustcolor("gray", alpha.f=0.5))
par(new=T)
plot(AnnualSS_Occupancy$Year1, AnnualSS_Occupancy$Mean_hab_Prop, type="l", lwd=4, xlab="Year", ylab="Proportion of \nOccupiable Shelf", main="Atl. mackerel",cex.lab=1.2, ylim=c(0,1.0), col="black")
dev.off()


tiff("Mackerel_Occupiable_Habitat_210831_Threshold_Comparison.tif", height=5, width=6, res=300, units="in")
par(mar=c(4,6,4,3))
plot(AnnualSS_Occupancy$Year1, AnnualSS_Occupancy$Mean_hab_Prop, type="l", lwd=4, xlab="Year", ylab="Proportion of \nOccupiable Shelf", main="Atl. mackerel",cex.lab=1.2, ylim=c(0,1.0), col="black")
polygon(x=c(AnnualSS_Occupancy$Year1,rev(AnnualSS_Occupancy$Year1)),y=c(AnnualSS_Occupancy$Low_hap_prop,rev(AnnualSS_Occupancy$High_hap_prop)), border=NA, col= adjustcolor("gray", alpha.f=0.5))
par(new=T)
plot(AnnualSS_Occupancy$Year1, AnnualSS_Occupancy$Mean_hab_Prop, type="l", lwd=4, xlab="Year", ylab="Proportion of \nOccupiable Shelf", main="Atl. mackerel",cex.lab=1.2, ylim=c(0,1.0), col="black")
polygon(x=c(AnnualSS_No_Vess_Effects$Year1,rev(AnnualSS_No_Vess_Effects$Year1)),y=c(AnnualSS_No_Vess_Effects$Low_hap_prop,rev(AnnualSS_No_Vess_Effects$High_hap_prop)), border=NA, col= adjustcolor("blue", alpha.f=0.25))
par(new=T)
plot(AnnualSS_No_Vess_Effects$Year1, AnnualSS_No_Vess_Effects$Mean_hab_Prop, type="l", lwd=4, lty=1,xlab="Year", ylab="Proportion of \nOccupiable Shelf", main="Atl. mackerel",cex.lab=1.2, ylim=c(0,1.0), col="blue")
polygon(x=c(AnnualSS_Partial_Effects$Year1,rev(AnnualSS_Partial_Effects$Year1)),y=c(AnnualSS_Partial_Effects$Low_hap_prop,rev(AnnualSS_Partial_Effects$High_hap_prop)), border=NA, col= adjustcolor("indianred", alpha.f=0.25))
par(new=T)
plot(AnnualSS_Partial_Effects$Year1, AnnualSS_Partial_Effects$Mean_hab_Prop, type="l", lwd=4, lty=1,xlab="Year", ylab="Proportion of \nOccupiable Shelf", main="Atl. mackerel",cex.lab=1.2, ylim=c(0,1.0), col="red")
dev.off()

#calculate trends in suitability and occupancy using betaregression 
library(betareg)
AnnualSS_Occupancy$Mean_hab_Prop<-AnnualSS_Occupancy$Mean_hab_Prop+0.00000001
SS_Spring_Full_Hab_Occ<-betareg(AnnualSS_Occupancy$Mean_hab_Prop~AnnualSS_Occupancy$Year1)
summary(SS_Spring_Full_Hab_Occ)

SS_Spring_Fixed_Hab_Occ<-betareg(AnnualSS_Partial_Effects$Mean_hab_Prop~AnnualSS_Partial_Effects$Year1)
summary(SS_Spring_Fixed_Hab_Occ)




#look at trends with latitude and depth
#Calculate weighted lat and lon
Grouped_Data_Full_SS$Weighted_Lat_Mean<-Grouped_Data_Full_SS$Latitude*Grouped_Data_Full_SS$PA
Grouped_Data_Full_SS$Weighted_Lat_LWR<-Grouped_Data_Full_SS$Latitude*Grouped_Data_Full_SS$PA_LWR
Grouped_Data_Full_SS$Weighted_Lat_UPR<-Grouped_Data_Full_SS$Latitude*Grouped_Data_Full_SS$PA_UPR

Grouped_Data_Full_SS$Weighted_Lat_Mean_Fixed<-Grouped_Data_Full_SS$Latitude*Grouped_Data_Full_SS$PA_Fixed
Grouped_Data_Full_SS$Weighted_Lat_LWR_Fixed<-Grouped_Data_Full_SS$Latitude*Grouped_Data_Full_SS$PA_Fixed_LWR
Grouped_Data_Full_SS$Weighted_Lat_UPR_Fixed<-Grouped_Data_Full_SS$Latitude*Grouped_Data_Full_SS$PA_Fixed_UPR


#divide these by sum of abundance for each year and it will work
library(plyr)
Annual_Centroid_Locations_SS<-ddply(Grouped_Data_Full_SS, .(Year1), summarize, Mean_Weighted_Lat=sum(Weighted_Lat_Mean, na.rm=TRUE)/(sum(PA, na.rm=TRUE)), Mean_Weighted_Lat_LWR=sum(Weighted_Lat_LWR, na.rm=TRUE)/(sum(PA_LWR, na.rm=TRUE)),Mean_Weighted_Lat_UPR=sum(Weighted_Lat_UPR, na.rm=TRUE)/(sum(PA_UPR, na.rm=TRUE)) )
Annual_Centroid_Locations_SS_Fixed<-ddply(Grouped_Data_Full_SS, .(Year1), summarize, Mean_Weighted_Lat=sum(Weighted_Lat_Mean_Fixed, na.rm=TRUE)/(sum(PA_Fixed, na.rm=TRUE)), Mean_Weighted_Lat_LWR=sum(Weighted_Lat_LWR_Fixed, na.rm=TRUE)/(sum(PA_Fixed_LWR, na.rm=TRUE)),Mean_Weighted_Lat_UPR=sum(Weighted_Lat_UPR_Fixed, na.rm=TRUE)/(sum(PA_Fixed_UPR, na.rm=TRUE)))

#Test relationship between lat-lon and year 
Lat_SS_Spring<-lm(Mean_Weighted_Lat~Year1, data=Annual_Centroid_Locations_SS)
summary(Lat_SS_Spring)

Lat_SS_Spring_Fixed<-lm(Mean_Weighted_Lat~Year1, data=Annual_Centroid_Locations_SS_Fixed)
summary(Lat_SS_Spring_Fixed)

tiff("Mackerel_Mean_Location_PA_Comparison_210714.tif", height=5, width=6, res=300, units="in")
par(mar=c(4,6,4,3))
plot(Annual_Centroid_Locations_SS$Year1, Annual_Centroid_Locations_SS$Mean_Weighted_Lat, type="l", lwd=4, xlab="Year", ylab="Mean Weighted \nLatitude", main="Atl. mackerel",cex.lab=1.2, ylim=c(39.5,41.1), col="black")
polygon(x=c(Annual_Centroid_Locations_SS$Year1,rev(Annual_Centroid_Locations_SS$Year1)), y=c(Annual_Centroid_Locations_SS$Mean_Weighted_Lat_LWR,rev(Annual_Centroid_Locations_SS$Mean_Weighted_Lat_UPR)), border=NA, col= adjustcolor("gray", alpha.f=0.5))
par(new=T)
plot(Annual_Centroid_Locations_SS$Year1, Annual_Centroid_Locations_SS$Mean_Weighted_Lat, type="l", lwd=4, xlab="Year", ylab="Mean Weighted \nLatitude", main="Atl. mackerel",cex.lab=1.2, ylim=c(39.5,41.1), col="black")
polygon(x=c(Annual_Centroid_Locations_SS_Fixed$Year1,rev(Annual_Centroid_Locations_SS_Fixed$Year1)),y=c(Annual_Centroid_Locations_SS_Fixed$Mean_Weighted_Lat_LWR,rev(Annual_Centroid_Locations_SS_Fixed$Mean_Weighted_Lat_UPR)), border=NA, col= adjustcolor("indianred", alpha.f=0.2))
par(new=T)
plot(Annual_Centroid_Locations_SS_Fixed$Year1, Annual_Centroid_Locations_SS_Fixed$Mean_Weighted_Lat, type="l", lwd=4,lty=1, xlab="Year", ylab="Mean Weighted \nLatitude", main="Atl. mackerel",cex.lab=1.2, ylim=c(39.5,41.1), col="red")
dev.off()


Grouped_Data_Full_SS$Weighted_Depth_Fixed<-Grouped_Data_Full_SS$Depth*Grouped_Data_Full_SS$PA_Fixed
Grouped_Data_Full_SS$Weighted_Depth_Fixed_LWR<-Grouped_Data_Full_SS$Depth*Grouped_Data_Full_SS$PA_Fixed_LWR
Grouped_Data_Full_SS$Weighted_Depth_Fixed_UPR<-Grouped_Data_Full_SS$Depth*Grouped_Data_Full_SS$PA_Fixed_UPR

Grouped_Data_Full_SS$Weighted_Depth<-Grouped_Data_Full_SS$Depth*Grouped_Data_Full_SS$PA
Grouped_Data_Full_SS$Weighted_Depth_LWR<-Grouped_Data_Full_SS$Depth*Grouped_Data_Full_SS$PA_LWR
Grouped_Data_Full_SS$Weighted_Depth_UPR<-Grouped_Data_Full_SS$Depth*Grouped_Data_Full_SS$PA_UPR



library(plyr)
Annual_Centroid_Depth_SS<-ddply(Grouped_Data_Full_SS, .(Year1), summarize, Mean_Weighted_Depth=sum(Weighted_Depth, na.rm=TRUE)/(sum(PA, na.rm=TRUE)),  Mean_Weighted_Depth_LWR=sum(Weighted_Depth_LWR, na.rm=TRUE)/(sum(PA_LWR, na.rm=TRUE)),  Mean_Weighted_Depth_UPR=sum(Weighted_Depth_UPR, na.rm=TRUE)/(sum(PA_UPR, na.rm=TRUE)))
Annual_Centroid_Depth_SS_Fixed<-ddply(Grouped_Data_Full_SS, .(Year1), summarize, Mean_Weighted_Depth=sum(Weighted_Depth_Fixed, na.rm=TRUE)/(sum(PA_Fixed, na.rm=TRUE)),  Mean_Weighted_Depth_LWR=sum(Weighted_Depth_Fixed_LWR, na.rm=TRUE)/(sum(PA_Fixed_LWR, na.rm=TRUE)),  Mean_Weighted_Depth_UPR=sum(Weighted_Depth_Fixed_UPR, na.rm=TRUE)/(sum(PA_Fixed_UPR, na.rm=TRUE)))

#test for correlation between depth and year for suitability and occupancy 
SS_Depth_Occupancy_Spring<-lm(Mean_Weighted_Depth~Year1, data=Annual_Centroid_Depth_SS)
summary(SS_Depth_Occupancy_Spring)

SS_Depth_Suitability_Spring<-lm(Mean_Weighted_Depth~Year1, data=Annual_Centroid_Depth_SS_Fixed)
summary(SS_Depth_Suitability_Spring)

tiff("Mackerel_Mean_Depth_PA_Comparison_210714_Regridded.tif", height=5, width=6, res=300, units="in")
par(mar=c(4,6,4,3))
plot(Annual_Centroid_Depth_SS$Year1, (Annual_Centroid_Depth_SS$Mean_Weighted_Depth)*-1, type="l", lwd=4, lty=1, xlab="Year", ylab="Mean Weighted \nDepth (m)", main="Atl. mackerel",cex.lab=1.2, ylim=rev(c(65,95)), col="black")
polygon(x=c(Annual_Centroid_Depth_SS$Year1,rev(Annual_Centroid_Depth_SS$Year1)), y=c((Annual_Centroid_Depth_SS$Mean_Weighted_Depth_LWR)*-1,rev((Annual_Centroid_Depth_SS$Mean_Weighted_Depth_UPR)*-1)), border=NA, col= adjustcolor("gray", alpha.f=0.5))
par(new=T)
plot(Annual_Centroid_Depth_SS$Year1, (Annual_Centroid_Depth_SS$Mean_Weighted_Depth)*-1, type="l", lwd=4,lty=1, xlab="Year", ylab="Mean Weighted \nDepth (m)", main="Atl. mackerel",cex.lab=1.2, ylim=rev(c(65,95)), col="black")
polygon(x=c(Annual_Centroid_Depth_SS_Fixed$Year1,rev(Annual_Centroid_Depth_SS_Fixed$Year1)), y=c((Annual_Centroid_Depth_SS_Fixed$Mean_Weighted_Depth_LWR)*-1,rev((Annual_Centroid_Depth_SS_Fixed$Mean_Weighted_Depth_UPR)*-1)), border=NA, col= adjustcolor("indianred", alpha.f=0.2))
par(new=T)
plot(Annual_Centroid_Depth_SS_Fixed$Year1, (Annual_Centroid_Depth_SS_Fixed$Mean_Weighted_Depth)*-1, type="l", lwd=4,lty=1, xlab="Year", ylab="Mean Weighted \nDepth (m)", main="Atl. mackerel",cex.lab=1.2, ylim=rev(c(65,95)), col="red")
dev.off()



#make response curves
View(Ss_PA_Dredge_Spring)

Best_SS_PA_GAM<-gam(PA~s(BT_Use, k=6)+s(Estimated_Grain_Size, k=6)
                       +s(SST_Use,k=6)
                       +s(EKE, k=6)+Gear+
                         s(Year, bs="re")+te(Longitude, Latitude)+s(SSS_Use, k=6), family=binomial,
                       data=Complete_Ss_PA, na.action = "na.fail")
tiff("Mackerel_SSS_Spring_210630.tif", height=5, width=5, units="in", res=300)
plot(Best_SS_PA_GAM,trans=function(x)exp(x)/(1+exp(x)), shade=T,xlab="Sea Surface Salinity", ylab="Probability of Occurrence", cex.lab=1.5, cex.axis=1.5 )
dev.off()

tiff("Mackerel_SSS_Supp_Spring_210630.tif", height=5, width=5, units="in", res=300)
plot(Best_SS_PA_GAM, shade=T,xlab="Sea Surface Salinity",  cex.lab=1.5, cex.axis=1.5 )
dev.off()

Best_SS_PA_GAM<-gam(PA~s(BT_Use, k=6)+s(Estimated_Grain_Size, k=6)
                    +s(EKE, k=6)+Gear+
                      s(Year, bs="re")+te(Longitude, Latitude)+s(SSS_Use, k=6) +s(SST_Use,k=6), family=binomial,
                    data=Complete_Ss_PA, na.action = "na.fail")


tiff("Mackerel_SST_210630.tif", height=5, width=5, units="in", res=300)
plot(Best_SS_PA_GAM,trans=function(x)exp(x)/(1+exp(x)), shade=T,xlab=expression(paste("Sea Surface Temperature ("^"o","C)")), ylab="Probability of Occurrence", cex.lab=1.5, cex.axis=1.5)
dev.off()

tiff("Mackerek_SST_Supp_210630.tif", height=5, width=5, units="in", res=300)
plot(Best_SS_PA_GAM, shade=T,xlab=expression(paste("Sea Surface Temperature ("^"o","C)")), cex.lab=1.5, cex.axis=1.5)
dev.off()

Best_SS_PA_GAM<-gam(PA~s(Estimated_Grain_Size, k=6)
                    +s(EKE, k=6)+Gear+
                      s(Year, bs="re")+te(Longitude, Latitude)+s(SSS_Use, k=6) +s(SST_Use,k=6)+s(BT_Use, k=6), family=binomial,
                    data=Complete_Ss_PA, na.action = "na.fail")
tiff("Mackerel_BT_Spring_210630.tif", height=5, width=5, units="in", res=300)
plot(Best_SS_PA_GAM,trans=function(x)exp(x)/(1+exp(x)), shade=T,xlab=expression(paste("Bottom Temperature (" ^ "o","C)")), ylab="Probability of Occurrence", cex.lab=1.5, cex.axis=1.5)
dev.off()

tiff("Mackerel_BT_Spring_Supp_210630.tif", height=5, width=5, units="in", res=300)
plot(Best_SS_PA_GAM, shade=T,xlab=expression(paste("Bottom Temperature (" ^ "o","C)")), cex.lab=1.5, cex.axis=1.5)
dev.off()

Best_SS_PA_GAM_RE<-gam(PA~s(Estimated_Grain_Size, k=6)
                    +s(EKE, k=6)+Gear+te(Longitude, Latitude)+s(SSS_Use, k=6) +s(SST_Use,k=6)+s(BT_Use, k=6)+ s(Year, bs="re"), family=binomial,
                    data=Complete_Ss_PA, na.action = "na.fail")

setwd("D:/Sand_Lance/SL_Chapter_2/Ch2_Manuscript/Revisions_Round_One/Updated_Temporary_Figure/Spring/RE")
tiff("Mackerel_Spring_RE.tif", height=5, width=5, units="in", res=300)
plot(Best_SS_PA_GAM_RE,cex.lab=1.5,cex.axis=1.5, main="Atl. mackerel")
dev.off()

Gridded_Final_Values_SS_Spring<-ddply(Grouped_Data_Full_SS[Grouped_Data_Full_SS$Year1>=2012,], .(Latitude, Longitude), summarize, Final_Probability=mean(PA_Fixed))
Gridded_Initial_Values_SS_Spring<-ddply(Grouped_Data_Full_SS[Grouped_Data_Full_SS$Year1<=1983,], .(Latitude, Longitude), summarize, Initial_Probability=mean(PA_Fixed))

SS_Spring_Space<-cbind(Gridded_Final_Values_SS_Spring, Gridded_Initial_Values_SS_Spring)
SS_Spring_Space<-SS_Spring_Space[,!duplicated(colnames(SS_Spring_Space), fromLast = TRUE)]

SS_Spring_Space$Occurrence_Diff<-SS_Spring_Space$Final_Probability-SS_Spring_Space$Initial_Probability

tiff("Spring_Mackerel_Spatial_Difference_Five_Yrs.tif", height=5, width=6, units="in", res=300)
ggplot(data=world) + geom_raster(data = SS_Spring_Space, aes(x=Longitude, y = Latitude, fill=Occurrence_Diff),linejoin = "mitre") + 
  coord_fixed(ratio = 1) +
  scale_fill_gradientn(colours=c("#0000FFFF","#F1ECE4","#FF0000FF"), limits=c(-0.8,0.8)) +
  theme_bw()+ggtitle("Atl. mackerel")+labs(fill = "Diff. in Occurr.")+geom_sf()+coord_sf(xlim=c(-75.5,-64), ylim=c(36, 44.5))+
theme(plot.title = element_text(size=18,face="bold"), legend.title = element_text(size=16,face="bold"), legend.text = element_text(size=12, face="bold"))

dev.off()

