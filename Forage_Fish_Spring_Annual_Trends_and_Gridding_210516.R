#Making estimates of habitat suitability and shelf occupancy for spring habitat models 
#JJS 21 05 11


########Data Preparation#############

# read in and grid the EcoMon (zooplankton) data 
setwd("D:/Sand_Lance/SL_Chapter_2/Matched_Data")
EcoMon<-read.csv("EcoMon_Plankton_Data_v3_v2.csv", header=TRUE)
#convert EcoMon to posixct value and then julian day for each and match 
library(lubridate)
EcoMon$Date2<-as.POSIXct(EcoMon$Date_Time, format="%m/%d/%Y %H:%M", tz="UTC")
EcoMon$Year<-year(EcoMon$Date2)
#get rid of any data that will not correspond to model year estimates
EcoMon<-EcoMon[EcoMon$Year>=1979 & EcoMon$Year<=2016,]
#separate to first half of the year for the spring data set
EcoMon_Spring<-EcoMon[EcoMon$Yday<=181,]

library(dplyr)
#binning and averageing the ecomon data by .25 x .25
Gridded_Zoops<-EcoMon_Spring %>% 
  mutate(binlon = cut(lon, seq(from = min(-75.5), to = max(-65), by = .25), include.lowest = T, right = F),
         binlat = cut(lat, seq(from = min(36.5), to = max(44.5), by = .25), include.lowest = T, right = F)) %>% 
  group_by(Year, binlat= forcats::fct_explicit_na(binlat), binlon=forcats::fct_explicit_na(binlon), .drop=FALSE) %>% 
  summarise(calfin_100m31 = mean(calfin_100m3, na.rm=TRUE), ctyp_100m31=mean(ctyp_100m3, na.rm=TRUE), pseudo_100m31=mean(pseudo_100m3, na.rm=TRUE), hyper_100m31=mean(hyper_100m3, na.rm=TRUE),
            poly_100m31=mean(poly_100m3, na.rm=TRUE), mlucens_100m31=mean(mlucens_100m3, na.rm=TRUE), larvaceans_100m31=mean(larvaceans_100m3, na.rm=TRUE),
            ammspp_100m31=mean(ammspp_100m3, na.rm=TRUE), tlong_100m31=mean(tlong_100m3, na.rm=TRUE), .groups="keep")

setwd("D:/Sand_Lance/SL_Chapter_2/Habitat_Occupancy")
#read in and grid the daily eke metric 
VM<-read.csv("Annual_EKE_Properties.csv", header=FALSE)
VM_2<-read.csv("Annual_EKE_Properties_2010_2016.csv", header=FALSE)
VM_Full<-rbind(VM, VM_2)
colnames(VM_Full)<-c("Year","Lon","Lat","EKE")
#Grid in the EKE data 
Gridded_EKE<-VM_Full %>% 
  mutate(binlon = cut(Lon, seq(from = min(-75.5), to = max(-65), by = .25), include.lowest = T, right = F),
         binlat = cut(Lat, seq(from = min(36.5), to = max(44.5), by = .25), include.lowest = T, right = F)) %>% 
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
#Grid the data 
Gridded_TS<-TS_Full %>% 
  mutate(binlon = cut(Lon, seq(from = min(-75.5), to = max(-65), by = .25), include.lowest = T, right = F),
         binlat = cut(Lat, seq(from = min(36.5), to = max(44.5), by = .25), include.lowest = T, right = F)) %>% 
  group_by(Year, binlat= forcats::fct_explicit_na(binlat), binlon= forcats::fct_explicit_na(binlon), .drop=FALSE) %>% 
  summarise(SST_Use = mean(SST),SSS_Use = mean(SSS), BS_Use = mean(BS), BT_Use = mean(BT), .groups="keep" )

#clean up missing data 
Gridded_TS<-Gridded_TS[Gridded_TS$binlat!="(Missing)",]
Gridded_TS<-Gridded_TS[Gridded_TS$binlon!="(Missing)",]

Gridded_EKE<-Gridded_EKE[Gridded_EKE$binlat!="(Missing)",]
Gridded_EKE<-Gridded_EKE[Gridded_EKE$binlon!="(Missing)",]

Gridded_Zoops<-Gridded_Zoops[Gridded_Zoops$binlat!="(Missing)",]
Gridded_Zoops<-Gridded_Zoops[Gridded_Zoops$binlon!="(Missing)",]

#merge all the dataframes
Grouped_Data_1<-merge(Gridded_TS,Gridded_EKE, by=c("Year","binlon","binlat"))
Grouped_Data<-merge(Grouped_Data_1, Gridded_Zoops, by=c("Year","binlon","binlat"))
library(splitstackshape)
Q<-cSplit(Grouped_Data, "binlon", sep=",")
Grouped_Data_Split<-cSplit(Q, "binlat", sep=",")

#convert the latitude and longitude into numerical values
Grouped_Data_Split$Latitude<-as.numeric(substr(Grouped_Data_Split$binlat_1, start=2, stop=5))
Grouped_Data_Split$Longitude<-as.numeric(substr(Grouped_Data_Split$binlon_1, start=2, stop=6))

Grouped_Data_Split$DecimalLon<-Grouped_Data_Split$Longitude-ceiling(Grouped_Data_Split$Longitude)
Grouped_Data_Split$DecimalLat<-Grouped_Data_Split$Latitude-floor(Grouped_Data_Split$Latitude)

#get rid of data that is missing from the grid
Grouped_Data_Split<-Grouped_Data_Split[!is.na(Grouped_Data_Split$DecimalLon),]
Grouped_Data_Split<-Grouped_Data_Split[!is.na(Grouped_Data_Split$DecimalLat),]

Grouped_Data_Split$DecimalLat<-as.character(Grouped_Data_Split$DecimalLat)
Grouped_Data_Split$DecimalLon<-as.character(Grouped_Data_Split$DecimalLon)

#dealing with a weird quirk of the output here (the decimals are not exact)
for (i in 1:nrow(Grouped_Data_Split)){
  if (Grouped_Data_Split$DecimalLon[i]== "-0.799999999999997"){
    Grouped_Data_Split$Longitude1[i]=-0.75+ceiling(Grouped_Data_Split$Longitude[i])
  }else if (Grouped_Data_Split$DecimalLon[i]=="-0.200000000000003"){
    Grouped_Data_Split$Longitude1[i]=ceiling(Grouped_Data_Split$Longitude[i])-0.25
  }else if (Grouped_Data_Split$DecimalLon[i]==0.0){
    Grouped_Data_Split$Longitude1[i]=Grouped_Data_Split$Longitude[i]
  }else if (Grouped_Data_Split$DecimalLon[i]==-0.5){
    Grouped_Data_Split$Longitude1[i]=Grouped_Data_Split$Longitude[i]
  }
  if (Grouped_Data_Split$DecimalLat[i]== "0.799999999999997"){
    Grouped_Data_Split$Latitude1[i]=0.75+floor(Grouped_Data_Split$Latitude[i])
  }else if (Grouped_Data_Split$DecimalLat[i]=="0.200000000000003"){
    Grouped_Data_Split$Latitude1[i]=floor(Grouped_Data_Split$Latitude[i])+0.25
  }else if (Grouped_Data_Split$DecimalLat[i]==0.0){
    Grouped_Data_Split$Latitude1[i]=Grouped_Data_Split$Latitude[i]
  }else if (Grouped_Data_Split$DecimalLat[i]==0.5){
    Grouped_Data_Split$Latitude1[i]=Grouped_Data_Split$Latitude[i]
  }
}


#merging the corrected values back  
Grouped_Data_Split$Latitude<-Grouped_Data_Split$Latitude1
Grouped_Data_Split$Longitude<-Grouped_Data_Split$Longitude1
#Need to split the data by region
#splling the zooplankton data by ecoregion and following abundance 
#regimes of Perretti et al 2017 
setwd("D:/Sand_Lance/Habitat_Model_Data/22561_FSCSTables")
#######zooplankton regimes##########
#estimating zooplankton values by regime
library(sf)
library(sp)
library(rgdal)
library(maps)
library(dplyr)
EPU<-readOGR("EPU_extended.shp")
gom<-EPU[2,]
gb<-EPU[1,]
mab<-EPU[4,]
ss<-EPU[3,]

#plot epus to make sure they are correct
plot(gom)
plot(mab)
plot(gb)
plot(EPU)
plot(ss)

#group all the data by polygon
gom@proj4string
mab@proj4string
gb@proj4string
ss@proj4string

#These shapefiles do not have a CRS, so I will need to make one
gom@proj4string<-CRS("+proj=longlat +datum=WGS84")
gb@proj4string<-CRS("+proj=longlat +datum=WGS84")
mab@proj4string<-CRS("+proj=longlat +datum=WGS84")
ss@proj4string<-CRS("+proj=longlat +datum=WGS84")
EPU@proj4string<-CRS("+proj=longlat +datum=WGS84")

#assign the data to polygon region (meaning EPU)
coordinates(Grouped_Data_Split)<-c("Longitude","Latitude")
as(Grouped_Data_Split,"SpatialPoints")
proj4string(Grouped_Data_Split)<-CRS("+proj=longlat +datum=WGS84")
proj4string(Grouped_Data_Split)<-proj4string(EPU)
pointsbyEPU <-over(Grouped_Data_Split, EPU)

#rejoin the datasets with assigned EPU
Grouped_Data_Split<- as.data.frame(Grouped_Data_Split)
Grouped_Data_Split1<-cbind(Grouped_Data_Split, pointsbyEPU)

#group zooplankton data by regime
#regimes are set by perretti et al 2017 abundance regimes
#2010 is set as another regime, although only speculative in their paper 
Grouped_Data_Split1<-Grouped_Data_Split1[!is.na(Grouped_Data_Split1$EPU),]
Grouped_Data_Split1$Zoops_Regime<-0
for (i in 1:nrow(Grouped_Data_Split1)){
  if (Grouped_Data_Split1$Year[i]<1991 & Grouped_Data_Split1$EPU[i]=="GOM"){
    Grouped_Data_Split1$Zoops_Regime[i]="Regime1"}
  else if (Grouped_Data_Split1$Year[i]<1991 & Grouped_Data_Split1$EPU[i]=="SS"){
    Grouped_Data_Split1$Zoops_Regime[i]="Regime1"}
  else if (Grouped_Data_Split1$Year[i]>=1991 & Grouped_Data_Split1$Year[i]<2001 & Grouped_Data_Split1$EPU[i]=="GOM"){
    Grouped_Data_Split1$Zoops_Regime[i]="Regime2"}
  else if (Grouped_Data_Split1$Year[i]>=1991 & Grouped_Data_Split1$Year[i]<2001 & Grouped_Data_Split1$EPU[i]=="SS"){
    Grouped_Data_Split1$Zoops_Regime[i]="Regime2"}
  else if (Grouped_Data_Split1$Year[i]>=2001 & Grouped_Data_Split1$Year[i]<2010 & Grouped_Data_Split1$EPU[i]=="GOM"){
    Grouped_Data_Split1$Zoops_Regime[i]="Regime3"}
  else if (Grouped_Data_Split1$Year[i]>=2001 & Grouped_Data_Split1$Year[i]<2010 & Grouped_Data_Split1$EPU[i]=="SS"){
    Grouped_Data_Split1$Zoops_Regime[i]="Regime3"}
  else if (Grouped_Data_Split1$Year[i]>=2010 & Grouped_Data_Split1$EPU[i]=="GOM"){
    Grouped_Data_Split1$Zoops_Regime[i]="Regime4"}
  else if (Grouped_Data_Split1$Year[i]>=2010 & Grouped_Data_Split1$EPU[i]=="SS"){
    Grouped_Data_Split1$Zoops_Regime[i]="Regime4"}
  else if (Grouped_Data_Split1$Year[i]<1996 & Grouped_Data_Split1$EPU[i]=="GB"){
    Grouped_Data_Split1$Zoops_Regime[i]="Regime1"}
  else if (Grouped_Data_Split1$Year[i]>=1996 & Grouped_Data_Split1$Year[i]<2003 & Grouped_Data_Split1$EPU[i]=="GB"){
    Grouped_Data_Split1$Zoops_Regime[i]="Regime2"}
  else if (Grouped_Data_Split1$Year[i]>=2003 & Grouped_Data_Split1$Year[i]<2010 & Grouped_Data_Split1$EPU[i]=="GB"){
    Grouped_Data_Split1$Zoops_Regime[i]="Regime3"}
  else if (Grouped_Data_Split1$Year[i]>=2010 & Grouped_Data_Split1$EPU[i]=="GB"){
    Grouped_Data_Split1$Zoops_Regime[i]="Regime4"}
  else if (Grouped_Data_Split1$Year[i]<1994 & Grouped_Data_Split1$EPU[i]=="MAB"){
    Grouped_Data_Split1$Zoops_Regime[i]="Regime1"}
  else if (Grouped_Data_Split1$Year[i]>=1994 & Grouped_Data_Split1$Year[i]<2001 & Grouped_Data_Split1$EPU[i]=="MAB"){
    Grouped_Data_Split1$Zoops_Regime[i]="Regime2"}
  else if (Grouped_Data_Split1$Year[i]>=2001 & Grouped_Data_Split1$Year[i]<2010 & Grouped_Data_Split1$EPU[i]=="MAB"){
    Grouped_Data_Split1$Zoops_Regime[i]="Regime3"}
  else if (Grouped_Data_Split1$Year[i]>=2010 & Grouped_Data_Split1$EPU[i]=="MAB"){
    Grouped_Data_Split1$Zoops_Regime[i]="Regime4"}
}


#Take binned averages by Regime to fill in holes 
library(plyr)
Gridded_Regime_Zoops<-ddply(Grouped_Data_Split1, .(Longitude, Latitude, Zoops_Regime),
                            summarize, Mean_Regime_Cal=mean(calfin_100m31, na.rm=TRUE), Mean_Regime_Ctyp=mean(ctyp_100m31, na.rm=TRUE),
                            Mean_Regime_Larv=mean(larvaceans_100m31, na.rm=TRUE), Mean_Regime_Met=mean(mlucens_100m31, na.rm=TRUE),
                            Mean_Regime_Ammodytes=mean(ammspp_100m31, na.rm=TRUE), Mean_Regime_Pcal=mean(pseudo_100m31, na.rm=TRUE),
                            Mean_Regime_Hyper=mean(hyper_100m31, na.rm=TRUE), Mean_Regime_Poly=mean(poly_100m31, na.rm=TRUE),
                            Mean_Regime_Tlong=mean(tlong_100m31, na.rm=TRUE))
Gridded_Regime_Zoops$Unique_Location<-paste(Gridded_Regime_Zoops$Latitude, Gridded_Regime_Zoops$Longitude)

#count the number of gris points lost in the process (ie how many "blank"bins there are)
Gridded_Regime_Zoops_Complete<-subset(Gridded_Regime_Zoops,duplicated(Unique_Location) | duplicated(Unique_Location, fromLast=TRUE))
length(unique(Gridded_Regime_Zoops_Complete$Unique_Location[is.na(Gridded_Regime_Zoops_Complete$Mean_Regime_Pcal)]))

#reorder dataset to look for issues
Gridded_Regime_Zoops_Complete<-Gridded_Regime_Zoops_Complete[with(Gridded_Regime_Zoops_Complete, order(Mean_Regime_Pcal, Unique_Location)),]

#Identify missing zooplankton data and fill with idw
library(sp)
library(gstat)
Gridded_Regime_NA<-Gridded_Regime_Zoops_Complete[is.na(Gridded_Regime_Zoops_Complete$Mean_Regime_Cal),]
Gridded_Regime_Full<-Gridded_Regime_Zoops_Complete[!is.na(Gridded_Regime_Zoops_Complete$Mean_Regime_Cal),]

coordinates(Gridded_Regime_Full) <- ~ Longitude + Latitude
coordinates(Gridded_Regime_NA) <- ~ Longitude + Latitude

#creating idw models for each of the important zooplankton taxa
idwmodel_Hyp_Spring = idw(Mean_Regime_Hyper~1, Gridded_Regime_Full,Gridded_Regime_NA,
                maxdist = Inf, idp = 2)

idwmodel_Cal_Spring = idw(Mean_Regime_Cal~1, Gridded_Regime_Full,Gridded_Regime_NA,
                maxdist = Inf, idp = 2)

idwmodel_Pcal_Spring = idw(Mean_Regime_Pcal~1, Gridded_Regime_Full,Gridded_Regime_NA,
                          maxdist = Inf, idp = 2)

idwmodel_Ctyp_Spring = idw(Mean_Regime_Ctyp~1, Gridded_Regime_Full,Gridded_Regime_NA,
                           maxdist = Inf, idp = 2)
idwmodel_Met_Spring = idw(Mean_Regime_Met~1, Gridded_Regime_Full,Gridded_Regime_NA,
                           maxdist = Inf, idp = 2)
idwmodel_Larv_Spring = idw(Mean_Regime_Larv~1, Gridded_Regime_Full,Gridded_Regime_NA,
                          maxdist = Inf, idp = 2)

idwmodel_Tlong_Spring = idw(Mean_Regime_Tlong~1, Gridded_Regime_Full,Gridded_Regime_NA,
                          maxdist = Inf, idp = 2)

#Extract estimates
Estimated_Hyper_Spring<-idwmodel_Hyp_Spring@data$var1.pred
Estimated_Cal_Spring<-idwmodel_Cal_Spring@data$var1.pred
Estimated_Pcal_Spring<-idwmodel_Pcal_Spring@data$var1.pred
Estimated_Ctyp_Spring<-idwmodel_Ctyp_Spring@data$var1.pred
Estimated_Larv_Spring<-idwmodel_Larv_Spring@data$var1.pred
Estimated_Met_Spring<-idwmodel_Met_Spring@data$var1.pred
Estimated_Tlong_Spring<-idwmodel_Tlong_Spring@data$var1.pred


#fill in the appropriate data gaps
Gridded_Regime_NA$Mean_Regime_Hyper<-Estimated_Hyper_Spring
Gridded_Regime_NA$Mean_Regime_Cal<-Estimated_Cal_Spring
Gridded_Regime_NA$Mean_Regime_Pcal<-Estimated_Pcal_Spring
Gridded_Regime_NA$Mean_Regime_Ctyp<-Estimated_Ctyp_Spring
Gridded_Regime_NA$Mean_Regime_Larv<-Estimated_Larv_Spring
Gridded_Regime_NA$Mean_Regime_Tlong<-Estimated_Tlong_Spring
Gridded_Regime_NA$Mean_Regime_Met<-Estimated_Met_Spring

#remerge the datasets of filled datat vs real data
Gridded_Regime_Zoops_Complete<-rbind(Gridded_Regime_Full, Gridded_Regime_NA)
Gridded_Regime_Zoops_Complete<-as.data.frame(Gridded_Regime_Zoops_Complete)


#reformatting the ichtyoplankton data (though we wound up excluding Ammodytes so this became irrelvant)
Gridded_Regime_Zoops_Cleaned_Up<-Gridded_Regime_Zoops_Complete[!is.na(Gridded_Regime_Zoops_Complete$Mean_Regime_Cal),]
for (i in 1:nrow(Gridded_Regime_Zoops_Cleaned_Up)){
  Gridded_Regime_Zoops_Cleaned_Up$Mean_Regime_Ammodytes[i]<-ifelse(is.na(Gridded_Regime_Zoops_Cleaned_Up$Mean_Regime_Ammodytes[i]),0.0,Gridded_Regime_Zoops_Cleaned_Up$Mean_Regime_Ammodytes[i]) 
}


#Fill in any gaps with the "mean" idw modeled data
Grouped_Data_Split_Regime<-merge(Gridded_Regime_Zoops_Cleaned_Up,Grouped_Data_Split1, by=c("Longitude", "Latitude","Zoops_Regime"))
for (i in 1:nrow(Grouped_Data_Split_Regime)){
  Grouped_Data_Split_Regime$calfin_100m3[i]<-ifelse(is.na(Grouped_Data_Split_Regime$calfin_100m31[i]),Grouped_Data_Split_Regime$Mean_Regime_Cal[i],Grouped_Data_Split_Regime$calfin_100m31[i])
  Grouped_Data_Split_Regime$ctyp_100m3[i]<-ifelse(is.na(Grouped_Data_Split_Regime$ctyp_100m31[i]),Grouped_Data_Split_Regime$Mean_Regime_Ctyp[i],Grouped_Data_Split_Regime$ctyp_100m31[i])
  Grouped_Data_Split_Regime$hyper_100m3[i]<-ifelse(is.na(Grouped_Data_Split_Regime$hyper_100m31[i]),Grouped_Data_Split_Regime$Mean_Regime_Hyper[i],Grouped_Data_Split_Regime$hyper_100m31[i])
  Grouped_Data_Split_Regime$pseudo_100m3[i]<-ifelse(is.na(Grouped_Data_Split_Regime$pseudo_100m31[i]),Grouped_Data_Split_Regime$Mean_Regime_Pcal[i],Grouped_Data_Split_Regime$pseudo_100m31[i])
  Grouped_Data_Split_Regime$tlong_100m3[i]<-ifelse(is.na(Grouped_Data_Split_Regime$tlong_100m31[i]),Grouped_Data_Split_Regime$Mean_Regime_Tlong[i],Grouped_Data_Split_Regime$tlong_100m31[i])
  Grouped_Data_Split_Regime$ammspp_100m3[i]<-ifelse(is.na(Grouped_Data_Split_Regime$ammspp_100m31[i]),Grouped_Data_Split_Regime$Mean_Regime_Ammodytes[i],Grouped_Data_Split_Regime$ammspp_100m31[i])
  Grouped_Data_Split_Regime$mlucens_100m3[i]<-ifelse(is.na(Grouped_Data_Split_Regime$mlucens_100m31[i]),Grouped_Data_Split_Regime$Mean_Regime_Met[i],Grouped_Data_Split_Regime$mlucens_100m31[i])
  Grouped_Data_Split_Regime$poly_100m3[i]<-ifelse(is.na(Grouped_Data_Split_Regime$poly_100m31[i]),Grouped_Data_Split_Regime$Mean_Regime_Poly[i],Grouped_Data_Split_Regime$poly_100m31[i])
  Grouped_Data_Split_Regime$larvaceans_100m3[i]<-ifelse(is.na(Grouped_Data_Split_Regime$larvaceans_100m31[i]),Grouped_Data_Split_Regime$Mean_Regime_Larv[i],Grouped_Data_Split_Regime$larvaceans_100m31[i])
  }


#plot maps of properties to ensure they make sense
library(ggplot2)
library(viridis)
tiff("BS_Gridded.tif",height=5, width=6, units="in", res=300)
ggplot(data=world) + geom_raster(data = Grouped_Data_Split1, aes(x=Longitude, y = Latitude, fill=BS_Use)) + 
  coord_fixed(ratio = 1) +
  scale_fill_viridis(direction = 1) +
  theme_bw()+ggtitle("Bottom Sal")+geom_sf()+coord_sf(xlim=c(-75.5,-64), ylim=c(36, 44.5))
dev.off()

tiff("EKE_Gridded.tif",height=5, width=6, units="in", res=300)
ggplot(data=world) + geom_raster(data = Grouped_Data_Split1, aes(x=Longitude, y = Latitude, fill=EKE)) + 
  coord_fixed(ratio = 1) +
  scale_fill_viridis(direction = 1) +
  theme_bw()+ggtitle("Daily EKE")+geom_sf()+coord_sf(xlim=c(-75.5,-64), ylim=c(36, 44.5))
dev.off()

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

coordinates(Grouped_Data_Split_Regime)<-c("Longitude","Latitude")

as(Grouped_Data_Split_Regime,"SpatialPoints")
proj4string(Grouped_Data_Split_Regime)<-CRS("+proj=utm +zone=19 +ellps=GRS80 +datum=NAD83")
Sed_Points<-over(Grouped_Data_Split_Regime,Sediments)
#macth sediment

#now match bathymetry
#get rid of land 
Bathymetry[Bathymetry>=0]<-NA

#take a mean of bathymetry to have spatial mean
Regrid_Bathy<-aggregate(Bathymetry, c(300),fun=mean, expand=TRUE, na.rm=TRUE) 

St_Coords<-Grouped_Data_Split_Regime@coords
pts<-SpatialPoints(St_Coords, proj4string = CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0 "))
SDN<-spTransform(pts,crs(Slope_Bath))
Bathy<-spTransform(pts,crs(Regrid_Bathy))
Depth<-extract(Regrid_Bathy,Bathy)

#get depth and slope estimates
Depth1<-extract(Bathymetry,Bathy)
Slope_try<-extract(Slope_Bath,SDN)
Grouped_Data_Split_Regime<- as.data.frame(Grouped_Data_Split_Regime)




#need to change all of this to match the gridded data
setwd("E:/Hab_Modeling_data/Spring_Survey/ecstdb2005")
Grain_Size<-readOGR("ecstdb2005.shp")
Grain_Size@proj4string<- CRS("+proj=utm +zone=19 +ellps=GRS80 +datum=NAD83") 
Grain_Size_Data<-as.data.frame(Grain_Size)
Grain_Size_Data_Red<-Grain_Size_Data[,c("LONGITUDE","LATITUDE","MEDIAN")]
Grain_Size_Data_Red<-Grain_Size_Data_Red[complete.cases(Grain_Size_Data_Red),]
coordinates(Grain_Size_Data_Red)  <- ~ LONGITUDE + LATITUDE
coordinates(Grouped_Data_Split_Regime)<-c("Longitude","Latitude")
Grain_Size_Data_Red@proj4string<- CRS("+proj=utm +zone=19 +ellps=GRS80 +datum=NAD83") 

as(Grouped_Data_Split_Regime,"SpatialPoints")
proj4string(Grouped_Data_Split_Regime)<-CRS("+proj=utm +zone=19 +ellps=GRS80 +datum=NAD83")

#use IDW for grain size estimates following Morley et al. 2018
library(gstat)
idwmodel = idw(MEDIAN ~1, Grain_Size_Data_Red,Grouped_Data_Split_Regime,
               maxdist = Inf, idp = 2)
Estimated_Grain_Size<-idwmodel@data$var1.pred

Grouped_Data_Split_Regime<- as.data.frame(Grouped_Data_Split_Regime)


#merge all of the extracted data
Grouped_Data_Split2<-cbind(Grouped_Data_Split_Regime, Sed_Points, Slope_try, Estimated_Grain_Size, Depth)
Grouped_Data_Split2$Unique_Location<-paste(Grouped_Data_Split2$Latitude, Grouped_Data_Split2$Longitude)

#find the number of locations with repeats per zooplankton regime
Proper_Locations<-ddply(Grouped_Data_Split2, .(Unique_Location), summarize, Count=length(unique(Zoops_Regime)))

#keep only locations that have zooplankton estimates for each zooplankton regime
Full_Locations<-Proper_Locations[Proper_Locations$Count==4,]

Grouped_Data_Split_Full<-Grouped_Data_Split2[Grouped_Data_Split2$Unique_Location %in% Full_Locations$Unique_Location, ]
Grouped_Data_Split_Full$Year1<-Grouped_Data_Split_Full$Year
#get rid of 1978 data (not enough FVCOM data for this) and the strata shift
Grouped_Data_Split_Full<-Grouped_Data_Split_Full[Grouped_Data_Split_Full$Year1>1978,]

#find the strata that do not pass the threshold via TS and remove them from consideration
setwd("D:/Sand_Lance/SL_Chapter_2/Matched_Data")
FVCOM_Data<-read.csv("FVCOM_Matched_Values_Full_Spring.csv", header=TRUE)
FVCOM_Data$Date1<-as.POSIXct(FVCOM_Data$Date, format="%m/%d/%Y %H:%M", tz="UTC")
setwd("D:/Sand_Lance/SL_Chapter_2/NEFSC_Spring_Survey")
Station_Data<-read.csv("NEFSC_Spring_Trawl_Survey_Stations.csv", header=TRUE)
Station_Data$Date1<-as.POSIXct(Station_Data$BEGIN_GMT_TOWDATE, format="%m/%d/%Y %H:%M", tz="UTC")
Station_FVCOM_Merge<-merge(Station_Data, FVCOM_Data, by=c("Date1"))
#merge datasets that have estimate and in situ data
Insitu_Stations<-Station_FVCOM_Merge[!is.na(Station_FVCOM_Merge$SURFTEMP) & !is.na(Station_FVCOM_Merge$SURFSALIN)
                                     & !is.na(Station_FVCOM_Merge$BOTTEMP) & !is.na(Station_FVCOM_Merge$BOTSALIN),]

#calculate bottom temperature redsiduals
Insitu_Stations$BTLM<-resid(lm(BOTTEMP~BT, data=Insitu_Stations))

library(plyr)

#calculate the stratum residuals by stratum
Stratum_Residuals<-ddply(Insitu_Stations, .(STRATUM), summarize, Mean_BT_Resid=mean(BTLM))
hist(Stratum_Residuals$Mean_BT_Resid)
Stratum_Residuals$Abs_Res<-abs(Stratum_Residuals$Mean_BT_Resid)

#use a 2C theshold for the spring models. 
Strata_to_Include<-Stratum_Residuals$STRATUM[Stratum_Residuals$Abs_Res<2]
Strata_to_Exclude<-Stratum_Residuals$STRATUM[Stratum_Residuals$Abs_Res>2]

Strata_to_Exclude<-Stratum_Residuals$STRATUM[Stratum_Residuals$Abs_Res>2]
Bad_Sal<-c(3360, 3370, 3380,3390, 3400, 3410, 3420, 3430, 3440)
All_Strata_to_Exclude<-append(Strata_to_Exclude, Bad_Sal)

setwd("D:/Sand_Lance/SL_Chapter_2/Trawl_Strata")

library(rgdal)
Strata<-readOGR("strata.shp")
plot(Strata)
K<-which(Strata$STRATA %in% All_Strata_to_Exclude)
Bad_Strata<-Strata[K,]

Bad_Strata@proj4string<- CRS("+proj=utm +zone=19 +ellps=GRS80 +datum=NAD83") 

coordinates(Grouped_Data_Split_Full)<-c("Longitude","Latitude")

as(Grouped_Data_Split_Full,"SpatialPoints")
proj4string(Grouped_Data_Split_Full)<-CRS("+proj=utm +zone=19 +ellps=GRS80 +datum=NAD83")
Strata_Overlap<-over(Grouped_Data_Split_Full,Bad_Strata)
Grouped_Data_Split_Full<-as.data.frame(Grouped_Data_Split_Full)

#edit the below to correspond with the spring dates
Grouped_Data_Split_Full1<-cbind(Grouped_Data_Split_Full, Strata_Overlap)

#remove the bad strata
Grouped_Data_Split_Full<-Grouped_Data_Split_Full1[is.na(Grouped_Data_Split_Full1$STRATA),]
#remove long island sound 
Grouped_Data_Split_Full<-Grouped_Data_Split_Full[!(Grouped_Data_Split_Full$Longitude<= -72 &Grouped_Data_Split_Full$Latitude>40.75), ]
#remove rest of LIS
Grouped_Data_Split_Full2<-Grouped_Data_Split_Full[!(Grouped_Data_Split_Full$Longitude<= -74 &Grouped_Data_Split_Full$Latitude>40.25), ]
#remove chesapeake 
Grouped_Data_Split_Full3<-Grouped_Data_Split_Full2[!(Grouped_Data_Split_Full2$Longitude<= -75 &Grouped_Data_Split_Full2$Latitude>38.5), ]
#remove weird maine spot
Grouped_Data_Split_Full4<-Grouped_Data_Split_Full3[!(Grouped_Data_Split_Full3$Longitude== -70 &Grouped_Data_Split_Full3$Latitude==43.75), ]
#remove nantucket sound 
Grouped_Data_Split_Full<-Grouped_Data_Split_Full4[!(Grouped_Data_Split_Full4$Longitude<= -70 &Grouped_Data_Split_Full4$Latitude==41.5), ]

########sand lance###############
Grouped_Data_Split_SL<-Grouped_Data_Split_Full
Grouped_Data_Split_SL$Year1<-Grouped_Data_Split_SL$Year

#fill in fake years by closest year available in abundance
Grouped_Data_Split_SL$Year[Grouped_Data_Split_SL$Year==2008]<-"1988"
Grouped_Data_Split_SL$Year[Grouped_Data_Split_SL$Year==2007]<-"2005"
Grouped_Data_Split_SL$Year[Grouped_Data_Split_SL$Year==2006]<-"1979"

#get rid of the bigelow data for sand lance 
Grouped_Data_Split_SL<-Grouped_Data_Split_SL[Grouped_Data_Split_SL$Year<2009,]

#get rid of any "land"
Grouped_Data_Split_SL2<-Grouped_Data_Split_SL[Grouped_Data_Split_SL$SEDNUM!="0",]


#duplicate the dataset to make predictions of suitability and occupancy
Grouped_Data_Split_SL3<-Grouped_Data_Split_SL2
Grouped_Data_Split_SL5<-Grouped_Data_Split_SL2

#last year used for fixed habitat suitability
Grouped_Data_Split_SL5$Year<-2005

#predict habitat suitbaility
Grouped_Data_Split_SL2$PA2<-predict(SL_Multi_Avg,Grouped_Data_Split_SL5,type="response")

#predict shelf occupancy 
Grouped_Data_Split_SL2$PA<-predict(SL_Multi_Avg,Grouped_Data_Split_SL2,type="response")

library(plyr)
#total number of grid points
Tile_Count<-length(unique(Grouped_Data_Split_SL2$Unique_Location))

#shelf occupancy 
AnnualSL_Quality<-ddply(Grouped_Data_Split_SL2, .(Year1), summarize, Hab_Prop=length(which(PA>0.5 ))/length(which(!is.na(PA))), Sample_Prop= length(which(!is.na(PA)))/Tile_Count)

#habitat suitability
AnnualSL_Quality_Constant2<-ddply(Grouped_Data_Split_SL2, .(Year1), summarize, Hab_Prop=length(which(PA2>0.5 ))/length(which(!is.na(PA1))), Sample_Prop= length(which(!is.na(PA1)))/Tile_Count)


setwd("D:/Sand_Lance/SL_Chapter_2/Habitat_Occupancy")

#perform betaregressions on occupancy with time
library(betareg)
AnnualSL_Quality$Hab_Prop<-AnnualSL_Quality$Hab_Prop+0.00000001
SL_Spring_Full_Hab_Occ<-betareg(AnnualSL_Quality$Hab_Prop~AnnualSL_Quality$Year1)
summary(SL_Spring_Full_Hab_Occ)

SL_Spring_Fixed_Hab_Occ<-betareg(AnnualSL_Quality_Constant2$Hab_Prop~AnnualSL_Quality_Constant2$Year1)
summary(SL_Spring_Fixed_Hab_Occ)

Rsq2<-round(cor(AnnualSL_Quality_Constant$Year1, AnnualSL_Quality_Constant$Hab_Prop)^2,3)
cor.test(AnnualSL_Quality_Constant2$Year1, AnnualSL_Quality_Constant2$Hab_Prop)
setwd("D:/Sand_Lance/SL_Chapter_2/Habitat_Occupancy/SL_Spring")
tiff("Sand_Lance_Occupiable_Habitat_210513_Threshold_Comparison.tif", height=5, width=6, res=300, units="in")
par(mar=c(4,6,4,3))
plot(AnnualSL_Quality$Year1, AnnualSL_Quality$Hab_Prop, type="l", lwd=3, xlab="Year", ylab="Proportion of \nViable Habitat", main="Sand lance",cex.lab=1.2, ylim=c(0,0.4), col="black")
par(new=T)
plot(AnnualSL_Quality_Constant2$Year1, AnnualSL_Quality_Constant2$Hab_Prop,  type="l",lwd=3, xlab="Year", ylab="Proportion of \nViable Habitat", main="Sand lance",cex.lab=1.2, ylim=c(0,0.4), col="red")
legend(1995,0.4,bty="n", legend =c("Year-Specific","Fixed"),col=c("black","red"), lwd=3)
dev.off()
#
setwd("D:/Sand_Lance/SL_Chapter_2/Habitat_Occupancy/SL_Spring")



#make averages over space for all years to make a habitat suitability map
Grouped_Data_Split_SL2 <- Grouped_Data_Split_SL2[, !duplicated(colnames(Grouped_Data_Split_SL2), fromLast = TRUE)]
Annual_Averages_SL_Spring<-ddply(Grouped_Data_Split_SL2, .(Longitude, Latitude), summarize, Mean_PA=mean(PA))
library(maps)
library(rnaturalearth)
library(rnaturalearthdata)
library(rgeos)
world<-ne_countries(scale="medium", returnclass = "sf")
setwd("D:/Sand_Lance/SL_Chapter_2/Habitat_Occupancy/SL_Spring")
tiff("Sand_lance_Model_Pred_210513.tif", width=6, height=5, units="in", res=300)
ggplot(data=world) + geom_raster(data = Annual_Averages_SL_Spring, aes(x=Longitude, y = Latitude, fill=Mean_PA),linejoin = "mitre") + 
  coord_fixed(ratio = 1) +
  scale_fill_viridis(direction = 1) +
  theme_bw()+ggtitle("Sand lance")+labs(fill = "Prob. Occurrence")+geom_sf()+coord_sf(xlim=c(-75.5,-64), ylim=c(36, 44.5))
dev.off()

#Calcualte weighted lat and lon
Grouped_Data_Split_SL2$Weighted_Lat_2005<-Grouped_Data_Split_SL2$Latitude*Grouped_Data_Split_SL2$PA2
Grouped_Data_Split_SL2$Weighted_Lon_2005<-Grouped_Data_Split_SL2$Longitude*Grouped_Data_Split_SL2$PA2
Grouped_Data_Split_SL2$Weighted_Lat<-Grouped_Data_Split_SL2$Latitude*Grouped_Data_Split_SL2$PA
Grouped_Data_Split_SL2$Weighted_Lon<-Grouped_Data_Split_SL2$Longitude*Grouped_Data_Split_SL2$PA


#dvide these by sum of abundance for each year and it will work
library(plyr)
Annual_Centroid_Locations_SL<-ddply(Grouped_Data_Split_SL2, .(Year1), summarize, Mean_Weighted_Lat=sum(Weighted_Lat, na.rm=TRUE)/(sum(PA, na.rm=TRUE)), Mean_Weighted_Lon=sum(Weighted_Lon, na.rm=TRUE)/(sum(PA, na.rm=TRUE)))
Annual_Centroid_Locations_SL_2005<-ddply(Grouped_Data_Split_SL2, .(Year1), summarize, Mean_Weighted_Lat=sum(Weighted_Lat_2005, na.rm=TRUE)/(sum(PA2, na.rm=TRUE)), Mean_Weighted_Lon=sum(Weighted_Lon_2005, na.rm=TRUE)/(sum(PA2, na.rm=TRUE)))

library(plyr)
library(ggplot2)
library(sf)
library(rnaturalearth)
library(rnaturalearthdata)
library(ggrepel)
library(viridis)
world<-ne_countries(scale="medium", returnclass = "sf")
setwd("D:/Sand_Lance/SL_Chapter_2/Habitat_Occupancy/SL_Spring")
tiff("Sand_Lance_Center_Location_Year_Specific_210513.tif", width=6, height=5, units="in", res=300)
Map1<-ggplot(data=world) + geom_point(data = Annual_Centroid_Locations_SL, aes(x=Mean_Weighted_Lon, y = Mean_Weighted_Lat, colour=Year1),linejoin = "mitre") + 
  coord_fixed(ratio = 1) +  scale_color_viridis(direction = 1) +
  theme_bw()+ggtitle("Sand lance")+geom_sf()+coord_sf(xlim=c(-75.5,-64), ylim=c(36, 44.5))+ylab("Latitude")+xlab("Longitude")
Map1 +geom_text_repel(data= Annual_Centroid_Locations_SL, aes(x=Mean_Weighted_Lon, y = Mean_Weighted_Lat, label = Year1), size = 4, max.overlaps = 50)
dev.off()

tiff("Sand_Lance_Center_Location_2013_210513.tif", width=6, height=5, units="in", res=300)
Map1<-ggplot(data=world) + geom_point(data = Annual_Centroid_Locations_SL_2005, aes(x=Mean_Weighted_Lon, y = Mean_Weighted_Lat, colour=Year1),linejoin = "mitre") + 
  coord_fixed(ratio = 1) +  scale_color_viridis(direction = 1) +
  theme_bw()+ggtitle("Sand lance")+geom_sf()+coord_sf(xlim=c(-75.5,-64), ylim=c(36, 44.5))+ylab("Latitude")+xlab("Longitude")
Map1 +geom_text_repel(data= Annual_Centroid_Locations_SL_2005, aes(x=Mean_Weighted_Lon, y = Mean_Weighted_Lat, label = Year1), size = 4, max.overlaps = 50)
dev.off()

#Test relationship between lat-lon and year 
Lat_SL_Spring<-lm(Mean_Weighted_Lat~Year1, data=Annual_Centroid_Locations_SL)
summary(Lat_SL_Spring)

Lat_SL_Spring_Suit<-lm(Mean_Weighted_Lat~Year1, data=Annual_Centroid_Locations_SL_2005)
summary(Lat_SL_Spring_Suit)

#look at mean lat and lon of location for sand lance
setwd("D:/Sand_Lance/SL_Chapter_2/Habitat_Occupancy/SL_Spring")
tiff("Sand_Lance_Mean_Location_PA_210513.tif", height=5, width=6, res=300, units="in")
par(mar=c(4,6,4,3))
plot(Annual_Centroid_Locations_SL$Year1, Annual_Centroid_Locations_SL$Mean_Weighted_Lat,type="l", lwd=3, xlab="Year", ylab="Mean Weighted \nLatitude", main="Sand lance",cex.lab=1.2)
abline(lm(Mean_Weighted_Lat~Year1, data=Annual_Centroid_Locations_SL),lwd=3, lty=3)
text(1980,40.35, labels = expression(paste("",R^2,"=")), cex=1.5)
text(1984,40.35, labels = Rsq2, cex=1.5)
dev.off()

Rsq2<-round(cor(Annual_Centroid_Locations_SL_2005$Year1, Annual_Centroid_Locations_SL_2005$Mean_Weighted_Lat)^2,3)
cor.test(Annual_Centroid_Locations_SL_2005$Year1, Annual_Centroid_Locations_SL_2005$Mean_Weighted_Lat)

tiff("Sand_Lance_Mean_Location_PA_Fixed_210513.tif", height=5, width=6, res=300, units="in")
par(mar=c(4,6,4,3))
plot(Annual_Centroid_Locations_SL_2005$Year1, Annual_Centroid_Locations_SL_20055$Mean_Weighted_Lat,type="l", lwd=3, xlab="Year", ylab="Mean Weighted \nLatitude", main="Sand lance",cex.lab=1.2)
abline(lm(Mean_Weighted_Lat~Year1, data=Annual_Centroid_Locations_SL_1985),lwd=3, lty=3)
text(1980,40.5, labels = expression(paste("",R^2,"=")), cex=1.5)
text(1984,40.5, labels = Rsq2, cex=1.5)
dev.off()

setwd("D:/Sand_Lance/SL_Chapter_2/Habitat_Occupancy/SL_Spring")

tiff("Sand_Lance_Mean_Location_PA_Comparison_210513.tif", height=5, width=6, res=300, units="in")
par(mar=c(4,6,4,3))
plot(Annual_Centroid_Locations_SL_2005$Year1, Annual_Centroid_Locations_SL_2005$Mean_Weighted_Lat,type="l", lwd=3, xlab="Year",col="red", ylim=c(40.25,40.55),ylab="Mean Weighted \nLatitude", main="Sand lance",cex.lab=1.2)
par(new=T)
plot(Annual_Centroid_Locations_SL$Year1, Annual_Centroid_Locations_SL$Mean_Weighted_Lat,type="l", lwd=3, xlab="Year", col="black",ylim=c(40.25,40.55), ylab="Mean Weighted \nLatitude", main="Sand lance",cex.lab=1.2)
legend(1995,40.32,bty="n", legend =c("Year-Specific","Fixed"),col=c("black","red"), lwd=3)
dev.off()


Grouped_Data_Split_SL2$Weighted_Depth_2005<-Grouped_Data_Split_SL2$Depth*Grouped_Data_Split_SL2$PA2
Grouped_Data_Split_SL2$Weighted_Depth<-Grouped_Data_Split_SL2$Depth*Grouped_Data_Split_SL2$PA



library(plyr)
Annual_Centroid_Depth_SL<-ddply(Grouped_Data_Split_SL2, .(Year1), summarize, Mean_Weighted_Depth=sum(Weighted_Depth, na.rm=TRUE)/(sum(PA, na.rm=TRUE)))
Annual_Centroid_Depth_SL_2005<-ddply(Grouped_Data_Split_SL2, .(Year1), summarize, Mean_Weighted_Depth=sum(Weighted_Depth_2005, na.rm=TRUE)/(sum(PA2, na.rm=TRUE)))

setwd("D:/Sand_Lance/SL_Chapter_2/Habitat_Occupancy/SL_Spring")
tiff("Sand_Lance_Mean_Depth_PA_Comparison_210513_Regridded.tif", height=5, width=6, res=300, units="in")
par(mar=c(4,6,4,3))
plot(Annual_Centroid_Depth_SL_2005$Year1, (Annual_Centroid_Depth_SL_2005$Mean_Weighted_Depth)*-1,type="l", lwd=3, xlab="Year",col="red", ylim=rev(c(40,65)),ylab="Mean Weighted \nDepth (m)", main="Sand lance",cex.lab=1.2)
par(new=T)
plot(Annual_Centroid_Depth_SL$Year1, (Annual_Centroid_Depth_SL$Mean_Weighted_Depth)*-1, type="l", lwd=3, xlab="Year", col="black", ylim=rev(c(40,65)), ylab="Mean Weighted \nDepth (m)", main="Sand lance",cex.lab=1.2)
legend(1995,40,bty="n", legend =c("Year-Specific","Fixed"),col=c("black","red"), lwd=3)
dev.off()

#test for correlation between depth and year for suitability and occupancy 
SL_Depth_Occupancy_Spring<-lm(Mean_Weighted_Depth~Year1, data=Annual_Centroid_Depth_SL)
summary(SL_Depth_Occupancy_Spring)

SL_Depth_Suitability_Spring<-lm(Mean_Weighted_Depth~Year1, data=Annual_Centroid_Depth_SL_2005)
summary(SL_Depth_Suitability_Spring)



#plotting response curves for each "important" variable
View(SL_PA_Dredge)
#eke, metridia, and larvaceans 
library(mgcv)
library(ggplot2)
theme_set(theme_classic())
library(dplyr)
Best_SL_PA_GAM<-gam(PA~
                       s(Year, bs="re")+te(Longitude, Latitude)+s(EKE, k=4)+s(log10(mlucens_100m3+1),k=4)
                    +s(log10(larvaceans_100m3+1), k=4)
                    ,family=binomial,
                       data=SL_PA_Fitting, na.action = "na.fail")
setwd("D:/Sand_Lance/SL_Chapter_2/Ch2_Manuscript")
tiff("Sand_Lance_App_210428.tif", height=5, width=5, units="in", res=300)
plot(Best_SL_PA_GAM,trans=function(x)exp(x)/(1+exp(x)), shade=T,ylab="Probability of Occurrence",xlab=expression(paste("Appendicularians log10(ind/100m"^"3",")")), cex.lab=1.5, cex.axis=1.5 )
dev.off()

Best_SL_PA_GAM<-gam(PA~
                      s(Year, bs="re")+te(Longitude, Latitude)+s(log10(mlucens_100m3+1),k=4)
                    +s(log10(larvaceans_100m3+1), k=4)+s(EKE, k=4)
                    ,family=binomial,
                    data=SL_PA_Fitting, na.action = "na.fail")
tiff("Sand_Lance_EKE_210428.tif", height=5, width=5, units="in", res=300)
plot(Best_SL_PA_GAM,trans=function(x)exp(x)/(1+exp(x)), shade=T,ylab="Probability of Occurrence",xlab=expression(paste("Eddy kinetic energy (cm"^"-2","s"^"-2",")")), cex.axis=1.5, cex.lab=1.5 )
dev.off()

library(mgcv)
setwd("D:/Sand_Lance/SL_Chapter_2/Ch2_Manuscript")
Best_SL_PA_GAM<-gam(PA~
                      s(Year, bs="re")+te(Longitude, Latitude)
                    +s(log10(larvaceans_100m3+1), k=4)+s(EKE, k=4)+s(log10(mlucens_100m3+1),k=4)
                    ,family=binomial,
                    data=SL_PA_Fitting, na.action = "na.fail")
tiff("Sand_Lance_Mlucens_210428.tif", height=5, width=5, units="in", res=300)
plot(Best_SL_PA_GAM,trans=function(x)exp(x)/(1+exp(x)), shade=T,ylab="Probability of Occurrence",xlab=expression(paste(italic("Metridia "), "spp. log10(ind/100m"^"3",")")), cex.lab=1.5, cex.axis=1.5 )
dev.off()


#check year-specific variances
library(gammit)
Best_SL_PA_GAM<-gam(PA~
                      s(Year, bs="re")+te(Longitude, Latitude)+s(EKE, k=4)+s(log10(mlucens_100m3+1),k=4)
                    +s(log10(larvaceans_100m3+1), k=4)
                    ,family=binomial,
                    data=SL_PA_Fitting, na.action = "na.fail")
summary_gamm(Best_SL_PA_GAM)

#####butterfish#########

Grouped_Data_Split_PT<-Grouped_Data_Split_Full
Grouped_Data_Split_PT$Year<-as.factor(as.character(Grouped_Data_Split_PT$Year))
Grouped_Data_Split_PT$Longitude<-as.numeric(as.character(Grouped_Data_Split_PT$Longitude))
Grouped_Data_Split_PT$Latitude<-as.numeric(as.character(Grouped_Data_Split_PT$Latitude))

#set year blocks based on gear changes (pre and post Bigelow)
for (i in 1: nrow(Grouped_Data_Split_PT)){
  Grouped_Data_Split_PT$YearBlockB[i]<-ifelse(Grouped_Data_Split_PT$Year1[i]<2009,  0, 1)
  Grouped_Data_Split_PT$YearBlockC[i]<-ifelse(Grouped_Data_Split_PT$Year1[i]>=2009, 0, 1)
}

#choose replacement years for butterfish based on abundance
Grouped_Data_Split_PT$Year[Grouped_Data_Split_PT$Year==2016]<-"2012"
Grouped_Data_Split_PT$Year[Grouped_Data_Split_PT$Year==2015]<-"2009"
Grouped_Data_Split_PT$Year[Grouped_Data_Split_PT$Year==2014]<-"2011"

#make datasets for predictions
Grouped_Data_Split_PT2<-Grouped_Data_Split_PT
Grouped_Data_Split_PT2$Year<-2013
Grouped_Data_Split_PT2$YearBlockB<-1
Grouped_Data_Split_PT2$YearBlockC<-0


#make predictions of suitability and occupancy
Grouped_Data_Split_PT$PA<-predict(Trimmed_PT_PA_GAM,Grouped_Data_Split_PT,type="response")
Grouped_Data_Split_PT$PA_2013<-predict(Trimmed_PT_PA_GAM,Grouped_Data_Split_PT2,type="response")

#check the full length of grid
Grouped_Data_Split_Regime$Unique_Location<-paste(Grouped_Data_Split_Regime$Latitude, Grouped_Data_Split_Regime$Longitude)
Tile_Count<-length(unique(Grouped_Data_Split_Regime$Unique_Location))
AnnualPT_Quality<-ddply(Grouped_Data_Split_PT, .(Year1), summarize, Hab_Prop=length(which(PA>0.5 ))/length(which(!is.na(PA))), Sample_Prop= length(which(!is.na(PA)))/Tile_Count)


#make annual averages for plotting suitability
Annual_Averages_PT<-ddply(Grouped_Data_Split_PT, .(Longitude, Latitude), summarize, Mean_PA=mean(PA))


world<-ne_countries(scale="medium", returnclass = "sf")
setwd("D:/Sand_Lance/SL_Chapter_2/Habitat_Occupancy/PT_Spring")
tiff("Butterfish_Model_Pred_210514.tif", width=6, height=5, units="in", res=300)
ggplot(data=world) + geom_raster(data = Annual_Averages_PT, aes(x=Longitude, y = Latitude, fill=Mean_PA),linejoin = "mitre") + 
  coord_fixed(ratio = 1) +
  scale_fill_viridis(direction = 1) +
  theme_bw()+ggtitle("Butterfish")+labs(fill = "Prob. Occurrence")+geom_sf()+coord_sf(xlim=c(-75.5,-64), ylim=c(36, 44.5))
dev.off()

#make annual averages for quality based on suitability
AnnualPT_Quality_2013<-ddply(Grouped_Data_Split_PT, .(Year1), summarize, Hab_Prop=length(which(PA_2013>0.5 ))/length(which(!is.na(PA_2013))), Sample_Prop= length(which(!is.na(PA_2013)))/Tile_Count)


setwd("D:/Sand_Lance/SL_Chapter_2/Habitat_Occupancy/PT_Spring")
tiff("Butterfish_Occupiable_Habitat_210514_Comparison.tif", height=5, width=6, res=300, units="in")
par(mar=c(4,6,4,3))
plot(AnnualPT_Quality$Year1, AnnualPT_Quality$Hab_Prop, type="l", lwd=3,xlab="Year", ylab="Proportion of \nViable Habitat", main="Butterfish",cex.lab=1.2, ylim=c(0,0.55), col="black")
par(new=T)
plot(AnnualPT_Quality_2013$Year1, AnnualPT_Quality_2013$Hab_Prop,  type="l",lwd=3, xlab="Year", ylab="Proportion of \nViable Habitat", main="Butterfish",cex.lab=1.2, ylim=c(0,0.55), col="red")
legend(1995,0.55,bty="n", legend =c("Year-Specific","Fixed"),col=c("black","red"), lwd=3)
dev.off()
#

#calculate trends in suitability and occupancy using betaregression 
library(betareg)
AnnualPT_Quality$Hab_Prop<-AnnualPT_Quality$Hab_Prop+0.00000001
PT_Spring_Full_Hab_Occ<-betareg(AnnualPT_Quality$Hab_Prop~AnnualPT_Quality$Year1)
summary(PT_Spring_Full_Hab_Occ)

PT_Spring_Fixed_Hab_Occ<-betareg(AnnualPT_Quality_2013$Hab_Prop~AnnualPT_Quality_2013$Year1)
summary(PT_Spring_Fixed_Hab_Occ)



#look at trends with latitude and depth

Grouped_Data_Split_PT$Weighted_Lat_2013<-Grouped_Data_Split_PT$Latitude*Grouped_Data_Split_PT$PA_2013
Grouped_Data_Split_PT$Weighted_Lon_2013<-Grouped_Data_Split_PT$Longitude*Grouped_Data_Split_PT$PA_2013
Grouped_Data_Split_PT$Weighted_Lat<-Grouped_Data_Split_PT$Latitude*Grouped_Data_Split_PT$PA
Grouped_Data_Split_PT$Weighted_Lon<-Grouped_Data_Split_PT$Longitude*Grouped_Data_Split_PT$PA

library(plyr)
Annual_Centroid_Locations_PT<-ddply(Grouped_Data_Split_PT, .(Year1), summarize, Mean_Weighted_Lat=sum(Weighted_Lat, na.rm=TRUE)/(sum(PA, na.rm=TRUE)), Mean_Weighted_Lon=sum(Weighted_Lon, na.rm=TRUE)/(sum(PA, na.rm=TRUE)))
Annual_Centroid_Locations_PT_2013<-ddply(Grouped_Data_Split_PT, .(Year1), summarize, Mean_Weighted_Lat=sum(Weighted_Lat_2013, na.rm=TRUE)/(sum(PA_2013, na.rm=TRUE)), Mean_Weighted_Lon=sum(Weighted_Lon_2013, na.rm=TRUE)/(sum(PA_2013, na.rm=TRUE)))

library(plyr)
library(ggplot2)
library(sf)
library(rnaturalearth)
library(rnaturalearthdata)
library(ggrepel)
library(viridis)
#plot annual centroids
world<-ne_countries(scale="medium", returnclass = "sf")
setwd("D:/Sand_Lance/SL_Chapter_2/Habitat_Occupancy/PT_Spring")
tiff("Butterfish_Center_Location_Year_Specific_210514.tif", width=6, height=5, units="in", res=300)
Map1<-ggplot(data=world) + geom_point(data = Annual_Centroid_Locations_PT, aes(x=Mean_Weighted_Lon, y = Mean_Weighted_Lat, colour=Year1),linejoin = "mitre") + 
  coord_fixed(ratio = 1) +  scale_color_viridis(direction = 1) +
  theme_bw()+ggtitle("Butterfish")+geom_sf()+coord_sf(xlim=c(-75.5,-64), ylim=c(36, 44.5))+ylab("Latitude")+xlab("Longitude")
Map1 #+geom_text_repel(data= Annual_Centroid_Locations_PT, aes(x=Mean_Weighted_Lon, y = Mean_Weighted_Lat, label = Year1), size = 4, max.overlaps = 50)
dev.off()

tiff("Butterfish_Center_Location_2013_210514.tif", width=6, height=5, units="in", res=300)
Map1<-ggplot(data=world) + geom_point(data = Annual_Centroid_Locations_PT_2013, aes(x=Mean_Weighted_Lon, y = Mean_Weighted_Lat, colour=Year1),linejoin = "mitre") + 
  coord_fixed(ratio = 1) +  scale_color_viridis(direction = 1) +
  theme_bw()+ggtitle("Butterfish")+geom_sf()+coord_sf(xlim=c(-75.5,-64), ylim=c(36, 44.5))+ylab("Latitude")+xlab("Longitude")
Map1 #+geom_text_repel(data= Annual_Centroid_Locations_SL_1985, aes(x=Mean_Weighted_Lon, y = Mean_Weighted_Lat, label = Year1), size = 4, max.overlaps = 50)
dev.off()


PT_Spring_Lat_Occupancy<-lm(Mean_Weighted_Lat~Year1, data=Annual_Centroid_Locations_PT)
summary(PT_Spring_Lat_Occupancy)

PT_Spring_Lat_Suitability<-lm(Mean_Weighted_Lat~Year1, data=Annual_Centroid_Locations_PT_2013)
summary(PT_Spring_Lat_Suitability)


setwd("D:/Sand_Lance/SL_Chapter_2/Habitat_Occupancy/PT_Spring")
tiff("Butterfish_Mean_Location_PA_Comparison_210514.tif", height=5, width=6, res=300, units="in")
par(mar=c(4,6,4,3))
plot(Annual_Centroid_Locations_PT_2013$Year1, Annual_Centroid_Locations_PT_2013$Mean_Weighted_Lat,type="l", lwd=3, xlab="Year",col="red", ylim=c(39.2,40.7),ylab="Mean Weighted \nLatitude", main="Butterfish",cex.lab=1.2)
par(new=T)
plot(Annual_Centroid_Locations_PT$Year1, Annual_Centroid_Locations_PT$Mean_Weighted_Lat,type="l", lwd=3, xlab="Year", col="black",ylim=c(39.2,40.7), ylab="Mean Weighted \nLatitude", main="Butterfish",cex.lab=1.2)
legend(1978,40.6,bty="n", legend =c("Year-Specific","Fixed"),col=c("black","red"), lwd=3)
dev.off()


#calculate weighted depth
Grouped_Data_Split_PT$Weighted_Depth_2013<-Grouped_Data_Split_PT$Depth*Grouped_Data_Split_PT$PA_2013
Grouped_Data_Split_PT$Weighted_Depth<-Grouped_Data_Split_PT$Depth*Grouped_Data_Split_PT$PA


library(plyr)
Annual_Centroid_Depth_PT<-ddply(Grouped_Data_Split_PT, .(Year1), summarize, Mean_Weighted_Depth=sum(Weighted_Depth, na.rm=TRUE)/(sum(PA, na.rm=TRUE)))
Annual_Centroid_Depth_PT_2013<-ddply(Grouped_Data_Split_PT, .(Year1), summarize, Mean_Weighted_Depth=sum(Weighted_Depth_2013, na.rm=TRUE)/(sum(PA_2013, na.rm=TRUE)))

setwd("D:/Sand_Lance/SL_Chapter_2/Habitat_Occupancy/PT_Spring")
tiff("Butterfish_Mean_Depth_PA_Comparison_210514.tif", height=5, width=6, res=300, units="in")
par(mar=c(4,6,4,3))
plot(Annual_Centroid_Depth_PT_2013$Year1, (Annual_Centroid_Depth_PT_2013$Mean_Weighted_Depth)*-1,type="l", lwd=3, xlab="Year",col="red", ylim=rev(c(70,140)),ylab="Mean Weighted \nDepth (m)", main="Butterfish",cex.lab=1.2)
par(new=T)
plot(Annual_Centroid_Depth_PT$Year1, (Annual_Centroid_Depth_PT$Mean_Weighted_Depth)*-1, type="l", lwd=3, xlab="Year", col="black", ylim=rev(c(70,140)), ylab="Mean Weighted \nDepth (m)", main="Butterfish",cex.lab=1.2)
legend(1977,70,bty="n", legend =c("Year-Specific","Fixed"),col=c("black","red"), lwd=3)
dev.off()

PT_Depth_Spring_Occupancy<-lm(Mean_Weighted_Depth~Year1, data=Annual_Centroid_Depth_PT)
summary(PT_Depth_Spring_Occupancy)

PT_Depth_Spring_Suitability<-lm(Mean_Weighted_Depth~Year1, data=Annual_Centroid_Depth_PT_2013)
summary(PT_Depth_Spring_Suitability)



#make response cuves 
Trimmed_PT_PA_GAM<-gam(PA~s(Year, bs="re", by=ordered(!YearBlockB))+s(Year, bs="re", by=ordered(!YearBlockC))+te(Longitude, Latitude)+s(BT_Use, k=4), family=binomial, data=Complete_PT_PA, na.action = "na.fail")

setwd("D:/Sand_Lance/SL_Chapter_2/Ch2_Manuscript")
tiff("Butterfish_Bottom_Temp_210514.tif", height=5, width=5, units="in", res=300)
plot(Trimmed_PT_PA_GAM,trans=function(x)exp(x)/(1+exp(x)), shade=T,ylab="Probability of Occurrence",xlab=expression(paste("Bottom Temperature ("^"o","C)")),cex.lab=1.5,cex.axis=1.5)
dev.off()

library(gammit)
summary_gamm(Trimmed_PT_PA_GAM_REML)


#compare with bottom temperature
Mean_annual_BT<-ddply(Grouped_Data_Split_PT, .(Year1), summarize, MeanBT=mean(BT_Use, na.rm=T))
BT_Prediction_St<-as.data.frame(seq(from=min(Mean_annual_BT$MeanBT), to=max(Mean_annual_BT$MeanBT), by=0.01))
colnames(BT_Prediction_St)<-"MeanBT"


PT_BT_Data<-cbind(Mean_annual_BT$MeanBT,AnnualPT_Quality$Hab_Prop, AnnualPT_Quality_2013$Hab_Prop) 
colnames(PT_BT_Data)<-c("MeanBT","Hab_Prop","Hab_Prop_Fixed")
PT_BT_Data<-as.data.frame(PT_BT_Data)

PT_Spring_BT_Model<-betareg(Hab_Prop~MeanBT, data=PT_BT_Data)
summary(PT_Spring_BT_Model)

PT_Spring_BT_Model_Fixed<-betareg(Hab_Prop_Fixed~MeanBT, data=PT_BT_Data)
summary(PT_Spring_BT_Model_Fixed)


PT_BT_Reg<-predict(PT_Spring_BT_Model, BT_Prediction_St, type="response")
PT_BT_Reg_Fixed<-predict(PT_Spring_BT_Model_Fixed, BT_Prediction_St, type="response")

setwd("D:/Sand_Lance/SL_Chapter_2/Habitat_Occupancy/PT_Spring")
tiff("Butterfish_Bottom_temp_210514.tif", height=5, width=6, res=300, units="in")
par(mar=c(4,6,4,3))
plot(Mean_Bottom_annual_Bottom_Temp$MeanBT, AnnualPT_Quality_1985$Hab_Prop, pch=19,col="red",ylim=c(0,0.5),xlab="Bottom Temperature", ylab="Proportion of \nViable Habitat", main="Butterfish",cex.lab=1.2)
par(new=T)
plot(BT_Prediction_St$MeanBT,PT_BT_Reg_Fixed, type="l", lwd=3, lty=1,col="red", ylim=c(0,0.5),xlab="Bottom Temperature", ylab="Proportion of \nViable Habitat", main="Butterfish",cex.lab=1.2)
#text(5.5,0.308, labels = expression(paste("",R^2,"=")), cex=1.5)
#text(6.2,0.307, labels = Rsq2, cex=1.5)
par(new=T)
plot(Mean_Bottom_annual_Bottom_Temp$MeanBT, AnnualPT_Quality$Hab_Prop, pch=19,xlab="Bottom Temperature",ylim=c(0,0.5), ylab="Proportion of \nViable Habitat", main="Butterfish",cex.lab=1.2)
par(new=T)
plot(BT_Prediction_St$MeanBT,PT_BT_Reg, type="l", lwd=3, lty=1, ylim=c(0,0.5),xlab="Bottom Temperature", ylab="Proportion of \nViable Habitat", main="Butterfish",cex.lab=1.2)
#legend(5.2,0.5,bty="n", legend =c("Year-Specific","Fixed"),col=c("black","red"), pch=19)
dev.off()



########atl herring##########
#
Grouped_Data_Split_CH<-Grouped_Data_Split_Full
Grouped_Data_Split_CH$Year<-as.factor(as.character(Grouped_Data_Split_CH$Year))
Grouped_Data_Split_CH$Longitude<-as.numeric(as.character(Grouped_Data_Split_CH$Longitude))
Grouped_Data_Split_CH$Latitude<-as.numeric(as.character(Grouped_Data_Split_CH$Latitude))

#get rid of years with no data
Grouped_Data_Split_CH<-Grouped_Data_Split_CH[Grouped_Data_Split_CH$Year!="1978",]
Grouped_Data_Split_CH<-Grouped_Data_Split_CH[Grouped_Data_Split_CH$Year!="1991",]


#split into variabce blocks (A=pre door change, B=post door change, C=Bigelow)
Grouped_Data_Split_CH$Year1<-as.numeric(as.character(Grouped_Data_Split_CH$Year))
for (i in 1: nrow(Grouped_Data_Split_CH)){
  Grouped_Data_Split_CH$YearBlockA[i]<-ifelse(Grouped_Data_Split_CH$Year1[i]<1985, 0, 1)
  Grouped_Data_Split_CH$YearBlockB[i]<-ifelse(Grouped_Data_Split_CH$Year1[i]>1985 & Grouped_Data_Split_CH$Year1[i]<2009,  0, 1)
  Grouped_Data_Split_CH$YearBlockC[i]<-ifelse(Grouped_Data_Split_CH$Year1[i]>=2009, 0, 1)
}

#fake years based on abudance for intercept
Grouped_Data_Split_CH$Year[Grouped_Data_Split_CH$Year==2016]<-"2009"
Grouped_Data_Split_CH$Year[Grouped_Data_Split_CH$Year==2015]<-"2009"
Grouped_Data_Split_CH$Year[Grouped_Data_Split_CH$Year==2014]<-"2009"

Grouped_Data_Split_CH2<-Grouped_Data_Split_CH
Grouped_Data_Split_CH3<-Grouped_Data_Split_CH

#make dataset for suitability
Grouped_Data_Split_CH2$Year<-2013

Grouped_Data_Split_CH2$YearBlockB<-1
Grouped_Data_Split_CH2$YearBlockC<-0
Grouped_Data_Split_CH2$YearBlockA<-1

#make predictions
Grouped_Data_Split_CH$PA_2013<-predict(CH_Multi_Avg_Spring,Grouped_Data_Split_CH2,type="response")
Grouped_Data_Split_CH$PA<-predict(CH_Multi_Avg_Spring,Grouped_Data_Split_CH,type="response")

library(plyr)
AnnualCH_Quality<-ddply(Grouped_Data_Split_CH, .(Year1), summarize, Hab_Prop=length(which(PA>0.5 ))/length(which(!is.na(PA))), Sample_Prop= length(which(!is.na(PA)))/Tile_Count)
AnnualCH_Quality_2013<-ddply(Grouped_Data_Split_CH, .(Year1), summarize, Hab_Prop=length(which(PA_2013>0.5 ))/length(which(!is.na(PA_2013))), Sample_Prop= length(which(!is.na(PA_2013)))/Tile_Count)


library(sf)
library(rnaturalearth)
library(rnaturalearthdata)
library(viridis)
library(ggplot2)

#make habitat quality maps
Annual_Averages_CH<-ddply(Grouped_Data_Split_CH, .(Longitude, Latitude), summarize, Mean_PA=mean(PA))
world<-ne_countries(scale="medium", returnclass = "sf")
setwd("D:/Sand_Lance/SL_Chapter_2/Habitat_Occupancy/CH_Spring")
tiff("Atl_Herring_Model_Pred_210514.tif", width=6, height=5, units="in", res=300)
ggplot(data=world) + geom_raster(data = Annual_Averages_CH, aes(x=Longitude, y = Latitude, fill=Mean_PA),linejoin = "mitre") + 
  coord_fixed(ratio = 1) +
  scale_fill_viridis(direction = 1) +
  theme_bw()+ggtitle("Atl. herring")+labs(fill = "Prob. Occurrence")+geom_sf()+coord_sf(xlim=c(-75.5,-64), ylim=c(36, 44.5))
dev.off()



#compare occupancy and suitability
setwd("D:/Sand_Lance/SL_Chapter_2/Habitat_Occupancy/CH_Spring")
tiff("Atl_herring_Occupiable_Habitat_210514_Comparison.tif", height=5, width=6, res=300, units="in")
par(mar=c(4,6,4,3))
plot(AnnualCH_Quality$Year1, AnnualCH_Quality$Hab_Prop, type="l", lwd=3,xlab="Year", ylab="Proportion of \nViable Habitat", main="Atl. herring",cex.lab=1.2, ylim=c(0,0.92), col="black")
par(new=T)
plot(AnnualCH_Quality_2013$Year1, AnnualCH_Quality_2013$Hab_Prop,  type="l",lwd=3, xlab="Year", ylab="Proportion of \nViable Habitat", main="Atl. herring",cex.lab=1.2, ylim=c(0,0.92), col="red")
legend(1995,0.14,bty="n", legend =c("Year-Specific","Fixed"),col=c("black","red"), lwd=3)
dev.off()


#use betaregression for habitat proportions
AnnualCH_Quality$Hab_Prop<-AnnualCH_Quality$Hab_Prop+0.00000001
CH_Spring_Full_Hab_Occ<-betareg(AnnualCH_Quality$Hab_Prop~AnnualCH_Quality$Year1)
summary(CH_Spring_Full_Hab_Occ)

CH_Spring_Fixed_Hab_Occ<-betareg(AnnualCH_Quality_2013$Hab_Prop~AnnualCH_Quality_2013$Year1)
summary(CH_Spring_Fixed_Hab_Occ)


#compare latitude with time
Grouped_Data_Split_CH$Weighted_Lat_2013<-Grouped_Data_Split_CH$Latitude*Grouped_Data_Split_CH$PA_2013
Grouped_Data_Split_CH$Weighted_Lon_2013<-Grouped_Data_Split_CH$Longitude*Grouped_Data_Split_CH$PA_2013
Grouped_Data_Split_CH$Weighted_Lat<-Grouped_Data_Split_CH$Latitude*Grouped_Data_Split_CH$PA
Grouped_Data_Split_CH$Weighted_Lon<-Grouped_Data_Split_CH$Longitude*Grouped_Data_Split_CH$PA


#get mean weighted values
library(plyr)
Annual_Centroid_Locations_CH<-ddply(Grouped_Data_Split_CH, .(Year1), summarize, Mean_Weighted_Lat=sum(Weighted_Lat, na.rm=TRUE)/(sum(PA, na.rm=TRUE)), Mean_Weighted_Lon=sum(Weighted_Lon, na.rm=TRUE)/(sum(PA, na.rm=TRUE)))
Annual_Centroid_Locations_CH_2013<-ddply(Grouped_Data_Split_CH, .(Year1), summarize, Mean_Weighted_Lat=sum(Weighted_Lat_2013, na.rm=TRUE)/(sum(PA_2013, na.rm=TRUE)), Mean_Weighted_Lon=sum(Weighted_Lon_2013, na.rm=TRUE)/(sum(PA_2013, na.rm=TRUE)))

library(plyr)
library(ggplot2)
library(sf)
library(rnaturalearth)
library(rnaturalearthdata)
library(ggrepel)
library(viridis)
world<-ne_countries(scale="medium", returnclass = "sf")
setwd("D:/Sand_Lance/SL_Chapter_2/Habitat_Occupancy/CH_Spring")
tiff("Atl_herring_Center_Location_Year_Specific_210514.tif", width=6, height=5, units="in", res=300)
Map1<-ggplot(data=world) + geom_point(data = Annual_Centroid_Locations_CH, aes(x=Mean_Weighted_Lon, y = Mean_Weighted_Lat, colour=Year1),linejoin = "mitre") + 
  coord_fixed(ratio = 1) +  scale_color_viridis(direction = 1) +
  theme_bw()+ggtitle("Atl. herring")+geom_sf()+coord_sf(xlim=c(-75.5,-64), ylim=c(36, 44.5))+ylab("Latitude")+xlab("Longitude")
Map1 #+geom_text_repel(data= Annual_Centroid_Locations_PT, aes(x=Mean_Weighted_Lon, y = Mean_Weighted_Lat, label = Year1), size = 4, max.overlaps = 50)
dev.off()

tiff("Atl_herring_Center_Location_2013_210514.tif", width=6, height=5, units="in", res=300)
Map1<-ggplot(data=world) + geom_point(data = Annual_Centroid_Locations_CH_2013, aes(x=Mean_Weighted_Lon, y = Mean_Weighted_Lat, colour=Year1),linejoin = "mitre") + 
  coord_fixed(ratio = 1) +  scale_color_viridis(direction = 1) +
  theme_bw()+ggtitle("Atl. herring")+geom_sf()+coord_sf(xlim=c(-75.5,-64), ylim=c(36, 44.5))+ylab("Latitude")+xlab("Longitude")
Map1 #+geom_text_repel(data= Annual_Centroid_Locations_SL_1985, aes(x=Mean_Weighted_Lon, y = Mean_Weighted_Lat, label = Year1), size = 4, max.overlaps = 50)
dev.off()

#regressions with latitude
CH_Lat_Spring_Occupancy<-lm(Mean_Weighted_Lat~Year1,data=Annual_Centroid_Locations_CH)
summary(CH_Lat_Spring_Occupancy)

CH_Lat_Spring_Suitability<-lm(Mean_Weighted_Lat~Year1,data=Annual_Centroid_Locations_CH_2013)
summary(CH_Lat_Spring_Suitability)



setwd("D:/Sand_Lance/SL_Chapter_2/Habitat_Occupancy/CH_Spring")
tiff("Atl_herring_Mean_Location_PA_Comparison_210514.tif", height=5, width=6, res=300, units="in")
par(mar=c(4,6,4,3))
plot(Annual_Centroid_Locations_CH_2013$Year1, Annual_Centroid_Locations_CH_2013$Mean_Weighted_Lat,type="l", lwd=3, xlab="Year",col="red", ylim=c(41.3,41.8),ylab="Mean Weighted \nLatitude", main="Atl. herring",cex.lab=1.2)
par(new=T)
plot(Annual_Centroid_Locations_CH$Year1, Annual_Centroid_Locations_CH$Mean_Weighted_Lat,type="l", lwd=3, xlab="Year", col="black",ylim=c(41.3,41.8), ylab="Mean Weighted \nLatitude", main="Atl. herring",cex.lab=1.2)
legend(1978,41.78,bty="n", legend =c("Year-Specific","Fixed"),col=c("black","red"), lwd=3)
dev.off()


#comparisons of depth with time
Grouped_Data_Split_CH$Weighted_Depth_2013<-Grouped_Data_Split_CH$Depth*Grouped_Data_Split_CH$PA_2013
Grouped_Data_Split_CH$Weighted_Depth<-Grouped_Data_Split_CH$Depth*Grouped_Data_Split_CH$PA



library(plyr)
Annual_Centroid_Depth_CH<-ddply(Grouped_Data_Split_CH, .(Year1), summarize, Mean_Weighted_Depth=sum(Weighted_Depth, na.rm=TRUE)/(sum(PA, na.rm=TRUE)))
Annual_Centroid_Depth_CH_2013<-ddply(Grouped_Data_Split_CH, .(Year1), summarize, Mean_Weighted_Depth=sum(Weighted_Depth_2013, na.rm=TRUE)/(sum(PA_2013, na.rm=TRUE)))

setwd("D:/Sand_Lance/SL_Chapter_2/Habitat_Occupancy/CH_Spring")
tiff("Atl_herring_Mean_Depth_PA_Comparison_210514_Regridded.tif", height=5, width=6, res=300, units="in")
par(mar=c(4,6,4,3))
plot(Annual_Centroid_Depth_CH_2013$Year1, (Annual_Centroid_Depth_CH_2013$Mean_Weighted_Depth)*-1,type="l", lwd=3, xlab="Year",col="red", ylim=rev(c(70,95)),ylab="Mean Weighted \nDepth (m)", main="Atl. herring",cex.lab=1.2)
par(new=T)
plot(Annual_Centroid_Depth_CH$Year1, (Annual_Centroid_Depth_CH$Mean_Weighted_Depth)*-1, type="l", lwd=3, xlab="Year", col="black", ylim=rev(c(70,95)), ylab="Mean Weighted \nDepth (m)", main="Atl. herring",cex.lab=1.2)
legend(2001, 70,bty="n", legend =c("Year-Specific","Fixed"),col=c("black","red"), lwd=3)
dev.off()

CH_Depth_Spring_Occupancy<-lm(Mean_Weighted_Depth~Year1, data=Annual_Centroid_Depth_CH)
summary(CH_Depth_Spring_Occupancy)

CH_Depth_Spring_Suitability<-lm(Mean_Weighted_Depth~Year1, data=Annual_Centroid_Depth_CH_2013)
summary(CH_Depth_Spring_Suitability)


#make response curves
View(CH_PA_Dredge_Spring)
Best_CH_PA_GAM<-gam(PA~s(SST_Use,k=4)
                       +s(log10(pseudo_100m3+1),k=4)+s(log10(larvaceans_100m3+1), k=4)
                       +s(log10(ctyp_100m3+1), k=4)
                       +s(Estimated_Grain_Size, k=4)+s(Year, bs="re", by=ordered(!YearBlockA))+s(Year, bs="re", by=ordered(!YearBlockB))+s(Year, bs="re", by=ordered(!YearBlockC))+te(Longitude, Latitude)
                    +s(BS_Use, k=4),family=binomial,
                       data=Complete_CH_PA, na.action = "na.fail")


setwd("D:/Sand_Lance/SL_Chapter_2/Ch2_Manuscript")
tiff("Atl_herring_Bottom_Sal_210514.tif", height=5, width=5, units="in", res=300)
plot(Best_CH_PA_GAM,trans=function(x)exp(x)/(1+exp(x)), shade=T,ylab="Probability of Occurrence",xlab="Bottom Salinity", cex.lab=1.5, cex.axis=1.5 )
dev.off()

Best_CH_PA_GAM<-gam(PA~s(SST_Use,k=4)
                    +s(log10(larvaceans_100m3+1), k=4)
                    +s(log10(ctyp_100m3+1), k=4)
                    +s(Estimated_Grain_Size, k=4)+s(Year, bs="re", by=ordered(!YearBlockA))+s(Year, bs="re", by=ordered(!YearBlockB))+s(Year, bs="re", by=ordered(!YearBlockC))+te(Longitude, Latitude)
                    +s(BS_Use, k=4)+s(log10(pseudo_100m3+1),k=4),family=binomial,
                    data=Complete_CH_PA, na.action = "na.fail")



tiff("Atl_herring_Bottom_Pseudo_210514.tif", height=5, width=5, units="in", res=300)
plot(Best_CH_PA_GAM,trans=function(x)exp(x)/(1+exp(x)), shade=T,ylab="Probability of Occurrence",xlab=expression(paste(italic("Pseudocalanus "),"spp."," log10(ind/100m"^"3",")")), cex.lab=1.5, cex.axis=1.5)
dev.off()

Best_CH_PA_GAM<-gam(PA~
                    s(log10(larvaceans_100m3+1), k=4)
                    +s(log10(ctyp_100m3+1), k=4)
                    +s(Estimated_Grain_Size, k=4)+s(Year, bs="re", by=ordered(!YearBlockA))+s(Year, bs="re", by=ordered(!YearBlockB))+s(Year, bs="re", by=ordered(!YearBlockC))+te(Longitude, Latitude)
                    +s(BS_Use, k=4)+s(log10(pseudo_100m3+1),k=4)+s(SST_Use,k=4),family=binomial,
                    data=Complete_CH_PA, na.action = "na.fail")



tiff("Atl_herring_Bottom_SST_210514.tif", height=5, width=5, units="in", res=300)
plot(Best_CH_PA_GAM,trans=function(x)exp(x)/(1+exp(x)), shade=T,xlab=expression(paste("Sea Surface Temperature ("^"o","C)")), ylab="Probability of Occurrence", cex.lab=1.5, cex.axis=1.5 )
dev.off()

Best_CH_PA_GAM<-gam(PA~
                      s(log10(larvaceans_100m3+1), k=4)
                    +s(log10(ctyp_100m3+1), k=4)
                    +s(Estimated_Grain_Size, k=4)+s(Year, bs="re", by=ordered(!YearBlockA))+s(Year, bs="re", by=ordered(!YearBlockB))+s(Year, bs="re", by=ordered(!YearBlockC))+te(Longitude, Latitude)
                    +s(BS_Use, k=4)+s(log10(pseudo_100m3+1),k=4)+s(SST_Use,k=4),family=binomial,
                    data=Complete_CH_PA, na.action = "na.fail", method="REML")
summary_gamm(Best_CH_PA_GAM)

#compare with bottom salinity and SST
Mean_annual_Bottom_Sal<-ddply(Grouped_Data_Split_CH, .(Year1), summarize, MeanBS=mean(BS_Use, na.rm=T))
Mean_annual_SST<-ddply(Grouped_Data_Split_CH, .(Year1), summarize, MeanSST=mean(SST_Use, na.rm=T))

Herring_Sal_Data<-cbind(Mean_annual_Bottom_Sal$MeanBS,AnnualCH_Quality$Hab_Prop, AnnualCH_Quality_2013$Hab_Prop) 
colnames(Herring_Sal_Data)<-c("MeanBS","Hab_Prop","Hab_Prop_Fixed")
Herring_Sal_Data<-as.data.frame(Herring_Sal_Data)
CH_Spring_BS_Model<-betareg(Hab_Prop~MeanBS, data=Herring_Sal_Data)
summary(CH_Spring_BS_Model)

CH_Spring_BS_Model_Fixed<-betareg(Hab_Prop_Fixed~MeanBS, data=Herring_Sal_Data)
summary(CH_Spring_BS_Model_Fixed)

CH_Spring_SST_Model<-betareg(AnnualCH_Quality$Hab_Prop~Mean_annual_SST$MeanSST)
summary(CH_Spring_SST_Model)

CH_Spring_SST_Model_Fixed<-betareg(AnnualCH_Quality_2013$Hab_Prop~Mean_annual_SST$MeanSST)
summary(CH_Spring_SST_Model_Fixed)


#make figure for bottom salinity
BS_Prediction_St<-as.data.frame(seq(from=min(Mean_annual_Bottom_Sal$MeanBS), to=max(Mean_annual_Bottom_Sal$MeanBS), by=0.01))
colnames(BS_Prediction_St)<-"MeanBS"

Herr_BS_Reg<-predict(CH_Spring_BS_Model, BS_Prediction_St, type="response")
Herr_BS_Reg_Fixed<-predict(CH_Spring_BS_Model_Fixed, BS_Prediction_St, type="response")

setwd("D:/Sand_Lance/SL_Chapter_2/Habitat_Occupancy/CH_Spring")
tiff("Atl_herring_Bottom_Sal_210514.tif", height=5, width=6, res=300, units="in")
par(mar=c(4,6,4,3))
plot(Mean_annual_Bottom_Sal$MeanBS, AnnualCH_Quality_1985$Hab_Prop, pch=19,col="red",ylim=c(0,1),xlab="Bottom Salinity", ylab="Proportion of \nViable Habitat", main="Atl. herring",cex.lab=1.2)
par(new=T)
plot(BS_Prediction_St$MeanBS, Herr_BS_Reg_Fixed, type="l", lwd=3, lty=1,col="red", ylim=c(0,1),xlab="Bottom Salinity", ylab="Proportion of \nViable Habitat", main="Atl. herring",cex.lab=1.2)
#text(5.5,0.308, labels = expression(paste("",R^2,"=")), cex=1.5)
#text(6.2,0.307, labels = Rsq2, cex=1.5)
par(new=T)
plot(Mean_annual_Bottom_Sal$MeanBS, AnnualCH_Quality$Hab_Prop, pch=19,xlab="Bottom Salinity",ylim=c(0,1), ylab="Proportion of \nViable Habitat", main="Atl. herring",cex.lab=1.2)
par(new=T)
plot(BS_Prediction_St$MeanBS, Herr_BS_Reg, type="l", lwd=4, lty=3, ylim=c(0,1),xlab="Bottom Salinity", ylab="Proportion of \nViable Habitat", main="Atl. herring",cex.lab=1.2)
#legend(5.2,0.5,bty="n", legend =c("Year-Specific","Fixed"),col=c("black","red"), pch=19)
dev.off()



######### alewife###########
Grouped_Data_Split_AP<-Grouped_Data_Split_Full
Grouped_Data_Split_AP$Year<-as.factor(as.character(Grouped_Data_Split_AP$Year))

Grouped_Data_Split_AP$Longitude<-as.numeric(as.character(Grouped_Data_Split_AP$Longitude))
Grouped_Data_Split_AP$Latitude<-as.numeric(as.character(Grouped_Data_Split_AP$Latitude))

#getting rid of years with no data 
Grouped_Data_Split_AP<-Grouped_Data_Split_AP[Grouped_Data_Split_AP$Year!="1991",]
Grouped_Data_Split_AP<-Grouped_Data_Split_AP[Grouped_Data_Split_AP$Year!="1978",]

Grouped_Data_Split_AP<-Grouped_Data_Split_AP[Grouped_Data_Split_AP$SEDNUM!="0",]


#set year blocks, same as CH
Grouped_Data_Split_AP$Year1<-as.numeric(as.character(Grouped_Data_Split_AP$Year))
for (i in 1: nrow(Grouped_Data_Split_AP)){
  Grouped_Data_Split_AP$YearBlockA[i]<-ifelse(Grouped_Data_Split_AP$Year1[i]<1985, 0, 1)
  Grouped_Data_Split_AP$YearBlockB[i]<-ifelse(Grouped_Data_Split_AP$Year1[i]>1985 & Grouped_Data_Split_AP$Year1[i]<2009,  0, 1)
  Grouped_Data_Split_AP$YearBlockC[i]<-ifelse(Grouped_Data_Split_AP$Year1[i]>=2009, 0, 1)
}

#substiture year intercepts
Grouped_Data_Split_AP$Year[Grouped_Data_Split_AP$Year==2016]<-"2011"
Grouped_Data_Split_AP$Year[Grouped_Data_Split_AP$Year==2015]<-"2010"
Grouped_Data_Split_AP$Year[Grouped_Data_Split_AP$Year==2014]<-"2010"

Grouped_Data_Split_AP2<-Grouped_Data_Split_AP
#suitability dataset
Grouped_Data_Split_AP2$Year<-2013

Grouped_Data_Split_AP2$YearBlockB<-1
Grouped_Data_Split_AP2$YearBlockC<-0
Grouped_Data_Split_AP2$YearBlockA<-1

#make predicitions
Grouped_Data_Split_AP$PA<-predict(AP_Multi_Avg_Spring,Grouped_Data_Split_AP,type="response")
Grouped_Data_Split_AP$PA_2013<-predict(AP_Multi_Avg_Spring,Grouped_Data_Split_AP2,type="response")


AnnualAP_Quality<-ddply(Grouped_Data_Split_AP, .(Year1), summarize, Hab_Prop=length(which(PA>0.5 ))/length(which(!is.na(PA))), Sample_Prop= length(which(!is.na(PA)))/Tile_Count)
AnnualAP_Quality_2013<-ddply(Grouped_Data_Split_AP, .(Year1), summarize, Hab_Prop=length(which(PA_2013>0.5 ))/length(which(!is.na(PA_2013))), Sample_Prop= length(which(!is.na(PA_2013)))/Tile_Count)

plot(AnnualAP_Quality$Year, AnnualAP_Quality$Hab_Prop)
AnnualAP_Quality$Year1<-as.numeric(as.character(AnnualAP_Quality$Year))
AnnualAP_Quality_2013$Year1<-as.numeric(as.character(AnnualAP_Quality_2013$Year))

library(sf)
library(rnaturalearth)
library(rnaturalearthdata)
Annual_Averages_AP<-ddply(Grouped_Data_Split_AP, .(Longitude, Latitude), summarize, Mean_PA=mean(PA))

world<-ne_countries(scale="medium", returnclass = "sf")
setwd("D:/Sand_Lance/SL_Chapter_2/Habitat_Occupancy/AP_Spring")

tiff("Alewife_Model_Pred_210514.tif", width=6, height=5, units="in", res=300)
ggplot(data=world) + geom_raster(data = Annual_Averages_AP, aes(x=Longitude, y = Latitude, fill=Mean_PA),linejoin = "mitre") + 
  coord_fixed(ratio = 1) +
  scale_fill_viridis(direction = 1) +
  theme_bw()+ggtitle("Alewife")+labs(fill = "Prob. Occurrence")+geom_sf()+coord_sf(xlim=c(-75.5,-64), ylim=c(36, 44.5))
dev.off()

#estimate trends in habitat suitability thriugh betaregression

AnnualAP_Quality$Hab_Prop<-AnnualAP_Quality$Hab_Prop+0.00000001
AP_Spring_Full_Hab_Occ<-betareg(AnnualAP_Quality$Hab_Prop~AnnualAP_Quality$Year1)
summary(AP_Spring_Full_Hab_Occ)

AP_Spring_Fixed_Hab_Occ<-betareg(AnnualAP_Quality_2013$Hab_Prop~AnnualAP_Quality_2013$Year1)
summary(AP_Spring_Fixed_Hab_Occ)


setwd("D:/Sand_Lance/SL_Chapter_2/Habitat_Occupancy/AP_Spring")
tiff("Alewife_Occupiable_Habitat_Comparison_210514.tif", height=5, width=6, res=300, units="in")
par(mar=c(4,6,4,3))
plot(AnnualAP_Quality$Year1, AnnualAP_Quality$Hab_Prop, type="l", lwd=3,xlab="Year", ylab="Proportion of \nViable Habitat", main="Alewife",cex.lab=1.2, ylim=c(0,0.8), col="black")
par(new=T)
plot(AnnualAP_Quality_2013$Year1, AnnualAP_Quality_2013$Hab_Prop,  type="l",lwd=3, xlab="Year", ylab="Proportion of \nViable Habitat", main="Alewife",cex.lab=1.2, ylim=c(0,0.8), col="red")
legend(1995,0.2,bty="n", legend =c("Year-Specific","Fixed"),col=c("black","red"), lwd=3)
dev.off()


#calculate mean weighted latitude

Grouped_Data_Split_AP$Weighted_Lat_2013<-Grouped_Data_Split_AP$Latitude*Grouped_Data_Split_AP$PA_2013
Grouped_Data_Split_AP$Weighted_Lon_2013<-Grouped_Data_Split_AP$Longitude*Grouped_Data_Split_AP$PA_2013
Grouped_Data_Split_AP$Weighted_Lat<-Grouped_Data_Split_AP$Latitude*Grouped_Data_Split_AP$PA
Grouped_Data_Split_AP$Weighted_Lon<-Grouped_Data_Split_AP$Longitude*Grouped_Data_Split_AP$PA

library(plyr)
Annual_Centroid_Locations_AP<-ddply(Grouped_Data_Split_AP, .(Year1), summarize, Mean_Weighted_Lat=sum(Weighted_Lat, na.rm=TRUE)/(sum(PA, na.rm=TRUE)), Mean_Weighted_Lon=sum(Weighted_Lon, na.rm=TRUE)/(sum(PA, na.rm=TRUE)))
Annual_Centroid_Locations_AP_2013<-ddply(Grouped_Data_Split_AP, .(Year1), summarize, Mean_Weighted_Lat=sum(Weighted_Lat_2013, na.rm=TRUE)/(sum(PA_2013, na.rm=TRUE)), Mean_Weighted_Lon=sum(Weighted_Lon_2013, na.rm=TRUE)/(sum(PA_2013, na.rm=TRUE)))

library(plyr)
library(ggplot2)
library(sf)
library(rnaturalearth)
library(rnaturalearthdata)
library(ggrepel)
library(viridis)
world<-ne_countries(scale="medium", returnclass = "sf")
setwd("D:/Sand_Lance/SL_Chapter_2/Habitat_Occupancy/AP_Spring")
tiff("Alewife_Center_Location_Year_Specific_210514.tif", width=6, height=5, units="in", res=300)
Map1<-ggplot(data=world) + geom_point(data = Annual_Centroid_Locations_AP, aes(x=Mean_Weighted_Lon, y = Mean_Weighted_Lat, colour=Year1),linejoin = "mitre") + 
  coord_fixed(ratio = 1) +  scale_color_viridis(direction = 1) +
  theme_bw()+ggtitle("Alewife")+geom_sf()+coord_sf(xlim=c(-75.5,-64), ylim=c(36, 44.5))+ylab("Latitude")+xlab("Longitude")
Map1 #+geom_text_repel(data= Annual_Centroid_Locations_PT, aes(x=Mean_Weighted_Lon, y = Mean_Weighted_Lat, label = Year1), size = 4, max.overlaps = 50)
dev.off()

setwd("D:/Sand_Lance/SL_Chapter_2/Habitat_Occupancy/AP_Spring")
tiff("Alewife_Center_Location_2013_210514.tif", width=6, height=5, units="in", res=300)
Map1<-ggplot(data=world) + geom_point(data = Annual_Centroid_Locations_AP_2013, aes(x=Mean_Weighted_Lon, y = Mean_Weighted_Lat, colour=Year1),linejoin = "mitre") + 
  coord_fixed(ratio = 1) +  scale_color_viridis(direction = 1) +
  theme_bw()+ggtitle("Alewife")+geom_sf()+coord_sf(xlim=c(-75.5,-64), ylim=c(36, 44.5))+ylab("Latitude")+xlab("Longitude")
Map1 #+geom_text_repel(data= Annual_Centroid_Locations_SL_1985, aes(x=Mean_Weighted_Lon, y = Mean_Weighted_Lat, label = Year1), size = 4, max.overlaps = 50)
dev.off()

#calculate annual trends in latitude 
AP_Spring_Lat_Occupancy<-lm(Mean_Weighted_Lat~Year1, data=Annual_Centroid_Locations_AP)
summary(AP_Spring_Lat_Occupancy)

AP_Spring_Lat_Suitability<-lm(Mean_Weighted_Lat~Year1, data=Annual_Centroid_Locations_AP_2013)
summary(AP_Spring_Lat_Suitability)


setwd("D:/Sand_Lance/SL_Chapter_2/Habitat_Occupancy/AP_Spring")
tiff("Alewife_Mean_Location_PA_Comparison_210514.tif", height=5, width=6, res=300, units="in")
par(mar=c(4,6,4,3))
plot(Annual_Centroid_Locations_AP_2013$Year1, Annual_Centroid_Locations_AP_2013$Mean_Weighted_Lat,type="l", lwd=3, xlab="Year",col="red", ylim=c(41.5,42.4),ylab="Mean Weighted \nLatitude", main="Alewife",cex.lab=1.2)
par(new=T)
plot(Annual_Centroid_Locations_AP$Year1, Annual_Centroid_Locations_AP$Mean_Weighted_Lat,type="l", lwd=3, xlab="Year", col="black",ylim=c(41.5,42.4), ylab="Mean Weighted \nLatitude", main="Alewife",cex.lab=1.2)
legend(1978,42.38,bty="n", legend =c("Year-Specific","Fixed"),col=c("black","red"), lwd=3)
dev.off()

#Calculate mean weighted depth
Grouped_Data_Split_AP$Weighted_Depth_2013<-Grouped_Data_Split_AP$Depth*Grouped_Data_Split_AP$PA_2013
Grouped_Data_Split_AP$Weighted_Depth<-Grouped_Data_Split_AP$Depth*Grouped_Data_Split_AP$PA

library(plyr)
Annual_Centroid_Depth_AP<-ddply(Grouped_Data_Split_AP, .(Year1), summarize, Mean_Weighted_Depth=sum(Weighted_Depth, na.rm=TRUE)/(sum(PA, na.rm=TRUE)))
Annual_Centroid_Depth_AP_2013<-ddply(Grouped_Data_Split_AP, .(Year1), summarize, Mean_Weighted_Depth=sum(Weighted_Depth_2013, na.rm=TRUE)/(sum(PA_2013, na.rm=TRUE)))

setwd("D:/Sand_Lance/SL_Chapter_2/Habitat_Occupancy/AP_Spring")
tiff("Alewife_Mean_Depth_PA_Comparison_210514_Regridded.tif", height=5, width=6, res=300, units="in")
par(mar=c(4,6,4,3))
plot(Annual_Centroid_Depth_AP_2013$Year1, (Annual_Centroid_Depth_AP_2013$Mean_Weighted_Depth)*-1,type="l", lwd=3, xlab="Year",col="red", ylim=rev(c(100,120)),ylab="Mean Weighted \nDepth (m)", main="Alewife",cex.lab=1.2)
par(new=T)
plot(Annual_Centroid_Depth_AP$Year1, (Annual_Centroid_Depth_AP$Mean_Weighted_Depth)*-1, type="l", lwd=3, xlab="Year", col="black", ylim=rev(c(100,120)), ylab="Mean Weighted \nDepth (m)", main="Alewife",cex.lab=1.2)
legend(1978,100,bty="n", legend =c("Year-Specific","Fixed"),col=c("black","red"), lwd=3)
dev.off()

AP_Depth_Spring_Occupancy<-lm(Mean_Weighted_Depth~Year1, data=Annual_Centroid_Depth_AP)
summary(AP_Depth_Spring_Occupancy)

AP_Depth_Spring_Suitability<-lm(Mean_Weighted_Depth~Year1, data=Annual_Centroid_Depth_AP_2013)
summary(AP_Depth_Spring_Suitability)

#response curves 

Best_AP_PA_GAM<-gam(PA~s(SST_Use, k=4)+s(EKE, k=4)+s(Slope_try,k=4)
                    +s(log10(pseudo_100m3+1),k=4)+s(log10(hyper_100m3+1), k=4)
                    +s(log10(ctyp_100m3+1), k=4)
                    +s(log10(calfin_100m3+1), k=4)+s(Year, bs="re", by=ordered(!YearBlockA))+s(Year, bs="re", by=ordered(!YearBlockB))+s(Year, bs="re", by=ordered(!YearBlockC))+te(Longitude, Latitude)
                    +s(BT_Use,k=4),family=binomial,
                    data=Complete_AP_PA, na.action = "na.fail")

#View(AP_PA_Dredge)

setwd("D:/Sand_Lance/SL_Chapter_2/Ch2_Manuscript")
tiff("Alewife_bottom_temp_210514.tif", height=5, width=5, units="in", res=300)
plot(Best_AP_PA_GAM,trans=function(x)exp(x)/(1+exp(x)), shade=T,ylab="Probability of Occurrence",xlab=expression(paste("Bottom Temperature ("^"o","C)" )), cex.lab=1.5, cex.axis=1.5)
dev.off()

Best_AP_PA_GAM<-gam(PA~s(EKE, k=4)+s(Slope_try,k=4)
                    +s(log10(pseudo_100m3+1),k=4)+s(log10(hyper_100m3+1), k=4)
                    +s(log10(ctyp_100m3+1), k=4)
                    +s(log10(calfin_100m3+1), k=4)+s(Year, bs="re", by=ordered(!YearBlockA))+s(Year, bs="re", by=ordered(!YearBlockB))+s(Year, bs="re", by=ordered(!YearBlockC))+te(Longitude, Latitude)
                    +s(BT_Use,k=4)+s(SST_Use, k=4),family=binomial,
                    data=Complete_AP_PA, na.action = "na.fail")

tiff("Alewife_Bottom_SST_210514.tif", height=5, width=5, units="in", res=300)
plot(Best_AP_PA_GAM,trans=function(x)exp(x)/(1+exp(x)), shade=T,xlab=expression(paste("Sea Surface Temperature ("^"o","C)")), ylab="Probability of Occurrence", cex.lab=1.5, cex.axis=1.5 )
dev.off()

Best_AP_PA_GAM<-gam(PA~s(EKE, k=4)+s(Slope_try,k=4)
                    +s(log10(hyper_100m3+1), k=4)
                    +s(log10(ctyp_100m3+1), k=4)
                    +s(log10(calfin_100m3+1), k=4)+s(Year, bs="re", by=ordered(!YearBlockA))+s(Year, bs="re", by=ordered(!YearBlockB))+s(Year, bs="re", by=ordered(!YearBlockC))+te(Longitude, Latitude)
                    +s(BT_Use,k=4)+s(SST_Use, k=4)+s(log10(pseudo_100m3+1),k=4),family=binomial,
                    data=Complete_AP_PA, na.action = "na.fail")
tiff("Alewife_Bottom_Pseudo_210514.tif", height=5, width=5, units="in", res=300)
plot(Best_AP_PA_GAM,trans=function(x)exp(x)/(1+exp(x)), shade=T,ylab="Probability of Occurrence",xlab=expression(paste(italic("Pseudocalanus "),"spp. ","log10(ind/100m"^"3",")")),cex.lab=1.5, cex.axis=1.5 )
dev.off()

Best_AP_PA_GAM<-gam(PA~s(EKE, k=4)+s(Slope_try,k=4)
                    +s(log10(hyper_100m3+1), k=4)
                    +s(log10(ctyp_100m3+1), k=4)
                    +s(log10(calfin_100m3+1), k=4)+s(Year, bs="re", by=ordered(!YearBlockA))+s(Year, bs="re", by=ordered(!YearBlockB))+s(Year, bs="re", by=ordered(!YearBlockC))+te(Longitude, Latitude)
                    +s(BT_Use,k=4)+s(SST_Use, k=4)+s(log10(pseudo_100m3+1),k=4),family=binomial,
                    data=Complete_AP_PA, na.action = "na.fail", method="REML")

summary_gamm(Best_AP_PA_GAM)
library(plyr)

#compare SST and BT and suitability and occupancy 
Mean_annual_SST<-ddply(Grouped_Data_Split_AP, .(Year1), summarize, MeanSST=mean(SST_Use, na.rm=T))
Mean_annual_BT<-ddply(Grouped_Data_Split_AP, .(Year1), summarize, MeanBT=mean(BT_Use, na.rm=T))
BT_Prediction_St<-as.data.frame(seq(from=min(Mean_annual_BT$MeanBT), to=max(Mean_annual_BT$MeanBT), by=0.01))
colnames(BT_Prediction_St)<-"MeanBT"


AP_BT_Data<-cbind(Mean_annual_BT$MeanBT,AnnualAP_Quality$Hab_Prop, AnnualAP_Quality_2013$Hab_Prop) 
colnames(AP_BT_Data)<-c("MeanBT","Hab_Prop","Hab_Prop_Fixed")
AP_BT_Data<-as.data.frame(AP_BT_Data)

AP_Spring_BT_Model<-betareg(Hab_Prop~MeanBT, data=AP_BT_Data)
summary(AP_Spring_BT_Model)

AP_Spring_BT_Model_Fixed<-betareg(Hab_Prop_Fixed~MeanBT, data=AP_BT_Data)
summary(AP_Spring_BT_Model_Fixed)


AP_BT_Reg<-predict(AP_, BT_Prediction_St, type="response")
AP_BT_Reg_Fixed<-predict(CH_Spring_BS_Model_Fixed, BS_Prediction_St, type="response")

AP_Spring_SST_Model<-betareg(AnnualAP_Quality$Hab_Prop~Mean_annual_SST$MeanSST)
summary(AP_Spring_SST_Model)

AP_Spring_SST_Model_Fixed<-betareg(AnnualAP_Quality_1985$Hab_Prop~Mean_annual_SST$MeanSST)
summary(AP_Spring_SST_Model_Fixed)

AP_BT_Reg<-predict(AP_Spring_BT_Model, BT_Prediction_St, type="response")
AP_BT_Reg_Fixed<-predict(AP_Spring_BT_Model_Fixed, BT_Prediction_St, type="response")


setwd("D:/Sand_Lance/SL_Chapter_2/Habitat_Occupancy/AP_Spring")
tiff("Alewife_Bottom_Temp_210514.tif", height=5, width=6, res=300, units="in")
par(mar=c(4,6,4,3))
plot(Mean_annual_BT$MeanBT, AnnualAP_Quality_1985$Hab_Prop, pch=19,col="red",ylim=c(0.2,0.8),xlab="Bottom Temperature", ylab="Proportion of \nViable Habitat", main="Alewife",cex.lab=1.2)
par(new=T)
plot(BT_Prediction_St$MeanBT,AP_BT_Reg_Fixed, type="l", lwd=3, lty=1,col="red", ylim=c(0.2,0.8),xlab="Bottom Temperature", ylab="Proportion of \nViable Habitat", main="Alewife",cex.lab=1.2)
#text(5.5,0.308, labels = expression(paste("",R^2,"=")), cex=1.5)
#text(6.2,0.307, labels = Rsq2, cex=1.5)
par(new=T)
plot(BT_Prediction_St$MeanBT, AP_BT_Reg, type="l", lwd=3, lty=3, ylim=c(0.2,0.8),xlab="Bottom Temperature", ylab="Proportion of \nViable Habitat", main="Alewife",cex.lab=1.2)
par(new=T)
plot(Mean_annual_BT$MeanBT, AnnualAP_Quality$Hab_Prop, pch=19,ylim=c(0.2,0.8),xlab="Bottom Temperature", ylab="Proportion of \nViable Habitat", main="Alewife",cex.lab=1.2)
#legend(5.2,0.5,bty="n", legend =c("Year-Specific","Fixed"),col=c("black","red"), pch=19)
dev.off()





######blueback#########
Grouped_Data_Split_AA<-Grouped_Data_Split_Full
Grouped_Data_Split_AA$Year<-as.factor(as.character(Grouped_Data_Split_AA$Year))
Grouped_Data_Split_AA$Longitude<-as.numeric(as.character(Grouped_Data_Split_AA$Longitude))
Grouped_Data_Split_AA$Latitude<-as.numeric(as.character(Grouped_Data_Split_AA$Latitude))

#exclude years without data
Grouped_Data_Split_AA<-Grouped_Data_Split_AA[Grouped_Data_Split_AA$Year!="1991",]
Grouped_Data_Split_AA<-Grouped_Data_Split_AA[Grouped_Data_Split_AA$Year!="1978",]

#Get rid of land 
Grouped_Data_Split_AA<-Grouped_Data_Split_AA[Grouped_Data_Split_AA$SEDNUM!="0",]


#assign by gear changes
Grouped_Data_Split_AA$Year1<-as.numeric(as.character(Grouped_Data_Split_AA$Year))
for (i in 1: nrow(Grouped_Data_Split_AA)){
  Grouped_Data_Split_AA$YearBlockA[i]<-ifelse(Grouped_Data_Split_AA$Year1[i]<1985, 0, 1)
  Grouped_Data_Split_AA$YearBlockB[i]<-ifelse(Grouped_Data_Split_AA$Year1[i]>1985 & Grouped_Data_Split_AA$Year1[i]<2009,  0, 1)
  Grouped_Data_Split_AA$YearBlockC[i]<-ifelse(Grouped_Data_Split_AA$Year1[i]>=2009, 0, 1)
}

#assign replacement year with abundance
Grouped_Data_Split_AA$Year[Grouped_Data_Split_AA$Year==2016]<-"2012"
Grouped_Data_Split_AA$Year[Grouped_Data_Split_AA$Year==2015]<-"2010"
Grouped_Data_Split_AA$Year[Grouped_Data_Split_AA$Year==2014]<-"2011"

#making a suitability dataframe
Grouped_Data_Split_AA2<-Grouped_Data_Split_AA
Grouped_Data_Split_AA2$Year<-2013

Grouped_Data_Split_AA2$YearBlockB<-1
Grouped_Data_Split_AA2$YearBlockC<-0
Grouped_Data_Split_AA2$YearBlockA<-1

#make predictions 
Grouped_Data_Split_AA$PA<-predict(AA_Multi_Avg_Spring,Grouped_Data_Split_AA,type="response")
Grouped_Data_Split_AA$PA_2013<-predict(AA_Multi_Avg_Spring,Grouped_Data_Split_AA2,type="response")

#make annual estimates
AnnualAA_Quality<-ddply(Grouped_Data_Split_AA, .(Year1), summarize, Hab_Prop=length(which(PA>0.5 ))/length(which(!is.na(PA))), Sample_Prop= length(which(!is.na(PA)))/Tile_Count)
AnnualAA_Quality_2013<-ddply(Grouped_Data_Split_AA, .(Year1), summarize, Hab_Prop=length(which(PA_2013>0.5 ))/length(which(!is.na(PA_2013))), Sample_Prop= length(which(!is.na(PA)))/Tile_Count)

AnnualAA_Quality$Year1<-as.numeric(as.character(AnnualAA_Quality$Year))
AnnualAA_Quality_2013$Year1<-as.numeric(as.character(AnnualAA_Quality_2013$Year))

library(sf)
library(rnaturalearth)
library(rnaturalearthdata)
Annual_Averages_AA<-ddply(Grouped_Data_Split_AA, .(Longitude, Latitude), summarize, Mean_PA=mean(PA))
library(ggplot2)
library(viridis)
world<-ne_countries(scale="medium", returnclass = "sf")
setwd("D:/Sand_Lance/SL_Chapter_2/Habitat_Occupancy/AA_Spring")
tiff("Blueback_Model_Pred_210514.tif", width=6, height=5, units="in", res=300)
ggplot(data=world) + geom_raster(data = Annual_Averages_AA, aes(x=Longitude, y = Latitude, fill=Mean_PA),linejoin = "mitre") + 
  coord_fixed(ratio = 1) +
  scale_fill_viridis(direction = 1) +
  theme_bw()+ggtitle("Blueback herring")+labs(fill = "Prob. Occurrence")+geom_sf()+coord_sf(xlim=c(-75.5,-64), ylim=c(36, 44.5))
dev.off()

#check changes in occupancy and suitability with betaregression

AnnualAA_Quality$Hab_Prop<-AnnualAA_Quality$Hab_Prop+0.00000001
AA_Spring_Full_Hab_Occ<-betareg(AnnualAA_Quality$Hab_Prop~AnnualAA_Quality$Year1)
summary(AA_Spring_Full_Hab_Occ)

AA_Spring_Fixed_Hab_Occ<-betareg(AnnualAA_Quality_2013$Hab_Prop~AnnualAA_Quality_2013$Year1)
summary(AA_Spring_Fixed_Hab_Occ)


setwd("D:/Sand_Lance/SL_Chapter_2/Habitat_Occupancy/AA_Spring")
tiff("Blueback_Occupiable_Habitat_Comparison_210514.tif", height=5, width=6, res=300, units="in")
par(mar=c(4,6,4,3))
plot(AnnualAA_Quality$Year1, AnnualAA_Quality$Hab_Prop, type="l", lwd=3,xlab="Year", ylab="Proportion of \nViable Habitat", main="Blueback herring",cex.lab=1.2, ylim=c(0,0.55), col="black")
par(new=T)
plot(AnnualAA_Quality_2013$Year1, AnnualAA_Quality_2013$Hab_Prop,  type="l",lwd=3, xlab="Year", ylab="Proportion of \nViable Habitat", main="Blueback herring",cex.lab=1.2, ylim=c(0,0.55), col="red")
legend(1995,0.55,bty="n", legend =c("Year-Specific","Fixed"),col=c("black","red"), lwd=3)
dev.off()


#annual changes in latitude 
Grouped_Data_Split_AA$Weighted_Lat_2013<-Grouped_Data_Split_AA$Latitude*Grouped_Data_Split_AA$PA_2013
Grouped_Data_Split_AA$Weighted_Lon_2013<-Grouped_Data_Split_AA$Longitude*Grouped_Data_Split_AA$PA_2013
Grouped_Data_Split_AA$Weighted_Lat<-Grouped_Data_Split_AA$Latitude*Grouped_Data_Split_AA$PA
Grouped_Data_Split_AA$Weighted_Lon<-Grouped_Data_Split_AA$Longitude*Grouped_Data_Split_AA$PA
library(plyr)
Annual_Centroid_Locations_AA<-ddply(Grouped_Data_Split_AA, .(Year1), summarize, Mean_Weighted_Lat=sum(Weighted_Lat, na.rm=TRUE)/(sum(PA, na.rm=TRUE)), Mean_Weighted_Lon=sum(Weighted_Lon, na.rm=TRUE)/(sum(PA, na.rm=TRUE)))
Annual_Centroid_Locations_AA_2013<-ddply(Grouped_Data_Split_AA, .(Year1), summarize, Mean_Weighted_Lat=sum(Weighted_Lat_2013, na.rm=TRUE)/(sum(PA_2013, na.rm=TRUE)), Mean_Weighted_Lon=sum(Weighted_Lon_2013, na.rm=TRUE)/(sum(PA_2013, na.rm=TRUE)))

library(plyr)
library(ggplot2)
library(sf)
library(rnaturalearth)
library(rnaturalearthdata)
library(ggrepel)
library(viridis)
world<-ne_countries(scale="medium", returnclass = "sf")
setwd("D:/Sand_Lance/SL_Chapter_2/Habitat_Occupancy/AA_Spring")
tiff("Blueback_Center_Location_Year_Specific_210514.tif", width=6, height=5, units="in", res=300)
Map1<-ggplot(data=world) + geom_point(data = Annual_Centroid_Locations_AA, aes(x=Mean_Weighted_Lon, y = Mean_Weighted_Lat, colour=Year1),linejoin = "mitre") + 
  coord_fixed(ratio = 1) +  scale_color_viridis(direction = 1) +
  theme_bw()+ggtitle("Blueback herring")+geom_sf()+coord_sf(xlim=c(-75.5,-64), ylim=c(36, 44.5))+ylab("Latitude")+xlab("Longitude")
Map1 #+geom_text_repel(data= Annual_Centroid_Locations_PT, aes(x=Mean_Weighted_Lon, y = Mean_Weighted_Lat, label = Year1), size = 4, max.overlaps = 50)
dev.off()

setwd("D:/Sand_Lance/SL_Chapter_2/Habitat_Occupancy/AA_Spring")
tiff("Blueback_Center_Location_2013_210514.tif", width=6, height=5, units="in", res=300)
Map1<-ggplot(data=world) + geom_point(data = Annual_Centroid_Locations_AA_2013, aes(x=Mean_Weighted_Lon, y = Mean_Weighted_Lat, colour=Year1),linejoin = "mitre") + 
  coord_fixed(ratio = 1) +  scale_color_viridis(direction = 1) +
  theme_bw()+ggtitle("Blueback herring")+geom_sf()+coord_sf(xlim=c(-75.5,-64), ylim=c(36, 44.5))+ylab("Latitude")+xlab("Longitude")
Map1 #+geom_text_repel(data= Annual_Centroid_Locations_SL_1985, aes(x=Mean_Weighted_Lon, y = Mean_Weighted_Lat, label = Year1), size = 4, max.overlaps = 50)
dev.off()

AA_Lat_Spring_Occupancy<-lm(Mean_Weighted_Lat~Year1, data=Annual_Centroid_Locations_AA)
summary(AA_Lat_Spring_Occupancy)

AA_Lat_Spring_Suitability<-lm(Mean_Weighted_Lat~Year1, data=Annual_Centroid_Locations_AA_2013)
summary(AA_Lat_Spring_Suitability)

setwd("D:/Sand_Lance/SL_Chapter_2/Habitat_Occupancy/AA_Spring")
tiff("Blueback_Mean_Location_PA_Comparison_210514.tif", height=5, width=6, res=300, units="in")
par(mar=c(4,6,4,3))
plot(Annual_Centroid_Locations_AA_2013$Year1, Annual_Centroid_Locations_AA_2013$Mean_Weighted_Lat,type="l", lwd=3, xlab="Year",col="red", ylim=c(40.7,41.8),ylab="Mean Weighted \nLatitude", main="Blueback",cex.lab=1.2)
par(new=T)
plot(Annual_Centroid_Locations_AA$Year1, Annual_Centroid_Locations_AA$Mean_Weighted_Lat,type="l", lwd=3, xlab="Year", col="black",ylim=c(40.7,41.8), ylab="Mean Weighted \nLatitude", main="Blueback",cex.lab=1.2)
legend(1978,41.75,bty="n", legend =c("Year-Specific","Fixed"),col=c("black","red"), lwd=3)
dev.off()

#trends in depth and year
Grouped_Data_Split_AA$Weighted_Depth_2013<-Grouped_Data_Split_AA$Depth*Grouped_Data_Split_AA$PA_2013
Grouped_Data_Split_AA$Weighted_Depth<-Grouped_Data_Split_AA$Depth*Grouped_Data_Split_AA$PA

library(plyr)
Annual_Centroid_Depth_AA<-ddply(Grouped_Data_Split_AA, .(Year1), summarize, Mean_Weighted_Depth=sum(Weighted_Depth, na.rm=TRUE)/(sum(PA, na.rm=TRUE)))
Annual_Centroid_Depth_AA_2013<-ddply(Grouped_Data_Split_AA, .(Year1), summarize, Mean_Weighted_Depth=sum(Weighted_Depth_2013, na.rm=TRUE)/(sum(PA_2013, na.rm=TRUE)))

setwd("D:/Sand_Lance/SL_Chapter_2/Habitat_Occupancy/AA_Spring")
tiff("Blueback_Mean_Depth_PA_Comparison_210514_Regridded.tif", height=5, width=6, res=300, units="in")
par(mar=c(4,6,4,3))
plot(Annual_Centroid_Depth_AA_2013$Year1, (Annual_Centroid_Depth_AA_2013$Mean_Weighted_Depth)*-1,type="l", lwd=3, xlab="Year",col="red", ylim=rev(c(70,95)),ylab="Mean Weighted \nDepth (m)", main="Blueback",cex.lab=1.2)
par(new=T)
plot(Annual_Centroid_Depth_AA$Year1, (Annual_Centroid_Depth_AA$Mean_Weighted_Depth)*-1, type="l", lwd=3, xlab="Year", col="black", ylim=rev(c(70,95)), ylab="Mean Weighted \nDepth (m)", main="Blueback",cex.lab=1.2)
legend(2001,70,bty="n", legend =c("Year-Specific","Fixed"),col=c("black","red"), lwd=3)
dev.off()


AA_Depth_Spring_Occupancy<-lm(Mean_Weighted_Depth~Year1, data=Annual_Centroid_Depth_AA)
summary(AA_Depth_Spring_Occupancy)

AA_Depth_Spring_Suitability<-lm(Mean_Weighted_Depth~Year1, data=Annual_Centroid_Depth_AA_2013)
summary(AA_Depth_Spring_Suitability)

setwd("D:/Sand_Lance/SL_Chapter_2/Habitat_Occupancy/AA_Spring")

Best_AA_PA_GAM<-gam(PA~s(SST_Use, k=4)
                    +s(log10(hyper_100m3+1), k=4)
                    +s(log10(ctyp_100m3+1), k=4)
                    +s(Year, bs="re", by=ordered(!YearBlockA))+s(Year, bs="re", by=ordered(!YearBlockB))+s(Year, bs="re", by=ordered(!YearBlockC))+te(Longitude, Latitude)
                    +s(log10(pseudo_100m3+1),k=4),family=binomial,
                    data=Complete_Aa_PA, na.action = "na.fail")

View(Aa_PA_Dredge)
setwd("D:/Sand_Lance/SL_Chapter_2/Ch2_Manuscript")

tiff("Blueback_Spring_Pseudo_210514.tif", height=5, width=5, units="in", res=300)
plot(Best_AA_PA_GAM,trans=function(x)exp(x)/(1+exp(x)), shade=T,ylab="Probability of Occurrence",xlab=expression(paste(italic("Pseudocalanus "),"spp. ","log10(ind/100m"^"3",")")), cex.lab=1.5, cex.axis=1.5 )
dev.off()

Best_AA_PA_GAM<-gam(PA~s(SST_Use, k=4)
                    +s(log10(hyper_100m3+1), k=4)
                    +s(Year, bs="re", by=ordered(!YearBlockA))+s(Year, bs="re", by=ordered(!YearBlockB))+s(Year, bs="re", by=ordered(!YearBlockC))+te(Longitude, Latitude)
                    +s(log10(pseudo_100m3+1),k=4) +s(log10(ctyp_100m3+1), k=4),family=binomial,
                    data=Complete_Aa_PA, na.action = "na.fail")

tiff("Blueback_Spring_Centropages_210514.tif", height=5, width=5, units="in", res=300)
plot(Best_AA_PA_GAM,trans=function(x)exp(x)/(1+exp(x)), shade=T,ylab="Probability of Occurrence",xlab=expression(paste(italic("Centropages "),"spp. ","log10(ind/100m"^"3",")")), cex.lab=1.5, cex.axis=1.5 )
dev.off()

Best_AA_PA_GAM<-gam(PA~s(log10(hyper_100m3+1), k=4)
                    +s(Year, bs="re", by=ordered(!YearBlockA))+s(Year, bs="re", by=ordered(!YearBlockB))+s(Year, bs="re", by=ordered(!YearBlockC))+te(Longitude, Latitude)
                    +s(log10(pseudo_100m3+1),k=4) +s(log10(ctyp_100m3+1), k=4)+s(SST_Use, k=4),family=binomial,
                    data=Complete_Aa_PA, na.action = "na.fail")
tiff("Blueback_SST_210514.tif", height=5, width=5, units="in", res=300)
plot(Best_AA_PA_GAM,trans=function(x)exp(x)/(1+exp(x)), shade=T,xlab=expression(paste("Sea Surface Temperature ("^"o","C)")), ylab="Probability of Occurrence", cex.lab=1.5, cex.axis=1.5)
dev.off()

Best_AA_PA_GAM<-gam(PA~s(log10(hyper_100m3+1), k=4)
                    +s(Year, bs="re", by=ordered(!YearBlockA))+s(Year, bs="re", by=ordered(!YearBlockB))+s(Year, bs="re", by=ordered(!YearBlockC))+te(Longitude, Latitude)
                    +s(log10(pseudo_100m3+1),k=4) +s(log10(ctyp_100m3+1), k=4)+s(SST_Use, k=4),family=binomial,
                    data=Complete_Aa_PA, na.action = "na.fail",method="REML")
summary_gamm(Best_AA_PA_GAM)


#compare trends with sst 
Mean_annual_SST<-ddply(Grouped_Data_Split_AA, .(Year1), summarize, MeanSST=mean(SST_Use, na.rm=T))
AA_Spring_SST_Model<-betareg(AnnualAA_Quality$Hab_Prop~Mean_annual_SST$MeanSST)
summary(AA_Spring_SST_Model)

AA_Spring_SST_Model_Fixed<-betareg(AnnualAA_Quality_2013$Hab_Prop~Mean_annual_SST$MeanSST)
summary(AA_Spring_SST_Model_Fixed)

Mean_annual_SST<-ddply(Grouped_Data_Split_AA, .(Year1), summarize, MeanSST=mean(SST_Use, na.rm=T))
SST_Prediction_St<-as.data.frame(seq(from=min(Mean_annual_SST$MeanSST), to=max(Mean_annual_SST$MeanSST), by=0.01))
colnames(SST_Prediction_St)<-"MeanSST"


AA_SST_Data<-cbind(Mean_annual_SST$MeanSST,AnnualAA_Quality$Hab_Prop, AnnualAA_Quality_2013$Hab_Prop) 
colnames(AA_SST_Data)<-c("MeanSST","Hab_Prop","Hab_Prop_Fixed")
AA_SST_Data<-as.data.frame(AA_SST_Data)

AA_Spring_SST_Model<-betareg(Hab_Prop~MeanSST, data=AA_SST_Data)
summary(AA_Spring_SST_Model)

AA_Spring_SST_Model_Fixed<-betareg(Hab_Prop_Fixed~MeanSST, data=AA_SST_Data)
summary(AA_Spring_SST_Model_Fixed)


AA_SST_Reg<-predict(AA_Spring_SST_Model, SST_Prediction_St, type="response")
AA_SST_Reg_Fixed<-predict(AA_Spring_SST_Model_Fixed, SST_Prediction_St, type="response")





setwd("D:/Sand_Lance/SL_Chapter_2/Habitat_Occupancy/AA_Spring")
tiff("Blueback_SST_210514.tif", height=5, width=6, res=300, units="in")
par(mar=c(4,6,4,3))
plot(Mean_annual_SST$MeanSST, AnnualAA_Quality_1985$Hab_Prop, pch=19,col="red",ylim=c(0,0.4),xlab="Sea Surface Temperature", ylab="Proportion of \nViable Habitat", main="Blueback",cex.lab=1.2)
par(new=T)
plot(SST_Prediction_St$MeanSST, AA_SST_Reg_Fixed, type="l", lwd=3, lty=3,col="red", ylim=c(0,0.4),xlab="Sea Surface Temperature", ylab="Proportion of \nViable Habitat", main="Blueback",cex.lab=1.2)
#text(5.5,0.308, labels = expression(paste("",R^2,"=")), cex=1.5)
#text(6.2,0.307, labels = Rsq2, cex=1.5)
par(new=T)
plot(Mean_annual_SST$MeanSST, AnnualAA_Quality$Hab_Prop, pch=19,xlab="Sea Surface Temperature",ylim=c(0,0.4), ylab="Proportion of \nViable Habitat", main="Blueback",cex.lab=1.2)
par(new=T)
plot(SST_Prediction_St$MeanSST, AA_SST_Reg, type="l", lwd=3, lty=1, ylim=c(0,0.4),xlab="Sea Surface Temperature", ylab="Proportion of \nViable Habitat", main="Blueback",cex.lab=1.2)
#legend(5.2,0.5,bty="n", legend =c("Year-Specific","Fixed"),col=c("black","red"), pch=19)
dev.off()


###########atl mackerel################
Grouped_Data_Split_SS<-Grouped_Data_Split_Full
Grouped_Data_Split_SS$Year<-as.factor(as.character(Grouped_Data_Split_SS$Year))
Grouped_Data_Split_SS$Year1<-as.numeric(as.character(Grouped_Data_Split_SS$Year))

Grouped_Data_Split_SS$Longitude<-as.numeric(as.character(Grouped_Data_Split_SS$Longitude))
Grouped_Data_Split_SS$Latitude<-as.numeric(as.character(Grouped_Data_Split_SS$Latitude))

#split by bigelow
for (i in 1: nrow(Grouped_Data_Split_SS)){
  Grouped_Data_Split_SS$YearBlockB[i]<-ifelse(Grouped_Data_Split_SS$Year1[i]<2009,  0, 1)
  Grouped_Data_Split_SS$YearBlockC[i]<-ifelse(Grouped_Data_Split_SS$Year1[i]>=2009, 0, 1)
}

#assign approximate years
Grouped_Data_Split_SS$Year[Grouped_Data_Split_SS$Year==2016]<-"2010"
Grouped_Data_Split_SS$Year[Grouped_Data_Split_SS$Year==2015]<-"2011"
Grouped_Data_Split_SS$Year[Grouped_Data_Split_SS$Year==2014]<-"2010"

#get rid of years to be fair (years where clupeids were not available)
Grouped_Data_Split_SS<-Grouped_Data_Split_AA[Grouped_Data_Split_SS$Year!="1991",]
Grouped_Data_Split_SS<-Grouped_Data_Split_AA[Grouped_Data_Split_SS$Year!="1978",]

Grouped_Data_Split_SS2<-Grouped_Data_Split_SS
Grouped_Data_Split_SS2$Year<-2013
Grouped_Data_Split_SS2$YearBlockB<-1
Grouped_Data_Split_SS2$YearBlockC<-0


Grouped_Data_Split_SS$PA<-predict(SS_Multi_Avg_Spring,Grouped_Data_Split_SS,type="response")
Grouped_Data_Split_SS$PA_2013<-predict(SS_Multi_Avg_Spring,Grouped_Data_Split_SS2,type="response")


AnnualSS_Quality<-ddply(Grouped_Data_Split_SS, .(Year1), summarize, Hab_Prop=length(which(PA>0.5 ))/length(which(!is.na(PA))), Sample_Prop= length(which(!is.na(PA)))/Tile_Count)
AnnualSS_Quality_2013<-ddply(Grouped_Data_Split_SS, .(Year1), summarize, Hab_Prop=length(which(PA_2013>0.5 ))/length(which(!is.na(PA_2013))), Sample_Prop= length(which(!is.na(PA)))/Tile_Count)

AnnualSS_Quality<-AnnualSS_Quality[!is.na(AnnualSS_Quality$Year1),]
AnnualSS_Quality_2013<-AnnualSS_Quality_2013[!is.na(AnnualSS_Quality_2013$Year1),]


#make suitability maps
Annual_Averages_SS<-ddply(Grouped_Data_Split_SS, .(Longitude, Latitude), summarize, Mean_PA=mean(PA, na.rm=TRUE))
world<-ne_countries(scale="medium", returnclass = "sf")
setwd("D:/Sand_Lance/SL_Chapter_2/Habitat_Occupancy/SS_Spring")
tiff("Mackerel_Model_Pred_210514.tif", width=6, height=5, units="in", res=300)
ggplot(data=world) + geom_raster(data = Annual_Averages_SS, aes(x=Longitude, y = Latitude, fill=Mean_PA),linejoin = "mitre") + 
  coord_fixed(ratio = 1) +
  scale_fill_viridis(direction = 1) +
  theme_bw()+ggtitle("Atl. mackerel")+labs(fill = "Prob. Occurrence")+geom_sf()+coord_sf(xlim=c(-75.5,-64), ylim=c(36, 44.5))
dev.off()

AnnualSS_Quality_2013<-ddply(Grouped_Data_Split_SS, .(Year1), summarize, Hab_Prop=length(which(PA_2013>0.5 ))/length(which(!is.na(PA_2013))), Sample_Prop= length(which(!is.na(PA_2013)))/Tile_Count)

setwd("D:/Sand_Lance/SL_Chapter_2/Habitat_Occupancy/SS_Spring")
tiff("Mackerel_Occupiable_Habitat_210514_Comparison.tif", height=5, width=6, res=300, units="in")
par(mar=c(4,6,4,3))
plot(AnnualSS_Quality$Year1, AnnualSS_Quality$Hab_Prop, type="l", lwd=3,xlab="Year", ylab="Proportion of \nViable Habitat", main="Atl. mackerel",cex.lab=1.2, ylim=c(0,0.55), col="black")
par(new=T)
plot(AnnualSS_Quality_2013$Year1, AnnualSS_Quality_2013$Hab_Prop,  type="l",lwd=3, xlab="Year", ylab="Proportion of \nViable Habitat", main="Atl. mackerel",cex.lab=1.2, ylim=c(0,0.55), col="red")
legend(1995,0.55,bty="n", legend =c("Year-Specific","Fixed"),col=c("black","red"), lwd=3)
dev.off()

#betaregression for occupancy and suitability
AnnualSS_Quality_2013<-AnnualSS_Quality_2013[1:37,]

AnnualSS_Quality$Hab_Prop<-AnnualSS_Quality$Hab_Prop+0.00000001
SS_Spring_Full_Hab_Occ<-betareg(AnnualSS_Quality$Hab_Prop~AnnualSS_Quality$Year1)
summary(SS_Spring_Full_Hab_Occ)

SS_Spring_Fixed_Hab_Occ<-betareg(AnnualSS_Quality_2013$Hab_Prop~AnnualSS_Quality_2013$Year1)
summary(SS_Spring_Fixed_Hab_Occ)

#latitude by year 
Grouped_Data_Split_SS$Weighted_Lat_2013<-Grouped_Data_Split_SS$Latitude*Grouped_Data_Split_SS$PA_2013
Grouped_Data_Split_SS$Weighted_Lon_2013<-Grouped_Data_Split_SS$Longitude*Grouped_Data_Split_SS$PA_2013
Grouped_Data_Split_SS$Weighted_Lat<-Grouped_Data_Split_SS$Latitude*Grouped_Data_Split_SS$PA
Grouped_Data_Split_SS$Weighted_Lon<-Grouped_Data_Split_SS$Longitude*Grouped_Data_Split_SS$PA

library(plyr)
Annual_Centroid_Locations_SS<-ddply(Grouped_Data_Split_SS, .(Year1), summarize, Mean_Weighted_Lat=sum(Weighted_Lat, na.rm=TRUE)/(sum(PA, na.rm=TRUE)), Mean_Weighted_Lon=sum(Weighted_Lon, na.rm=TRUE)/(sum(PA, na.rm=TRUE)))
Annual_Centroid_Locations_SS_2013<-ddply(Grouped_Data_Split_SS, .(Year1), summarize, Mean_Weighted_Lat=sum(Weighted_Lat_2013, na.rm=TRUE)/(sum(PA_2013, na.rm=TRUE)), Mean_Weighted_Lon=sum(Weighted_Lon_2013, na.rm=TRUE)/(sum(PA_2013, na.rm=TRUE)))

library(plyr)
library(ggplot2)
library(sf)
library(rnaturalearth)
library(rnaturalearthdata)
library(ggrepel)
library(viridis)
world<-ne_countries(scale="medium", returnclass = "sf")
setwd("D:/Sand_Lance/SL_Chapter_2/Habitat_Occupancy/SS_Spring")
tiff("Atl_mackerel_Center_Location_Year_Specific_210514.tif", width=6, height=5, units="in", res=300)
Map1<-ggplot(data=world) + geom_point(data = Annual_Centroid_Locations_SS, aes(x=Mean_Weighted_Lon, y = Mean_Weighted_Lat, colour=Year1),linejoin = "mitre") + 
  coord_fixed(ratio = 1) +  scale_color_viridis(direction = 1) +
  theme_bw()+ggtitle("Atl. mackerel")+geom_sf()+coord_sf(xlim=c(-75.5,-64), ylim=c(36, 44.5))+ylab("Latitude")+xlab("Longitude")
Map1 #+geom_text_repel(data= Annual_Centroid_Locations_PT, aes(x=Mean_Weighted_Lon, y = Mean_Weighted_Lat, label = Year1), size = 4, max.overlaps = 50)
dev.off()

tiff("Atl_mackerel_Center_Location_2013_210514.tif", width=6, height=5, units="in", res=300)
Map1<-ggplot(data=world) + geom_point(data = Annual_Centroid_Locations_SS_1985, aes(x=Mean_Weighted_Lon, y = Mean_Weighted_Lat, colour=Year1),linejoin = "mitre") + 
  coord_fixed(ratio = 1) +  scale_color_viridis(direction = 1) +
  theme_bw()+ggtitle("Atl. mackerel")+geom_sf()+coord_sf(xlim=c(-75.5,-64), ylim=c(36, 44.5))+ylab("Latitude")+xlab("Longitude")
Map1 #+geom_text_repel(data= Annual_Centroid_Locations_SL_1985, aes(x=Mean_Weighted_Lon, y = Mean_Weighted_Lat, label = Year1), size = 4, max.overlaps = 50)
dev.off()


Annual_Centroid_Locations_SS<-Annual_Centroid_Locations_SS[!is.na(Annual_Centroid_Locations_SS$Mean_Weighted_Lat),]

SS_Lat_Spring_Occupancy<-lm(Mean_Weighted_Lat~Year1, data=Annual_Centroid_Locations_SS)
summary(SS_Lat_Spring_Occupancy)

SS_Lat_Spring_Suitability<-lm(Mean_Weighted_Lat~Year1, data=Annual_Centroid_Locations_SS_2013)
summary(SS_Lat_Spring_Suitability)


#make response curves
View(Ss_PA_Dredge_Spring)
setwd("D:/Sand_Lance/SL_Chapter_2/Ch2_Manuscript")

Best_SS_PA_GAM<-gam(PA~s(EKE,k=4)+s(Year, bs="re", by=ordered(!YearBlockB))+s(Year, bs="re", by=ordered(!YearBlockC))+te(Longitude, Latitude)
                    +s(log10(pseudo_100m3+1),k=4) +s(log10(calfin_100m3+1), k=4)+s(BT_Use, k=4)+s(SSS_Use, k=4),family=binomial,
                    data=Complete_Ss_PA, na.action = "na.fail")
tiff("Mackerel_SSS_Spring_210514.tif", height=5, width=5, units="in", res=300)
plot(Best_SS_PA_GAM,trans=function(x)exp(x)/(1+exp(x)), shade=T,xlab="Sea Surface Salinity", ylab="Probability of Occurrence", cex.lab=1.5, cex.axis=1.5 )
dev.off()

Best_SS_PA_GAM<-gam(PA~s(EKE,k=4)+s(Year, bs="re", by=ordered(!YearBlockB))+s(Year, bs="re", by=ordered(!YearBlockC))+te(Longitude, Latitude)
                    +s(log10(pseudo_100m3+1),k=4) +s(log10(calfin_100m3+1), k=4)+s(SSS_Use, k=4)+s(BT_Use, k=4),family=binomial,
                    data=Complete_Ss_PA, na.action = "na.fail")
tiff("Mackerel_BT_Spring_210514.tif", height=5, width=5, units="in", res=300)
plot(Best_SS_PA_GAM,trans=function(x)exp(x)/(1+exp(x)), shade=T,xlab=expression(paste("Bottom Temperature (" ^ "o","C)")), ylab="Probability of Occurrence", cex.lab=1.5, cex.axis=1.5)
dev.off()

Best_SS_PA_GAM<-gam(PA~s(EKE,k=4)+s(Year, bs="re", by=ordered(!YearBlockB))+s(Year, bs="re", by=ordered(!YearBlockC))+te(Longitude, Latitude)
                     +s(log10(calfin_100m3+1), k=4)+s(SSS_Use, k=4)+s(BT_Use, k=4)+s(log10(pseudo_100m3+1),k=4),family=binomial,
                    data=Complete_Ss_PA, na.action = "na.fail")
tiff("Mackerel_Pseudo_Spring_210514.tif", height=5, width=5, units="in", res=300)
plot(Best_SS_PA_GAM,trans=function(x)exp(x)/(1+exp(x)), shade=T, ylab="Probability of Occurrence", xlab=expression(paste(italic("Pseudocalanus "),"spp. ","log10(ind/100m"^"3",")")), cex.lab=1.5, cex.axis=1.5)
dev.off()

Best_SS_PA_GAM<-gam(PA~s(EKE,k=4)+s(Year, bs="re", by=ordered(!YearBlockB))+s(Year, bs="re", by=ordered(!YearBlockC))+te(Longitude, Latitude)
                    +s(log10(calfin_100m3+1), k=4)+s(SSS_Use, k=4)+s(BT_Use, k=4)+s(log10(pseudo_100m3+1),k=4),family=binomial,
                    data=Complete_Ss_PA, na.action = "na.fail", method="REML")
summary_gamm(Best_SS_PA_GAM)

#make comparisons with bottom temperature and sss
Mean_annual_BT<-ddply(Grouped_Data_Split_SS, .(Year1), summarize, MeanBT=mean(BT_Use, na.rm=T))
Mean_annual_BT<-Mean_annual_BT[!is.na(Mean_annual_BT$Year1),]
SS_Spring_BT_Model<-betareg(AnnualSS_Quality$Hab_Prop~Mean_annual_BT$MeanBT)
summary(SS_Spring_BT_Model)

SS_Spring_BT_Model_Fixed<-betareg(AnnualSS_Quality_2013$Hab_Prop~Mean_annual_BT$MeanBT)
summary(SS_Spring_BT_Model_Fixed)

Mean_annual_SSS<-ddply(Grouped_Data_Split_SS, .(Year1), summarize, MeanSSS=mean(SSS_Use, na.rm=T))
Mean_annual_SSS<-Mean_annual_SSS[!is.na(Mean_annual_SSS$Year1),]
SS_Spring_SSS_Model<-betareg(AnnualSS_Quality$Hab_Prop~Mean_annual_SSS$MeanSSS)
summary(SS_Spring_SSS_Model)

SS_Spring_SSS_Model_Fixed<-betareg(AnnualSS_Quality_2013$Hab_Prop~Mean_annual_SSS$MeanSSS)
summary(SS_Spring_SSS_Model_Fixed)




#latitude plots by time
setwd("D:/Sand_Lance/SL_Chapter_2/Habitat_Occupancy/SS_Spring")
tiff("Atl_mackerel_Mean_Location_PA_Comparison_210514.tif", height=5, width=6, res=300, units="in")
par(mar=c(4,6,4,3))
plot(Annual_Centroid_Locations_SS_2013$Year1, Annual_Centroid_Locations_SS_2013$Mean_Weighted_Lat,type="l", lwd=3, xlab="Year",col="red", ylim=c(39.6,41.2),ylab="Mean Weighted \nLatitude", main="Atl. mackerel",cex.lab=1.2)
par(new=T)
plot(Annual_Centroid_Locations_SS$Year1, Annual_Centroid_Locations_SS$Mean_Weighted_Lat,type="l", lwd=3, xlab="Year", col="black",ylim=c(39.6,41.2), ylab="Mean Weighted \nLatitude", main="Atl. mackerel",cex.lab=1.2)
legend(1978,41.1,bty="n", legend =c("Year-Specific","Fixed"),col=c("black","red"), lwd=3)
dev.off()

#plot depth by time
Grouped_Data_Split_SS$Weighted_Depth_2013<-Grouped_Data_Split_SS$Depth*Grouped_Data_Split_SS$PA_2013
Grouped_Data_Split_SS$Weighted_Depth<-Grouped_Data_Split_SS$Depth*Grouped_Data_Split_SS$PA

library(plyr)
Annual_Centroid_Depth_SS<-ddply(Grouped_Data_Split_SS, .(Year1), summarize, Mean_Weighted_Depth=sum(Weighted_Depth, na.rm=TRUE)/(sum(PA, na.rm=TRUE)))
Annual_Centroid_Depth_SS_2013<-ddply(Grouped_Data_Split_SS, .(Year1), summarize, Mean_Weighted_Depth=sum(Weighted_Depth_2013, na.rm=TRUE)/(sum(PA_2013, na.rm=TRUE)))

setwd("D:/Sand_Lance/SL_Chapter_2/Habitat_Occupancy/SS_Spring")
tiff("Atl_mackerel_Mean_Depth_PA_Comparison_210514_Regridded.tif", height=5, width=6, res=300, units="in")
par(mar=c(4,6,4,3))
plot(Annual_Centroid_Depth_SS_2013$Year1, (Annual_Centroid_Depth_SS_2013$Mean_Weighted_Depth)*-1,type="l", lwd=3, xlab="Year",col="red", ylim=rev(c(75,120)),ylab="Mean Weighted \nDepth (m)", main="Atl. mackerel",cex.lab=1.2)
par(new=T)
plot(Annual_Centroid_Depth_SS$Year1, (Annual_Centroid_Depth_SS$Mean_Weighted_Depth)*-1, type="l", lwd=3, xlab="Year", col="black", ylim=rev(c(75,120)), ylab="Mean Weighted \nDepth (m)", main="Atl. mackerel",cex.lab=1.2)
legend(1999,74,bty="n", legend =c("Year-Specific","Fixed"),col=c("black","red"), lwd=3)
dev.off()

SS_Depth_Spring_Occupancy<-lm(Mean_Weighted_Depth~Year1, data=Annual_Centroid_Depth_SS)
summary(SS_Depth_Spring_Occupancy)

SS_Depth_Spring_Suitability<-lm(Mean_Weighted_Depth~Year1, data=Annual_Centroid_Depth_SS_2013)
summary(SS_Depth_Spring_Suitability)


Mean_annual_BT<-ddply(Grouped_Data_Split_SS, .(Year1), summarize, MeanBT=mean(BT_Use, na.rm=T))
Mean_annual_BT<-Mean_annual_BT[!is.na(Mean_annual_BT$Year1),]

BT_Prediction_St<-as.data.frame(seq(from=min(Mean_annual_BT$MeanBT), to=max(Mean_annual_BT$MeanBT), by=0.01))
colnames(BT_Prediction_St)<-"MeanBT"


SS_BT_Data<-cbind(Mean_annual_BT$MeanBT,AnnualSS_Quality$Hab_Prop, AnnualSS_Quality_1985$Hab_Prop) 
colnames(SS_BT_Data)<-c("MeanBT","Hab_Prop","Hab_Prop_Fixed")
SS_BT_Data<-as.data.frame(SS_BT_Data)

SS_Spring_BT_Model<-betareg(Hab_Prop~MeanBT, data=SS_BT_Data)
summary(SS_Spring_BT_Model)

SS_Spring_BT_Model_Fixed<-betareg(Hab_Prop_Fixed~MeanBT, data=SS_BT_Data)
summary(SS_Spring_BT_Model_Fixed)


SS_BT_Reg<-predict(SS_Spring_BT_Model, BT_Prediction_St, type="response")
SS_BT_Reg_Fixed<-predict(SS_Spring_BT_Model_Fixed, BT_Prediction_St, type="response")



setwd("D:/Sand_Lance/SL_Chapter_2/Habitat_Occupancy/SS_Spring")
tiff("Atl_mackerel_BT_210405.tif", height=5, width=6, res=300, units="in")
par(mar=c(4,6,4,3))
plot(Mean_annual_BT$MeanBT, AnnualSS_Quality_1985$Hab_Prop, pch=19,col="red",ylim=c(0,0.5),xlab="Bottom Temperature", ylab="Proportion of \nViable Habitat", main="Atl. mackerel",cex.lab=1.2)
par(new=T)
plot(BT_Prediction_St$MeanBT,SS_BT_Reg_Fixed, type="l", lwd=3, lty=1,col="red", ylim=c(0,0.5),xlab="Bottom Temperature", ylab="Proportion of \nViable Habitat", main="Atl. mackerel",cex.lab=1.2)
#text(5.5,0.308, labels = expression(paste("",R^2,"=")), cex=1.5)
#text(6.2,0.307, labels = Rsq2, cex=1.5)
par(new=T)
plot(Mean_annual_BT$MeanBT, AnnualSS_Quality$Hab_Prop, pch=19,xlab="Bottom Temperature",ylim=c(0,0.5), ylab="Proportion of \nViable Habitat", main="Atl. mackerel",cex.lab=1.2)
par(new=T)
plot(BT_Prediction_St$MeanBT,SS_BT_Reg, type="l", lwd=3, lty=3, ylim=c(0,0.5),xlab="Bottom Temperature", ylab="Proportion of \nViable Habitat", main="Atl. mackerel",cex.lab=1.2)
#legend(5.2,0.5,bty="n", legend =c("Year-Specific","Fixed"),col=c("black","red"), pch=19)
dev.off()


Mean_annual_SSS<-ddply(Grouped_Data_Split_SS, .(Year1), summarize, MeanSSS=mean(SSS_Use, na.rm=T))
Mean_annual_SSS<-Mean_annual_SSS[!is.na(Mean_annual_SSS$Year1),]


SSS_Prediction_St<-as.data.frame(seq(from=min(Mean_annual_SSS$MeanSSS), to=max(Mean_annual_SSS$MeanSSS), by=0.01))
colnames(SSS_Prediction_St)<-"MeanSSS"


SS_SSS_Data<-cbind(Mean_annual_SSS$MeanSSS,AnnualSS_Quality$Hab_Prop, AnnualSS_Quality_1985$Hab_Prop) 
colnames(SS_SSS_Data)<-c("MeanSSS","Hab_Prop","Hab_Prop_Fixed")
SS_SSS_Data<-as.data.frame(SS_SSS_Data)

SS_Spring_SSS_Model<-betareg(Hab_Prop~MeanSSS, data=SS_SSS_Data)
summary(SS_Spring_SSS_Model)

SS_Spring_SSS_Model_Fixed<-betareg(Hab_Prop_Fixed~MeanSSS, data=SS_SSS_Data)
summary(SS_Spring_SSS_Model_Fixed)


SS_SSS_Reg<-predict(SS_Spring_SSS_Model, SSS_Prediction_St, type="response")
SS_SSS_Reg_Fixed<-predict(SS_Spring_SSS_Model_Fixed, SSS_Prediction_St, type="response")


tiff("Atl_mackerel_SSS_210405.tif", height=5, width=6, res=300, units="in")
par(mar=c(4,6,4,3))
plot(Mean_annual_SSS$MeanSSS, AnnualSS_Quality_1985$Hab_Prop, pch=19,col="red",ylim=c(0,0.5),xlab="Sea Surface Salinity", ylab="Proportion of \nViable Habitat", main="Atl. mackerel",cex.lab=1.2)
par(new=T)
plot(SSS_Prediction_St$MeanSSS,SS_SSS_Reg_Fixed, type="l", lwd=3, lty=1,col="red", ylim=c(0,0.5),xlab="Sea Surface Salinity", ylab="Proportion of \nViable Habitat", main="Atl. mackerel",cex.lab=1.2)
#text(5.5,0.308, labels = expression(paste("",R^2,"=")), cex=1.5)
#text(6.2,0.307, labels = Rsq2, cex=1.5)
par(new=T)
plot(Mean_annual_SSS$MeanSSS, AnnualSS_Quality$Hab_Prop, pch=19,xlab="Sea Surface Salinity",ylim=c(0,0.5), ylab="Proportion of \nViable Habitat", main="Atl. mackerel",cex.lab=1.2)
par(new=T)
plot(SSS_Prediction_St$MeanSSS,SS_SSS_Reg, type="l", lwd=3, lty=3, ylim=c(0,0.5),xlab="Sea Surface Salinity", ylab="Proportion of \nViable Habitat", main="Atl. mackerel",cex.lab=1.2)
#legend(5.2,0.5,bty="n", legend =c("Year-Specific","Fixed"),col=c("black","red"), pch=19)
dev.off()