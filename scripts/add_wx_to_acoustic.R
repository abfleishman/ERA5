# weather_from_model
# install.packages("rNOMADS")
library(tidyverse)
library(lubridate)
library(rNOMADS)
library(raster)
library(ncdf4)
library(stringr)
library(velox)
library(maptools)

files<-list.files("D:/CM,Inc/Dropbox (CMI)/CMI_Team/Analysis/Audit_Subset_Workflow/weather/weather_scripts/",pattern = "\\.nc$",full.names = T,recursive = T)[51:59]
files
# stack(files[30])
# for(filenames in files){
# hm<-nc_open(filenames)
# print(filenames)
# print(names(hm$var))
# nc_close(hm)
# 
# }

tp_temp<-lapply(files, function(x)stack(x,varname="tp"))
tp<-stack(tp_temp)
rm(tp_temp)
wind_temp<-lapply(files, function(x)stack(x,varname="wind"))
wind<-stack(wind_temp)
rm(wind_temp)

v10_temp<-lapply(files, function(x)stack(x,varname="v10"))
v10<-stack(v10_temp)
rm(v10_temp)

u10_temp<-lapply(files, function(x)stack(x,varname="u10"))
u10<-stack(u10_temp)
rm(u10_temp)

a<-names(tp)


# Create a date table for the global-analysis-forecast-phy vars
DateTable<-NULL
names(tp)==names(wind)

DateTable<-data.frame(xdate=names(tp),  DateTime=ymd_hms(gsub("X","",names(tp)))-hours(10)) %>%
  mutate(
    year=year(DateTime),month=month(DateTime),day=day(DateTime),
    hour=hour(DateTime),minute=minute(DateTime),second=second(DateTime),
    Date=as.Date(DateTime)
  )

tz(dat$DateTime_h)
load("D:/CM,Inc/Dropbox (CMI)/CMI_Team/Analysis/Audit_Subset_Workflow/R_output/subsample_audit_1500/Kauai_HAPE_2017_audit1500.RData")
dat=data_ALL_15min
rm(data_ALL_15min)

dat$DateTime_h<-ymd_hms(paste(dat$Date,dat$TimeStepStart))%>% round_date(unit = "1 hours")
# Get the unique Dates in the dat data
Dates<-sort(unique(dat$DateTime_h))

# Loop for global-analysis-forecast-phy vars ---------------------------------

# Create empty columns to populate
dat$tp<-NA
dat$wind_u<-NA

dat$wind_v<-NA
dat$wind_speed<-NA


# go through each Date in the dat data, extract the vars at each of the
# points with a buffer, aggragate the buffered data to end up with a single
# value for each point.
#
# Currently using the scale of error 925km to buffer and taking the mean.
# install.packages("velox")

for(i in 1:length(Dates)){
  print(Dates[i])
  
  datex<-as.character(DateTable$xdate[DateTable$DateTime==Dates[i]])[1]
  if(length(datex)<1|is.na(datex)) next
  
  pt<-data.frame(Longitude=dat$Longitude[which(dat$DateTime_h==Dates[i]&is.na(dat$tp))],
                 Latitude=dat$Latitude[which(dat$DateTime_h==Dates[i]&is.na(dat$tp))])
  
  coordinates(pt)<-cbind(dat$Longitude[which(dat$DateTime_h==Dates[i]&is.na(dat$tp))],
                         dat$Latitude[which(dat$DateTime_h==Dates[i]&is.na(dat$tp))])
  crs(pt)<-CRS("+proj=longlat +ellps=WGS84 +datum=WGS84")
  
  pt_sp<-spTransform(pt,CRS("+proj=utm +zone=4 +north"))
  
  spol <- rgeos::gBuffer(pt_sp, width=20000, byid=TRUE)
  
  spdf <- SpatialPolygonsDataFrame(spol, data.frame(id=1:length(spol)), FALSE)
  spdf <- spTransform(spdf,CRS("+proj=longlat +ellps=WGS84 +datum=WGS84"))
  
  # plot(tp[[datex]])
  # library(mapdata)
  # points(pt)
  # plot(spdf,add=T)
  # map('world',add = T)
  # tp
  vx<- velox(tp[[datex]])
  dat$tp[which(dat$DateTime_h==Dates[i])] <- vx$extract(spdf, fun=function(x)mean(x,na.rm=T))
  # wind_u
  vx<- velox(u10[[datex]])
  dat$wind_u[which(dat$DateTime_h==Dates[i])] <- vx$extract(spdf, fun=function(x)mean(x,na.rm=T))
  # wind_v
  vx<- velox(v10[[datex]])
  dat$wind_v[which(dat$DateTime_h==Dates[i])] <- vx$extract(spdf, fun=function(x)mean(x,na.rm=T))
  # wind_speed
  vx<- velox(wind[[datex]])
  dat$wind_speed[which(dat$DateTime_h==Dates[i])] <- vx$extract(spdf, fun=function(x)mean(x,na.rm=T))
  
  
}

save(dat, file="D:/CM,Inc/Dropbox (CMI)/CMI_Team/Analysis/Audit_Subset_Workflow/R_output/subsample_audit_1500/Kauai_HAPE_2017_audit1500_with_weather_19Sep18.RData")

library(patchwork)
sites=unique(dat$SPID)
for(site in sites){
  ppp<-dat %>% filter(SPID==site) %>% ggplot(data=.,aes(wind_speed,level_absolute))+geom_point(alpha=0.3)+geom_smooth()+
    dat %>% filter(SPID==site) %>% ggplot(data=.,aes(wind_speed,flux_sensitive))+geom_point(alpha=0.3)+geom_smooth()+
    dat %>% filter(SPID==site) %>% ggplot(data=.,aes(wind_speed,burst))+geom_point(alpha=0.3)+geom_smooth()+
    dat %>% filter(SPID==site) %>% ggplot(data=.,aes(wind_speed,click))+geom_point(alpha=0.3)+geom_smooth()+
    
    dat %>% filter(SPID==site) %>% ggplot(data=.,aes(tp,level_absolute))+geom_point(alpha=0.3)+geom_smooth()+
    dat %>% filter(SPID==site) %>% ggplot(data=.,aes(tp,flux_sensitive))+geom_point(alpha=0.3)+geom_smooth()+
    dat %>% filter(SPID==site) %>% ggplot(data=.,aes(tp,burst))+geom_point(alpha=0.3)+geom_smooth()+
    dat %>% filter(SPID==site) %>% ggplot(data=.,aes(tp,click))+geom_point(alpha=0.3)+geom_smooth()+
    
    dat %>% filter(SPID==site) %>% ggplot(data=.,aes(wind_u,level_absolute))+geom_point(alpha=0.3)+geom_smooth()+
    dat %>% filter(SPID==site) %>% ggplot(data=.,aes(wind_u,flux_sensitive))+geom_point(alpha=0.3)+geom_smooth()+
    dat %>% filter(SPID==site) %>% ggplot(data=.,aes(wind_u,burst))+geom_point(alpha=0.3)+geom_smooth()+
    dat %>% filter(SPID==site) %>% ggplot(data=.,aes(wind_u,click))+geom_point(alpha=0.3)+geom_smooth()+
    
    dat %>% filter(SPID==site) %>% ggplot(data=.,aes(wind_v,level_absolute))+geom_point(alpha=0.3)+geom_smooth()+
    dat %>% filter(SPID==site) %>% ggplot(data=.,aes(wind_v,flux_sensitive))+geom_point(alpha=0.3)+geom_smooth()+
    dat %>% filter(SPID==site) %>% ggplot(data=.,aes(wind_v,burst))+geom_point(alpha=0.3)+geom_smooth()+
    dat %>% filter(SPID==site) %>% ggplot(data=.,aes(wind_v,click))+geom_point(alpha=0.3)+geom_smooth()
  names(dat)
  ggsave(ppp,filename = paste0("weather_",site,".jpg"),height=12,width=12)
}
