library(raster)
library(adehabitatLT)
library(sp)

library(readr)
library(lubridate)
library(tidyr)
library(dplyr)

# load tracks
tracks<-read.csv("~/Dropbox (CMI)/ERA5_4_caitie/Chapter3_Tracks_ALL_ready2go_filtered_adjusted_28april2019.csv") %>%
  select(-X) %>% # remove rowname artifact
  mutate(datetime=ymd_hms(datetime,tz="UTC")) %>% #set datetime to posixct with utc tz
  unite(col = birdID, Band, Season, Species, Stage) %>% # combine into a birdID column
  distinct() %>% # remove doubled data for bird 248377
  filter(birdID!="NA_NA_NA_NA") #remove na rows


# Interpolate tracks to 10 minute intervals -------------------------------


# make ltraj (adehabitat)
# I used the wraped lon to fix strange datetime issues.
# without the wrap you get a crazy point every time a track crosses the date
# line during interpolation
tracks_lt<-adehabitatLT::as.ltraj(xy = cbind(tracks$lon,tracks$lat),
                                  date = tracks$datetime,
                                  id = tracks$birdID,
                                  burst = tracks$birdID,
                                  typeII = T,slsp="remove",
                                  # added the wrap to fix wierd date line issues
                                  proj4string = CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs +lon_wrap=180"))
# plot(tracks_lt)
# resample at 10 minutes
tracks_lt_10min<-adehabitatLT::redisltraj(l = tracks_lt,u = 10*60,type = "time")

# convert back to data.frame
tracks10<-adehabitatLT::ld(tracks_lt_10min) %>%
  separate(id,into = c("Band", "Season", "Species", "Stage")) %>%
  rename(lon=x,lat=y,datetime=date)
head(tracks10)




# Attach wind, set, swh to tracks -----------------------------------------

# list the data files.  each .nc file has 4 variables in it.  u10, v10, sst, and swh
files<-list.files("~/Dropbox (CMI)/ERA5_4_caitie",pattern = "\\.nc$",full.names = T,recursive = T)
files

# stack all the stacks of data for each var
# these stacks have names and each name is a date/hour for a var.  we are combining all the data into one raster stack.
u10_temp<-lapply(files, function(x)stack(x,varname="u10"))
u10<-stack(u10_temp)
rm(u10_temp)

v10_temp<-lapply(files, function(x)stack(x,varname="v10"))
v10<-stack(v10_temp)
rm(v10_temp)

sst_temp<-lapply(files, function(x)stack(x,varname="sst"))
sst<-stack(sst_temp)
rm(sst_temp)
plot(sst[[1]]-273.15)

swh_temp<-lapply(files, function(x)stack(x,varname="swh"))
swh<-stack(swh_temp)
rm(swh_temp)

# Create a date table for the ERA5 vars
#
# The dates are stored in the names or each raster
DateTable<-NULL
table(names(v10)==names(u10)) # all varshave same names so can do this for a single var and use for all


DateTable<-data.frame(xdate=names(u10),
                      datetime=ymd_hms(gsub("X","",names(u10)),tz="UTC")) %>%
  mutate(
    year=year(datetime),
    month=month(datetime),
    day=day(datetime),
    hour=hour(datetime),
    Date=as.Date(datetime)
  )

tracks$datetime_h<-round_date(tracks$datetime,unit = "1 hours")


# loop through each hour of each date with tracking data and extract the
# environmental data from the grid cells the points overlay

# Get the unique Dates in the tracks data
Dates<-sort(unique(tracks$datetime_h))

tracks$u10<-NA
tracks$v10<-NA
tracks$sst<-NA
tracks$swh<-NA

for(i in 1:length(Dates)){
  print(Dates[i])

  datex<-as.character(DateTable$xdate[DateTable$datetime==Dates[i]])[1]
  if(length(datex)<1|is.na(datex)) next

  pt<-data.frame(lon=tracks$lon[which(tracks$datetime_h==Dates[i]&is.na(tracks$u10))],
                 lat=tracks$lat[which(tracks$datetime_h==Dates[i]&is.na(tracks$u10))])

  coordinates(pt)<-cbind(tracks$lon[which(tracks$datetime_h==Dates[i]&is.na(tracks$u10))],
                         tracks$lat[which(tracks$datetime_h==Dates[i]&is.na(tracks$u10))])
  crs(pt)<-CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +lon_0=180")

  # extract the values for each raster under each point

  # wind_u
  tracks$u10[which(tracks$datetime_h==Dates[i])] <- raster::extract(u10[[datex]],pt)

  # wind_v
  tracks$v10[which(tracks$datetime_h==Dates[i])] <- raster::extract(v10[[datex]],pt)
  # sst
  tracks$sst[which(tracks$datetime_h==Dates[i])] <-raster::extract(sst[[datex]],pt)
  # swh
  tracks$swh[which(tracks$datetime_h==Dates[i])] <- raster::extract(swh[[datex]],pt)

}

