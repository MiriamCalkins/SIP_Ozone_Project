###ENVH 548 SIP Group Project Spring 2015###

install.packages(c("maptools", "sp", "raster", "rgdal", "rgeos", "spdep", "spatstat", 
                   "reshape2", "scales", "shapefiles", "maps"))
#trouble shooting rgdal install
install.packages('rgdal',repos="http://www.stats.ox.ac.uk/pub/RWin")

#EPA ozone data
#source: http://www.epa.gov/airdata/ad_data_daily.html

ozone12<-read.csv("/Users/miriamcalkins/Documents/UWDEOHS/PhD Degree/Q3_Spring 2015/ENVH548/Homework/SIPProject/Data/WAozone2012.csv")
ozone13<-read.csv("/Users/miriamcalkins/Documents/UWDEOHS/PhD Degree/Q3_Spring 2015/ENVH548/Homework/SIPProject/Data/WAozone2013.csv")
ozone14<-read.csv("/Users/miriamcalkins/Documents/UWDEOHS/PhD Degree/Q3_Spring 2015/ENVH548/Homework/SIPProject/Data/WAozone2014.csv")

library(Hmisc)
describe(ozone12)
#Data includes Clark County, which is not of interest to this SIP

#Project 8-hr NAAQS of 0.065 ppm
#Select entries at or above NAAQS
Nonattainment12<-subset(ozone12, Daily.Max.8.hour.Ozone.Concentration>=0.065)
describe(Nonattainment12)
nrow(Nonattainment12)

Nonattainment13<-subset(ozone13, Daily.Max.8.hour.Ozone.Concentration>=0.065)
describe(Nonattainment13)
nrow(Nonattainment13)

Nonattainment14<-subset(ozone14, Daily.Max.8.hour.Ozone.Concentration>=0.065)
describe(Nonattainment14)
nrow(Nonattainment14)

#Merge three years of data into one file
Nonattall<-rbind(Nonattainment12, Nonattainment13, Nonattainment14)
nrow(Nonattall)

Nonat<-subset(Nonattall, COUNTY!="Clark")
nrow(Nonatt)

#Remove unnecessary columns
keepvars<-c("Date", "AQS_SITE_ID", "Daily.Max.8.hour.Ozone.Concentration", "UNITS", "DAILY_AQI_VALUE", 
            "CBSA_CODE", "CBSA_NAME", "COUNTY_CODE", "COUNTY", "SITE_LATITUDE", "SITE_LONGITUDE")
Nonatt<-Nonat[keepvars]

#Formatting date
Nonatt$date <- as.Date(Nonatt$Date, "%m/%d/%Y")
Nonatt$month <- format(Nonatt$date, "%b")
Nonatt$month
Nonatt$year <- format(Nonatt$date, "%Y")
Nonatt$year

#Descriptive stats
summary(Nonatt)

table(Nonatt$Daily.Max.8.hour.Ozone.Concentration, Nonatt$COUNTY)
table(Nonatt$Daily.Max.8.hour.Ozone.Concentration, Nonatt$month)
table(Nonatt$COUNTY, Nonatt$month)
hist(Nonatt$Daily.Max.8.hour.Ozone.Concentration)
plot(Nonatt$COUNTY, Nonatt$Daily.Max.8.hour.Ozone.Concentration)

#Mapping
library(maptools) #contains overlay commands
library(sp) # vector data
library(raster) # raster data
library(rgdal) #input/output, projections (read shapefiles)
library(rgeos) # geometry ops (necessary for ggplot2)
library(spdep) # spatial dependence
library(spatstat) # generate random points drawn from specific data)
library(reshape2) # visualization and manipulation
library(scales) # visualization and manipulation
library(shapefiles)
library(maps)

#Basemap
WAshape<-readOGR(dsn="/Users/miriamcalkins/Documents/UWDEOHS/PhD Degree/Q3_Spring 2015/ENVH548/Homework/SIPProject/Data/WA_State_Bndy", 
                 layer="WA_State_Bndy") #source: WA DNR
summary(WAshape)
plot(WAshape, col="goldenrod1")
names(WAshape)

washshape<-readOGR(dsn="/Users/miriamcalkins/Documents/UWDEOHS/PhD Degree/Q3_Spring 2015/ENVH548/Homework/SIPProject/Data/wash", 
                   layer="washington") #source: WA DOE
summary(washshape)
plot(washshape)

srshape<-readOGR(dsn="/Users/miriamcalkins/Documents/UWDEOHS/PhD Degree/Q3_Spring 2015/ENVH548/Homework/SIPProject/Data/sr", 
                   layer="sr500k") #source: WA DOT
summary(srshape)
plot(srshape)

#Create spatial data frame for Ozone
coordinates(Nonatt) = c("SITE_LONGITUDE", "SITE_LATITUDE")
proj4string(Nonatt) <- CRS("+init=epsg:4269 +proj=longlat +ellps=GRS80 +datum=NAD83 +no_defs +towgs84=0,0,0") #EPA's CRS
NonattWA <- spTransform(Nonatt, CRS("+proj=lcc +lat_1=45.83333333333334 +lat_2=47.33333333333334 +lat_0=45.33333333333334 +lon_0=-120.5
                                    +x_0=500000.0000000001 +y_0=0 +ellps=GRS80 +units=us-ft +no_defs"))
Nonattwash

#Overlay data on WA state
plot(WAshape)
plot(srshape, add=TRUE, col="orange")
plot(NonattWA, add=TRUE, col="blue", cex=1)

plot(washshape)
plot(srshape, add=TRUE, col="orange")
plot(NonattWA, add=TRUE, col="blue", cex=1)
plot(Nonattres, col="blue")

