###ENVH 548 SIP Group Project Spring 2015###

# Load each package. Install packages when necessary.
for (pkg in c("Hmisc", "maptools", "sp", "raster", "rgeos", "spdep", "spatstat", 
              "reshape2", "scales", "shapefiles", "maps", "ggplot2")){
  if (! require(pkg, character.only=TRUE)){
    install.packages(pkg, dependencies=TRUE)
    if (! require(pkg, character.only=TRUE) ) {
      stop(paste0(c("Can't load package: ", pkg, "!"), collapse = ""))
    }
  }
}

# Load each package. Install packages when necessary.
for (pkg in c("rgdal")) {
  if (! require(pkg, character.only=TRUE)) {
    install.packages(pkg, dependencies=TRUE)
    if (! require(pkg, character.only=TRUE) ) {
      stop(paste0(c("Can't load package: ", pkg, "!"), collapse = ""))
    }
  }
}

#EPA ozone data
#source: http://www.epa.gov/airdata/ad_data_daily.html

WD<-getwd()
if(!is.null(WD)) setwd("/Users/miriamcalkins/Documents/UWDEOHS/PhD Degree/Q3_Spring 2015/ENVH548/Homework/SIPProject/")

# Create the data folder if needed.
datadir <- "Data"
dir.create(file.path(datadir), showWarnings=FALSE, recursive=TRUE)

ozone12<-read.csv("Data/WAozone2012.csv")
ozone13<-read.csv("Data/WAozone2013.csv")
ozone14<-read.csv("Data/WAozone2014.csv")

library(Hmisc)
describe(ozone12)
#Note:Data includes Clark County, which is not of interest to this SIP
#Merge three years of data into one file
ozoneall<-rbind(ozone12, ozone13, ozone14)
nrow(ozoneall)
describe(ozoneall)

#Remove unnecessary columns
keepvars<-c("Date", "AQS_SITE_ID", "Daily.Max.8.hour.Ozone.Concentration", "UNITS", "DAILY_AQI_VALUE", 
            "CBSA_CODE", "CBSA_NAME", "COUNTY_CODE", "COUNTY", "SITE_LATITUDE", "SITE_LONGITUDE")
ozoneSIP<-ozoneall[keepvars]
str(ozoneSIP)

#Remove Clark County (as an unused COUNTY level)
ozoneSIP$COUNTY[ozoneSIP$COUNTY=="Clark"]<-NA
ozoneSIP$COUNTY<-factor(ozoneSIP$COUNTY)
str(ozoneSIP)
ozoneSIP<-na.omit(ozoneSIP)
str(ozoneSIP)

#Select entries at or above project 8-hr NAAQS of 0.065 ppm
Nonatt<-subset(ozoneSIP, Daily.Max.8.hour.Ozone.Concentration>=0.065)
describe(Nonattall)

#Formatting date
Nonatt$date <- as.Date(Nonatt$Date, "%m/%d/%Y")
Nonatt$month <- format(Nonatt$date, "%b")
Nonatt$month
Nonatt$year <- format(Nonatt$date, "%Y")
Nonatt$year

#Selecting Specific Counties
is.King<-(ozoneall$COUNTY=="King")
is.King

is.Clallam<-ozoneall$COUNTY=="Clallam"
is.Pierce<-(ozoneall$COUNTY=="Pierce")
is.Skagit<-(ozoneall$COUNTY=="Skagit")
is.Spokane<-(ozoneall$COUNTY=="Spokane")
is.Thurston<-(ozoneall$COUNTY=="Thurston")
is.Whatcom<-(ozoneall$COUNTY=="Whatcom")

#####################
##Descriptive stats##
#####################
#Remember Skagit County removed from data because it is has no nonattainment values
summary(Nonatt)

#Total number of AQI Sites per county
length(unique(ozoneall$AQS_SITE_ID[is.Clallam]))
length(unique(ozoneall$AQS_SITE_ID[is.King]))
length(unique(ozoneall$AQS_SITE_ID[is.Pierce]))
length(unique(ozoneall$AQS_SITE_ID[is.Skagit]))
length(unique(ozoneall$AQS_SITE_ID[is.Spokane]))
length(unique(ozoneall$AQS_SITE_ID[is.Thurston]))
length(unique(ozoneall$AQS_SITE_ID[is.Whatcom]))

#Total number of AQS sites in exceedance per county with AQS_SITE_ID
table(Nonatt$AQS_SITE_ID, Nonatt$COUNTY) #Why is Clark County still appearing?

#Concentration by county, AQS, and month
table(Nonatt$Daily.Max.8.hour.Ozone.Concentration, Nonatt$COUNTY)
table(Nonatt$Daily.Max.8.hour.Ozone.Concentration, Nonatt$AQS_SITE_ID)

table(Nonatt$Daily.Max.8.hour.Ozone.Concentration, Nonatt$month)
table(Nonatt$COUNTY, Nonatt$month)

hist(Nonatt$Daily.Max.8.hour.Ozone.Concentration, main="", xlab="Ozone Concentration (ppm)", col="tomato")
title(main=list("NAAQS Exceedence Frequency in Nonattainment Areas of
     the Puget Sound and Spokane", cex=0.9))

plot(Nonatt$COUNTY, Nonatt$Daily.Max.8.hour.Ozone.Concentration, col=rainbow(6), cex.axis=0.5,
     xlab="Counties", ylab="Ozone Concentration (ppm)", main="Ozone Nonattainment by County in WA
     2012-2014")

###########
##Mapping##
###########
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
library(ggplot)

#Basemap
WAshape<-readOGR(dsn="/Users/miriamcalkins/Documents/UWDEOHS/PhD Degree/Q3_Spring 2015/ENVH548/Homework/SIPProject/Data/WA_State_Bndy", 
                 layer="WA_State_Bndy") #source: WA DNR
summary(WAshape)
plot(WAshape)
proj4string(WAshape)
str(attributes(WAshape))
geometry(WAshape)

Countyshape<-readOGR(dsn="/Users/miriamcalkins/Documents/UWDEOHS/PhD Degree/Q3_Spring 2015/ENVH548/Homework/SIPProject/Data/WA_County_Bndys", 
                     layer="WA_County_Bndys") #source: WA DNR)
summary(Countyshape)
plot(Countyshape)
names(Countyshape)

srshape<-readOGR(dsn="/Users/miriamcalkins/Documents/UWDEOHS/PhD Degree/Q3_Spring 2015/ENVH548/Homework/SIPProject/Data/sr", 
                   layer="sr500k") #source: WA DOT
summary(srshape)
plot(srshape)

NHDAreashape<-readOGR(dsn="/Users/miriamcalkins/Documents/UWDEOHS/PhD Degree/Q3_Spring 2015/ENVH548/Homework/SIPProject/Data/WA_area_shape", 
                      layer="NHDArea") #source: WA DOT
summary(NHDAreashape)
plot(NHDAreashape)

coastshape<-readOGR(dsn="/Users/miriamcalkins/Documents/UWDEOHS/PhD Degree/Q3_Spring 2015/ENVH548/Homework/SIPProject/Data/coast", 
                 layer="CoastTrimmed") #source: USGS NHD
summary(coastshape)
plot(coastshape)

shoreshape<-readOGR(dsn="/Users/miriamcalkins/Documents/UWDEOHS/PhD Degree/Q3_Spring 2015/ENVH548/Homework/SIPProject/Data/shore", 
                    layer="shore_poly") #source: WA DOE
summary(shoreshape)
plot(shoreshape)

#Select Regions on base map by attribute
attributes(WAshape@data)
WAshape@data$id<-rownames(WAshape@data)


spplot(WAshape)
WApugetsound<-(WAshape@data$DNR_ADMIN_REGION_CODE==900)


####Create spatial data frame for Ozone
#All AQS
coordinates(ozoneSIP) = c("SITE_LONGITUDE", "SITE_LATITUDE")
proj4string(ozoneSIP) <- CRS("+init=epsg:4269 +proj=longlat +ellps=GRS80 +datum=NAD83 +no_defs +towgs84=0,0,0") #EPA's CRS
ozoneSIPWA <- spTransform(ozoneSIP, CRS("+proj=lcc +lat_1=45.83333333333334 +lat_2=47.33333333333334 +lat_0=45.33333333333334 +lon_0=-120.5
                                    +x_0=500000.0000000001 +y_0=0 +ellps=GRS80 +units=us-ft +no_defs"))

#Nonattainment areas
coordinates(Nonatt) = c("SITE_LONGITUDE", "SITE_LATITUDE")
proj4string(Nonatt) <- CRS("+init=epsg:4269 +proj=longlat +ellps=GRS80 +datum=NAD83 +no_defs +towgs84=0,0,0") #EPA's CRS
NonattWA <- spTransform(Nonatt, CRS("+proj=lcc +lat_1=45.83333333333334 +lat_2=47.33333333333334 +lat_0=45.33333333333334 +lon_0=-120.5
                                    +x_0=500000.0000000001 +y_0=0 +ellps=GRS80 +units=us-ft +no_defs"))

#Overlay data on WA state
plot(WAshape)
plot(Countyshape, att=TRUE, col="papayawhip")
plot(srshape, add=TRUE, col="gray81")
plot(coastshape, add=TRUE, col="blue")
plot(ozoneSIPWA, add=TRUE, pch=23, col="thistle4", bg="thistle3", cex=0.8)
plot(NonattWA, add=TRUE, pch=23, col="slateblue4", bg="slateblue4", cex=0.8)
title(main=list("Puget Sound and Spokane Air Quality Monitoring
     Sites in Exceedance of 8-hr Ozone NAAQS", cex=0.9))
legend("top", c("AQS below NAAQS", "AQS above NAAQS", "State Routes"), 
       fill=c("thistle", "slateblue", "gray"), horiz=T, cex=0.5)

