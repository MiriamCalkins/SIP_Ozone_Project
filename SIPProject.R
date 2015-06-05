###ENVH 548 SIP Group Project Spring 2015###
#
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

#Remove Clark, Whatcom, and Clallam Counties (as an unused COUNTY level)
ozoneSIP$COUNTY[ozoneSIP$COUNTY=="Clark"]<-NA
ozoneSIP$COUNTY[ozoneSIP$COUNTY=="Whatcom"]<-NA
ozoneSIP$COUNTY[ozoneSIP$COUNTY=="Clallam"]<-NA
ozoneSIP$COUNTY[ozoneSIP$COUNTY=="Thurston"]<-NA
ozoneSIP$COUNTY[ozoneSIP$COUNTY=="Skagit"]<-NA
ozoneSIP$COUNTY<-factor(ozoneSIP$COUNTY)
str(ozoneSIP)
ozoneSIP<-na.omit(ozoneSIP)
str(ozoneSIP)

##Sort data for every 4th highest value by year and AQS (NEED TO WRITE LOOPING FUNCTION FOR THIS)
#Enumclaw
n<-length(ozone12$Daily.Max.8.hour.Ozone.Concentration[ozone12$AQS_SITE_ID=="53-033-0023"])
sort(ozone12$Daily.Max.8.hour.Ozone.Concentration[ozone12$AQS_SITE_ID=="53-033-0023"], partial=n-3)[n-3]
n<-length(ozone13$Daily.Max.8.hour.Ozone.Concentration[ozone13$AQS_SITE_ID=="53-033-0023"])
sort(ozone13$Daily.Max.8.hour.Ozone.Concentration[ozone13$AQS_SITE_ID=="53-033-0023"], partial=n-3)[n-3]
n<-length(ozone14$Daily.Max.8.hour.Ozone.Concentration[ozone14$AQS_SITE_ID=="53-033-0023"])
sort(ozone14$Daily.Max.8.hour.Ozone.Concentration[ozone14$AQS_SITE_ID=="53-033-0023"], partial=n-3)[n-3]

#North Bend
n<-length(ozone12$Daily.Max.8.hour.Ozone.Concentration[ozone12$AQS_SITE_ID=="53-033-0017"])
sort(ozone12$Daily.Max.8.hour.Ozone.Concentration[ozone12$AQS_SITE_ID=="53-033-0017"], partial=n-3)[n-3]
n<-length(ozone13$Daily.Max.8.hour.Ozone.Concentration[ozone13$AQS_SITE_ID=="53-033-0017"])
sort(ozone13$Daily.Max.8.hour.Ozone.Concentration[ozone13$AQS_SITE_ID=="53-033-0017"], partial=n-3)[n-3]
n<-length(ozone14$Daily.Max.8.hour.Ozone.Concentration[ozone14$AQS_SITE_ID=="53-033-0017"])
sort(ozone14$Daily.Max.8.hour.Ozone.Concentration[ozone14$AQS_SITE_ID=="53-033-0017"], partial=n-3)[n-3]

#Issaquah
n<-length(ozone12$Daily.Max.8.hour.Ozone.Concentration[ozone12$AQS_SITE_ID=="53-033-0010"])
sort(ozone12$Daily.Max.8.hour.Ozone.Concentration[ozone12$AQS_SITE_ID=="53-033-0010"], partial=n-3)[n-3]
n<-length(ozone13$Daily.Max.8.hour.Ozone.Concentration[ozone13$AQS_SITE_ID=="53-033-0010"])
sort(ozone13$Daily.Max.8.hour.Ozone.Concentration[ozone13$AQS_SITE_ID=="53-033-0010"], partial=n-3)[n-3]
n<-length(ozone14$Daily.Max.8.hour.Ozone.Concentration[ozone14$AQS_SITE_ID=="53-033-0010"])
sort(ozone14$Daily.Max.8.hour.Ozone.Concentration[ozone14$AQS_SITE_ID=="53-033-0010"], partial=n-3)[n-3]

#Beacon Hill
n<-length(ozone12$Daily.Max.8.hour.Ozone.Concentration[ozone12$AQS_SITE_ID=="53-033-0080"])
sort(ozone12$Daily.Max.8.hour.Ozone.Concentration[ozone12$AQS_SITE_ID=="53-033-0080"], partial=n-3)[n-3]
n<-length(ozone13$Daily.Max.8.hour.Ozone.Concentration[ozone13$AQS_SITE_ID=="53-033-0080"])
sort(ozone13$Daily.Max.8.hour.Ozone.Concentration[ozone13$AQS_SITE_ID=="53-033-0080"], partial=n-3)[n-3]
n<-length(ozone14$Daily.Max.8.hour.Ozone.Concentration[ozone14$AQS_SITE_ID=="53-033-0080"])
sort(ozone14$Daily.Max.8.hour.Ozone.Concentration[ozone14$AQS_SITE_ID=="53-033-0080"], partial=n-3)[n-3]

#Mt Rainier
n<-length(ozone12$Daily.Max.8.hour.Ozone.Concentration[ozone12$AQS_SITE_ID=="53-053-0012"])
sort(ozone12$Daily.Max.8.hour.Ozone.Concentration[ozone12$AQS_SITE_ID=="53-053-0012"], partial=n-3)[n-3]
n<-length(ozone13$Daily.Max.8.hour.Ozone.Concentration[ozone13$AQS_SITE_ID=="53-053-0012"])
sort(ozone13$Daily.Max.8.hour.Ozone.Concentration[ozone13$AQS_SITE_ID=="53-053-0012"], partial=n-3)[n-3]
n<-length(ozone14$Daily.Max.8.hour.Ozone.Concentration[ozone14$AQS_SITE_ID=="53-053-0012"])
sort(ozone14$Daily.Max.8.hour.Ozone.Concentration[ozone14$AQS_SITE_ID=="53-053-0012"], partial=n-3)[n-3]

#Skagit?
n<-length(ozone12$Daily.Max.8.hour.Ozone.Concentration[ozone12$AQS_SITE_ID=="53-057-0011"])
sort(ozone12$Daily.Max.8.hour.Ozone.Concentration[ozone12$AQS_SITE_ID=="553-057-0011"], partial=n-3)[n-3]
n<-length(ozone13$Daily.Max.8.hour.Ozone.Concentration[ozone13$AQS_SITE_ID=="53-057-0011"])
sort(ozone13$Daily.Max.8.hour.Ozone.Concentration[ozone13$AQS_SITE_ID=="53-057-0011"], partial=n-3)[n-3]
n<-length(ozone14$Daily.Max.8.hour.Ozone.Concentration[ozone14$AQS_SITE_ID=="53-057-0011"])
sort(ozone14$Daily.Max.8.hour.Ozone.Concentration[ozone14$AQS_SITE_ID=="53-057-0011"], partial=n-3)[n-3]

#Skagit?
n<-length(ozone12$Daily.Max.8.hour.Ozone.Concentration[ozone12$AQS_SITE_ID=="53-057-0020"])
sort(ozone12$Daily.Max.8.hour.Ozone.Concentration[ozone12$AQS_SITE_ID=="553-057-0020"], partial=n-3)[n-3]
n<-length(ozone13$Daily.Max.8.hour.Ozone.Concentration[ozone13$AQS_SITE_ID=="53-057-0020"])
sort(ozone13$Daily.Max.8.hour.Ozone.Concentration[ozone13$AQS_SITE_ID=="53-057-0020"], partial=n-3)[n-3]
n<-length(ozone14$Daily.Max.8.hour.Ozone.Concentration[ozone14$AQS_SITE_ID=="53-057-0020"])
sort(ozone14$Daily.Max.8.hour.Ozone.Concentration[ozone14$AQS_SITE_ID=="53-057-0020"], partial=n-3)[n-3]

#Yelm
n<-length(ozone12$Daily.Max.8.hour.Ozone.Concentration[ozone12$AQS_SITE_ID=="53-067-0005"])
sort(ozone12$Daily.Max.8.hour.Ozone.Concentration[ozone12$AQS_SITE_ID=="53-067-0005"], partial=n-3)[n-3]
n<-length(ozone13$Daily.Max.8.hour.Ozone.Concentration[ozone13$AQS_SITE_ID=="53-067-0005"])
sort(ozone13$Daily.Max.8.hour.Ozone.Concentration[ozone13$AQS_SITE_ID=="53-067-0005"], partial=n-3)[n-3]
n<-length(ozone14$Daily.Max.8.hour.Ozone.Concentration[ozone14$AQS_SITE_ID=="53-067-0005"])
sort(ozone14$Daily.Max.8.hour.Ozone.Concentration[ozone14$AQS_SITE_ID=="53-067-0005"], partial=n-3)[n-3]

#Turnbull
n<-length(ozone12$Daily.Max.8.hour.Ozone.Concentration[ozone12$AQS_SITE_ID=="53-063-0001"])
sort(ozone12$Daily.Max.8.hour.Ozone.Concentration[ozone12$AQS_SITE_ID=="53-063-0001"], partial=n-3)[n-3]
n<-length(ozone13$Daily.Max.8.hour.Ozone.Concentration[ozone13$AQS_SITE_ID=="53-063-0001"])
sort(ozone13$Daily.Max.8.hour.Ozone.Concentration[ozone13$AQS_SITE_ID=="53-063-0001"], partial=n-3)[n-3]
n<-length(ozone14$Daily.Max.8.hour.Ozone.Concentration[ozone14$AQS_SITE_ID=="53-063-0001"])
sort(ozone14$Daily.Max.8.hour.Ozone.Concentration[ozone14$AQS_SITE_ID=="53-063-0001"], partial=n-3)[n-3]

#August $ Fiske
n<-length(ozone12$Daily.Max.8.hour.Ozone.Concentration[ozone12$AQS_SITE_ID=="53-063-0021"])
sort(ozone12$Daily.Max.8.hour.Ozone.Concentration[ozone12$AQS_SITE_ID=="53-063-0021"], partial=n-3)[n-3]
n<-length(ozone13$Daily.Max.8.hour.Ozone.Concentration[ozone13$AQS_SITE_ID=="53-063-0021"])
sort(ozone13$Daily.Max.8.hour.Ozone.Concentration[ozone13$AQS_SITE_ID=="53-063-0021"], partial=n-3)[n-3]
n<-length(ozone14$Daily.Max.8.hour.Ozone.Concentration[ozone14$AQS_SITE_ID=="53-063-0021"])
sort(ozone14$Daily.Max.8.hour.Ozone.Concentration[ozone14$AQS_SITE_ID=="53-063-0021"], partial=n-3)[n-3]

#Greenbluff
n<-length(ozone12$Daily.Max.8.hour.Ozone.Concentration[ozone12$AQS_SITE_ID=="53-063-0046"])
sort(ozone12$Daily.Max.8.hour.Ozone.Concentration[ozone12$AQS_SITE_ID=="53-063-0046"], partial=n-3)[n-3]
n<-length(ozone13$Daily.Max.8.hour.Ozone.Concentration[ozone13$AQS_SITE_ID=="53-063-0046"])
sort(ozone13$Daily.Max.8.hour.Ozone.Concentration[ozone13$AQS_SITE_ID=="53-063-0046"], partial=n-3)[n-3]
n<-length(ozone14$Daily.Max.8.hour.Ozone.Concentration[ozone14$AQS_SITE_ID=="53-063-0046"])
sort(ozone14$Daily.Max.8.hour.Ozone.Concentration[ozone14$AQS_SITE_ID=="53-063-0046"], partial=n-3)[n-3]


#Select entries at or above project 8-hr NAAQS of 0.061 ppm
Nonatt<-subset(ozoneSIP, Daily.Max.8.hour.Ozone.Concentration>=0.061)
describe(Nonatt)

#Formatting date
Nonatt$date <- as.Date(Nonatt$Date, "%m/%d/%Y")
Nonatt$month <- format(Nonatt$date, "%b")
Nonatt$month
Nonatt$year <- format(Nonatt$date, "%Y")
Nonatt$year

ozoneall$date <- as.Date(ozoneall$Date, "%m/%d/%Y")
ozoneall$month <- format(ozoneall$date, "%b")
ozoneall$month
ozoneall$year <- format(ozoneall$date, "%Y")
ozoneall$year
#Selecting Specific Counties
is.King<-(ozoneall$COUNTY=="King")
is.King

is.Pierce<-(ozoneall$COUNTY=="Pierce")
is.Skagit<-(ozoneall$COUNTY=="Skagit")
is.Spokane<-(ozoneall$COUNTY=="Spokane")
is.Thurston<-(ozoneall$COUNTY=="Thurston")

#####################
##Descriptive stats##
#####################
#Remember Skagit County removed from data because it is has no nonattainment values
summary(Nonatt)

#Total number of AQI Sites per county
length(unique(ozoneall$AQS_SITE_ID[is.King]))
length(unique(ozoneall$AQS_SITE_ID[is.Pierce]))
length(unique(ozoneall$AQS_SITE_ID[is.Skagit]))
length(unique(ozoneall$AQS_SITE_ID[is.Spokane]))
length(unique(ozoneall$AQS_SITE_ID[is.Thurston]))

#Total number of AQS sites in exceedance per county with AQS_SITE_ID
table(Nonatt$AQS_SITE_ID, Nonatt$COUNTY)

#Concentration by county, AQS, and month
table(Nonatt$Daily.Max.8.hour.Ozone.Concentration, Nonatt$COUNTY)
table(Nonatt$Daily.Max.8.hour.Ozone.Concentration, Nonatt$AQS_SITE_ID)
table(Nonatt$AQS_SITE_ID, Nonatt$month)
table(ozoneall$month, ozoneall$COUNTY)
table(ozoneall$month[ozoneall$COUNTY=="Skagit"], ozoneall$AQS_SITE_ID[ozoneall$COUNTY=="Skagit"])

table(Nonatt$Daily.Max.8.hour.Ozone.Concentration, Nonatt$month)
table(Nonatt$COUNTY, Nonatt$month)

hist(Nonatt$Daily.Max.8.hour.Ozone.Concentration, main="", xlab="Ozone Concentration (ppm)", col="tomato")
title(main=list("NAAQS Exceedence Frequency in Nonattainment Areas of
     the Puget Sound and Spokane", cex=0.9))

plot(Nonatt$COUNTY, Nonatt$Daily.Max.8.hour.Ozone.Concentration, col=rainbow(12), cex.axis=0.5,
     xlab="Counties", ylab="Ozone Concentration (ppm)", main="Ozone Nonattainment by County in WA
     2012-2014")

###########
##Mapping##
###########
library(sp) # vector data
library(rgdal) #input/output, projections (read shapefiles)

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

coastshape<-readOGR(dsn="/Users/miriamcalkins/Documents/UWDEOHS/PhD Degree/Q3_Spring 2015/ENVH548/Homework/SIPProject/Data/coast", 
                 layer="CoastTrimmed") #source: WA DOT
summary(coastshape)
plot(coastshape, col="lightblue")
proj4string(coastshape)
coastshapenew<-coastshape <- spTransform(coastshape, CRS("+proj=lcc +lat_1=45.83333333333334 +lat_2=47.33333333333334 +lat_0=45.33333333333334 +lon_0=-120.5
                                    +x_0=500000.0000000001 +y_0=0 +ellps=GRS80 +units=us-ft +no_defs"))

####Create spatial data frame for Ozone
#All AQS
coordinates(ozoneSIP) = c("SITE_LONGITUDE", "SITE_LATITUDE")
proj4string(ozoneSIP) <- CRS("+init=epsg:4269 +proj=longlat +ellps=GRS80 +datum=NAD83 +no_defs +towgs84=0,0,0") #EPA's CRS
ozoneSIPWA <- spTransform(ozoneSIP, CRS("+proj=lcc +lat_1=45.83333333333334 +lat_2=47.33333333333334 +lat_0=45.33333333333334 +lon_0=-120.5
                                    +x_0=500000.0000000001 +y_0=0 +ellps=GRS80 +units=us-ft +no_defs"))

#Exceedance values
coordinates(Nonatt) = c("SITE_LONGITUDE", "SITE_LATITUDE")
proj4string(Nonatt) <- CRS("+init=epsg:4269 +proj=longlat +ellps=GRS80 +datum=NAD83 +no_defs +towgs84=0,0,0") #EPA's CRS
NonattWA <- spTransform(Nonatt, CRS("+proj=lcc +lat_1=45.83333333333334 +lat_2=47.33333333333334 +lat_0=45.33333333333334 +lon_0=-120.5
                                    +x_0=500000.0000000001 +y_0=0 +ellps=GRS80 +units=us-ft +no_defs"))

#Nonattainment areas
Nonattsites<-subset(Nonatt, AQS_SITE_ID=="53-033-0023"|AQS_SITE_ID=="53-063-0001"|AQS_SITE_ID=="53-063-0021"|
                      AQS_SITE_ID=="53-063-0046")
coordinates(Nonattsites) = c("SITE_LONGITUDE", "SITE_LATITUDE")
proj4string(Nonattsites) <- CRS("+init=epsg:4269 +proj=longlat +ellps=GRS80 +datum=NAD83 +no_defs +towgs84=0,0,0") #EPA's CRS
NonattsitesWA <- spTransform(Nonattsites, CRS("+proj=lcc +lat_1=45.83333333333334 +lat_2=47.33333333333334 +lat_0=45.33333333333334 +lon_0=-120.5
                                    +x_0=500000.0000000001 +y_0=0 +ellps=GRS80 +units=us-ft +no_defs"))


#Overlay data on WA state
plot(WAshape)
plot(Countyshape, add=TRUE, col="papayawhip")
plot(coastshapenew, add=T, col="lightblue")
plot(srshape, add=TRUE, col="gray74", cex=1.2)
plot(ozoneSIPWA, add=TRUE, pch=23, col="thistle4", bg="thistle3", cex=1.75)
plot(NonattWA, add=TRUE, pch=23, col="slateblue1", bg="slateblue1", cex=1.75)
plot(NonattsitesWA, add=TRUE, pch=23, col="slateblue4", bg="slateblue4", cex=1.75)
title(main=list("Puget Sound and Spokane Air Quality Monitoring
     Sites with Any Exceedances of 8-hr Ozone NAAQS", cex=1.2))
legend("top", c("AQS below NAAQS", "AQS above NAAQS", "Nonattainment AQS", "State Routes"), 
       fill=c("thistle", "slateblue1", "slateblue4", "gray"), horiz=T, cex=0.5)

#No roads
plot(WAshape)
plot(Countyshape, add=TRUE, col="papayawhip")
plot(coastshapenew, add=T, col="lightblue")
plot(ozoneSIPWA, add=TRUE, pch=23, col="thistle4", bg="thistle3", cex=1.75)
plot(NonattWA, add=TRUE, pch=23, col="slateblue4", bg="slateblue4", cex=1.75)
title(main=list("Puget Sound and Spokane Air Quality Monitoring
     Sites with Any Exceedances of 8-hr Ozone NAAQS", cex=1.2))
legend("top", c("AQS below NAAQS", "AQS above NAAQS"), 
       fill=c("thistle", "slateblue"), horiz=T, cex=0.5)

