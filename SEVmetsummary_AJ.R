# This script reads in a daily met file from Sevilleta weather stations and compiles seasonal and annual summaries

# Load required libraries
library(car)
library(reshape2)
library(date)
library(lubridate)
library(ggplot2) 
library(zoo)
library(SPEI)
library(plyr)
library(data.table)

# Read in daily meteorological data
# Note: This initial dataset needs to be high quality and gapfilled
dailyMet <- read.csv("~/Desktop/unsorted_AJdata/AJ_SevMet_gapfill14Dec18.csv", strip.white = T)

# Replace -999 values in the dataset with NA's
dailyMet[dailyMet==(-999)] <- NA

# Take a look at the data
head(dailyMet)
str(dailyMet)

# Format date column and create columns for year, month, day of month, and julian day (day of year) 
dailyMet$Date <- as.Date(dailyMet$Date)
dailyMet$Year <- year(dailyMet$Date)
dailyMet$Month <- month(dailyMet$Date)
dailyMet$Day <- mday(dailyMet$Date)
dailyMet$JulDay <- yday(dailyMet$Date)

#ggplot(dailyMet[dailyMet$Sta == 49 & dailyMet$Year == 2007,], aes(x = Date, y = Max_Temp, group = Sta, colour = as.factor(Sta))) + geom_line()

# Create new column for Growing Degree Day (GDD) calculation
dailyMet$GDD_calc <- (dailyMet$Max_Temp + dailyMet$Min_Temp)/2
dailyMet$GDD_calc[dailyMet$GDD_calc < 0] <- 0
dailyMet$GDD_calcW <- (dailyMet$Max_Temp + dailyMet$Min_Temp + 2)/2
dailyMet$GDD_calcW[dailyMet$GDD_calcW < 0] <- 0
#tapply(dailyMet$GDD_calc, dailyMet$Sta, summary)

# Assign water year (Oct. 1 of previous year - Sept. 30 of current year)
dailyMet$water.year <- dailyMet$Year
dailyMet$water.year[dailyMet$Month %in% c(10:12)] <- 
  dailyMet$water.year[dailyMet$Month %in% c(10:12)] + 1

# Calculate extreme event size threshold (95th percentile) for each station
ExtremeEvents <- ddply(dailyMet, "Sta", function(x) data.frame(
  bigevents = quantile(x$Precip[x$Precip > 0.3], probs = 0.95, na.rm=T)))

# Add lat, long, and elevation to each station
latlong <- as.data.frame(rbind(
  c("Deep Well", 40, 34.3601, -106.6891, 1571), 
  c("Five Points", 49, 34.3351, -106.7287, 1611), 
  c("Southern Gate", 41, 34.2186, -106.7950, 1515), 
  c("Cerro Montosa", 42, 34.3682, -106.5347, 1944), 
  c("Goat Draw", 48, 34.4145, -106.5229, 1794), 
  c("Blue Grama", 50, 34.3350, -106.6313, 1669), 
  c("Bronco Well", 45, 34.4068, -106.9328, 1524), 
  c("Red Tank", 43, 34.3987, -107.0376, 1748), 
  c("Headquarters", 1, 34.3512, -106.8815, 1440), 
  c("Rio Salado", 44, 34.2952, -106.9241, 1485)))
colnames(latlong) <- c("Station.Name", "Sta", "lat", "long", "elevation")

latlong$Sta <- as.numeric(as.character(latlong$Sta))
latlong$lat <- as.numeric(as.character(latlong$lat))
latlong$long <- as.numeric(as.character(latlong$long))
latlong$elevation <- as.numeric(as.character(latlong$elevation))
dailyMet <- join_all(list(dailyMet, latlong), type = "full")

# Make a monthly dataset for SPEI calculations
# Calculate sum.precip and mean.temp separately for each met station
# but then calculate SPEI across all site-months
monthlyMet <- as.data.frame(data.table(dailyMet)[, list(
  mean.temp = mean(Avg_Temp, na.rm = T),
  mean.maxtemp = mean(Max_Temp, na.rm = T),
  mean.mintemp = mean(Min_Temp, na.rm = T),
  mean.humidity = mean(Avg_RH, na.rm = T),
  mean.VPD = mean(VPD, na.rm = T),
  mean.windsp = mean(Avg_Wind_SP, na.rm = T),
  total.solarrad = mean(Solar_Rad, na.rm = T),
  mean.soiltemp = mean(Avg_Soil_Temp, na.rm = T),
  precip = sum(Precip, na.rm = T),
  mean.precip = mean(Precip, na.rm = T)), #only calculated to find NA's in next step
  by = list(Sta, Station.Name, lat, long, elevation, Year, Month)])
# This is annoying! For some reason, the sum() function returns a zero when all values are NA. If mean.precip is zero, then change precip to NA. 
monthlyMet$precip[is.na(monthlyMet$mean.precip)] <- NA

#ggplot(monthlyMet[monthlyMet$Sta %in% c(1,40,50) & monthlyMet$Year == 1989,], aes(x = as.Date(paste(15, Month, Year), "%d %m %Y"), y = precip, group = Sta, colour = as.factor(Sta))) + geom_point(alpha = 0.5) + geom_line(alpha = 0.5) + geom_point(data = monthlyMet[monthlyMet$Sta %in% 1 & monthlyMet$Year == 1989,], size = 2)

# still a problem with temps at two sites
ggplot(monthlyMet[monthlyMet$Year > 2015,], aes(x = as.Date(paste(15, Month, Year), "%d %m %Y"), y = mean.temp, colour = Station.Name)) + geom_line()

# there are a few too-low wind speeds to look into
ggplot(monthlyMet, aes(x = as.Date(paste(15, Month, Year), "%d %m %Y"), y = mean.windsp, colour = Station.Name)) + geom_line()

# there are a few problems with total solar radiation
ggplot(monthlyMet, aes(x = as.Date(paste(15, Month, Year), "%d %m %Y"), y = total.solarrad, colour = Station.Name)) + geom_line()

# there are a few problems with mean soil temperature
ggplot(monthlyMet, aes(x = as.Date(paste(15, Month, Year), "%d %m %Y"), y = mean.soiltemp, colour = Station.Name)) + geom_line()


# Calculate PET using thornthwaite method
monthlyMet$PET <- NA
for (i in 1:length(unique(monthlyMet$Sta))) {
  this.Sta <- unique(monthlyMet$Sta)[i]
  monthlyMet$PET[monthlyMet$Sta %in% this.Sta] <- 
    thornthwaite(na.rm=T,
                 Tave = monthlyMet$mean.temp[monthlyMet$Sta %in% this.Sta],
                 lat = unique(monthlyMet$lat[monthlyMet$Sta %in% this.Sta])) }

### Calculate SPEI individually for each site
# Order data.frame
monthlyMet <- monthlyMet[with(monthlyMet, order(Sta, Year, Month)),]
monthlyMet$SPEI_12 <- monthlyMet$SPEI_6 <- NA
  
for (i in 1:length(unique(monthlyMet$Sta))) {
  # Only include dates
  #monthlyMet <- monthlyMet[!is.na(monthlyMet$precip) & !is.na(monthlyMet$PET),]
  this.Sta <- unique(monthlyMet$Sta)[i]
  # Calculate SPEI
  monthlyMet$SPEI_12[monthlyMet$Sta %in% this.Sta & !is.na(monthlyMet$precip) & !is.na(monthlyMet$PET)] <- as.vector(spei(scale = 12, na.rm = T, data = (monthlyMet$precip[monthlyMet$Sta %in% this.Sta & !is.na(monthlyMet$precip) & !is.na(monthlyMet$PET)] - monthlyMet$PET[monthlyMet$Sta %in% this.Sta & !is.na(monthlyMet$precip) & !is.na(monthlyMet$PET)]))$fitted)
  monthlyMet$SPEI_6[monthlyMet$Sta %in% this.Sta & !is.na(monthlyMet$precip) & !is.na(monthlyMet$PET)] <- as.vector(spei(scale = 6, na.rm = T, data = (monthlyMet$precip[monthlyMet$Sta %in% this.Sta & !is.na(monthlyMet$precip) & !is.na(monthlyMet$PET)] - monthlyMet$PET[monthlyMet$Sta %in% this.Sta & !is.na(monthlyMet$precip) & !is.na(monthlyMet$PET)]))$fitted)
}

monthlyMet$SPEI_12[is.na(monthlyMet$PET)] <- NA
monthlyMet$SPEI_6[is.na(monthlyMet$PET)] <- NA

#ggplot(monthlyMet, aes(x = as.Date(paste(15, Month, Year), "%d %m %Y"), y = SPEI_12, group = Station.Name, colour = as.factor(Station.Name))) + geom_line()
#ggplot(monthlyMet, aes(x = as.Date(paste(15, Month, Year), "%d %m %Y"), y = PET, group = Station.Name, colour = as.factor(Station.Name))) + geom_line()


# For comparative SPEI, subset to stations of interest
monthlyMet.core <- monthlyMet[monthlyMet$Sta %in% c(40, 42, 49, 50),]

#ggplot(monthlyMet, aes(x = as.Date(paste(15, Month, Year), "%d %m %Y"), y = precip, group = Sta, colour = as.factor(Sta))) + geom_line()

# trim to similar years
monthlyMet.core <- monthlyMet.core[monthlyMet.core$Year >= 1999,]

# order data.frame
monthlyMet.core <- monthlyMet.core[with(monthlyMet.core, order(Sta, Year, Month)),]

# Calculate SPEI.core
monthlyMet.core$SPEI_12.core <- 
  as.vector(spei(data = (monthlyMet.core$precip - monthlyMet.core$PET), scale = 12, na.rm = T)$fitted)
monthlyMet.core$SPEI_6.core <- 
  as.vector(spei(data = (monthlyMet.core$precip - monthlyMet.core$PET), scale = 6, na.rm = T)$fitted)

monthlyMet.core$SPEI_12.core[is.na(monthlyMet.core$PET) | is.na(monthlyMet.core$precip)] <- NA
monthlyMet.core$SPEI_6.core[is.na(monthlyMet.core$PET) | is.na(monthlyMet.core$precip)] <- NA

# The first 11 or 5 months for EACH site should be NA's (SPEI12 or SPEI6)
# Double check that these numbers still work with new precip filters
monthlyMet.core[monthlyMet.core$Sta == 40 & monthlyMet.core$Year == 1991 & monthlyMet.core$Month <= 11, c("SPEI_12.core")] <- NA
monthlyMet.core[monthlyMet.core$Sta == 42 & (monthlyMet.core$Year == 1990 | (monthlyMet.core$Year == 1991 & monthlyMet.core$Month <= 10)), c("SPEI_12.core")] <- NA
monthlyMet.core[monthlyMet.core$Sta == 49 & monthlyMet.core$Year == 1999 & monthlyMet.core$Month <= 11, c("SPEI_12.core")] <- NA
monthlyMet.core[monthlyMet.core$Sta == 50 & monthlyMet.core$Year == 2002 & monthlyMet.core$Month <= 11, c("SPEI_12.core")] <- NA

monthlyMet.core[monthlyMet$Sta == 40 & monthlyMet.core$Year == 1991 & monthlyMet.core$Month <= 5, c("SPEI_6.core")] <- NA
monthlyMet.core[monthlyMet.core$Sta == 42 & (monthlyMet.core$Year == 1990 | (monthlyMet.core$Year == 1991 & monthlyMet$Month <= 4)), c("SPEI_6.core")] <- NA
monthlyMet.core[monthlyMet.core$Sta == 49 & monthlyMet.core$Year == 1999 & monthlyMet.core$Month <= 5, c("SPEI_6.core")] <- NA
monthlyMet.core[monthlyMet.core$Sta == 50 & monthlyMet.core$Year == 2002 & monthlyMet.core$Month <= 5, c("SPEI_6.core")] <- NA

# Join SPEI.core with monthly data
monthlyMet <- join_all(list(monthlyMet, monthlyMet.core), type = "full")

# Summarize per site-year
sy_SPEI <- as.data.frame(data.table(monthlyMet)[, list(
  SPEI_12.May = SPEI_12[Month %in% 5],
  SPEI_6.May = SPEI_6[Month %in% 5],
  SPEI_12.Sept = SPEI_12[Month %in% 9],
  SPEI_6.Sept = SPEI_6[Month %in% 9],
  SPEI_12.May.core = SPEI_12.core[Month %in% 5],
  SPEI_6.May.core = SPEI_6.core[Month %in% 5],
  SPEI_12.Sept.core = SPEI_12.core[Month %in% 9],
  SPEI_6.Sept.core = SPEI_6.core[Month %in% 9]),
  by = list(Year, Sta, Station.Name, lat, long, elevation)])
colnames(sy_SPEI)[colnames(sy_SPEI) %in% "Year"] <- c("water.year")


#Initiate dataframe for climate summaries----------------------
dailyMet$Station_years <- paste(dailyMet$Sta, dailyMet$water.year, sep="_")
Stationyears <- unique(dailyMet$Station_years) #list of station-years
Stations <- unique(dailyMet$Sta)

column.names <- c("Sta", "water.year", "precip.annual", "precip.JJAS", "precip.FMAM", "precip.ONDJ", "precip.intra_CV", "events", "ev_size", "extreme_size", "CDD", "extreme.CDD", "GDD.AMJJAS", "GDD.FMAM", "soil_T", "soil_T.MJJAS", "VPD.ASO", "VPD.MJJ", "air_T", "air_T.MJJAS", "maxmonthly.air_T.JJA", "minmonthly.air_T.DJF")

Met <- data.frame(matrix(NA, nrow=length(Stationyears), ncol=length(column.names)))
colnames(Met) <- column.names

# Initiate dryDays
# AJ note: This currently uses all dry periods from all the sites, which means cross-site dry periods are counted many (~10?) times each. Therefore the current 95% "extreme" is super-extreme. Maybe should find the 95% extreme within each site-year and take the mean of all those?
dryDays <- c()

mindate <- NA
for (i in 1:length(Stationyears)) {
  mindate <- c(mindate, as.character(dailyMet[dailyMet$Station_years %in% Stationyears[i], "Date"])[1]) }

# Calculate variables
for (i in 1:length(Stationyears)) {
  
  subdata <- dailyMet[dailyMet$Station_years == Stationyears[i],]
  
  Met$Sta[i] <- subdata$Sta[1]
  Met$water.year[i] <- subdata$water.year[1]
  
  FirstDate <- yday(subdata$Date[1])
  FirstDate[FirstDate >= yday(as.Date("September 28", "%B %d"))] <- 0
  
  # If we have data after May 1 of the station-year, calculate the following:
  if (FirstDate <= yday(as.Date("May 1", "%B %d"))) {
    Met$soil_T.MJJAS[i] <- mean(subdata$Avg_Soil_Temp[subdata$Month %in% 5:9], na.rm=T)
    Met$air_T.MJJAS[i] <- mean(subdata$Avg_Temp[subdata$Month %in% 5:9], na.rm=T)
    Met$meanmax.air_T.JJA[i] <- mean(subdata$Max_Temp[subdata$Month %in% 6:8], na.rm=T)    
    Met$VPD.MJJ[i] <- mean(subdata$VPD[subdata$Month %in% 5:7], na.rm=T)
    Met$precip.JJAS[i] <- sum(subdata$Precip[subdata$Month %in% 6:9], na.rm=T)  
    Met$VPD.ASO[i] <- mean(subdata$VPD[subdata$Month %in% 8:10], na.rm=T)
  }
  
  # If we have data after April 1 of the station-year, calculate the following:
  if (FirstDate <= yday(as.Date("April 1", "%B %d"))) {
    Met$GDD.AMJJAS[i] <- sum(subdata$GDD_calc[subdata$GDD_calc > 0 & 
                                         subdata$Month %in% 4:9], na.rm=T)
  }
  
  # If we have data after February 1 of the station-year, calculate the following:
  if (FirstDate <= yday(as.Date("February 1", "%B %d"))) {
    Met$precip.FMAM[i] <- sum(subdata$Precip[subdata$Month %in% 2:5], na.rm=T)
    Met$GDD.FMAM[i] <- sum(subdata$GDD_calc[subdata$GDD_calc > 0 & 
                                              subdata$Month %in% 2:5], na.rm=T)
  }
  
  # If we have data for the entire water year, calculate the following:
  if (FirstDate == 0) {
    # if we have the entire water year
    Met$precip.ONDJ[i] <- sum(subdata$Precip[subdata$Month %in% c(1,10:12)], na.rm=T)
    Met$precip.annual[i] <- sum(subdata$Precip, na.rm = T)
    Met$precip.intra_CV[i] <- sd(subdata$Precip, na.rm=T) / mean(subdata$Precip, na.rm=T)
    Met$soil_T[i] <- mean(subdata$Avg_Soil_Temp, na.rm=T)
    Met$air_T[i] <- mean(subdata$Avg_Temp, na.rm=T)
    Met$meanmin.air_T.DJF[i] <- mean(subdata$Min_Temp[subdata$Month %in% c(12,1,2)], na.rm=T)   
    
    eventdata <- subdata[!is.na(subdata$Precip) & subdata$Precip > 0.3,]
    threshold <- quantile(eventdata$Precip, probs = 0.95, na.rm = T)
    Met$events[i] <- length(eventdata[,1])
    Met$ev_size[i] <- mean(eventdata$Precip, na.rm=T) 
    Met$extreme_size[i] <- length(eventdata[!is.na(eventdata$Precip) & eventdata$Precip > threshold, 1]) #95th percentile
    
    diffs <- c() # reinitiate each station-year
    for (j in 1:(length(eventdata[,1])-1)) {
      diffs[j] <- eventdata$JulDay[j+1] - eventdata$JulDay[j]
    }
    Met$CDD[i] <- mean(diffs[diffs>1])
    dryDays <- c(dryDays, diffs[diffs>1])
  }
  
  # If our data don't fit any of those date requirements, skip:
  if (FirstDate > yday(as.Date("May 1", "%B %d"))) next
  
}
#the skipped first years are all zeros

# Calculate extreme CDD
dry.quant <- quantile(dryDays, probs=0.95, na.rm = T)
#97.5% 2, 45.025, 95% 2, 36
#relax to 95th percentile because otherwise too little data
Met$extreme_CDD<-c()

for (i in 1:length(Stationyears)){
  subdata <- dailyMet[dailyMet$Station_years %in% Stationyears[i],]
  if (subdata$water.year[1] %in% min(dailyMet$water.year[dailyMet$Sta%in%subdata$Sta[1]])) next    
  eventdata <- subdata[subdata$Precip > 0.3,]
  diffs <- c() #reinitiate each time
  for (j in 1:(length(eventdata[,1])-1)){
    diffs[j] <- eventdata$JulDay[j+1] - eventdata$JulDay[j]
    dry <- diffs[diffs > 1]
    Met$extreme_CDD[i] <- length(dry[dry > dry.quant]) 
      #use 0.95 percentile from quantile as the number here
  }
}
  
# Subset to years with precip data
Met <- Met[!is.na(Met$Sta) & Met$water.year > 0 & Met$precip.annual > 0,]

# Filter out some extreme values
#ggplot(Met, aes(x = water.year, y = events, group = Sta, colour = as.factor(Sta))) + geom_line()
Met$GDD.FMAM[Met$GDD.FMAM < 500] <- NA
Met$GDD.AMJJAS[Met$GDD.AMJJAS < 3000] <- NA
Met$soil_T[Met$soil_T > 19.7 | Met$soil_T < 14] <- NA
Met$soil_T.MJJAS[Met$soil_T.MJJAS < 22] <- NA
Met$air_T[Met$air_T > 18 | Met$air_T < 11] <- NA
Met$extreme_CDD[Met$extreme_CDD > 10 ] <- NA


# Make incomplete summary data NA
Met <- Met[Met$water.year <= max(dailyMet$Year),]
if (month(max(dailyMet$Date)) < 10) {
  Met$precip.annual <- Met$precip.JJAS <- Met$events <- Met$ev_size <- Met$CDD <- Met$GDD.monsoon <- Met$soil_T <- Met$soil_T.MJJAS <- Met$VPD.MJJ <- Met$air_T.MJJAS <- Met$max.air_T.MJJAS <- Met$min.air_T.MJJAS <- Met$extreme.CDD <- NA
}

# Merge on SPEI numbers
Metmerge <- join_all(list(Met, sy_SPEI[!is.na(sy_SPEI$Sta),]), type = "left")


#ggplot(Met, aes(x = water.year, y = precip.monsoon, group = Sta, colour = as.factor(Sta))) + geom_line()


#WARNING: do not use CDD or precip data from Met_45 - has a huge number of CDD in one year
#Doug was going to fix this

#Write out
write.csv(Metmerge[!is.na(Metmerge$Sta), c("Sta", "Station.Name", "lat", "long", "elevation", "water.year", "precip.annual", "precip.ONDJ", "precip.FMAM", "precip.JJAS", "precip.intra_CV", "GDD.FMAM", "GDD.AMJJAS", "soil_T", "soil_T.MJJAS", "air_T", "air_T.MJJAS", "meanmax.air_T.JJA", "meanmin.air_T.DJF", "CDD", "extreme_CDD", "VPD.MJJ", "VPD.ASO", "SPEI_12.May", "SPEI_6.May", "SPEI_12.Sept", "SPEI_6.Sept", "SPEI_12.May.core", "SPEI_6.May.core", "SPEI_12.Sept.core", "SPEI_6.Sept.core", "ev_size", "extreme_size", "events")],file=paste0("~/Desktop/SEVmetAJ_", format(Sys.Date(), "%d%b%y"), ".csv"), row.names = F)


#ggplot(sy_SPEI, aes(x = water.year, y = SPEI_12.Sept, group = Station.Name, colour = Station.Name)) + geom_line()

#ggplot(Metmerge, aes(x = water.year, y = soil_T, group = Station.Name, colour = Station.Name)) + geom_line()


