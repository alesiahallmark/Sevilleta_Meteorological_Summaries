# This script reads in 30 minute met data from the Sevilleta and computes daily summary statistics

# load required libraries
library(car)
library(reshape2)
library(date)
library(lubridate)
library(ggplot2) 
library(zoo)
library(SPEI)
library(plyr)
library(dplyr)
library(data.table)
library(tidyr)
library(rlang)



# Read in data
data <- unique(read.csv("~/Desktop/unsorted_data_fromothers/Met_All091917.csv"))
colnames(data)
# Change -999 values to NA
data[data == -999] <- NA


# Read in 2017 met data
data17 <- unique(read.csv("~/Desktop/unsorted_data_fromothers/hourly_filtered_17.csv"))
colnames(data17)
data18 <- unique(read.csv("~/Desktop/unsorted_data_fromothers/hourly_filtered_18.csv"))
colnames(data18)

newdata <- full_join(data17, data18)
# Change -999 values to NA
#newdata[newdata %in% -999] <- NA

# Create 2017 daily records
newdata <- as.data.frame(data.table(newdata)[, list(
  Avg_Temp = mean(c(min(airt, na.rm = T), max(airt, na.rm = T)), na.rm = T),
  Max_Temp = max(airt, na.rm = T),
  Min_Temp = min(airt, na.rm = T),
  Avg_RH = mean(rh, na.rm = T),
  Precip = sum(ppt, na.rm = T),
  Avg_VP = mean(vp, na.rm = T),
  Avg_Wind_SP = mean(ms, na.rm = T),
  Avg_Resultant_WindSP = mean(vms, na.rm = T),
  Avg_Wind_Dir = mean(dir, na.rm = T),
  Solar_Rad = mean(sol, na.rm = T),
  Avg_Soil_Temp = mean(cm10, na.rm = T),
  Max_Soil_Temp = max(cm10, na.rm = T),
  Min_Soil_Temp = min(cm10, na.rm = T)),
  by = list(date, month, day_of_month, year, sta)])

# Kris's new column names are better, but I'm too lazy to change all code right now
colnames(newdata) <- c("Date", "Month", "Day", "Year", "Sta", "Avg_Temp", "Max_Temp", "Min_Temp", "Avg_RH", "Precip", "Avg_VP", "Avg_Wind_SP", "Avg_Resultant_WindSP", "Avg_Wind_Dir", "Solar_Rad", "Avg_Soil_Temp", "Max_Soil_Temp", "Min_Soil_Temp")

data <- full_join(data, newdata)


# There are duplicate dates for some reason
# This is just a patch to get rid of them. Should actually be fixed at previous summarization step
data <- as.data.frame(data.table(data)[, list(
  Avg_Temp = mean(Avg_Temp, na.rm = T),
  Max_Temp = mean(Max_Temp, na.rm = T),
  Min_Temp = mean(Min_Temp, na.rm = T),
  Avg_RH = mean(Avg_RH, na.rm = T),
  Precip = mean(Precip, na.rm = T),
  Avg_VP = mean(Avg_VP, na.rm = T),
  Avg_Wind_SP = mean(Avg_Wind_SP, na.rm = T),
  Avg_Resultant_WindSP = mean(Avg_Resultant_WindSP, na.rm = T),
  Avg_Wind_Dir = mean(Avg_Wind_Dir, na.rm = T),
  Solar_Rad = mean(Solar_Rad, na.rm = T),
  Avg_Soil_Temp = mean(Avg_Soil_Temp, na.rm = T),
  Max_Soil_Temp = mean(Max_Soil_Temp, na.rm = T),
  Min_Soil_Temp = mean(Min_Soil_Temp, na.rm = T)),
  by = list(Date, Month, Day, Year, Sta)])



# Format columns
data$Date <- as.Date(paste(data$Month, data$Day, data$Year), 
                     format = "%m %d %Y")
data$Sta <- as.factor(data$Sta)

# VP for 2016-2018 is 10x higher than previous data...
#ggplot(data, aes(x = Date, y = Avg_VP, group = Sta, colour = Sta)) + geom_line()
data$Avg_VP[data$Year %in% 2016:2018] <- data$Avg_VP[data$Year %in% 2016:2018] / 10
# Get rid of some anomalous VP data 
data$Avg_VP[data$Avg_VP < 0.05] <- NA

# Solar_rad for station 48 is messed up in 2012 Nov. 11, 2012
#ggplot(data[data$Sta == "48" & data$Date > as.Date("2011-01-01") & data$Date < as.Date("2013-12-30"),], aes(x = Date, y = Solar_Rad, group = Sta, colour = Sta)) + geom_line()
data$Solar_Rad[data$Sta == "48" & data$Date > as.Date("2011-11-11") & data$Date < as.Date("2012-11-11")] <- NA

# Solar_rad is ~5 times higher in 2017 and 2018?
data$Solar_Rad[data$Year %in% 2017:2018] <- data$Solar_Rad[data$Year %in% 2017:2018] /5
data$Solar_Rad[data$Solar_Rad > 50] <- NA

# Temps at Station 48 are messed up in 2016-2017
data$Avg_Temp[data$Avg_Temp < -25] <- NA
data$Max_Temp[data$Max_Temp < -25] <- NA
data$Min_Temp[data$Min_Temp < -35] <- NA

# RH
data$Avg_RH[data$Avg_RH < 5] <- NA

# Soil Temps
data$Avg_Soil_Temp[data$Avg_Soil_Temp < -10] <- NA
data$Max_Soil_Temp[data$Max_Soil_Temp < -10] <- NA
data$Min_Soil_Temp[data$Min_Soil_Temp < -12] <- NA

#Station 1 (starts 1/1/92) - use 45 to gapfill
#Station 40 (starts 1/1/88) - use 49 to gapfill
#Station 41 (starts 2/15/89) - use 49 to gapfill
#Station 42 (starts 2/22/89) - use 48 to gapfill
#Station 43 (starts 2/17/89) - use 45 to gapfill
#Station 44 (starts 3/31/89) - use 01 to gapfill
#Station 45 (starts 3/31/89) - use 43 to gapfill
#Station 48 (starts 10/8/98) - use 42 to gapfill
#Station 49 (starts 1/1/99) - use 40 to gapfill
#Station 50 (starts 1/26/02) - use 40 to gapfill

# Define a function to calculate the trimmed standard deviation
sd.trim <- function(x, trim=0, na.rm=FALSE, ...)
{
  if(!is.numeric(x) && !is.complex(x) && !is.logical(x)) {
    warning("argument is not numeric or logical: returning NA")
    return(NA_real_)
  }
  if(na.rm) x <- x[!is.na(x)]
  if(!is.numeric(trim) || length(trim) != 1)
    stop("'trim' must be numeric of length one")
  n <- length(x)
  if(trim > 0 && n > 0) {
    if(is.complex(x)) stop("trimmed sd are not defined for complex data")
    if(trim >= 0.5) return(0)
    lo <- floor(n * trim) + 1
    hi <- n + 1 - lo
    x <- sort.int(x, partial = unique(c(lo, hi)))[lo:hi]
  }
  sd(x)
}


# Calculate average for each variable every day, trim outliers
yearly.averages <- as.data.frame(data.table(data)[, list(
  mAvg_Temp = mean(Avg_Temp, na.rm = T, trim = 0.025),
  sdAvg_Temp = sd.trim(Avg_Temp, na.rm = T, trim = 0.025),
  mMax_Temp = mean(Max_Temp, na.rm = T, trim = 0.025),
  sdMax_Temp = sd.trim(Max_Temp, na.rm = T, trim = 0.025),
  mMin_Temp = mean(Min_Temp, na.rm = T, trim = 0.025),
  sdMin_Temp = sd.trim(Min_Temp, na.rm = T, trim = 0.025),
  mAvg_RH = mean(Avg_RH, na.rm = T, trim = 0.025),
  sdAvg_RH = sd.trim(Avg_RH, na.rm = T, trim = 0.025),
  mPrecip = mean(Precip, na.rm = T, trim = 0.025),
  sdPrecip = sd.trim(Precip, na.rm = T, trim = 0.025),
  mAvg_VP = mean(Avg_VP, na.rm = T, trim = 0.025),
  sdAvg_VP = sd.trim(Avg_VP, na.rm = T, trim = 0.025),
  mAvg_Wind_SP = mean(Avg_Wind_SP, na.rm = T, trim = 0.025),
  sdAvg_Wind_SP = sd.trim(Avg_Wind_SP, na.rm = T, trim = 0.025),
  mAvg_Resultant_WindSP = mean(Avg_Resultant_WindSP, na.rm = T, trim = 0.025),
  sdAvg_Resultant_WindSP = sd.trim(Avg_Resultant_WindSP, na.rm = T, trim = 0.025),
  mAvg_Wind_Dir = mean(Avg_Wind_Dir, na.rm = T, trim = 0.025),
  sdAvg_Wind_Dir = sd.trim(Avg_Wind_Dir, na.rm = T, trim = 0.025),
  mSolar_Rad = mean(Solar_Rad, na.rm = T, trim = 0.025),
  sdSolar_Rad = sd.trim(Solar_Rad, na.rm = T, trim = 0.025),
  mAvg_Soil_Temp = mean(Avg_Soil_Temp, na.rm = T, trim = 0.025),
  sdAvg_Soil_Temp = sd.trim(Avg_Soil_Temp, na.rm = T, trim = 0.025),
  mMax_Soil_Temp = mean(Max_Soil_Temp, na.rm = T, trim = 0.025),
  sdMax_Soil_Temp = sd.trim(Max_Soil_Temp, na.rm = T, trim = 0.025),
  mMin_Soil_Temp = mean(Min_Soil_Temp, na.rm = T, trim = 0.025),
  sdMin_Soil_Temp = sd.trim(Min_Soil_Temp, na.rm = T, trim = 0.025)),
  by = list(Month, Day)])

data.m <- merge(data, unique(yearly.averages), all = T)

ggplot(data.m, aes(x = Date, y = Min_Soil_Temp)) + 
  geom_line(aes(group = Sta, colour = Sta)) +
  geom_line(aes(x = Date, y = data.m$mMin_Soil_Temp + (data.m$sdMin_Soil_Temp*6)), colour = "black") + 
  geom_line(aes(x = Date, y = data.m$mMin_Soil_Temp - (data.m$sdMin_Soil_Temp*6)), colour = "black") +
  geom_hline(yintercept = -12)

# Get rid of baaad data
data.m$Avg_Temp[!is.na(data.m$Avg_Temp) & abs(data.m$Avg_Temp - data.m$mAvg_Temp) > data.m$sdAvg_Temp*6] <- NA
data.m$Max_Temp[!is.na(data.m$Max_Temp) & abs(data.m$Max_Temp - data.m$mMax_Temp) > data.m$sdMax_Temp*6] <- NA
data.m$Min_Temp[!is.na(data.m$Min_Temp) & abs(data.m$Min_Temp - data.m$mMin_Temp) > data.m$sdMin_Temp*6] <- NA
#data.m$Avg_RH[] <- NA
#data.m$Precip[] <- NA
data.m$Avg_VP[!is.na(data.m$Avg_VP) & abs(data.m$Avg_VP - data.m$mAvg_VP) > data.m$sdAvg_VP*6] <- NA
#data.m$Avg_Wind_SP[data.m$Avg_Wind_SP < 0.2] <- NA
#data.m$Avg_Resultant_WindSP[] <- NA
#data.m$Avg_Wind_Dir[] <- NA
#data.m$Solar_Rad[] <- NA
data.m$Avg_Soil_Temp[!is.na(data.m$Avg_Soil_Temp) & abs(data.m$Avg_Soil_Temp - data.m$mAvg_Soil_Temp) > data.m$sdAvg_Soil_Temp*6] <- NA
data.m$Max_Soil_Temp[!is.na(data.m$Max_Soil_Temp) & abs(data.m$Max_Soil_Temp - data.m$mMax_Soil_Temp) > data.m$sdMax_Soil_Temp*6] <- NA
data.m$Min_Soil_Temp[!is.na(data.m$Min_Soil_Temp) & abs(data.m$Min_Soil_Temp - data.m$mMin_Soil_Temp) > data.m$sdMin_Soil_Temp*6] <- NA

# Make sure that precip is NA (not 0) for dates before precip gages go online
# Here, I treat the first full month of data with rainfall that matches nearby sites as trustworthy
data.m$Precip[data.m$Sta == 1 & data.m$Date < 1992] <- NA
data.m$Precip[data.m$Sta == 40 & data.m$Date < as.Date("1988-01-01")] <- NA
data.m$Precip[data.m$Sta == 41 & data.m$Date < as.Date("1989-03-01")] <- NA
data.m$Precip[data.m$Sta == 42 & data.m$Date < as.Date("1989-03-01")] <- NA
data.m$Precip[data.m$Sta == 43 & data.m$Date < as.Date("1989-08-01")] <- NA
data.m$Precip[data.m$Sta == 44 & data.m$Date < as.Date("1989-06-01")] <- NA
data.m$Precip[data.m$Sta == 45 & data.m$Date < as.Date("1989-08-01")] <- NA
data.m$Precip[data.m$Sta == 48] <- NA
data.m$Precip[data.m$Sta == 49 & data.m$Date < as.Date("1999-02-01")] <- NA
data.m$Precip[data.m$Sta == 50 & data.m$Date < as.Date("2002-06-01")] <- NA

#visualize rainfall data
ggplot(filled.data.two[filled.data.two$Sta %in% c(1,40,41,42,43,44,45,48,49,50) & filled.data.two$Year == 1989,], aes(x = Date, y = Precip, group = Sta, colour = as.factor(Sta))) + geom_point(alpha = 0.5) + geom_line(alpha = 0.5) + geom_point(data = filled.data.two[filled.data.two$Sta %in% 1 & filled.data.two$Year == 1989,], size = 2)

# Plot for visualizng data errors
ggplot(filled.data, aes(x = Date, y = Avg_Temp, group = Sta, colour = as.factor(Sta))) + 
  geom_line() + 
  geom_point(data = data.m[!is.na(data.m$Min_Soil_Temp) & abs(data.m$Min_Soil_Temp - data.m$mMin_Soil_Temp) > data.m$sdMin_Soil_Temp*6,], aes(x = Date, y = Min_Soil_Temp), size = 1.2, colour = "red") +
  xlim(as.Date("2012-01-01"), as.Date("2017-01-01"))


# Create new data.frame for gap-filled data
# Practice with one variable and two sites?
#modeled.data <- data.m[,c("Date", "Sta", "Avg_Temp", "Max_Temp", "Min_Temp", "Avg_RH", "Precip", "Avg_VP", "Avg_Wind_SP", "Avg_Resultant_WindSP", "Avg_Wind_Dir", "Solar_Rad", "Avg_Soil_Temp", "Max_Soil_Temp", "Min_Soil_Temp")]

# write a function that reads through each variable
create.filled.df <- function(input.df, vector.to.fill) {
  sub.df <- unique(input.df[,c("Date", "Sta", vector.to.fill)])
  sub.df <- spread_(sub.df, key_col = "Sta", value_col = vector.to.fill)
  #Station 1 (starts 1/1/92) - use 45 to gapfill
  sub.df[,"1"][is.na(sub.df[,"1"]) & sub.df$Date >= "1992-01-01"] <- 
    sub.df[,"45"][is.na(sub.df[,"1"]) & sub.df$Date >= "1992-01-01"]
  #Station 40 (starts 1/1/88) - use 49 to gapfill
  sub.df[,"40"][is.na(sub.df[,"40"]) & sub.df$Date >= "1988-01-01"] <- 
    sub.df[,"49"][is.na(sub.df[,"40"]) & sub.df$Date >= "1988-01-01"]
  #Station 41 (starts 2/15/89) - use 49 to gapfill
  sub.df[,"41"][is.na(sub.df[,"41"]) & sub.df$Date >= "1989-02-15"] <- 
    sub.df[,"49"][is.na(sub.df[,"41"]) & sub.df$Date >= "1989-02-15"]
  #Station 42 (starts 2/22/89) - use 48 to gapfill
  sub.df[,"42"][is.na(sub.df[,"42"]) & sub.df$Date >= "1989-02-22"] <- 
    sub.df[,"48"][is.na(sub.df[,"42"]) & sub.df$Date >= "1989-02-22"]
  #Station 43 (starts 2/17/89) - use 45 to gapfill
  sub.df[,"43"][is.na(sub.df[,"43"]) & sub.df$Date >= "1989-02-17"] <- 
    sub.df[,"45"][is.na(sub.df[,"43"]) & sub.df$Date >= "1989-02-17"]
  #Station 44 (starts 3/31/89) - use 01 to gapfill
  sub.df[,"44"][is.na(sub.df[,"44"]) & sub.df$Date >= "1989-03-31"] <- 
    sub.df[,"1"][is.na(sub.df[,"44"]) & sub.df$Date >= "1989-03-31"]
  #Station 45 (starts 3/31/89) - use 43 to gapfill
  sub.df[,"45"][is.na(sub.df[,"45"]) & sub.df$Date >= "1989-03-31"] <- 
    sub.df[,"43"][is.na(sub.df[,"45"]) & sub.df$Date >= "1989-03-31"]
  #Station 48 (starts 10/8/98) - use 42 to gapfill
  sub.df[,"48"][is.na(sub.df[,"48"]) & sub.df$Date >= "1998-10-08"] <- 
    sub.df[,"42"][is.na(sub.df[,"48"]) & sub.df$Date >= "1998-10-08"]
  #Station 49 (starts 1/1/99) - use 40 to gapfill
  sub.df[,"49"][is.na(sub.df[,"49"]) & sub.df$Date >= "1999-01-01"] <- 
    sub.df[,"40"][is.na(sub.df[,"49"]) & sub.df$Date >= "1999-01-01"]
  #Station 50 (starts 1/26/02) - use 40 to gapfill
  sub.df[,"50"][is.na(sub.df[,"50"]) & sub.df$Date >= "2002-01-26"] <- 
    sub.df[,"40"][is.na(sub.df[,"50"]) & sub.df$Date >= "2002-01-26"]
  filled.df <- gather(sub.df, key = Sta, value = vector.to.fill, -Date)
  colnames(filled.df)[3] <- "filler"
  input.df <- merge(input.df, filled.df, all = T)
  input.df[,vector.to.fill][is.na(input.df[,vector.to.fill])] <- 
    input.df$filler[is.na(input.df[,vector.to.fill])]
  input.df$filler <- NULL
  return(input.df)
}

# Fill data with sister station data, when it exists
filled.data <- data.m
filled.data <- create.filled.df(input.df = filled.data, 
                                vector.to.fill = "Avg_Temp")
filled.data <- create.filled.df(input.df = filled.data, 
                                vector.to.fill = "Max_Temp")
filled.data <- create.filled.df(input.df = filled.data, 
                                vector.to.fill = "Min_Temp")
filled.data <- create.filled.df(input.df = filled.data, 
                                vector.to.fill = "Avg_RH")
filled.data <- create.filled.df(input.df = filled.data, 
                                vector.to.fill = "Precip")
filled.data <- create.filled.df(input.df = filled.data, 
                                vector.to.fill = "Avg_VP")
filled.data <- create.filled.df(input.df = filled.data, 
                                vector.to.fill = "Avg_Wind_SP")
filled.data <- create.filled.df(input.df = filled.data, 
                                vector.to.fill = "Avg_Resultant_WindSP")
filled.data <- create.filled.df(input.df = filled.data, 
                                vector.to.fill = "Avg_Wind_Dir")
filled.data <- create.filled.df(input.df = filled.data, 
                                vector.to.fill = "Solar_Rad")
filled.data <- create.filled.df(input.df = filled.data, 
                                vector.to.fill = "Avg_Soil_Temp")
filled.data <- create.filled.df(input.df = filled.data, 
                                vector.to.fill = "Max_Soil_Temp")
filled.data <- create.filled.df(input.df = filled.data, 
                                vector.to.fill = "Min_Soil_Temp")

# For remaining NA's just fill with linear na.approx
# This isn't ideal, but will give us ballpark numbers to use
filled.data.two <- as.data.frame(data.table(filled.data)[, list(
  Date = Date,
  Month = Month,
  Day = Day,
  Year = Year,
  Avg_Temp = na.approx(Avg_Temp, x = Date, na.rm = F, maxgap = 14),
  Max_Temp = na.approx(Max_Temp, x = Date, na.rm = F, maxgap = 14),
  Min_Temp = na.approx(Min_Temp, x = Date, na.rm = F, maxgap = 14),
  Avg_RH = na.approx(Avg_RH, x = Date, na.rm = F, maxgap = 14), 
  Precip = Precip,
  Avg_VP = na.approx(Avg_VP, x = Date, na.rm = F, maxgap = 14), 
  Avg_Wind_SP = na.approx(Avg_Wind_SP, x = Date, na.rm = F, maxgap = 14), 
  Avg_Resultant_WindSP = na.approx(Avg_Resultant_WindSP, x = Date, na.rm = F, maxgap = 14), 
  Avg_Wind_Dir = na.approx(Avg_Wind_Dir, x = Date, na.rm = F, maxgap = 14), 
  Solar_Rad = na.approx(Solar_Rad, x = Date, na.rm = F, maxgap = 14),
  Avg_Soil_Temp = na.approx(Avg_Soil_Temp, x = Date, na.rm = F, maxgap = 14), 
  Max_Soil_Temp = na.approx(Max_Soil_Temp, x = Date, na.rm = F, maxgap = 14), 
  Min_Soil_Temp = na.approx(Min_Soil_Temp, x = Date, na.rm = F, maxgap = 14)),
  by = list(Sta)])

ggplot(filled.data.two[year(filled.data.two$Date) == 2007 & filled.data.two$Sta == 49,], aes(x = Date, y = Max_Temp, group = Sta, colour = as.factor(Sta))) + 
  geom_line(colour = "red") +
  geom_line(data = data.m[data.m$Year == 2007 & data.m$Sta == 49,], colour = "black")


# Gap-filling algorithm 

# Keep track of error rates ???
#errors <- data.frame(Variable = names(gap.filled)[6:18],
#                     Error = rep(0,length(6:18)))
# Start loop
#for (j in 6:18) {
#  k<-0
#for (i in 1:dim(gap.filled[1])) {
#  if (newdata40[i,j]>(-400)) next #don't do anything to good data
#  else {
#    k <- k+1
#    if (newdata40$Jul_Day[i]%in%data49$Jul_Day) {
#      fillIndex<-which(data49$Jul_Day==newdata40$Jul_Day[i])#finds matching row in data49
#      if(data49[fillIndex,j]>(-400)){
#        newdata40[i,j]<-data49[fillIndex,j]
#      }
#      else newdata40[i,j]<-NA
#    }
#    else newdata40[i,j]<-NA
#  }
#}
#errors40$Error[j-5]<-k
#}
#errors40#error rates of instruments vary


#calculate VPD per day
# VPD is the saturated vapour pressure minus the actual vapour pressure 
# (SVP - VPactual)
# VPactual = (RH*SVP)/100
# http://cronklab.wikidot.com/calculation-of-vapour-pressure-deficit
# 1) get SVP = (VPactual*100)/RH
# 2) get VPD = SVP - VPactual

filled.data.two$VPD <- (filled.data.two$Avg_VP * 100 / filled.data.two$Avg_RH) - filled.data.two$Avg_VP
filled.data.two$VPD[is.infinite(filled.data.two$VPD)] <- NA
# if VPD is less than zero, set it to zero
filled.data.two$VPD[!is.na(filled.data.two$VPD) & filled.data.two$VPD < 0] <- 0

# Fill date info back in
filled.data.two$Month <- month(filled.data.two$Date)
filled.data.two$Day <- mday(filled.data.two$Date)
filled.data.two$Year <- year(filled.data.two$Date)

# Some stations started with non-working rain gages. Set early values to NA
# Station 44 has first precip event on July 16, 1989. Other stations show precip on the 14th. Delete 14th and before
# Station 50 has first precip event on June 14, 2002. Other stations show precip on the June 4. Delete June 4 and before
#filled.data.two$Precip[filled.data.two$Sta == 44 & filled.data.two$Date < as.Date("1989-07-15")] <- NA
#filled.data.two$Precip[filled.data.two$Sta == 50 & filled.data.two$Date < as.Date("2002-06-05")] <- NA



# Export new data as csv
write.csv(filled.data.two[,c("Date", "Sta", "Month", "Day", "Year", "Avg_Temp", "Max_Temp", "Min_Temp", "Avg_RH", "Precip", "Avg_VP", "Avg_Wind_SP", "Avg_Resultant_WindSP", "Avg_Wind_Dir", "Solar_Rad", "Avg_Soil_Temp", "Max_Soil_Temp", "Min_Soil_Temp", "VPD")],
          "~/Desktop/AJ_SevMet_gapfill14Dec18.csv",
          row.names = F)


