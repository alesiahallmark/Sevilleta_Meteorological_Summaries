# Sevilleta_Meteorological_Summaries
The repository contains code that gapfills and summarizes meteorological data from Sevilleta LTER met stations

### Metadata for columns produced in Sevmetsummary_AJ
Throughout the file, "NA" values represent missing data. Missing data might result from missing or malfunctioning equipment, incomplete data, or questionable data.

**Sta** is a categorical column that includes a unique code for each Sevilleta meteorlogical station. Possible values: 1, 40, 41, 42, 43, 44, 45, 48, 49, 50

**Station.Name** is a categorical column that includes a unique name for each Sevilleta meteorlogical station. Possible values: Headquarters, Deep Well, Southern Gate, Cerro Montosa, Red Tank, Rio Salado, Bronco Well, Goat Draw, Five Points, Blue Grama

**lat** is a numeric column that includes the decimal degree coordinate of latitude for each Sevilleta meteorlogical station. Values range from 34.2186 to 34.4145

**long** is a numeric column that includes the decimal degree coordinate of longitude for each Sevilleta meteorlogical station. Values range from -107.0376 to -106.5229

**elevation** is a numeric column that includes the elevation in meters of each Sevilleta meteorlogical station. Values range from 1440 to 1944

**water.year** is a numeric column that includes the water year during which meteorological data were summarized. Here, a water year is defined as the time from October 1st of the previous calendar year to September 30th of the current calendar year. So the water year 2000 ranges from 01-Oct-1999 to 30-Sept-2000. Values range from 1989 to 2018

**precip.annual** is a numeric column that includes the summed total precipitation in mm for that water year at a particular Sevilleta meteorlogical station. Values range from 22.8 to 622.8

**precip.ONDJ** is a numeric column that includes the summed total precipitation in mm for October-January (winter months) of that water year at a particular Sevilleta meteorlogical station. Values range from 0.0 to 158.7

**precip.FMAM** is a numeric column that includes the summed total precipitation in mm for February-May (spring months) of that water year at a particular Sevilleta meteorlogical station. Values range from 0.0 to 137.7

**precip.JJAS** is a numeric column that includes the summed total precipitation in mm for June-September (summer-monsoon/fall months) of that water year at a particular Sevilleta meteorlogical station. Values range from 13.2 to 418.4

**precip.intra_CV** is a numeric column that includes the intra-water year (within water year) coefficient of variation in daily precipitation (unitless) for that water year at a particular Sevilleta meteorlogical station. Values range from 2.338856 to 8.691578

**GDD.FMAM** is a numeric column that includes the accumulated growing degree days (GDD) in degrees Centigrade for February-May (spring months) of that water year at a particular Sevilleta meteorlogical station. GDD is calculated as the sum of all daily mean temperatures during a time period, for those days that have a daily mean temperature above freezing (0 degrees Centigrade). Values range from 1012.765 to 1811.096

**GDD.AMJJAS** is a numeric column that includes the accumulated growing degree days (GDD) in degrees Celcius for April-September (late spring-monsoon/fall months) of that water year at a particular Sevilleta meteorlogical station. GDD is calculated as the sum of all daily mean temperatures during a time period, for those days that have a daily mean temperature above freezing (0 degrees Celcius). Values range from 3379.350 to 4370.706

**soil_T** is a numeric column that includes the mean soil temperature in degrees Celcius for that water year at a particular Sevilleta meteorlogical station. [[Question for Kris or Jenn: what is the depth of this probe?]] Values range from 14.74768 to 19.48005

**soil_T.MJJAS** is a numeric column that includes the mean soil temperature in degrees Celcius for May-September (summer-monsoon/fall months) of that water year at a particular Sevilleta meteorlogical station. [[Question for Kris or Jenn: what is the depth of this probe?]] Values range from 23.84212 to 29.67190

**air_T** is a numeric column that includes the mean air temperature in degrees Celcius for that water year at a particular Sevilleta meteorlogical station. Values range from 11.89016 to 16.82509

**air_T.MJJAS** is a numeric column that includes the mean air temperature in degrees Celcius for May-September (summer-monsoon/fall months) of that water year at a particular Sevilleta meteorlogical station. Values range from 19.70327 to 25.20915

**meanmax.air_T.JJA** is a numeric column that includes the mean daily maximum air temperature in degrees Celcius for June-August (hottest three summer months) of that water year at a particular Sevilleta meteorlogical station. Values range from 28.60435 to 35.92391

**meanmin.air_T.DJF** is a numeric column that includes the mean daily minimum air temperature in degrees Celcius for December-February (coldest three winter months) of that water year at a particular Sevilleta meteorlogical station. Values range from -9.9533333 to 0.5453889

**CDD** is a numeric column that includes the mean consecutive dry days (CDD) in days for that water year at a particular Sevilleta meteorlogical station. CDD is the number of days in a row receiving 0 mm of rainfall. Values range from 6.684211 to 19.875000

**extreme_CDD** [[Note for Kris and Jenn: I don't know if this column is behaving properly]] is a numeric column that includes the number of extreme consecutive dry days (CDD) for that water year at a particular Sevilleta meteorlogical station. Extreme consecutive dry days are the longest (95% percentile and higher) time periods with no rainfall across the Sevilleta. The current threshhold is 36 consecutive days. Values range from 0 to 8

**VPD.MJJ** is a numeric column that includes the mean vapor pressure deficit in MPa for May-July (summer months) of that water year at a particular Sevilleta meteorlogical station. Values range from 1.242553 to 2.466548

**VPD.ASO** is a numeric column that includes the mean vapor pressure deficit in MPa for August-October (monsoon/fall months) of that water year at a particular Sevilleta meteorlogical station. Values range from 0.622854 to 1.981875

**SPEI_12.May** is a numeric column that includes the Standardized Precipitation Evapotranspiration Index (SPEI) for a 12-month inegration window ending on May 31st of that water year, standardized to values from that particular Sevilleta meteorlogical station. SPEI is a drought index which compares the actual precipitation with estimated Potential Evapotranspiration (calculated using temperature and latitude - Thornthwaite method) at monthly timesteps within an integration window for a given site. Lower values indicate that integration period was drier than other integration periods at that site. Values range from -2.045800 to 2.217524

**SPEI_6.May** is a numeric column that includes the Standardized Precipitation Evapotranspiration Index (SPEI) for a 6-month inegration window ending on May 31st of that water year, standardized to values from that particular Sevilleta meteorlogical station. SPEI is a drought index which compares the actual precipitation with estimated Potential Evapotranspiration (calculated using temperature and latitude - Thornthwaite method) at monthly timesteps within an integration window for a given site. Lower values indicate that integration period was drier than other integration periods at that site. Values range from -1.891259 to 2.293227

**SPEI_12.Sept** is a numeric column that includes the Standardized Precipitation Evapotranspiration Index (SPEI) for a 12-month inegration window ending on September 30th of that water year, standardized to values from that particular Sevilleta meteorlogical station. SPEI is a drought index which compares the actual precipitation with estimated Potential Evapotranspiration (calculated using temperature and latitude - Thornthwaite method) at monthly timesteps within an integration window for a given site. Lower values indicate that integration period was drier than other integration periods at that site. Values range from -2.062215 to 2.542083

**SPEI_6.Sept** is a numeric column that includes the Standardized Precipitation Evapotranspiration Index (SPEI) for a 6-month inegration window ending on September 30th of that water year, standardized to values from that particular Sevilleta meteorlogical station. SPEI is a drought index which compares the actual precipitation with estimated Potential Evapotranspiration (calculated using temperature and latitude - Thornthwaite method) at monthly timesteps within an integration window for a given site. Lower values indicate that integration period was drier than other integration periods at that site. Values range from -2.045800 to 2.217524

**SPEI_12.May.core** is a numeric column that includes the Standardized Precipitation Evapotranspiration Index (SPEI) for a 12-month inegration window ending on May 31st of that water year, standardized to values from the four core Sevilleta meteorlogical stations. SPEI is a drought index which compares the actual precipitation with estimated Potential Evapotranspiration (calculated using temperature and latitude - Thornthwaite method) at monthly timesteps within an integration window for a given site. Lower values indicate that integration period was drier than other integration periods across the core sites. Values range from -2.133664 to 1.655874

**SPEI_6.May.core** is a numeric column that includes the Standardized Precipitation Evapotranspiration Index (SPEI) for a 6-month inegration window ending on May 31st of that water year, standardized to values from the four core Sevilleta meteorlogical stations. SPEI is a drought index which compares the actual precipitation with estimated Potential Evapotranspiration (calculated using temperature and latitude - Thornthwaite method) at monthly timesteps within an integration window for a given site. Lower values indicate that integration period was drier than other integration periods across the core sites. Values range from -2.151800 to 1.570551

**SPEI_12.Sept.core** is a numeric column that includes the Standardized Precipitation Evapotranspiration Index (SPEI) for a 12-month inegration window ending on September 30th of that water year, standardized to values from the four core Sevilleta meteorlogical stations. SPEI is a drought index which compares the actual precipitation with estimated Potential Evapotranspiration (calculated using temperature and latitude - Thornthwaite method) at monthly timesteps within an integration window for a given site. Lower values indicate that integration period was drier than other integration periods across the core sites. Values range from -2.069271 to 2.064628

**SPEI_6.Sept.core** is a numeric column that includes the Standardized Precipitation Evapotranspiration Index (SPEI) for a 6-month inegration window ending on September 30th of that water year, standardized to values from the four core Sevilleta meteorlogical stations. SPEI is a drought index which compares the actual precipitation with estimated Potential Evapotranspiration (calculated using temperature and latitude - Thornthwaite method) at monthly timesteps within an integration window for a given site. Lower values indicate that integration period was drier than other integration periods across the core sites. Values range from -2.368332 to 1.681672

**ev_size** is a numeric column that includes the mean cumulative daily precipitation size in mm for that water year at a particular Sevilleta meteorlogical station. Values range from 2.036842 to 11.454142

**extreme_size** is a numeric column that includes the number of cumulative daily precipitation eventsthat exceed an extreme threshold (17.2 mm in this case) for that water year at a particular Sevilleta meteorlogical station. Values range from 1 to 5

**events** is a numeric column that includes the number of cumulative daily precipitation events that exceed a minimum threshold (0.3 mm in this case) for that water year at a particular Sevilleta meteorlogical station. Values range from 6 to 88
