############ Make a data set that will be used for the analysis #################
rm(list=ls())
library(data.table)
library(HEIfunctions)
library(maps)
set.seed(51)

## Read in the data file that has the imputed values of baseline PM and temperature
load('pmdat_withbaselineimp.RData')
datorig = dat

with(datorig, table(is.na(Tot_den_for_death_MA_FFS.2012)))
with(datorig, table(Tot_den_for_death_MA_FFS.2012 >= 20))


dat = subset(datorig, Tot_den_for_death_MA_FFS.2012>=20) ## 829 locations

############################################################################
######              Select Variables for Analysis                    #######
############################################################################

medyears = 2004:2012
aqsyears = 1997:2014

fixedcovs = c("Monitor", "FIPS", "State.Name", "TotPop", "PctUrban", "PctBlack", "PctHisp", "PctHighSchool", "MedianHHInc", "PctPoor", "PctFemale",
             "PctOccupied", "PctMovedIn5", "MedianHValue", "a", "a_pm10", "a_ozone", "a_so2", "smokerate2000", "Longitude", "Latitude",
             "pmbase2002_2004", "pmfu", "pmfu95", "avgdewpt", "avgtemp", "avgrelhum", "logpop", "CompletelyRural", 
             "baseorig2002_2004", "predmean2002_2004", "ppbase2002_20041", "ppbase2002_20042", "ppbase2002_20043", "ppbase2002_20044", "ppbase2002_20045")
aqscovs = c("Arithmetic.Mean")
medcovs = c("mean_age", "Female_rate", "White_rate", "Black_rate", "Person_year_FFS", "Total_den_FFS", "ALL_CVD", "COPD", "CV_stroke", "HF", "HRD", "IHD",
            "PVD", "RTI", "Tot_den_for_death_MA_FFS", "total_death_FFS_MA")
             
keepvars = c(fixedcovs, 
             paste(aqscovs, rep(aqsyears, each = length(aqscovs)), sep = "."), 
             paste(medcovs, rep(medyears, each = length(medcovs)), sep = "."))
 

dat = dat[, names(dat) %in% keepvars, with = FALSE]

save(dat, file = 'pmanalysis.RData')
