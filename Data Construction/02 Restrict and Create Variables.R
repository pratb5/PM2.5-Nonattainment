library(maps)
library(data.table)

load('alldata.RData')
## reads in a object called 'alldat'

## For some reason the Arithmetic.Mean variable needs to be converted to numeric
alldat$Arithmetic.Mean = as.numeric(alldat$Arithmetic.Mean)

dat = alldat
length(unique(dat$Monitor)) #1625 Monitors

## Select years
Years = 1997:2013
dat = subset(dat, Year %in% Years)
length(unique(dat$Monitor)) #1625 Monitors

## ----------------------------------------
## ----------  Select only the variables that will be used
## ----------------------------------------

keepvars = c("FIPS", "Year", "Monitor", "Latitude", "Longitude", "Parameter.Name", "Sample.Duration",
             "Metric.Used", "State.Name", "County.Name",  
             "Arithmetic.Mean", "X98th.Percentile", "X95th.Percentile", "X90th.Percentile",                     
             "mean_age", "Female_rate", "White_rate", "Black_rate", "Person_year_FFS", "Total_den_FFS",
             "ALL_CVD", "COPD", "CV_stroke", "HF", "HRD", "IHD", "PVD", "RTI", "Tot_den_for_death_MA_FFS",
             "total_death_FFS_MA", "nzip",    
             "a", "a_pm10", "a_ozone", "a_so2",
             "smokerate2000",
             "avdewf", "avtmpf", "avrelh",
             "TotPop", "PctUrban", "PctInUAs","PctInUCs", "PctRural","PctWhite", "PctBlack",  
             "PctHisp", "PctHighSchool", "MedianHHInc", "PctPoor", "PctFemale", "TotHUs", "PctUrbanHUs", 
             "PctRuralHUs", "PctOccupied", "PctMovedIn5", "MedianHValue", "PopPerSQM")

dat = dat[, names(dat) %in% keepvars, with = FALSE]  


## Reshape
datwide = reshape(dat, direction = "wide", 
      v.names = c("Latitude", "Longitude", "Arithmetic.Mean", "X98th.Percentile", "X95th.Percentile", "X90th.Percentile",                     
                  "mean_age", "Female_rate", "White_rate", "Black_rate", "Person_year_FFS", "Total_den_FFS",
                  "ALL_CVD", "COPD", "CV_stroke", "HF", "HRD", "IHD", "PVD", "RTI", "Tot_den_for_death_MA_FFS",
                  "total_death_FFS_MA", "nzip",  "avdewf", "avtmpf", "avrelh"), 
                  timevar = "Year", idvar = "Monitor")
dim(datwide)

### Need to select a single lat/long for each monitor.  If you just take one year, then latitude and longitude will be missing for any monitor
### that was not in the data for that year (e.g., if you take 1990 then you will miss a monitor that didn't start until 1999, and if you take 1999
### then you will miss a monitor that stopped operating in 1991).

getlast = function(x){
  return(x[max(which(!is.na(x)))])
}

datwide$Longitude = apply(datwide[, paste("Longitude.", Years, sep=""), with = FALSE], 1, getlast)
datwide$Latitude = apply(datwide[, paste("Latitude.", Years, sep=""), with = FALSE], 1, getlast)

datwide = datwide[, !(names(datwide) %in% paste("Longitude.", Years, sep="")), with = FALSE]
datwide = datwide[, !(names(datwide) %in% paste("Latitude.", Years, sep="")), with = FALSE]

#############################################
######     Calculate Variables          #####
#############################################

#Baseline and follow up PM 
makeavgvar=function(d,varprefix, years, whichdata){
tempmat=d[, names(datwide) %in% paste(varprefix,".", years,whichdata, sep=""), with = FALSE ]
return(rowMeans(tempmat, na.rm=TRUE))
}

## Baseline PM - use 2004
datwide$pmbase2004 = datwide$Arithmetic.Mean.2004

## Baseline PM - use 2002 - 2004
datwide$pmbase2002_2004 = makeavgvar(d=datwide, varprefix="Arithmetic.Mean", years=c("2002", "2003", "2004"), "")

## Follow up PM - use average of 2010 - 2012
datwide$pmfu = makeavgvar(d=datwide, varprefix="Arithmetic.Mean", years=c("2010", "2011", "2012"), "")

## Follow up PM - use average 95th %tile from 2010-2012
datwide$pmfu95 = makeavgvar(d=datwide, varprefix="X95th.Percentile", years=c("2010", "2011", "2012"), "")

datwide$housedens = datwide$TotHUs/datwide$TotPop

## Weather variables
## ASOS data only availabel from 1997 - 2014
datwide$avgdewpt = makeavgvar(d=datwide, varprefix = "avdewf", years=2004:2006, "")
datwide$avgtemp = makeavgvar(d=datwide, varprefix = "avtmpf", years=2004:2006, "")
datwide$avgrelhum = makeavgvar(d=datwide, varprefix = "avrelh", years=2004:2008, "")

###############################################
# Make Data Set for Imputing Missing Baseline #
###############################################
## Restrict to Eastern US
whichstates_abb = c("MO", "AR", "IA", "IL", "IN", "MI", "KY", "TN", "MS", "AL", "GA", "SC", "NC", "VA", "WV", "OH", "PA", "DE","DC", "NY", "MA", "CT", "MD", "NH", "VT", "WI", "LA", "MN", "FL", "ME") 
whichstates_fips = state.fips[state.fips$abb %in% whichstates_abb, "fips"]
map("state", region = state.fips[state.fips$abb %in% whichstates_abb, "polyname"])
with(datwide, points(Longitude, Latitude, pch = 16, cex=.5, col = a+1))

east = datwide[as.numeric(substr(datwide$FIPS, 1, 2)) %in% whichstates_fips, ]
dim(east) #946 monitors
map("state", region = state.fips[state.fips$abb %in% whichstates_abb, "polyname"])
with(east, points(Longitude, Latitude, pch = 16, cex=.5, col = a+1))

colMeans(is.na(east[, c("pmbase2004", "pmbase2002_2004", "pmfu", "smokerate2000", "TotPop", "PctBlack", "avgtemp", "Tot_den_for_death_MA_FFS.2012"), with = FALSE]))

## Check observed Medicare data in 2012
dim(subset(east, !is.na(Tot_den_for_death_MA_FFS.2012))) ## Max 909 Monitors with Medicare info

## Remove observations that have (missing baseline AND follow up PM) OR (missing covariates)
## Don't worry about Medicare data here because this data set will be used to impute pollution only (and does not require Medicare data)
check_cov_missing = c("TotPop", "smokerate2000", "avgtemp", "avgrelhum", "avgdewpt")
colMeans(is.na(east[, check_cov_missing, with = FALSE]))
east$covkeep = ( apply(is.na(east[, check_cov_missing, with = FALSE]), 1, sum ) == 0 )

east_restrict = subset(east, (!is.na(pmbase2002_2004) | !is.na(pmfu)) & ( covkeep == TRUE))
colMeans(is.na(east_restrict[, c("pmbase2002_2004", "pmfu", "Tot_den_for_death_MA_FFS.2012"), with = FALSE])) 
dim(east_restrict) # 858 Monitors
dim(subset(east_restrict, !is.na(Tot_den_for_death_MA_FFS.2012))) # 829 monitors with Medicare data

## Exclude these based on strong colinearity/redundancy
censvars_exclude = c("PctInUAs", "PctRural", "PctUrbanHUs", "PctRuralHUs", "TotHUs", "PctInUCs", "housedens", "PopPerSQM","PctWhite") 

# Some transformations
east_restrict$logpop=log(east_restrict$TotPop)
east_restrict$CompletelyRural = (east_restrict$PctUrban==0)

east_restrict = east_restrict[, !(names(east_restrict) %in% censvars_exclude), with = FALSE]

## Use this for imputing baseline data
datforimpute = east_restrict

## zipcode level census
save(datforimpute, file = 'datforimpute_2000census.RData')

