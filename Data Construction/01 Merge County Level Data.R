library(data.table)
library(maps)
library(arepa)

## unique site identifier = Monitor (FIPS-SiteID)
## unique monitor identifier = (Monitor, POC)

## ----------------------------------------------------- ##
##   Load Linked Monitor Level Data                      ##
## ----------------------------------------------------- ##
load('PM_merged.Rda')
dat = PM_merged
length(unique(dat$Monitor)) #1625 Monitors

dat$FIPS = substr(dat$Monitor, 1, 5)

## ----------------------------------------------------- ##
##   Add Attainment/Nonattainment Data                  ##
## ----------------------------------------------------- ##
attaindat=read.csv('Other Data Files/EPA Nonattainment Table.csv')
attaindat$FIPS=paste(formatC(attaindat$fips_state, width=2, flag="0"), formatC(attaindat$fips_cnty, width=3, flag="0"), sep="")
attaindat_ozone = subset(attaindat, pollutant =='8-Hr Ozone 1997')
attaindat_so2 = subset(attaindat, pollutant == 'SO2')
attaindat_pm10 = subset(attaindat, pollutant=='PM-10')
attaindat_pm25_2006 = subset(attaindat, pollutant=='PM-2.5 2006')
attaindat = subset(attaindat, pollutant=='PM-2.5 1997')

apply(           attaindat[, paste("pw_", 1997:2012, sep = "")], 2, function(x) sum(x %in% c("P", "W")))
apply(     attaindat_ozone[, paste("pw_", 1997:2012, sep = "")], 2, function(x) sum(x %in% c("P", "W")))
apply(       attaindat_so2[, paste("pw_", 1997:2012, sep = "")], 2, function(x) sum(x %in% c("P", "W")))
apply(      attaindat_pm10[, paste("pw_", 1997:2012, sep = "")], 2, function(x) sum(x %in% c("P", "W")))
apply( attaindat_pm25_2006[, paste("pw_", 1997:2012, sep = "")], 2, function(x) sum(x %in% c("P", "W")))

### DEFINE CROSS-SECTIONAL NONATTAINENT STATUS
##   0= attainment, 1=nonattainment if ever designated for PM10
attaindat$a = ifelse(attaindat$pw_2005 %in% c("P", "W"), 1, 0) #0= attainment, 1=nonattainment if designated in 2005

###   Define other nonattainment designations as covariates   #####
## ozone
attaindat_ozone$a_ozone = ifelse(attaindat_ozone$pw_2005 %in% c("P", "W"), 1, 0)
attaindat_ozone = attaindat_ozone[, (names(attaindat_ozone) %in% c("FIPS", "a_ozone"))]

## SO2
attaindat_so2$a_so2 = ifelse(attaindat_so2$pw_2005 %in% c("P", "W"), 1, 0)
attaindat_so2 = attaindat_so2[, (names(attaindat_so2) %in% c("FIPS", "a_so2"))]

## TSP
# attaindat_tsp$a_tsp = ifelse(attaindat_tsp$pw_1990 %in% c("P", "W"), 1, 0)
# attaindat_tsp = attaindat_tsp[, (names(attaindat_tsp) %in% c("FIPS", "a_tsp"))]

## PM10
attaindat_pm10$a_pm10 = ifelse(attaindat_pm10$pw_2005 %in% c("P", "W"), 1, 0)
attaindat_pm10 = attaindat_pm10[, (names(attaindat_pm10) %in% c("FIPS", "a_pm10"))]


## Merge all attaindat
attaindat = merge(attaindat, attaindat_pm10, by = "FIPS", all.x=TRUE, all.y=TRUE)
# attaindat = merge(attaindat, attaindat_tsp, by = "FIPS", all.x=TRUE, all.y=TRUE)
attaindat = merge(attaindat, attaindat_ozone, by = "FIPS", all.x=TRUE, all.y=TRUE)
attaindat = merge(attaindat, attaindat_so2, by = "FIPS", all.x=TRUE, all.y=TRUE)


## ------------------------------------------------------------------------------------ ##
##   Load County Level Smoking Rates from Small Area Estimation/BRFSS                   ##
## ------------------------------------------------------------------------------------ ##
smoking = read.csv('Other Data Files/smokedatwithfips.csv')
smoking$FIPS = as.character(formatC(smoking$FIPS, format="d", width=5, flag="0"))
## duplicate FIPS = 08013
deleteid = which(smoking$FIPS=="08013")[[1]]
smoking = smoking[-deleteid, ]
smoking = smoking[, names(smoking) %in% c("FIPS", "smokerate2000")]



########################################
###            Merges                ###
########################################
d1 = merge(dat, attaindat, by="FIPS", all.x=TRUE)
d1$a[is.na(d1$a)] = 0
d1$a_ozone[is.na(d1$a_ozone)] = 0
d1$a_pm10[is.na(d1$a_pm10)] = 0
d1$a_so2[is.na(d1$a_so2)] = 0

d2 = merge(d1, smoking, by="FIPS", all.x=TRUE, )

########################################
###            Output                ###
########################################
length(unique(d2$Monitor)) # 1625 Monitors
alldat = d2
save(alldat, file ='alldata.RData')






