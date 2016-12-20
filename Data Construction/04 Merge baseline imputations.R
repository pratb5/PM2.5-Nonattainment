rm(list=ls())
library(data.table)
library(HEIfunctions)
set.seed(51)

load('datforimpute_2000census.RData')  # loads an object called datforimpute
dim(datforimpute)
dat = datforimpute

dat$baseorig2002_2004 = dat$pmbase2002_2004


#################################################################################
##########    Merge Baseline Predictions for pm10base1990   ##############
#################################################################################
load(file="20151210pm2002_2004preds.Rda")

nsamp    <- 50000
nburn  	<- 10000
obsdat = subset(dat, !is.na(pmbase2002_2004))
misdat = subset(dat, is.na(pmbase2002_2004))

#Grab the predicted mean, sd, and 5 individual samples from the posterior-predictive distribution (for possible sensitivity analysis later)
whichpostpreds = sample(nburn:nsamp, size=5)
imputations = cbind(outmodel$pred[, -1], outmodel$predmodel$p.y.predictive.samples[, whichpostpreds])
impdat = as.data.frame(cbind(as.character(outmodel$pred$Monitor), imputations))
names(impdat) = c("Monitor", "predmean2002_2004", "predsd2002_2004", paste("ppbase2002_2004", 1:length(whichpostpreds), sep=""))

datmerged = merge(dat, impdat, by="Monitor", all.x=TRUE)
with(datmerged, table(is.na(baseorig2002_2004), is.na(predmean2002_2004)))
datmerged$pmbase2002_2004[is.na(datmerged$pmbase2002_2004)] = datmerged$predmean2002_2004[is.na(datmerged$pmbase2002_2004)]

dat = datmerged

save(dat, file = 'pmdat_withbaselineimp.RData')
