rm(list=ls())
library(data.table)
library(HEIfunctions)
library(pscl)
set.seed(51)

load('datforimpute_2000census.RData') # loads an object called datforimpute
dim(datforimpute)
dat = datforimpute


dat$pmbase = dat$pmbase2002_2004

obsdat 	<- dat[which(is.na(dat$pmbase)==F),]
misdat 	<- dat[which(is.na(dat$pmbase)==T),]

varprior 	<- c(.15,1)
samp <- sqrt(rigamma(10000,varprior[1], varprior[2]))

obscoords 	<- cbind(obsdat$Longitude, obsdat$Latitude)
miscoords 	<- cbind(misdat$Longitude, misdat$Latitude)
phiest 	<- makephi(obscoords, scale=10) #1.12531
phiprior 	<- c(makephi(obscoords, scale=3), makephi(obscoords, scale=50)) #(.3376, 5.6265)


starting 	<- list("phi"=phiest,"sigma.sq"=10, "tau.sq"=10)
tuning 	<- list("phi"=0.01, "sigma.sq"=0.1, "tau.sq"=0.1)
priors 	<- list("phi.Unif"=phiprior, "sigma.sq.IG"=varprior,"tau.sq.IG"=varprior)

nsamp		<- 50000
nburn		<- 10000

## zipcode level census
whichvars = c("PctUrban", "PctBlack", "PctHisp", "PctHighSchool", "MedianHHInc", "PctPoor", "PctFemale",
              "PctOccupied", "PctMovedIn5", "MedianHValue", "logpop",
              "a", "a_pm10", "a_ozone", "a_so2",
              "smokerate2000",
              "avgdewpt", "avgtemp", "avgrelhum")


fullmod = lm(pmbase~., data = dat[, names(dat) %in% c(whichvars, "pmbase"), with = FALSE])

formula = as.formula(paste("pmbase ~", paste(names(fullmod$coef[-1]), collapse = "+")))

summary(lm(formula, data=obsdat))

outmodel <- spatpred(formula, data=obsdat, data.p = misdat,  
                     coords = obscoords, coords.p = miscoords, nburn=nburn,
                     starting=starting, tuning=tuning, priors = priors,
                     cov.model="exponential", n.samples = nsamp,
                     verbose = T)

save(outmodel, file="201512010pm2002_2004preds.Rda")

