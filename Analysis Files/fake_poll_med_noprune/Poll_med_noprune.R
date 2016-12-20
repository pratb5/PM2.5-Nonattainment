set.seed(51)

load(file="~/PM2.5/pm_withps.Rda")
#-- Instead of pruning observations, re-define PS strata to include all
dat$outofrange = 0
cuts = c(0.25, 0.5, 0.75) # manual - use this for sensitivity analysis without pruning
dat$pscat[dat$outofrange==0] = as.numeric(cut(dat$ps[dat$outofrange==0], breaks=c(0,cuts,1), include.lowest=TRUE))

dat$pscat = as.factor(dat$pscat)

library(splines)
library(HEIfunctions)
library(xtable)
library(coda)
library(corpcor)
library(data.table)

######## Set the Working Directory so that output is saved in the right place ######## 
wd <- "~/PM2.5/fake_poll_med_noprune/"
setwd(wd)


#### Choose the file name for the output object #######
outfile = "pollutionmodel.RData"


######## Choose how to transform pollution ######## 
dat$y = log(dat$pmfu)    #dat$fu - dat$base


### Medium Models
load('~/PM2.5/vars_with_imbalance.RData')
vars_with_imbalance
toadjust = vars_with_imbalance[!(vars_with_imbalance %in% c("pmbase", "Tot_den_for_death_MA_FFS.2012"))]
medformula = as.formula(paste("y ~", paste(c("pscat", "pmbase2002_2004", toadjust), collapse = "+")))
my = glm(medformula, data = dat)
formula = my$formula

coords 	<- cbind(dat$Longitude, dat$Latitude)

phistart 	<- makephi(coords, 10)
philower 	<- makephi(coords, 4)
phiupper 	<- makephi(coords, 30)

tuning 	<- list(A = 0.1, psi = 0.2, theta = 1)
prior 		<- list(KIG = rep(.5,2), psi = rep(.5,2), theta1 = rep(philower, 2), theta2 = rep(phiupper,2))
starting 	<- list(B=NULL, A = NULL, psi = NULL, theta = phistart)

starttime=proc.time()
mod 		<- mvpsmod(formula, dat, "a", coords, nsamp=51, thin=1,
			tuning=tuning, prior=prior, starting=starting, outputbinsize = 10, outputfilename = "pollutionmodel_temp.Rda")
rtime=proc.time() - starttime

save(mod, file = outfile)

rtime



