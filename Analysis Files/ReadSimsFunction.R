
#### Function to Read in posterior simulations from the model and output posterior simulations of causal quantites
getsims = function(outtrans, healthoutname, healthout, denomname, Cede, Ceae, resultsname, fixsamples = NULL, temp = FALSE){
  # Load Results #
  load(resultsname)
  #### If using temporary output, this object is called 'out', have to rename.
  if (temp == TRUE)
    pstratamod = out
   
  nsampcount = dim(pstratamod$samples)[[1]]
  nsamp = fixsamples
  if (is.null(fixsamples))
    nsamp = nsampcount
  
  if (outtrans == "log"){
    y = log(dat$pmfu)
    y0out = exp(pstratamod$y0[seq(nburn,nsamp,thin),])
    y1out = exp(pstratamod$y1[seq(nburn,nsamp,thin),])
  }
  if (outtrans == "change"){
    y = dat$pmfu - dat$pmbase2002_2004
    y0out = pstratamod$y0[seq(nburn,nsamp,thin),]
    y1out = pstratamod$y1[seq(nburn,nsamp,thin),]
  }
  if (outtrans == "95"){
    y = log(dat$pmfu95)
    y0out = exp(pstratamod$y0[seq(nburn,nsamp,thin),])
    y1out = exp(pstratamod$y1[seq(nburn,nsamp,thin),])
  }
  
  if (outtrans == "avtmpf.2012"){
    y = dat$avtmpf.2012
    y0out = (pstratamod$y0[seq(nburn,nsamp,thin),])
    y1out = (pstratamod$y1[seq(nburn,nsamp,thin),])
  }
  
  h = as.matrix(dat[, healthoutname, with = FALSE])
  denom = as.matrix(dat[, denomname, with = FALSE])
  
  h0out=t(apply(pstratamod$h0[seq(nburn, nsamp, thin),], 1, getrate, denom)) 
  h1out=t(apply(pstratamod$h1[seq(nburn, nsamp ,thin),], 1, getrate, denom)) 
  
  
  # Calculate ATTs #
  ceymat = y1out-y0out
  atty = rowMeans(ceymat[, dat$a==1])
  
  cehmat = h1out-h0out
  atth = rowMeans(cehmat[, dat$a==1])
  
  
  ede = rep(NA, dim(ceymat)[[1]])
  eae=ede
  nede=ede
  neae=ede
  
  for (i in 1:length(ede)){
    ede[i] = mean(cehmat[i,dat$a==1][abs(ceymat[i,dat$a==1]) <= Cede])
    nede[i] = sum( abs(ceymat[i, dat$a==1]) <= Cede )
    eae[i] = mean(cehmat[i,dat$a==1][ceymat[i,dat$a==1] < Ceae])
    neae[i] = sum( ceymat[i, dat$a==1] < Ceae )
  }  
  
  retlist = list(atty, atth, ede, nede, eae, neae, ceymat, nsampcount)
  names(retlist) = c("atty", "atth", "ede", "nede", "eae", "neae", "ceymat", "nsamp")
  return(retlist)
}