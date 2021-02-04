### Started 6 Oct 2020 by Cat
## Source function to build data for the shiny app
### Need to eventually integrate hypothesis tests and provenance vs urban!

# housekeeping
if(FALSE){
  question = TRUE ## if TRUE then testing urban effect, if FALSE then testing provenance
  txeff <- -20   ### effect of urban or provenance
  txeff.sd <- 5
  methodeff <- -20  ### effect of weather station
  methodeff.sd <- 5
  txmethod <- -20  ### interaction
  txmethod.sd <- 5
}


simfunc <- function(question, txeff, txeff.sd, methodeff, methodeff.sd, txmethod, txmethod.sd){
  
  use.urban = question  ### This is either TRUE or FALSE
  
  # Step 1: Set up years, days per year, temperatures, sampling frequency, required GDD (fstar)
  daysperyr <- 300 #### just to make sure we don't get any NAs
  nspps <- 15 
  ninds <- 10 
  nobs <- nspps*ninds
  nsites <- 2  ### Arboretum versus the Forest
  nmicros <- 10  ### Number microsites per site so 20 total 
  nmethods <- 2
  ntot <- nobs * nmethods * nsites
  
  ### These are our fstar thresholds
  fstar <- 300 ### mu_a_sp in model output
  fstarspeciessd <- 50 ### sigma_a_sp in model output
  
  ## Sigma_y to be added at the end
  sigma_y <- 2
  
  ### Started 25 May 2020 by Cat
  # Updated 2 Feb 2021 by Cat
  ## Source file to build fake data simulations for urban versus provenance lat effects
  ## TAKE II: randomizing day of budburst and then calculate GDD from there
  
  if(use.urban==TRUE){
    
    urbeffect <- txeff 
    urbsd <- txeff.sd 
    methodeffect <- methodeff 
    methodsd <- methodeff.sd 
    urbmethod <- txmethod
    urbmethodsd <- txmethod.sd
    
    #### Next I set up an fstar or a GDD threshold for each individual
    spind <- paste(rep(1:nspps, each=ninds), rep(1:ninds, nspps), sep="_")
    
    fstarspp <- round(rnorm(nspps, fstar, fstarspeciessd), digits=0)
    df.fstar <- as.data.frame(cbind(species=rep(1:nspps, each=ninds*nsites*nmethods), ind=rep(1:ninds, nmethods), 
                                    fstarspp=rep(fstarspp, each=ninds*nsites*nmethods),
                                    site=rep(c("arb", "hf"), each=ninds*nmethods),
                                    method=rep(rep(c("ws", "hobo"), each=ninds), nsites*nspps)))
    
    
    df.fstar$fstarspp <- as.numeric(df.fstar$fstarspp)
    df.fstar$sp_ind <- paste(df.fstar$species, df.fstar$ind, sep="_")
    
    ##### We should get these back in parameter estimates
    df.fstar$urbtx <- ifelse(df.fstar$site=="arb", 1, 0)
    df.fstar$gdd.noise  <- df.fstar$urbtx * rep(rnorm(n=nspps, mean=urbeffect, sd=urbsd), each=ninds*nmethods)  
    
    df.fstar$tx <- ifelse(df.fstar$method=="ws", 1, 0)
    df.fstar$gdd.noise <- df.fstar$gdd.noise + df.fstar$tx * rep(rnorm(n=nspps, mean=methodeffect, sd=methodsd), each=ninds*nmethods)
    
    df.fstar$urbmethodtx <- ifelse(df.fstar$method=="ws" & df.fstar$site=="arb", 1, 0)
    df.fstar$gdd.noise <- df.fstar$gdd.noise + df.fstar$urbmethodtx * rep(rnorm(n=nspps, mean=urbmethod, sd=urbmethodsd), each=ninds*nmethods)  
    
    df.fstar$gdd <- df.fstar$fstarspp + df.fstar$gdd.noise + rnorm(n=ntot, mean=0, sd=sigma_y)
    df.fstar$species <- as.numeric(df.fstar$species)
    
    ##### Now add in provenance so better able to compare to other simulations
    spind <- paste(rep(c(1:nspps), each=ninds), rep(1:ninds, nspps), sep="_")
    provenance.hf <- 42.5
    provenance.arb <- round(rnorm(nobs, provenance.hf, 5), digits=2)
    
    df.prov <- as.data.frame(cbind(sp_ind = rep(rep(spind, nsites),each=nmethods), 
                                   site = rep(c("arb", "hf"), each=nobs*nmethods),
                                   provenance = c(rep(provenance.arb, each=nmethods), rep(provenance.hf, ninds*nspps*nmethods)),
                                   method = rep(c("ws", "hobo"), nsites*nobs)))
    df.prov$species <- as.numeric(gsub("\\_.*" , "", df.prov$sp_ind))
    df.prov$ind <- gsub(".*_" , "", df.prov$sp_ind)
    df.prov$sp_ind <- NULL
    
    df.bb <- full_join(df.fstar, df.prov)
    
    ##### Clean up the dataframe to prepare for analyses
    df.bb <- subset(df.bb, select=c("site", "method", "species", "ind", "gdd.noise", "fstarspp", "gdd", "provenance")) # 
    
    bball <- df.bb[!duplicated(df.bb),]
    
    
    ##### Now let's do some checks...
    bball$gdd_accuracy <- bball$gdd - bball$fstarspp
    bball$type <- ifelse(bball$method=="ws", 1, 0)
    
    bball <- na.omit(bball)
    
  }
  
  if(use.urban==FALSE){ ### This is the provenance model
    
    proveffect <- txeff 
    provsd <- txeff.sd 
    methodeffect <- methodeff 
    methodsd <- methodeff.sd 
    provmethod <- txmethod
    provmethodsd <- txmethod.sd
    
    fstarspp <- round(rnorm(nspps, fstar, fstarspeciessd), digits=0)
    df.fstar <- as.data.frame(cbind(species=rep(1:nspps, each=ninds*nsites*nmethods), ind=rep(1:ninds, nmethods), 
                                    fstarspp=rep(fstarspp, each=ninds*nsites*nmethods),
                                    site=rep(c("arb", "hf"), each=ninds*nmethods),
                                    method=rep(rep(c("ws", "hobo"), each=ninds), nsites*nspps)))
    
    table(df.fstar$species, df.fstar$site, df.fstar$method) # emw -- checking
    
    df.fstar$fstarspp <- as.numeric(df.fstar$fstarspp)
    #df.fstar$sp_ind <- paste(df.fstar$species, df.fstar$ind, sep="_")
    
    ##### Now add in provenance so better able to compare to other simulations
    spind <- paste(rep(c(1:nspps), each=ninds), rep(1:ninds, nspps), sep="_")
    provenance.hf <- 42.5
    provenance.arb <- round(rnorm(nspps*ninds, provenance.hf, 2), digits=2)
    
    df.prov <- as.data.frame(cbind(sp_ind = rep(rep(spind, nsites),each=nmethods), 
                                   site = rep(c("arb", "hf"), each=nobs*nmethods),
                                   provenance = as.numeric(c(rep(provenance.arb, each=nmethods), rep(provenance.hf, nobs*nmethods))),
                                   method = rep(c("ws", "hobo"), nsites*nobs)))
    df.prov$species <- as.numeric(gsub("\\_.*" , "", df.prov$sp_ind))
    df.prov$ind <- gsub(".*_" , "", df.prov$sp_ind)
    df.prov$sp_ind <- NULL
    df.fstar$species <- as.numeric(df.fstar$species)
    
    df.bb <- full_join(df.fstar, df.prov)
    df.bb$provenance <- as.numeric(df.bb$provenance)
    df.bb$prov.z <- (df.bb$provenance-mean(df.bb$provenance,na.rm=TRUE))/(2*sd(df.bb$provenance,na.rm=TRUE))
    
    
    # Generate random parameters (by species)
    df.bb$gdd.noise <- df.bb$prov.z * rep(rnorm(n=nspps, mean=proveffect, sd=provsd), each=ninds*nsites)
    
    df.bb$gdd <- df.bb$fstarspp + df.bb$gdd.noise + rnorm(n=ntot, mean=0, sd=sigma_y)
    
    df.bb$wstx <- ifelse(df.bb$method=="ws", 1, 0)
    df.bb$gdd.noise <- df.bb$gdd.noise + df.bb$wstx * rep(rnorm(n=nspps, mean=methodeffect, sd=methodsd), each=ninds*nmethods)
    
    df.bb$provmethodtx <- ifelse(df.bb$method=="ws" & df.bb$site=="arb", 1, 0)
    df.bb$gdd.noise <- df.bb$gdd.noise + df.bb$provmethodtx * rep(rnorm(n=nspps, mean=provmethod, sd=provmethodsd), each=ninds*nmethods)  
    
    df.bb$gdd <- df.bb$fstarspp + df.bb$gdd.noise + rnorm(n=ntot, mean=0, sd=sigma_y)
    df.bb$species <- as.numeric(df.bb$species)
    
    ##### Clean up the dataframe to prepare for analyses
    df.bb <- subset(df.bb, select=c("site", "method", "species", "ind", "gdd.noise", "fstarspp", "gdd", "provenance")) # 
    
    bball <- df.bb[!duplicated(df.bb),]
    
    ##### Now let's do some checks...
    bball$gdd_accuracy <- bball$gdd - bball$fstarspp
    bball$type <- ifelse(bball$method=="ws", 1, 0)
    
    bball <- na.omit(bball)
    
    
  }
  
  mylist <- list(bball, df)
  
  
  return(mylist)
  
}

#bblist <- simfunc(question, txeff, txeff.sd, methodeff, methodeff.sd, txmethod, txmethod.sd)

#bball <- bblist[[1]]
#df <- bblist[[2]]
