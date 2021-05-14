### Started 6 Oct 2020 by Cat
## Source function to build data for the shiny app
### Need to eventually integrate hypothesis tests and provenance vs urban!

# Load Libraries
library(dplyr)
library(tidyr)

set.seed(12321)

if(FALSE){
  hypoth <- "urban"  ## hobo, urban, prov
  hypoth.para <- "ws" ## ws, hobo, NA
  hypoth.mu <- -30
  hypoth.sd <- 5   ### This just adds that amount of imprecision to the hypothesis question
  hypoth.para.mu <- -5
  hypoth.para.sd <- 15
  fstar.num <- 300  ## GDD threshold
  fstar.sd <- 20
  #meantemp <- 10
  #meantemp.sd <- 5   
  #micro.sd <- 0
  arbclim <- 5   ### tmean arb climate
  arbclim.sd <- 2
  arbmicroclim <- 10  ### tmean added to arb climate base, if positive then hobos are recording warmer temperatures
  arbmicroclim.sd <- 2
  hfclim <- 10  ## tmean hf climate
  hfclim.sd <- 2
  hfmicroclim <- -5  ### tmean added to hf climate base, if positive then hobos are recording warmer temperatures
  hfmicroclim.sd <- 2
}


gddfunc <- function(hypoth, hypoth.para, hypoth.mu, hypoth.sd, hypoth.para.mu, hypoth.para.sd, fstar.num, fstar.sd, arbclim, arbclim.sd, arbmicroclim, arbmicroclim.sd, hfclim, hfclim.sd, hfmicroclim, hfmicroclim.sd){
  
  # Step 1: Set up years, days per year, temperatures, sampling frequency, required GDD (fstar)
  daysperyr <- 120 #### just to make sure we don't get any NAs
  nspps <- 15 
  ninds <- 50 
  nobs <- nspps*ninds
  nsites <- 2  ### Arboretum versus the Forest
  nmicros <- 15  ### Number microsites per site so 20 total 
  nmethods <- 2
  ntot <- nobs * nmethods * nsites
  
  ### These are our fstar thresholds
  fstar <- fstar.num  ### mu_a_sp in model output
  fstarspeciessd <- fstar.sd ### sigma_a_sp in model output
  
  ## Sigma_y to be added at the end
  sigma_y <- 20
  
  ### Now the climate data 
  dayz <- rep(1:daysperyr, nobs)
  cc.arb <- arbclim
  sigma.arb <- arbclim.sd
  microarb.effect <- arbmicroclim
  microsigmaarb.effect <- arbmicroclim.sd  #### by keeping the sigmas the same for the microsites (line 94 & 99) we assume that the microclimatic effects are the same across sites
  
  cc.hf <- hfclim
  sigma.hf <- hfclim.sd
  microhf.effect <- hfmicroclim
  microsigmahf.effect <- hfmicroclim.sd  #### by keeping the sigmas the same for the microsites (line 94 & 99) we assume that the microclimatic effects are the same across sites
  
  
  #### Next I set up an fstar or a GDD threshold for each individual
  spind <- paste(rep(1:nspps, each=ninds), rep(1:ninds, nspps), sep="_")
  
  fstarspp <- round(rnorm(nspps, fstar, fstarspeciessd), digits=0)
  df.fstar <- as.data.frame(cbind(species=rep(1:nspps, each=ninds*nsites*nmethods), 
                                  fstarspp=rep(fstarspp, each=ninds*nsites*nmethods),
                                  site=rep(c("arb", "hf"), each=ninds*nmethods),
                                  method=rep(rep(c("ws", "hobo"), each=ninds), nsites*nspps)))
  
  df.fstar$fstarspp <- as.numeric(df.fstar$fstarspp)
  df.fstar <- df.fstar[!duplicated(df.fstar),]
  
  #### Here, I set up provenance for each individual
  ### # Step 2: find GDDs
  #### Now I set up climate data for the Arboretum, this is the weather station data
  arbclim <- data.frame(microsite=rep(rep(c(1:nmicros), each=daysperyr*nmethods*(ninds/nmicros)),nspps),
                        ind=rep(rep(c(1:ninds), each=daysperyr*nmethods), nspps),
                        species = rep(c(1:nspps), each=daysperyr*nmicros*nmethods*ninds), 
                        day=rep(c(1:daysperyr), nmicros*nspps*nmethods),
                        method=rep(rep(c("ws", "hobo"), each=daysperyr), nspps*ninds),
                        site = as.character("arb"))
  
  ### This is how I get weather station data versus hobo logger data
  arbclim$tmean <- ifelse(arbclim$method=="hobo", rnorm(as.numeric(arbclim$day), cc.arb + microarb.effect, sigma.arb + microsigmaarb.effect), 
                          rnorm(as.numeric(arbclim$day), cc.arb, sigma.arb)) 
  ### and now we draw from mean and sigma for each day to find daily temp for each microsite
  
  #### Harvard Forest climate data, weather station data
  hfclim <- data.frame(microsite=rep(rep(c(1:nmicros), each=daysperyr*nmethods*(ninds/nmicros)),nspps),
                       ind=rep(rep(c(1:ninds), each=daysperyr*nmethods), nspps),
                       species = rep(c(1:nspps), each=daysperyr*nmicros*nmethods*ninds), 
                       day=rep(c(1:daysperyr), nmicros*nspps*nmethods),
                       method=rep(rep(c("ws", "hobo"), each=daysperyr),nspps*ninds),
                       site = as.character("hf"))
  
  ### Again, where I set up the difference between hobo logger and weather station
  hfclim$tmean <- ifelse(hfclim$method=="hobo", rnorm(as.numeric(hfclim$day), cc.hf + microhf.effect, sigma.hf + microsigmahf.effect), 
                          rnorm(as.numeric(hfclim$day), cc.hf, sigma.hf))
  ### and now we draw from mean and sigma for each day to find daily temp for each microsite
  
  
  ##Step 3: Make a data frame and get the mean temp
  df <- dplyr::full_join(arbclim, hfclim)
  df$tmean <- as.numeric(df$tmean)
  
  df$sp_ind <- paste(df$species, df$ind, sep="_")
  
  ### Let's just tidy everything up
  df$species <- as.numeric(as.factor(df$species))
  df.fstar$species <- as.numeric(as.factor(df.fstar$species))
  df <- full_join(df, df.fstar)
  df$spind_site_method <- paste0(df$sp_ind, df$site, df$method)
  df$nodupes <- paste0(df$sp_ind, df$site, df$method, df$microsite, df$day)
  df <- df[!duplicated(df$nodupes),]
  
  
  
  ### Calculate the OBSERVED GDDs!!!
  df$gdd.obs <- ave(df$tmean, df$spind_site_method, FUN=cumsum)
  
  ## Find the day of budburst to find the actual GDD versus the "observed GDD"
  for(i in c(unique(df$spind_site_method))){ # i="1_1arbws"
    
    bb <- which(df$gdd.obs[i==df$spind_site_method] >= df$fstarspp[i==df$spind_site_method])[1]
    df$bb[i==df$spind_site_method] <- bb
    
  }
  
  df.bb <- df[(df$bb==df$day),]
  df.bb <- na.omit(df.bb)
  
  ### Started 25 May 2020 by Cat
  # Updated 2 Feb 2021 by Cat
  ## Source file to build fake data simulations for urban versus provenance lat effects
  ## TAKE II: randomizing day of budburst and then calculate GDD from there
  
  if(hypoth=="hobo"){
    #### This is where I test our hypothesis. This doesn't come out of the model directly
    hypoth_sd <- hypoth.sd  ### adds variation to weather station estimates, rendering hobo loggers more accurate measures and therefore better able to capture urban effects
    hypoth_mu <- hypoth.mu
    hypoth_para <- hypoth.para
    ########################### ADDING IN HYPOTHESIS HERE! ################################
    ### I think this should just make the weather station less accurate...??? I hope.
    df.bb$hyp_b <- ifelse(df.bb$method==hypoth_para, 1, 0)  ## This won't be spit out of the model. If it's the weather station, make it a 1 if it's the hobo logger make it a 0
    ### Now, I am just adding more sigma to the weather station fstar values, seen by sd=ws_sd (which was 20) # emw -- deleted starter of df.fstar$gdd.noise + from above
    df.bb$gdd.noise <- df.bb$hyp_b * rep(rnorm(n=nspps, mean=hypoth_mu, sd=hypoth_sd), each=ninds*nsites) 
    
    df.bb$gdd <- df.bb$gdd.obs + df.bb$gdd.noise + rnorm(nrow(df.bb), mean=0, sd=sigma_y)
    
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
    df.bb$species <- as.numeric(df.bb$species)
    
    df.prov$ind <- as.integer(df.prov$ind)
    df.bb <- left_join(df.bb, df.prov)
    
  }
  
  if(hypoth=="urban" & hypoth.para=="NA"){
    urbeffect = hypoth.mu
    urbsd = hypoth.sd
    
    ##### We should get these back in parameter estimates
    df.bb$urbtx <- ifelse(df.bb$site=="arb", 1, 0)
    df.bb$gdd.noise  <- df.bb$urbtx * rep(rnorm(n=nspps, mean=urbeffect, sd=urbsd), each=ninds*nmethods)  
    
    df.bb$gdd <- df.bb$gdd.obs + df.bb$gdd.noise + rnorm(n=ntot, mean=0, sd=sigma_y)
    df.bb$species <- as.numeric(df.bb$species)
    
    ##### Now add in provenance so better able to compare to other simulations
    spind <- paste(rep(c(1:nspps), each=ninds), rep(1:ninds, nspps), sep="_")
    provenance.hf <- 42.5
    provenance.arb <- round(rnorm(nobs, provenance.hf, 5), digits=2)
    
    df.prov <- as.data.frame(cbind(sp_ind = rep(rep(spind, nsites),each=nmethods), 
                                   site = rep(c("arb", "hf"), each=nobs*nmethods),
                                   provenance = c(rep(provenance.arb, each=nmethods), rep(provenance.hf, nobs*nmethods)),
                                   method = rep(c("ws", "hobo"), nsites*nobs)))
    df.prov$species <- as.numeric(gsub("\\_.*" , "", df.prov$sp_ind))
    df.prov$ind <- gsub(".*_" , "", df.prov$sp_ind)
    df.prov$sp_ind <- NULL
    
    df.prov$ind <- as.integer(df.prov$ind)
    df.bb <- full_join(df.bb, df.prov)
    
  }
  
  if(hypoth=="prov" & hypoth.para=="NA"){ ### This is the provenance model
    prov_effect = hypoth.mu
    prov_sd = hypoth.sd
    
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
    
    df.prov$species <- as.numeric(df.prov$species)
    df.prov$provenance <- as.numeric(df.prov$provenance)
    df.prov$prov.z <- (df.prov$provenance-mean(df.prov$provenance,na.rm=TRUE))/(sd(df.prov$provenance,na.rm=TRUE))
    
    df.prov$ind <- as.numeric(df.prov$ind)
    df.bb <- full_join(df.bb, df.prov)
    
    # Generate random parameters (by species)
    df.bb$gdd.noise <- df.bb$prov.z * rep(rnorm(n=nspps, mean=prov_effect, sd=prov_sd), each=ninds*nsites)
    
    df.bb$gdd <- df.bb$gdd.obs + df.bb$gdd.noise + rnorm(n=ntot, mean=0, sd=sigma_y)
    df.bb <- df.bb[!duplicated(df.bb),]
    
    
    
  }
  
  if(hypoth=="urban" & hypoth.para%in%c("ws", "hobo")){
    urbeffect = hypoth.mu
    urbsd = hypoth.sd
    hypoth_para = hypoth.para.mu
    hypoth_parasd = hypoth.para.sd
    
    hypoth_para <- hypoth.para
    ########################### ADDING IN HYPOTHESIS HERE! ################################
    ### I think this should just make the weather station less accurate...??? I hope.
    df.bb$hyp_b <- ifelse(df.bb$method==hypoth_para, 1, 0)  ## This won't be spit out of the model. If it's the weather station, make it a 1 if it's the hobo logger make it a 0
    ### Now, I am just adding more sigma to the weather station fstar values, seen by sd=ws_sd (which was 20) # emw -- deleted starter of df.fstar$gdd.noise + from above
    df.bb$gdd.noise <- df.bb$hyp_b * rep(rnorm(n=nspps, mean=hypoth.para.mu, sd=hypoth_parasd), each=ninds*nsites) 

    ##### Now we add in the urban effect
    df.bb$urbtx <- ifelse(df.bb$site=="arb", 1, 0)
    df.bb$gdd.noise  <- df.bb$gdd.noise + df.bb$urbtx * rep(rnorm(n=nspps, mean=urbeffect, sd=urbsd), each=ninds*nmethods)  
    
    df.bb$gdd <- df.bb$gdd.obs + df.bb$gdd.noise + rnorm(n=ntot, mean=0, sd=sigma_y)
    df.bb$species <- as.numeric(df.bb$species)
    
    ##### Now add in provenance so better able to compare to other simulations
    spind <- paste(rep(c(1:nspps), each=ninds), rep(1:ninds, nspps), sep="_")
    provenance.hf <- 42.5
    provenance.arb <- round(rnorm(nobs, provenance.hf, 5), digits=2)
    
    df.prov <- as.data.frame(cbind(sp_ind = rep(rep(spind, nsites),each=nmethods), 
                                   site = rep(c("arb", "hf"), each=nobs*nmethods),
                                   provenance = c(rep(provenance.arb, each=nmethods), rep(provenance.hf, nobs*nmethods)),
                                   method = rep(c("ws", "hobo"), nsites*nobs)))
    df.prov$species <- as.numeric(gsub("\\_.*" , "", df.prov$sp_ind))
    df.prov$ind <- gsub(".*_" , "", df.prov$sp_ind)
    df.prov$sp_ind <- NULL
    
    df.prov$ind <- as.integer(df.prov$ind)
    df.bb <- full_join(df.bb, df.prov)
    
  }
  
  ##### Clean up the dataframe to prepare for analyses
  df.bb <- subset(df.bb, select=c("site", "method", "species", "ind", "gdd.noise", "fstarspp", "gdd", "provenance", "bb")) # 
  
  bball <- df.bb[!duplicated(df.bb),]
  
  
  ##### Now let's do some checks...
  bball$gdd_accuracy <- bball$gdd - bball$fstarspp
  bball$type <- ifelse(bball$method=="ws", 1, 0)
  
  bball <- na.omit(bball)
  
  #lme4::lmer(gdd ~ provenance + type + (provenance + type | species), data=bball)
  
  mylist <- list(bball, df, hfclim, arbclim)  
  
  return(mylist)
  
}

#bblist <- bbfunc(hypoth, hypoth.para, hypoth.mu, hypoth.sd, fstar.num, fstar.sd, arbclim, arbclim.sd, arbmicroclim, arbmicroclim.sd, hfclim, hfclim.sd, hfmicroclim, hfmicroclim.sd)

#bball <- bblist[[1]]
#df <- bblist[[2]]
#hfclim <- bblist[[3]]
#arbclim <- bblist[[4]]

if(FALSE){ ### This is to make the varying GDD plots... to move elsewhere later. 
  #bblist200 <- bbfunc(hypoth, question, hypoth.sd, fstar.num, urbeff, methodeff, urbmethod, arbclim, arbmicroclim, hfclim, hfmicroclim)
  #bblist250 <- bbfunc(hypoth, question, hypoth.sd, fstar.num, urbeff, methodeff, urbmethod, arbclim, arbmicroclim, hfclim, hfmicroclim)
  bblist300 <- bbfunc(hypoth, question, hypoth.sd, fstar.num, urbeff, methodeff, urbmethod, arbclim, arbmicroclim, hfclim, hfmicroclim)
  
  
  bb200 <- bblist200[[1]]
  bb250 <- bblist250[[1]]
  bb300 <- bblist300[[1]]
  
  bb200$gdd200 <- bb200$gdd
  bb200$gdd_accuracy200 <- bb200$gdd_accuracy
  bb200$gdd <- bb200$gdd_accuracy <- bb200$gdd.noise <- bb200$fstar.new <- bb200$bb <- NULL
  
  bb250$gdd250 <- bb250$gdd
  bb250$gdd_accuracy250 <- bb200$gdd_accuracy
  bb250$gdd <- bb250$gdd_accuracy <- bb250$gdd.noise <- bb250$fstar.new <- bb250$bb <- NULL
  
  bb <- full_join(bb200, bb250)
  
  bb300$gdd300 <- bb300$gdd
  bb300$gdd_accuracy300 <- bb300$gdd_accuracy
  bb300$gdd <- bb300$gdd_accuracy <- bb300$gdd.noise <- bb300$fstar.new <- bb300$bb <- NULL
  
  bb <- full_join(bb, bb300)
  
  
  ggplot(bb, aes(temp, gdd))
  
  
  
  
  
}

