### Started 6 Oct 2020 by Cat
## Source function to build data for the shiny app
### Need to eventually integrate hypothesis tests and provenance vs urban!

#Load Libraries
library(dplyr)
library(tidyr)

set.seed(20210322)

if(FALSE){
  sigma <- 0.1
  basetemp <- 0
}


daysperyr <- 50
nspps <- 100

warmfunc <- function(sigma, basetemp){
  
    cc <- 15
    sigma.cc <- sigma
    basetemp <- basetemp
    
    warm.min <- 0
    warm.max <- 10
    
    fstar.min <- 100
    fstar.max <- 500
    
    fstars <- seq(from=fstar.min, to=fstar.max, length.out=nspps)

    warms <- seq(from=warm.min, to=warm.max, by=1)
    
    tmeanbase <- rnorm(rep(1:daysperyr, each=nspps), cc, sigma.cc)
    
    tmeanadd <- c()
    tmean <- c()
    for(i in c(unique(warms))){
      tmeanadd <- tmeanbase + i ## i=0
      tmean <- c(tmean, tmeanadd)
    }
    tmeangdd <- ifelse(tmean>=basetemp, tmean-basetemp, 0)
    warming <- rep(warms, each=(daysperyr*nspps))
    tmeanwarm <- data.frame(cbind(tmeangdd, warming))
    tmeanwarm$fstars <- rep(fstars, each = daysperyr)
    tmeanwarm$doy <- rep(1:daysperyr, times=length(warms))
    tmeanwarm$gdd <- ave(tmeanwarm$tmeangdd, tmeanwarm$warming, tmeanwarm$fstars, FUN=cumsum)
    
    # get GDD for each budburst DOY for each fstar
    tmeanwarm$observedgdd <- NA
    tmeanwarm$bdoy <- NA
    tmeanwarm$spwarm <- paste0(tmeanwarm$warming, tmeanwarm$fstars)#, tmeanwarm$id)
    for(i in c(unique(tmeanwarm$spwarm))){ #i=850
      tmeanwarm$bdoy[tmeanwarm$spwarm==i] <- min(which(tmeanwarm$gdd[tmeanwarm$spwarm==i] >= tmeanwarm$fstars[tmeanwarm$spwarm==i]))
      tmeanwarm$observedgdd[tmeanwarm$spwarm==i] <- tmeanwarm$gdd[tmeanwarm$spwarm==i][(min(which(tmeanwarm$bdoy[tmeanwarm$spwarm==i] == tmeanwarm$doy[tmeanwarm$spwarm==i])))]
    }
    
    tmeanwarm$gddaccuracy <- tmeanwarm$observedgdd - tmeanwarm$fstars
    tmeanwarm$gddratio <- tmeanwarm$observedgdd/tmeanwarm$fstars
    
    gddstuff <- subset(tmeanwarm, select=c("bdoy", "observedgdd", "gddaccuracy", "gddratio", "warming", "fstars"))#, "id"))
    gddstuff <- gddstuff[!duplicated(gddstuff),]
    
    mylist <- list(fstars, gddstuff)
    
    return(mylist)
    
}

