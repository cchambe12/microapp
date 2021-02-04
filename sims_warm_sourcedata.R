### Started 6 Oct 2020 by Cat
## Source function to build data for the shiny app
### Need to eventually integrate hypothesis tests and provenance vs urban!

if(FALSE){
  fstar.num <- 300  ## GDD threshold
  fstar.sd <- 50
  warmmax <- 10
}


warmfunc <- function(fstar.num, fstar.sd, warmmax){
  
  # Step 1: Set up years, days per year, temperatures, sampling frequency, required GDD (fstar)
  daysperyr <- 150 #### just to make sure we don't get any NAs
  nspps <- 15 
  ninds <- 10 
  nobs <- nspps*ninds
  
  ### These are our fstar thresholds
  fstar <- fstar.num  ### mu_a_sp in model output
  fstarspeciessd <- fstar.sd ### sigma_a_sp in model output
  
  ## Sigma_y to be added at the end
  sigma_y <- 2
  
  ### Now the climate data 
  dayz <- rep(1:daysperyr, nobs)
  cc <- 10
  sigma.cc <- 2
  warmcc <- warmmax

  
  #### Next I set up an fstar or a GDD threshold for each individual
  spind <- paste(rep(1:nspps, each=10), rep(1:ninds, 20), sep="_")
  
  fstarspp <- round(rnorm(nspps, fstar, fstarspeciessd), digits=0)
  df.fstar <- as.data.frame(cbind(species=rep(1:nspps, each=ninds), 
                                  fstarspp=rep(fstarspp, each=ninds)))
  
  df.fstar$fstarspp <- as.numeric(df.fstar$fstarspp)
  
  #### Here, I set up provenance for each individual
  ### # Step 2: find GDDs
  #### Now I set up climate data for the Arboretum, this is the weather station data
  clim <- data.frame(ind=rep(rep(c(1:ninds), each=daysperyr), nspps),
                        species = rep(c(1:nspps), each=daysperyr*ninds), 
                        day=rep(c(1:daysperyr), nspps*ninds))
  
  clim$tmean0 <- rnorm(clim$day, cc + 0, sigma.cc) 
  clim$tmean1 <- rnorm(clim$day, cc + 1, sigma.cc) 
  clim$tmean2 <- rnorm(clim$day, cc + 2, sigma.cc) 
  clim$tmean3 <- rnorm(clim$day, cc + 3, sigma.cc) 
  clim$tmean4 <- rnorm(clim$day, cc + 4, sigma.cc) 
  clim$tmean5 <- rnorm(clim$day, cc + 5, sigma.cc) 
  clim$tmean6 <- rnorm(clim$day, cc + 6, sigma.cc) 
  clim$tmean7 <- rnorm(clim$day, cc + 7, sigma.cc) 
  clim$tmean8 <- rnorm(clim$day, cc + 8, sigma.cc) 
  clim$tmean9 <- rnorm(clim$day, cc + 9, sigma.cc) 
  clim$tmean10 <- rnorm(clim$day, cc + 10, sigma.cc) 
  
  ##Step 3: Make a data frame and get the mean temp
  df <- clim
  df$tmean0 <- as.numeric(df$tmean0)
  df$tmean1 <- as.numeric(df$tmean1)
  df$tmean2 <- as.numeric(df$tmean2)
  df$tmean3 <- as.numeric(df$tmean3)
  df$tmean4 <- as.numeric(df$tmean4)
  df$tmean5 <- as.numeric(df$tmean5)
  df$tmean6 <- as.numeric(df$tmean6)
  df$tmean7 <- as.numeric(df$tmean7)
  df$tmean8 <- as.numeric(df$tmean8)
  df$tmean9 <- as.numeric(df$tmean9)
  df$tmean10 <- as.numeric(df$tmean10)
  
  df$sp_ind <- paste(df$species, df$ind, sep="_")
  
  ### Calculate the OBSERVED GDDs!!!
  df$gdd.obs0 <- ave(df$tmean0, df$sp_ind, FUN=cumsum)
  df$gdd.obs1 <- ave(df$tmean1, df$sp_ind, FUN=cumsum)
  df$gdd.obs2 <- ave(df$tmean2, df$sp_ind, FUN=cumsum)
  df$gdd.obs3 <- ave(df$tmean3, df$sp_ind, FUN=cumsum)
  df$gdd.obs4 <- ave(df$tmean4, df$sp_ind, FUN=cumsum)
  df$gdd.obs5 <- ave(df$tmean5, df$sp_ind, FUN=cumsum)
  df$gdd.obs6 <- ave(df$tmean6, df$sp_ind, FUN=cumsum)
  df$gdd.obs7 <- ave(df$tmean7, df$sp_ind, FUN=cumsum)
  df$gdd.obs8 <- ave(df$tmean8, df$sp_ind, FUN=cumsum)
  df$gdd.obs9 <- ave(df$tmean9, df$sp_ind, FUN=cumsum)
  df$gdd.obs10 <- ave(df$tmean10, df$sp_ind, FUN=cumsum)
  
  ### Let's just tidy everything up
  df$species <- as.numeric(as.factor(df$species))
  df.fstar$species <- as.numeric(as.factor(df.fstar$species))
  df <- full_join(df, df.fstar)
  df <- df[!duplicated(df),]
  
  df$spind_site_method <- paste0(df$sp_ind, df$site, df$method)
  
  ## Find the day of budburst to find the actual GDD versus the "observed GDD"
  for(i in c(unique(df$spind_site_method))){ # i="1_1arbws"
    
    bb0 <- which(df$gdd.obs0[i==df$spind_site_method] >= df$fstarspp[i==df$spind_site_method])[1]
    df$bb0[i==df$spind_site_method] <- bb0
    bb1 <- which(df$gdd.obs1[i==df$spind_site_method] >= df$fstarspp[i==df$spind_site_method])[1]
    df$bb1[i==df$spind_site_method] <- bb1
    bb2 <- which(df$gdd.obs2[i==df$spind_site_method] >= df$fstarspp[i==df$spind_site_method])[1]
    df$bb2[i==df$spind_site_method] <- bb2
    bb3 <- which(df$gdd.obs3[i==df$spind_site_method] >= df$fstarspp[i==df$spind_site_method])[1]
    df$bb3[i==df$spind_site_method] <- bb3
    bb4 <- which(df$gdd.obs4[i==df$spind_site_method] >= df$fstarspp[i==df$spind_site_method])[1]
    df$bb4[i==df$spind_site_method] <- bb4
    bb5 <- which(df$gdd.obs5[i==df$spind_site_method] >= df$fstarspp[i==df$spind_site_method])[1]
    df$bb5[i==df$spind_site_method] <- bb5
    bb6 <- which(df$gdd.obs6[i==df$spind_site_method] >= df$fstarspp[i==df$spind_site_method])[1]
    df$bb6[i==df$spind_site_method] <- bb6
    bb7 <- which(df$gdd.obs7[i==df$spind_site_method] >= df$fstarspp[i==df$spind_site_method])[1]
    df$bb7[i==df$spind_site_method] <- bb7
    bb8 <- which(df$gdd.obs8[i==df$spind_site_method] >= df$fstarspp[i==df$spind_site_method])[1]
    df$bb8[i==df$spind_site_method] <- bb8
    bb9 <- which(df$gdd.obs9[i==df$spind_site_method] >= df$fstarspp[i==df$spind_site_method])[1]
    df$bb9[i==df$spind_site_method] <- bb9
    bb10 <- which(df$gdd.obs10[i==df$spind_site_method] >= df$fstarspp[i==df$spind_site_method])[1]
    df$bb10[i==df$spind_site_method] <- bb10
    
  }
  
  df.bb <- df[(df$bb0==df$day | df$bb1==df$day | df$bb2==df$day | df$bb3==df$day | df$bb4==df$day | df$bb5==df$day |
                 df$bb6==df$day | df$bb7==df$day | df$bb8==df$day | df$bb9==df$day | df$bb10==df$day),]
  
  df.bb <- subset(df.bb, select=c("species", "ind", "gdd.obs0",
                                  "gdd.obs1", "gdd.obs2", "gdd.obs3", "gdd.obs4", "gdd.obs5",
                                  "gdd.obs6", "gdd.obs7", "gdd.obs8", "gdd.obs9", "gdd.obs10",
                                  "fstarspp")) # , "bb1", "bb2", "bb3", "bb4", "bb5", "bb6", "bb7", "bb8", "bb9", "bb10"
  bball <- df.bb %>% tidyr::gather(warming, gdd, gdd.obs0:gdd.obs10, -species, -ind, -fstarspp)
  bball$warming <- as.numeric(substr(bball$warming, 8, 9))
  
  bball <- bball[!duplicated(bball),]
  
  
  ##### Now let's do some checks...
  bball$gdd_accuracy <- bball$gdd - bball$fstarspp
  
  bball <- bball[(bball$warming<=warmmax),]
  
  bball <- na.omit(bball)
  
  mylist <- list(bball, df)  
  
  return(mylist)
  
}

if(FALSE){ ### This is to make the varying GDD plots... to move elsewhere later. 
  bblist300 <- warmfunc(fstar.num, fstar.sd, warming)
  
  bb300 <- bblist300[[1]]
  library(ggplot2)
  library(viridis)
  
  quartz()
  ggplot(bball, aes(as.numeric(warming), abs(gdd_accuracy), col=fstarspp, group=fstarspp)) +
    #geom_ribbon(stat='smooth', method = "lm", se=TRUE, alpha=1, 
     #           aes(fill = as.factor(fstarspp), group = as.factor(fstarspp))) +
    geom_line(stat='smooth', method = "loess", alpha=1, aes(col=fstarspp)) +
    theme(panel.background = element_blank(), axis.line = element_line(colour = "black"),
          legend.text.align = 0,
          legend.key = element_rect(colour = "transparent", fill = "white"),
          plot.margin = margin(0.5, 0.5, 0.5, 1, "cm")) + 
    xlab("Amount of warming") + ylab("GDD accuracy") +
    scale_x_continuous(breaks = sort(c(seq(0, 10, by=1)))) +
    scale_color_continuous(type="viridis")
  
  
  
  
  
}

