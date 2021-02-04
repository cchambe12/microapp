### Started 23 April 2020 by Cat
## Building new dataframe with fake data to try and better understand hobo logger data versus microclimate data

# Maybe I should use estimates for fstar from real models?

# housekeeping
rm(list=ls()) 
options(stringsAsFactors = FALSE)
options(mc.cores = parallel::detectCores())

#### Overall model:
# GDD ~ urban + method + method*urban + (urban + method + method*urban|species) 

library(RColorBrewer)
library(viridis)
library(lme4)
library(ggplot2)
library(gridExtra)
library(rstan)
library(png)
library(shiny)
library(shinydashboard)
library(shinythemes)

source("~/Documents/git/microclimates/analyses/source/sims_hypoth_sourcedata.R")
source("~/Documents/git/microclimates/analyses/source/sims_params_sourcedata.R")
source("~/Documents/git/microclimates/analyses/source/sims_warm_sourcedata.R")

df <- read.csv("~/Documents/git/microclimates/analyses/output/cleanmicro_gdd_2019.csv")



ui <- fluidPage(theme = shinytheme("united"),
  
  
  navbarPage("Modeling & Interpreting GDD",
             tabPanel("Home",
                      mainPanel(h1("Methods in Ecological Modeling & Interpretation"),
                                                p(style="text-align: justify;","In ecology, we have the fundamental issue of understanding and applying methods to accurately predict shifts in climate and the broader impacts of these shifts. Often we use mixed models to answer ecological questions, though we do not always understand the intricacies of the model output, nor do we investigate what is missing from the model output. Here, we work to understand mixed models using simulation data and test myriad hypotheses through these simulations. These methods can be applied to many ecological questions investigating climate data across global habitats but here we will investigate the effects of climate measurements and site on spring plant phenology. "),
                                                br(),
                                                img(src="sitecompare_img.png", height=450, width=600),
                                                h3("Using simulations, we test four major hypotheses: "),
                                                h5("see page ``Hypothesis Testing''"),
                                                br(),
                                                p(style="text-align: justify;","1) Hobo loggers better capture urban or provenance effects. "),
                                                p(style="text-align: justify;","2) Hobo loggers are less accurate measures of the same weather as weather stations."),
                                                p(style="text-align: justify;","3) Urban environments require fewer GDDs to leafout than forest habitats."),
                                                p(style="text-align: justify;","4) Individuals with provenance latitudes from more northern locations require fewer GDDs to leafout."),
                                                br(),
                                                p(style="text-align: justify;","On this page, you can use the sidebar to select the hypothesis of interest and then adjust GDD threshold and climate data for the simulations."),
                                                br(),
                                                h3("Next, we build test data to test our rStan model.s "),
                                                h5( "see page ``Simulation Data for Model Testing''"),
                                                br(),
                                                p(style="text-align: justify;","On this page, you can use the sidebar to select the type of model of interest (i.e., Urban Model or Provenance Latitude Model) and then adjust the parameter inputs."),
                                                br(),
                                                h3("We then have real data collected from 2019 and look at results. "),
                                                h5( "see page ``Real Data and Analyze Results''"),
                                                br(),
                                                strong("**Pro Tip: toggle between this page and the ``Hypotesis Testing'' page to better interpret results. "),
                                                br(),
                                                h3("Finally, we look at how GDD accuracy is changing with warming and how different GDD thresholds impact accuracy. "),
                                                h5("see page ``Forecasting GDD with Warming''")
                        )
             ),
                                      
                      
                      
                 tabPanel("Hypothesis Testing",
                      sidebarLayout(
                        sidebarPanel(
                        tabPanel("Hypothesis Testing",
                        selectInput("Hypothesis", "Hypothesis",
                                    choices = c("---Choose One---",
                                                "Hypothesis Hobo Logger: hobo loggers are more accurate",
                                                "Hypothesis Hobo Logger: weather station is more accurate",
                                                "Hypothesis Urban: urban sites require fewer GDDs",
                                                "Hypothesis Provenance: more Northern provenances require fewer GDDs"),
                                    selected = ("---Choose One---")),
                        sliderInput(inputId = "HypothEffect",
                                    label = "Hypothesis Effect",
                                    value = 0, min = -100, max = 100),
                        sliderInput(inputId = "HypothEffectSD",
                                    label = "Hypothesis Effect SD",
                                    value = 10, min = 0, max = 30),
                        sliderInput(inputId = "Fstar",
                                    label = "GDD base threshold",
                                    value = 300, min = 50, max = 400),
                        sliderInput(inputId = "FstarSD",
                                    label = "GDD base threshold SD",
                                    value = 50, min = 0, max = 100),
                        sliderInput(inputId = "ArbClimate",
                                    label = "Arb Climate",
                                    value = 11, min = 0, max = 20),
                        sliderInput(inputId = "ArbClimateSD",
                                    label = "Arb Climate SD",
                                    value = 4, min = 0, max = 10),
                        sliderInput(inputId = "ArbMicroEffect",
                                    label = "Arb Micro Effect",
                                    value = 1, min = -10, max = 10),
                        sliderInput(inputId = "ArbMicroEffectSD",
                                    label = "Arb Micro Effect SD",
                                    value = 5, min = 0, max = 10),
                        sliderInput(inputId = "HFClimate",
                                    label = "HF Climate",
                                    value = 9, min = 0, max = 20),
                        sliderInput(inputId = "HFClimateSD",
                                    label = "HF Climate SD",
                                    value = 2, min = 0, max = 10),
                        sliderInput(inputId = "HFMicroEffect",
                                    label = "HF Micro Effect",
                                    value = -1, min = -10, max = 10),
                        sliderInput(inputId = "HFMicroEffectSD",
                                    label = "HF Micro Effect SD",
                                    value = 5, min = 0, max = 10),
                        #numericInput("steps", "How many steps?", 10),
                        textOutput("result"),
                        actionButton("run", "View Plots",
                                     style="color: #fff; background-color: #337ab7; border-color: #2e6da4")
                        )
             ),
             
             mainPanel(
               tabsetPanel(
                 tabPanel("Climate Data", 
                          #verbatimTextOutput("print_data"), verbatimTextOutput("strdata"),
                          plotOutput("climtypes"), 
                          column(2, align="center",plotOutput("hist"))), 
                 tabPanel("GDDs across Species", plotOutput("gddsites")), 
                 tabPanel("Method Accuracy", plotOutput("gdd_accuracy")),
                 tabPanel("Site Accuracy", plotOutput("site_accuracy")),
                 tabPanel("Site x Method", plotOutput("interaction")),
                 tabPanel("Model Output", 
                          actionButton("go" ,"Run Model and View muplot"),
                          plotOutput("muplot"))
               )
             ))
             
             ),
             tabPanel("Simulation Data for Model Testing",
                      sidebarLayout(
                        sidebarPanel(
                        tabPanel("Simulation Data",
                            selectInput("Question", "Question",
                                                   choices = c("---Choose One---",
                                                               "Urban Model", 
                                                               "Provenance Model"),
                                                   selected="---Choose One---"),
                                       sliderInput(inputId = "TXEffect",
                                                   label = "Treatment Effect",
                                                   value = -30, min = -100, max = 100),
                                       sliderInput(inputId = "TXEffectSD",
                                                  label = "Treatment Effect SD",
                                                  value = 5, min = -0, max = 20),
                                       
                                       sliderInput(inputId = "MethodEffect",
                                                   label = "Method Effect",
                                                   value = -20, min = -100, max = 100),
                                       sliderInput(inputId = "MethodEffectSD",
                                                  label = "Method Effect SD",
                                                  value = 5, min = 0, max = 20),
                                       
                                       sliderInput(inputId = "TXMethod",
                                                   label = "Treatment x Method Effect",
                                                   value = -40, min = -100, max = 100),
                                       sliderInput(inputId = "TXMethodSD",
                                                  label = "Treatment x Method Effect SD",
                                                  value = 5, min = 0, max = 20),
                                       actionButton("simsgo", "View Plots",
                                                    style="color: #fff; background-color: #337ab7; border-color: #2e6da4")
                      )
                        ),
                      mainPanel(
                        tabsetPanel(
                          tabPanel("GDDs across Species", plotOutput("gddsitessims")), 
                                tabPanel("Method Accuracy", plotOutput("gdd_accuracysims")),
                                tabPanel("Site Accuracy", plotOutput("site_accuracysims")),
                                tabPanel("Site x Method", plotOutput("interactionsims")),
                                tabPanel("Model Output",
                                         actionButton("simsrunmod" ,"Run Model and View muplot"),
                                   plotOutput("simsmuplot"))
                                ))
  )
             ),
  
  tabPanel("Real Data and Analyze Results",
           mainPanel(
             tabsetPanel(
               tabPanel("GDDs across Species", 
                        #verbatimTextOutput("print_data"),
                        plotOutput("gddsitesreal")), 
               tabPanel("Site x Method", plotOutput("interactionreal")),
               tabPanel("Model Output",
                        sidebarLayout(
                          sidebarPanel(
                            tabPanel("Real Data",
                                     selectInput("type", "Question",
                                                 choices = c("---Choose One---",
                                                             "Urban Model", 
                                                             "Provenance Latitude Model"),
                                                 selected="---Choose One---"),
                                     actionButton("realrunmod", "Run Model",
                                                  style="color: #fff; background-color: #337ab7; border-color: #2e6da4")
                            )
                          ),
                        mainPanel(plotOutput("realmuplot"))
             ))
  )
  )
  ),
  tabPanel("Forecasting GDD with Warming",
           tabPanel("Simulating Warming",
                    sidebarLayout(
                      sidebarPanel(
                        tabPanel("Simulating Warming",
                                 sliderInput(inputId = "fstar",
                                             label = "GDD base threshold",
                                             value = 300, min = 50, max = 400),
                                 sliderInput(inputId = "fstarsd",
                                             label = "GDD base threshold SD",
                                             value = 50, min = 0, max = 100),
                                     numericInput(inputId = "warmmax",
                                                 label = "Maximum Range of Warming (°C)",
                                                 value = 0, min = 0, max = 10),
                                     actionButton("warmrun", "View Plot",
                                                  style="color: #fff; background-color: #337ab7; border-color: #2e6da4")
                            )
                          ),
                          mainPanel(plotOutput("gddwarm"))
                        ))
             )
           )
  
)


server <- function(input, output) {
  
  
  get.data <- eventReactive(input$run, {
    
    progress <- Progress$new(max = 10)
    on.exit(progress$close())
    
    progress$set(message = "Compiling Simulation Data")
    for (i in seq_len(10)) {
      Sys.sleep(0.5)
      progress$inc(1)
    }
    
    bbfunc(if(input$Hypothesis=="Hypothesis Hobo Logger: hobo loggers are more accurate")
  {"hobo"}else if(input$Hypothesis=="Hypothesis Hobo Logger: weather station is more accurate")
    {"hobo"}else if(input$Hypothesis=="Hypothesis Urban: urban sites require fewer GDDs"){"urban"}else 
    if(input$Hypothesis=="Hypothesis Provenance: more Northern provenances require fewer GDDs"){"prov"}, 
  if(input$Hypothesis=="Hypothesis Hobo Logger: hobo loggers are more accurate")
  {"ws"}else if(input$Hypothesis=="Hypothesis Hobo Logger: weather station is more accurate"){"hobo"},
  as.numeric(input$HypothEffect), as.numeric(input$HypothEffectSD),
  as.numeric(input$Fstar), as.numeric(input$FstarSD),
  as.numeric(input$ArbClimate), as.numeric(input$ArbClimateSD),
  as.numeric(input$ArbMicroEffect), as.numeric(input$ArbMicroEffectSD), 
  as.numeric(input$HFClimate), as.numeric(input$HFClimateSD), 
  as.numeric(input$HFMicroEffect), as.numeric(input$HFMicroEffectSD))
    
  })
  
  if(TRUE){
  get.datasims <- eventReactive(input$simsgo, {
    
    progress <- Progress$new(max = 10)
    on.exit(progress$close())
    
    progress$set(message = "Compiling Simulation Data")
    for (i in seq_len(10)) {
      Sys.sleep(0.5)
      progress$inc(1)
    }
    
    simfunc(if(input$Question=="Urban Model")
    {TRUE}else if(input$Question=="Provenance Model")
    {FALSE},
    as.numeric(input$TXEffect), as.numeric(input$TXEffectSD),
    as.numeric(input$MethodEffect), as.numeric(input$MethodEffectSD),
    as.numeric(input$TXMethod), as.numeric(input$TXMethodSD)
    )
    
  })
  }
  
  get.datareal <- df
  
  get.warmsims <- eventReactive(input$warmrun, {
    
    progress <- Progress$new(max = 10)
    on.exit(progress$close())
    
    progress$set(message = "Compiling Simulation Data")
    for (i in seq_len(10)) {
      Sys.sleep(0.5)
      progress$inc(1)
    }
    
    warmfunc(as.numeric(input$fstar), as.numeric(input$fstarsd), as.numeric(input$warmmax)
    )
    
  })
  
  #output$print_data <- eventReactive(input$realgo,{renderPrint(get.datareal)})
  #output$strdata <- renderPrint(str(get.data()))
  
  #observeEvent(input$run, {
  output$gdd_accuracy <- renderPlot({
    bball <- get.data()[[1]]
    xtext <- seq(1, 2, by=1)
    cols <-viridis_pal(option="viridis")(3)
    plot(as.numeric(as.factor(bball$type)), as.numeric(bball$gdd_accuracy), 
         col=cols[as.factor(bball$method)], ylab="GDD accuracy", xaxt="none",xlab="")
    axis(side=1, at=xtext, labels = c("Hobo Logger", "Weather Station"))
    legend(0, -20, sort(unique(gsub("_", " ", bball$method))), pch=19,
           col=cols[as.factor(bball$method)],
           cex=1, bty="n")
  })
  
  output$gdd_accuracysims <- renderPlot({
    bball <- get.datasims()[[1]]
    xtext <- seq(1, 2, by=1)
    cols <-viridis_pal(option="viridis")(3)
    plot(as.numeric(as.factor(bball$type)), as.numeric(bball$gdd_accuracy), 
         col=cols[as.factor(bball$method)], ylab="GDD accuracy", xaxt="none",xlab="")
    axis(side=1, at=xtext, labels = c("Hobo Logger", "Weather Station"))
    legend(0, -20, sort(unique(gsub("_", " ", bball$method))), pch=19,
           col=cols[as.factor(bball$method)],
           cex=1, bty="n")
  })
  #})
  
  #observeEvent(input$run, {
  output$site_accuracy <- renderPlot({
    bball <- get.data()[[1]]
    xtext <- seq(1, 2, by=1)
    cols <-viridis_pal(option="plasma")(3)
    plot(as.numeric(as.factor(bball$site)), as.numeric(bball$gdd_accuracy), 
         col=cols[as.factor(bball$site)], xlab="", ylab="GDD accuracy", xaxt="none")
    axis(side=1, at=xtext, labels = c("Arnold Arboretum", "Harvard Forest"))
    legend(0, -20, sort(unique(gsub("_", " ", bball$site))), pch=19,
           col=cols[as.factor(bball$site)],
           cex=1, bty="n")
  })
  
  output$site_accuracysims <- renderPlot({
    bball <- get.datasims()[[1]]
    xtext <- seq(1, 2, by=1)
    cols <-viridis_pal(option="plasma")(3)
    plot(as.numeric(as.factor(bball$site)), as.numeric(bball$gdd_accuracy), 
         col=cols[as.factor(bball$site)], xlab="", ylab="GDD accuracy", xaxt="none")
    axis(side=1, at=xtext, labels = c("Arnold Arboretum", "Harvard Forest"))
    legend(0, -20, sort(unique(gsub("_", " ", bball$site))), pch=19,
           col=cols[as.factor(bball$site)],
           cex=1, bty="n")
  })
  #})
  
  #observeEvent(input$run, {
  output$gddsites <- renderPlot({
    bball <- get.data()[[1]]
    #quartz(width=6, height=5)
    par(mfrow=c(1,2))
    my.pal <- viridis_pal(option="magma")(20)
    my.pch <- c(15:16)
    plot(as.numeric(bball$gdd) ~ as.numeric(as.factor(bball$species)), col=my.pal[as.factor(bball$species)], 
         pch=my.pch[as.factor(bball$site)], data = bball[(bball$method=="ws"),], main="Weather Station",
         ylab="GDD", ylim=c(0, 600), xlab="Species")
    abline(h=mean(bball$gdd[bball$method=="ws"]), lwd=3)
    
    plot(as.numeric(gdd) ~ as.numeric(as.factor(species)), col=my.pal[as.factor(bball$species)], 
         pch=my.pch[as.factor(bball$site)], data = bball[(bball$method=="hobo"),], main="Hobo Logger",
         ylab="GDD", ylim=c(0, 600), xlab="Species")
    abline(h=mean(bball$gdd[bball$method=="hobo"]), lwd=3)
  })
  output$gddsitessims <- renderPlot({
    bball <- get.datasims()[[1]]
    #quartz(width=6, height=5)
    par(mfrow=c(1,2))
    my.pal <- viridis_pal(option="magma")(20)
    my.pch <- c(15:16)
    plot(as.numeric(bball$gdd) ~ as.numeric(as.factor(bball$species)), col=my.pal[as.factor(bball$species)], 
         pch=my.pch[as.factor(bball$site)], data = bball[(bball$method=="ws"),], main="Weather Station",
         ylab="GDD", ylim=c(0, 600), xlab="Species")
    abline(h=mean(bball$gdd[bball$method=="ws"]), lwd=3)
    
    plot(as.numeric(gdd) ~ as.numeric(as.factor(species)), col=my.pal[as.factor(bball$species)], 
         pch=my.pch[as.factor(bball$site)], data = bball[(bball$method=="hobo"),], main="Hobo Logger",
         ylab="GDD", ylim=c(0, 600), xlab="Species")
    abline(h=mean(bball$gdd[bball$method=="hobo"]), lwd=3)
  })
  #})
  
  output$gddsitesreal <- renderPlot({
    bball <- get.datareal
    #quartz(width=6, height=5)
    par(mfrow=c(1,2))
    my.pal <- viridis_pal(option="magma")(20)
    my.pch <- c(15:16)
    plot(as.numeric(bball$gdd) ~ as.numeric(as.factor(bball$spp)), col=my.pal[as.factor(bball$spp)], 
         pch=my.pch[as.factor(bball$site)], data = bball[(bball$method==1),], main="Weather Station",
         ylab="GDD", ylim=c(0, 600), xlab="Species")
    abline(h=mean(bball$gdd[bball$method=="ws"]), lwd=3)
    
    plot(as.numeric(gdd) ~ as.numeric(as.factor(spp)), col=my.pal[as.factor(bball$spp)], 
         pch=my.pch[as.factor(bball$site)], data = bball[(bball$method==0),], main="Hobo Logger",
         ylab="GDD", ylim=c(0, 600), xlab="Species")
    abline(h=mean(bball$gdd[bball$method=="hobo"]), lwd=3)
  })
  
  #observeEvent(input$run, {
  output$climtypes <- renderPlot({
    clim <- get.data()[[2]]
    cols <-viridis_pal(option="viridis")(3)
    ws <- ggplot(clim[(clim$method=="ws"),], aes(x=tmean)) + geom_histogram(aes(fill=site)) + theme_classic() +
      scale_fill_manual(name="Site", values=cols, labels=sort(unique(clim$site))) + ggtitle("Weather Station") +
      coord_cartesian(xlim=c(-10, 25)) + xlab("Mean Temp (C)") + ylab("")
    
    hl <- ggplot(clim[(clim$method=="hobo"),], aes(x=tmean)) + geom_histogram(aes(fill=site)) + theme_classic() +
      scale_fill_manual(name="Site", values=cols, labels=sort(unique(clim$site))) + ggtitle("Hobo Logger") +
      coord_cartesian(xlim=c(-10, 25)) + xlab("Mean Temp (C)") + ylab("")
    
    #quartz(width=6, height=4)
    grid.arrange(ws, hl, ncol=2)
  })
  #})
  
  #observeEvent(input$run, {
  output$hist <- renderPlot(res=150, height=500, width=500,{
    bball <- get.data()[[1]]
    cols <-viridis_pal(option="plasma")(3)
    ggplot(bball, aes(x=bb)) + geom_histogram(aes(fill=site)) + theme_classic() + theme(legend.position = "none") +
      scale_fill_manual(name="Site", values=cols, labels=sort(unique(bball$site))) +
      coord_cartesian(xlim=c(0, 100)) + xlab("Day of budburst (C)") + ylab("") +
      geom_text(label=paste0("Arb obs:",nrow(bball[bball$site=="arb",])), col=cols[[1]], aes(x = 80, y = 100)) +
      geom_text(label=paste0("Arb NAs:",nrow(bball[is.na(bball$site=="arb"),])), col=cols[[1]], aes(x = 79, y = 90)) +
      geom_text(label=paste0("HF obs:",nrow(bball[bball$site=="hf",])), col=cols[[2]], aes(x = 80, y = 80)) +
      geom_text(label=paste0("HF NAs:",nrow(bball[is.na(bball$site=="hf"),])), col=cols[[2]], aes(x = 79, y = 70)) 
  })
  #})
  
  #observeEvent(input$run, {
  output$interaction <- renderPlot({
    bball.site <- get.data()[[1]]
    bball.site$methodtype <- ifelse(bball.site$method=="ws", "\nWeather \nStation", "\nHobo \nLogger")
    
    cols <- viridis_pal(option="plasma")(3)
    gddcomparebb <- ggplot(bball.site, aes(x=methodtype, y=gdd, group=as.factor(site), fill=as.factor(site))) + 
      geom_ribbon(stat='smooth', method = "lm", se=TRUE, alpha=1, 
                  aes(fill = as.factor(site), group = as.factor(site))) +
      geom_line(stat='smooth', method = "lm", alpha=1, col="black") +
      theme(panel.background = element_blank(), axis.line = element_line(colour = "black"),
            legend.text.align = 0,
            legend.key = element_rect(colour = "transparent", fill = "white"),
            plot.margin = margin(0.5, 0.5, 0.5, 1, "cm")) +
      xlab("") + 
      ylab("Growing degree days to budburst") + 
      scale_fill_manual(name="Site", values=cols,
                        labels=c("Arnold Arboretum", "Harvard Forest")) + 
      coord_cartesian(expand=0, ylim=c(0,700))
    
    gddcomparebb
  })
  
  output$interactionsims <- renderPlot({
    bball.site <- get.datasims()[[1]]
    bball.site$methodtype <- ifelse(bball.site$method=="ws", "\nWeather \nStation", "\nHobo \nLogger")
    
    cols <- viridis_pal(option="plasma")(3)
    gddcomparebb <- ggplot(bball.site, aes(x=methodtype, y=gdd, group=as.factor(site), fill=as.factor(site))) + 
      geom_ribbon(stat='smooth', method = "lm", se=TRUE, alpha=1, 
                  aes(fill = as.factor(site), group = as.factor(site))) +
      geom_line(stat='smooth', method = "lm", alpha=1, col="black") +
      theme(panel.background = element_blank(), axis.line = element_line(colour = "black"),
            legend.text.align = 0,
            legend.key = element_rect(colour = "transparent", fill = "white"),
            plot.margin = margin(0.5, 0.5, 0.5, 1, "cm")) +
      xlab("") + 
      ylab("Growing degree days to budburst") + 
      scale_fill_manual(name="Site", values=cols,
                        labels=c("Arnold Arboretum", "Harvard Forest")) + 
      coord_cartesian(expand=0, ylim=c(0,700))
    
    gddcomparebb
  })
  
  output$interactionreal <- renderPlot({
    bball.site <- get.datareal
    bball.site$methodtype <- ifelse(bball.site$method==1, "\nWeather \nStation", "\nHobo \nLogger")
    bball.site$site <- bball.site$urban
    
    cols <- viridis_pal(option="plasma")(3)
    gddcomparebb <- ggplot(bball.site, aes(x=methodtype, y=gdd, group=as.factor(site), fill=as.factor(site))) + 
      geom_ribbon(stat='smooth', method = "lm", se=TRUE, alpha=1, 
                  aes(fill = as.factor(site), group = as.factor(site))) +
      geom_line(stat='smooth', method = "lm", alpha=1, col="black") +
      theme(panel.background = element_blank(), axis.line = element_line(colour = "black"),
            legend.text.align = 0,
            legend.key = element_rect(colour = "transparent", fill = "white"),
            plot.margin = margin(0.5, 0.5, 0.5, 1, "cm")) +
      xlab("") + 
      ylab("Growing degree days to budburst") + 
      scale_fill_manual(name="Site", values=cols,
                        labels=c("Arnold Arboretum", "Harvard Forest")) + 
      coord_cartesian(expand=0, ylim=c(0,700))
    
    gddcomparebb
  })
  
  
  use.urban <- eventReactive(input$go,{if(input$Hypothesis=="Hypothesis Hobo Logger: hobo loggers are more accurate")
  {"urban"}else if(input$Hypothesis=="Hypothesis Hobo Logger: weather station is more accurate")
  {"urban"}else if(input$Hypothesis=="Hypothesis Urban: urban sites require fewer GDDs"){"urban"}else 
    if(input$Hypothesis=="Hypothesis Provenance: more Northern provenances require fewer GDDs"){"prov"}
  })
  
  
  observeEvent(input$go, {
  output$muplot <- renderPlot(height=650,{
    use.urban <- use.urban()[1]
      bball <- get.data()[[1]]
      bball$treatmenttype <- if(use.urban=="urban"){ifelse(bball$site=="arb", 1, 0)}else if(use.urban=="prov"){
                                    as.numeric(bball$prov)}
      
      datalist.gdd <- with(bball, 
                           list(y = gdd, 
                                urban = treatmenttype,
                                method = type,
                                sp = as.numeric(as.factor(species)),
                                N = nrow(bball),
                                n_sp = length(unique(bball$species))
                           )
      )
      
      progress <- Progress$new(max=10)
      on.exit(progress$close())
      
      progress$set(message = "Running rStan Model", 
                   detail="\nThis may take a while...")
      
      urbmethod_fake = stan('~/Documents/git/microclimates/analyses/stan/urbanmethod_normal_ncp_inter.stan', data = datalist.gdd,
                                           iter = 1000, warmup=500, chains=4)#, control=list(adapt_delta=0.99, max_treedepth=15)) ### 
                     
      
  
    cols <- adjustcolor("indianred3", alpha.f = 0.3) 
    my.pal <-rep(viridis_pal(option="viridis")(9),2)
    my.pch <- rep(15:18, each=10)
    alphahere = 0.4
    
    modoutput <- summary(urbmethod_fake)$summary
    noncps <- modoutput[!grepl("_ncp", rownames(modoutput)),]
    use.urban <- use.urban()[1]
    labs <- if(use.urban=="urban"){c("Arboretum", "Weather Station", "Arboretum x\nWeather Station",
                                         "Sigma Arboretum", "Sigma \nWeather Station", 
                                         "Sigma Interaction")}else if(use.urban=="prov"){
                   c("Provenance", "Weather Station", "Provenance x\nWeather Station",
                     "Sigma Provenance", "Sigma \nWeather Station", 
                     "Sigma Interaction")}
    
    modelhere <- urbmethod_fake
    bball <- isolate(get.data()[[1]])
    spnum <- length(unique(bball$species))
    par(xpd=FALSE)
    par(mar=c(5,10,3,10))
    plot(x=NULL,y=NULL, xlim=c(-100,100), yaxt='n', ylim=c(0,6),
         xlab="Model estimate change in growing degree days to budburst", ylab="")
    axis(2, at=1:6, labels=rev(labs), las=1)
    abline(v=0, lty=2, col="darkgrey")
    rownameshere <- c("mu_b_urban_sp", "mu_b_method_sp", "mu_b_um_sp", "sigma_b_urban_sp",
                      "sigma_b_method_sp", "sigma_b_um_sp")
    for(i in 1:6){
      pos.y<-(6:1)[i]
      pos.x<-noncps[rownameshere[i],"mean"]
      lines(noncps[rownameshere[i],c("25%","75%")],rep(pos.y,2),col="darkgrey")
      points(pos.x,pos.y,cex=1.5,pch=19,col="darkblue")
      for(spsi in 1:spnum){
        pos.sps.i<-which(grepl(paste0("[",spsi,"]"),rownames(noncps),fixed=TRUE))[2:4]
        jitt<-(spsi/40) + 0.08
        pos.y.sps.i<-pos.y-jitt
        pos.x.sps.i<-noncps[pos.sps.i[i],"mean"]
        lines(noncps[pos.sps.i[i],c("25%","75%")],rep(pos.y.sps.i,2),
              col=alpha(my.pal[spsi], alphahere))
        points(pos.x.sps.i,pos.y.sps.i,cex=0.8, pch=my.pch[spsi], col=alpha(my.pal[spsi], alphahere))
        
      }
    }
    par(xpd=TRUE) # so I can plot legend outside
    legend(120, 6, sort(unique(gsub("_", " ", bball$species))), pch=my.pch[1:spnum],
           col=alpha(my.pal[1:spnum], alphahere),
           cex=1, bty="n", text.font=3)
  })
  })
  
  use.sims <- eventReactive(input$simsrunmod,{if(input$Question=="Urban Model"){"urban"}else if(input$Question=="Provenance Latitude Model"){"prov"}
  })
  
  observeEvent(input$simsrunmod, {
    output$simsmuplot <- renderPlot(height=650,{
      use.sims <- use.sims()[1]
      bball <- get.datasims()[[1]]
      bball$treatmenttype <- if(use.sims=="urban"){ifelse(bball$site=="arb", 1, 0)}else if(use.sims=="prov"){
        as.numeric(bball$prov)}
      
      datalist.gdd <- with(bball, 
                           list(y = gdd, 
                                urban = treatmenttype,
                                method = type,
                                sp = as.numeric(as.factor(species)),
                                N = nrow(bball),
                                n_sp = length(unique(bball$species))
                           )
      )
      
      
      progress <- Progress$new(max=10)
      on.exit(progress$close())
      
      progress$set(message = "Running rStan Model", 
                   detail="\nThis may take a while...")
      
      urbmethod_fake = stan('~/Documents/git/microclimates/analyses/stan/urbanmethod_normal_ncp_inter.stan', data = datalist.gdd,
                            iter = 1000, warmup=500, chains=4)#, control=list(adapt_delta=0.99, max_treedepth=15)) ### 
      
      
        
        
      #})
      
      cols <- adjustcolor("indianred3", alpha.f = 0.3) 
      my.pal <-rep(viridis_pal(option="viridis")(9),2)
      my.pch <- rep(15:18, each=10)
      alphahere = 0.4
      
      modoutput <- summary(urbmethod_fake)$summary
      noncps <- modoutput[!grepl("_ncp", rownames(modoutput)),]
      use.sims <- use.sims()[1]
      labs <- if(use.sims=="urban"){c("Arboretum", "Weather Station", "Arboretum x\nWeather Station",
                                       "Sigma Arboretum", "Sigma \nWeather Station", 
                                       "Sigma Interaction")}else if(use.sims=="prov"){
                                         c("Provenance", "Weather Station", "Provenance x\nWeather Station",
                                           "Sigma Provenance", "Sigma \nWeather Station", 
                                           "Sigma Interaction")}
      
      modelhere <- urbmethod_fake
      bball <- isolate(get.datasims()[[1]])
      spnum <- length(unique(bball$species))
      par(xpd=FALSE)
      par(mar=c(5,10,3,10))
      plot(x=NULL,y=NULL, xlim=c(-100,100), yaxt='n', ylim=c(0,6),
           xlab="Model estimate change in growing degree days to budburst", ylab="")
      axis(2, at=1:6, labels=rev(labs), las=1)
      abline(v=0, lty=2, col="darkgrey")
      rownameshere <- c("mu_b_urban_sp", "mu_b_method_sp", "mu_b_um_sp", "sigma_b_urban_sp",
                        "sigma_b_method_sp", "sigma_b_um_sp")
      for(i in 1:6){
        pos.y<-(6:1)[i]
        pos.x<-noncps[rownameshere[i],"mean"]
        lines(noncps[rownameshere[i],c("25%","75%")],rep(pos.y,2),col="darkgrey")
        points(pos.x,pos.y,cex=1.5,pch=19,col="darkblue")
        for(spsi in 1:spnum){
          pos.sps.i<-which(grepl(paste0("[",spsi,"]"),rownames(noncps),fixed=TRUE))[2:4]
          jitt<-(spsi/40) + 0.08
          pos.y.sps.i<-pos.y-jitt
          pos.x.sps.i<-noncps[pos.sps.i[i],"mean"]
          lines(noncps[pos.sps.i[i],c("25%","75%")],rep(pos.y.sps.i,2),
                col=alpha(my.pal[spsi], alphahere))
          points(pos.x.sps.i,pos.y.sps.i,cex=0.8, pch=my.pch[spsi], col=alpha(my.pal[spsi], alphahere))
          
        }
      }
      par(xpd=TRUE) # so I can plot legend outside
      legend(120, 6, sort(unique(gsub("_", " ", bball$species))), pch=my.pch[1:spnum],
             col=alpha(my.pal[1:spnum], alphahere),
             cex=1, bty="n", text.font=3)
    })
  })

  use.real <- eventReactive(input$realrunmod,{if(input$type=="Urban Model"){"urban"}else if(input$type=="Provenance Latitude Model"){"prov"}
  })
  
  observeEvent(input$realrunmod, {
    output$realmuplot <- renderPlot(height=650, width=750,{
      use.real <- use.real()[1]
      bball <- get.datareal
      bball$treatmenttype <- if(use.real=="urban"){as.numeric(bball$urban)}else if(use.real=="prov"){
        as.numeric(bball$provenance)}
      
      datalist.gdd <- with(bball, 
                           list(y = gdd, 
                                urban = treatmenttype,
                                method = method,
                                sp = as.numeric(as.factor(spp)),
                                N = nrow(bball),
                                n_sp = length(unique(bball$spp))
                           )
      )
      
      
      progress <- Progress$new(max=10)
      on.exit(progress$close())
      
      progress$set(message = "Running rStan Model", 
                   detail="This may take a while...")
      
      urbmethod_fake = stan('~/Documents/git/microclimates/analyses/stan/urbanmethod_normal_ncp_inter.stan', data = datalist.gdd,
                            iter = 1000, warmup=500, chains=4)#, control=list(adapt_delta=0.99, max_treedepth=15)) ### 
      
      
      
      
      #})
      
      cols <- adjustcolor("indianred3", alpha.f = 0.3) 
      my.pal <-rep(viridis_pal(option="viridis")(9),2)
      my.pch <- rep(15:18, each=10)
      alphahere = 0.4
      
      modoutput <- summary(urbmethod_fake)$summary
      noncps <- modoutput[!grepl("_ncp", rownames(modoutput)),]
      use.real <- use.real()[1]
      labs <- if(use.real=="urban"){c("Arboretum", "Weather Station", "Arboretum x\nWeather Station",
                                      "Sigma Arboretum", "Sigma \nWeather Station", 
                                      "Sigma Interaction")}else if(use.real=="prov"){
                                        c("Provenance", "Weather Station", "Provenance x\nWeather Station",
                                          "Sigma Provenance", "Sigma \nWeather Station", 
                                          "Sigma Interaction")}
      
      modelhere <- urbmethod_fake
      bball <- get.datareal
      spnum <- length(unique(bball$spp))
      par(xpd=FALSE)
      par(mar=c(5,10,3,10))
      plot(x=NULL,y=NULL, xlim=if(use.real=="prov"){c(-30,30)} else if(use.real=="urban"){c(-100,100)}, yaxt='n', ylim=c(0,6),
           xlab="Model estimate change in growing degree days to budburst", ylab="")
      axis(2, at=1:6, labels=rev(labs), las=1)
      abline(v=0, lty=2, col="darkgrey")
      rownameshere <- c("mu_b_urban_sp", "mu_b_method_sp", "mu_b_um_sp", "sigma_b_urban_sp",
                        "sigma_b_method_sp", "sigma_b_um_sp")
      for(i in 1:6){
        pos.y<-(6:1)[i]
        pos.x<-noncps[rownameshere[i],"mean"]
        lines(noncps[rownameshere[i],c("25%","75%")],rep(pos.y,2),col="darkgrey")
        points(pos.x,pos.y,cex=1.5,pch=19,col="darkblue")
        for(spsi in 1:spnum){
          pos.sps.i<-which(grepl(paste0("[",spsi,"]"),rownames(noncps),fixed=TRUE))[2:4]
          jitt<-(spsi/40) + 0.08
          pos.y.sps.i<-pos.y-jitt
          pos.x.sps.i<-noncps[pos.sps.i[i],"mean"]
          lines(noncps[pos.sps.i[i],c("25%","75%")],rep(pos.y.sps.i,2),
                col=alpha(my.pal[spsi], alphahere))
          points(pos.x.sps.i,pos.y.sps.i,cex=0.8, pch=my.pch[spsi], col=alpha(my.pal[spsi], alphahere))
          
        }
      }
      par(xpd=TRUE) # so I can plot legend outside
      legend(if(use.real=="prov"){40}else if(use.real=="urban"){120}, 6, sort(unique(gsub("_", " ", bball$spp))), pch=my.pch[1:spnum],
             col=alpha(my.pal[1:spnum], alphahere),
             cex=1, bty="n", text.font=3)
    })
  })
  
  
  observeEvent(input$warmrun, {
    output$gddwarm <- renderPlot({
      gdd.warm <- get.warmsims()[[1]]
      
      ggplot(gdd.warm, aes(as.numeric(warming), abs(gdd_accuracy), col=fstarspp, group=fstarspp)) +
        geom_line(stat='smooth', method = "loess", alpha=1, aes(col=fstarspp)) +
        theme(panel.background = element_blank(), axis.line = element_line(colour = "black"),
              legend.text.align = 0,
              legend.key = element_rect(colour = "transparent", fill = "white"),
              plot.margin = margin(0.5, 0.5, 0.5, 1, "cm")) + 
        xlab("Amount of warming (°C)") + ylab("GDD accuracy") +
        scale_x_continuous(breaks = sort(c(seq(0, 10, by=1)))) +
        scale_color_viridis_c("Species GDD Threshold")
      
      
    })
  })
  
  
  
}

shinyApp(ui = ui, server = server)

#runApp("~/Documents/microapp/")
