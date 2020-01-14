require(dplyr)
require(ggplot2)
if (!exists("intVec")){
  require(Rcpp)
  sourceCpp("integration.cpp")}
transformations <- list("None" = c("IntertidalArea",
                                   "SESA.Zeros",
                                   "Area", 
                                   "Birds"),
                        "logArea" = c("log10(IntertidalArea)",
                                  "SESA.Zeros",
                                  'log10(Area)',
                                  "Birds"),
                        "sqrt" = c("sqrt(IntertidalArea)",
                                   "SESA.Zeros",
                                   'sqrt(Area)',
                                   "Birds"),
                        "loglog" = c("log10(IntertidalArea)", 
                                     " log10(SESA.Zeros+1)",
                                     "log10(Area)",
                                     "log10(Birds + 1)"),
                        "logBirds"= c("IntertidalArea",
                                  "log10(SESA.Zeros+1)",
                                  'Area',
                                  "log10(Birds+1)"))





calculateDstat <- function(yr, data, method='trapezoid', useArea = FALSE){
  analysisData <- data %>% filter(Year == yr) %>% mutate(cdiff = cpropBirds -expectedpropBirds ,
                                                         diff = propBirds- propArea)
  


  ########### Calc AUC
  # Here is the key function that calculates the area under the curve of the expected distribution
  # vs. the observed distribution.
  if(isTRUE(useArea)) {
    aucExp <- intVec(analysisData$pArea,analysisData$expectedpropBirds, method = method)
    #caTools::trapz(analysisData$propSafety, analysisData$expectedpropBirds) 
    aucObs <- intVec(analysisData$pArea, analysisData$cpropBirds)
    #caTools::trapz(analysisData$propSafety, analysisData$cpropBirds)
  } else{
  aucExp <- intVec(analysisData$propSafety,analysisData$expectedpropBirds, method = method)
        #caTools::trapz(analysisData$propSafety, analysisData$expectedpropBirds) 
  aucObs <- intVec(analysisData$propSafety, analysisData$cpropBirds)
        #caTools::trapz(analysisData$propSafety, analysisData$cpropBirds)
  }
  aucRatio <- aucObs / aucExp
  maxSafe <- max(analysisData$propSafety, na.rm=T)
  minSafe <- min(analysisData$propSafety, na.rm=T)
  meanSiteD <- mean(analysisData$propDanger, na.rm=T)
  # cat(aucExp, aucObs, sep = "\n")
  maxPropDanger <- filter(analysisData, abs(diff) == max(abs(diff),na.rm=T)) %>% 
    select(diff, propDanger, propArea, Year)
  # print(maxPropDanger)
  # line <- readline()
  lgsitepresent <- ifelse(("45.72_-64.65" %in% analysisData$Site.Code | "45.81_-64.5" %in% analysisData$Site.Code), TRUE, FALSE)

  output.df <- data.frame(Year = yr,  aucExp = aucExp, aucObs = aucObs,meanSiteD = meanSiteD, maxSafe, minSafe,
  							MAPT_GRAN = lgsitepresent,
 							 #D_stat =dStat, pvalue = ks.output2sided$p.value,
                          aucRatio = aucRatio) %>% 
    full_join(maxPropDanger, by ="Year")
  return(output.df)
}

cutwidth <- function(x, width, center){
  x_range <- range(x, na.rm = TRUE, finite = TRUE)
  boundary <- center - width/2
  boundary <- as.numeric(boundary)
  min_x <- ggplot2:::find_origin(x_range, width, boundary)
  max_x <- max(x, na.rm = TRUE) + (1 - 1e-08) * width
  breaks <- seq(min_x, max_x, width)
  cut(x, breaks, include.lowest = TRUE, right = F, labels = F)
}



summarizeData <- function(data, seed, 
                            plotOut = FALSE, 
                            transform=FALSE, 
                            calcTrends = FALSE, 
                            average = FALSE, 
                            useMice = FALSE,  
                            modelType = "lm",
                          catalogueOnly = TRUE,
                          binIt = FALSE,
                          binSize=NULL,
                          yrbinSize = NULL,
                          areaNumBins = NULL,
                          useArea = FALSE,
                          rad = 2500){
  require(dplyr)
  require(ggplot2)
  set.seed(seed)
  if (isTRUE(useMice)){ # No longer supported. Attempts to fill in missing data.
    stop('Mice function depreciated')
    # require(mice)
    # m <- runif(1, 0,20)
    # data <- complete(data, m) %>% left_join(danger, by = c("Site.Code"= "SITE")) %>%
    #   mutate(Site.ID = SITEID,
    #          Site.Name= SITENAME,
    #          proportionDanger = proportionDanger.x)
  }
  if(identical(average, FALSE)){ # No longer updated. Used to sample 1 from each site rather than averaging.
    stop('Sampling depreciated. Use avaerage')
    # if (isTRUE(catalogueOnly)) {
    #   subSampleData <- data %>%
    #     group_by(Year, Site.Code, Site.ID, Site.Name, Lat, Lon, SiteName, HighDanger, IntertidalArea, proportionDanger) %>%
    #     sample_n(1) 
    # } else{
    # subSampleData <- data %>%
    #   group_by(Year, Lat, Lon) %>%
      
    #   sample_n(1) 
    # }
  }
  if (isTRUE(average)){ # Should always be taking the average
    if (identical(catalogueOnly, FALSE)) { # Using not only catalogue sites  Depreciated also I believe.
      stop('None catalogue function not relevant for total PRISM data depreciated')
    } else{ # If only using catalogue sites. Now used for all functions
      subSampleData <- data %>%
        group_by(Year, Site.Code, Site.ID, Site.Name, Lat, Lon, SiteName, HighDanger, IntertidalArea, proportionDanger) %>%
        summarize(
          SESA.Zeros = mean(SESA.Zeros, na.rm=T),
          sd.count = sd(SESA.Zeros, na.rm=T),
          n.days = n(),
          mean.date = mean(DayNum, na.rm=T)
        ) %>% ungroup
      duplicates <- subSampleData %>% group_by(Site.Code, Year) %>% dplyr::summarize(n.sites = n()) %>% filter(n.sites >1)
      if(nrow(duplicates)>0) stop("Duplicates in site codes in subSampleData")
    }
    }
    # Calculate the total area surveyed in each year and total number of birds surveyed in each year
    subSampleCounts <-
      subSampleData %>%
      group_by(Year) %>%
      mutate_(TotalSurveyed =paste0("sum(",transformations[[transform]][1],", na.rm=T)"),
             TotalBirdsCounted = paste0("sum(",transformations[[transform]][2],", na.rm=T)")) %>% ungroup %>%
      rename(propDanger = proportionDanger,
             Area = IntertidalArea,
             AreaDanger = HighDanger,
             Birds = SESA.Zeros
      ) %>% ungroup %>%
      mutate_(Area = transformations[[transform]][3],
              Birds = transformations[[transform]][4]) #%>% mutate(Area = Area - AreaDanger) ## CHanged to test for only safe area

    if (isTRUE(binIt)){
      dangerBins <- seq(0,1, binSize) + binSize/2
      yearBins <- seq(min(data$Year), max(data$Year), yrbinSize)
      areaBins <- seq(min(data$IntertidalArea), max(data$IntertidalArea), length.out = areaNumBins)
      calcMid <- function(a){a[-length(a)] + diff(a)/2}
      # print(dangerBins)
      summarizedCount <-  
              subSampleCounts %>%
              mutate(propDanger.bin = ifelse(rep(binSize < 0.001,nrow(.)), propDanger,
                                             dangerBins[cutwidth(propDanger, binSize, center = binSize/2)]),
                                             # cut_width(propDanger, binSize, center = binSize/2, labels =F)),
                                             # calcMid(dangerBins)[cut(propDanger,dangerBins, include.lowest = F)]),
                     Year.bin = ifelse(rep(yrbinSize <= 1,nrow(.)), Year, 
                                       calcMid(yearBins)[cut(Year,yearBins, include.lowest = T)]),
                     Area.bin = ifelse(rep(areaNumBins >= 500,nrow(.)),
                                       Area, calcMid(areaBins)[cut(Area,areaBins, include.lowest = T)])) %>%
              group_by(Year, propDanger.bin) %>% 
              mutate(n.sites = n()) %>%
              sample_n(1) %>% #min(ungroup(.)[['n.sites']], na.rm=T)) %>%
              group_by(Year.bin) %>% 
              mutate(TotalSurveyed = sum(Area.bin), TotalBirdsCounted = sum(Birds)
              	) %>%
              group_by(Site.Code, propDanger, Area.bin, Birds, Year.bin, propDanger.bin, Year) %>%
              summarize(
                # TotalSurveyed = mean(TotalSurveyed),
                propArea = Area.bin / mean(TotalSurveyed) ,
                propBirds = Birds / mean(TotalBirdsCounted),
                DayNum = mean(mean.date),
                Lat = mean(Lat),
                Lon = mean(Lon)) %>% 
              ungroup %>% mutate(Year = Year.bin, Area = Area.bin, propDanger = propDanger.bin)
      } else{
  summarizedCount <-  
    subSampleCounts %>%
    group_by(Year, Site.Code, Site.ID, Site.Name, Lat, Lon, SiteName, AreaDanger, Area, propDanger, TotalBirdsCounted) %>%
    summarize(
      propArea = Area / TotalSurveyed ,
      propBirds = Birds / TotalBirdsCounted,
      Birds = Birds,
      pArea = Area / (pi*rad**2)#,
      # TotalBirdsCounted =  TotalBirdsCounted
    ) 
      }
    if(isTRUE(useArea)){
      summarizedCountByYear <- summarizedCount %>%
        group_by(Year) %>%
        arrange(pArea) %>%
        mutate(cpropBirds = cumsum(propBirds), # Take the cumulative sum of birds counted
               
               expectedpropBirds = cumsum(propArea ), # Take the cumulative sum of area counted
               cArea = cumsum(Area),
               cBirds = cumsum(Birds),
               propSafety = 1 - propDanger) %>% ungroup 
    }else{
    summarizedCountByYear <- summarizedCount %>%
    group_by(Year) %>%
      
    arrange(desc(propDanger))%>% 
    # observed
    mutate(cpropBirds = cumsum(propBirds), # Take the cumulative sum of birds counted
           expectedpropBirds = cumsum(propArea ), # Take the cumulative sum of area counted
           cArea = cumsum(Area),
           cBirds = cumsum(Birds),
           propSafety = 1 - propDanger) %>% ungroup }
  # View(summarizedCountByYear)
  return(summarizedCountByYear)
  
  
}

calculateDstats <- function(data, seed, 
                            plotOut = FALSE, 
                            transform="None", 
                            calcTrends = FALSE, 
                            average = FALSE, 
                            useMice = FALSE,  
                            modelType = "lm", binIt=FALSE, binSize = NULL, yrbinSize=NULL,areaNumBins=NULL, 
                            trap.method='trapezoid', useArea = FALSE, rad_=2500){
  
  summarizedCountByYear <- summarizeData(data, seed, 
                                         plotOut, 
                                         transform, 
                                         calcTrends, 
                                         average, 
                                         useMice,  
                                         modelType, binIt=binIt, binSize=binSize, yrbinSize=yrbinSize, 
                                         areaNumBins=areaNumBins, useArea = useArea, rad=rad_)

  if(isTRUE(plotOut)){
    if(isTRUE(useArea)){
      cplot <- ggplot(summarizedCountByYear, aes(Area, cpropBirds)) + geom_line() + facet_wrap(~Year) + 
        geom_line(colour = 'red', aes(Area, expectedpropBirds))  + #theme_few() +
        labs(x = "Area of intertidal", y = "Cumulative proportion of total birds") 
    } else{
    cplot <- ggplot(summarizedCountByYear, aes(propSafety, cpropBirds)) + geom_line() + facet_wrap(~Year) + 
            geom_line(colour = 'red', aes(propSafety, expectedpropBirds))  + #theme_few() +
      labs(x = "Proportion of Site Safe", y = "Cumulative proportion of total birds") }
  
  
  }
  

  years2Analyze <- unique(summarizedCountByYear$Year)
  require(purrr)
 runDF <-  map_df(years2Analyze, calculateDstat, data = summarizedCountByYear, method =trap.method, useArea = useArea)

  if(isTRUE(plotOut)) {
          # print(ggplot(runDF, aes(Year, D_stat)) + geom_point() + geom_smooth(method="lm") + ylim(-1,1))
    aucPlot <- ggplot(runDF, aes(Year, aucRatio)) + geom_point() + #geom_smooth(method='lm', se=F, alpha =0.3) +

      ylab("Ratio of Areas under the curve (Observed) / Expected)")  + geom_abline(slope = 0, intercept = 1.0, colour = 'red')
    if(nrow(filter(runDF, !is.na(aucRatio))) >= 3){ aucPlot <- aucPlot + geom_smooth(method='lm', se=F, alpha =0.3) }
  }
  if (calcTrends){
    if (modelType == 'lm'){
    run.lm <- lm(aucRatio ~ Year, data = runDF)
    }
    if (modelType == 'lm2') {
      run.lm <- lm(aucRatio ~ Year + Year^2, data = runDF)
    }
    if (modelType == 'gam') {
      run.lm <- gam::gam(aucRatio ~ poly(Year,2), data = runDF)
    }
  }
  runDF$seed <- seed
  outputDF <- runDF %>% arrange(Year)
  if(isTRUE(plotOut)) {
    gridExtra::grid.arrange(cplot, aucPlot, ncol=2)
  }
  return(outputDF)
}

boot.fun <- function(data, i, dataTransform){
  df <- data[i,] %>% group_by(Year) %>% mutate(lg = 
  ifelse("45.72_-64.65" %in% Site.ID | "45.81_-64.5" %in% Site.ID, 0, 1)) %>% ungroup
  if(sum(df$lg) > 8){
  output <- calculateDstats(data = df, plotOut = F, transform = dataTransform, average = T, seed = 565465)
  if (nrow(output) != length(unique(data$Year))) {return(rep(NA, length(unique(data$Year))))} 
  else{return(output$aucRatio)}
  } else{return(rep(NA, length(unique(data$Year))))}
}

extract.ci <- function(boot.output , i, Print=F) {
  require(boot)
  ci.boot <- boot.ci(boot.output, type = c("norm", "basic", "perc"), index = i) 
  out.df <- data.frame(aucRatio = as.numeric(boot.output$t0[i]),
                       boot.lci.n = ci.boot$normal[2],
                       boot.uci.n = ci.boot$normal[3],
                       boot.lci.basic = ci.boot$basic[4],
                       boot.uci.basic = ci.boot$basic[5],
                       boot.lci.per = ci.boot$percent[4],
                       boot.uci.per = ci.boot$percent[5])
  if (Print) {plot(boot.output, index = i, main = paste(1974 + i))}
  return(out.df)
}

