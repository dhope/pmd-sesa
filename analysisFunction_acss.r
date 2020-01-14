calculateNewDataAnalysis <- function(dat, covars, outlier = 1995, plotOut= FALSE, binIt=FALSE, binD=NULL, seed = 57954 , species = NULL, lgonly = T, useArea = F, rad_=2500){
	dat <- filter(dat, proportionDanger != 0 )
	# require(ggthemes)
	if ( !exists("calculateDStats") ) source("../Rscripts/dstats.r")
	yrs.w.lg <-  dat %>% group_by(Year) %>% 
		summarize(lgsitepresent = ifelse(("45.72_-64.65" %in% Site.ID | "45.81_-64.5" %in% Site.ID), TRUE, FALSE)) %>% 
			filter(lgsitepresent) %>% .[['Year']]
	if(!isTRUE(lgonly)){yrs.w.lg <- unique(dat$Year)}
	largeSites <- c("45.72_-64.65" , "45.81_-64.5" )
	LgCounts <-  dat %>%
		  mutate(LgSite = ifelse(Site.ID %in% largeSites, 'Yes', 'No')) %>%
		  filter(LgSite == 'Yes') %>% group_by(Year, Site.Code) %>%
		  summarise(mean.count = mean(SESA.Zeros)) %>% group_by(Year) %>%
		  summarize(LgSiteCount = sum(mean.count)) %>% ungroup
		  
	yearly_summary <- dat %>%
	  group_by(Site.Code, Year) %>%
	  summarise(mean.count = mean(SESA.Zeros),
				mean.DoY = mean(DayNum),
				n.counts = n(), 
				danger=mean(proportionDanger)) %>%
	  group_by(Year) %>%
	  summarise(mean.counts = mean(n.counts),
				n.sites = n(),
				mean.mean.Doy = mean(mean.DoY),
				mean.mean.count = mean(mean.count),
				sum.count = sum(mean.count),
				mean.danger = mean(danger),
				max.danger = max(danger),
				ratio.danger = max.danger/ mean.danger)
	
	AUC <- calculateDstats(data = dat, seed = seed, plotOut = F, 
                            transform = "None", average = T,useMice = F,calcTrends = F,modelType = "lm", binIt = binIt,binSize = binD,
	                       yrbinSize = 1, areaNumBins = 5000, useArea = useArea, rad_=rad_) %>%
							filter(Year %in% yrs.w.lg & !Year %in% outlier) %>% mutate(Yr.st = arm::rescale(Year))
	sumYear <- summarizeData(dat,seed = seed, average = T,binIt = binIt, transform = "None", 
	                         binSize = binD, yrbinSize = 1, areaNumBins = 5000, useArea = useArea, rad=rad_)	
	if(nrow(AUC) < 2){return(AUC)}
	if(isTRUE(binIt)) {
	  yearly_summary <- 
	    sumYear %>%
	    group_by(Site.Code, Year) %>%
	    summarise(mean.count = mean(Birds),
	              mean.DoY = mean(DayNum),
	              n.counts = n(), 
	              danger=mean(propDanger)) %>%
	    group_by(Year) %>%
	    summarise(mean.counts = mean(n.counts),
	              n.sites = n(),
	              mean.mean.Doy = mean(mean.DoY),
	              mean.mean.count = mean(mean.count),
	              sum.count = sum(mean.count),
	              mean.danger = mean(danger),
	              max.danger = max(danger),
	              ratio.danger = max.danger/ mean.danger)
	}
	
	AUC.for.Analysis <- 
		AUC %>%
		  # left_join(select(AUC.results.w.bootstrap, -aucRatio, -aucExp, -aucObs), by = 'Year') %>% 
		  filter(Year %in% yrs.w.lg & !Year %in% outlier) %>%
		  # mutate( = mean.counts / n.sites) %>% 
		  left_join(LgCounts, by  = 'Year') %>%
		  left_join(yearly_summary, by = "Year") %>%
		  left_join(covars$hawkYr50, by = 'Year') %>% 
	    left_join(covars$nj_hawk, by = 'Year') %>% 
		  left_join(dat %>% group_by(Year) %>% summarise(SD_Doy = sd(DayNum),
																				   CV  = SD_Doy / mean(DayNum),
																				   CV_ubiased = CV * (1+1/(4*n()))), by = "Year") %>% 
		  left_join(sumYear %>% group_by(Year) %>% summarize(Lat = mean(Lat), Lon = mean(Lon)), by = "Year") %>% 
		  mutate(LgSiteCount = ifelse(is.na(LgSiteCount), 0, log(LgSiteCount))) %>%
		  left_join(covars$falc.predict, by = 'Year') %>%
		  # left_join(stormInc, by = "Year") %>% 
		  # left_join(allStormData, by = 'Year') %>% 
		  left_join(dplyr::select(covars$SESA.index, Year, Index), by = 'Year') %>% 
		  left_join(covars$snowmelt.both, by = "Year") %>% 
		  mutate(#weight = max(0.0001, 1-ci), 
				 Yr.st = arm::rescale(Year), 
				 Lat.st = arm::rescale(Lat),
				 Lon.st = arm::rescale(Lon),
				 BOF.falc.factor = ifelse(Year<=1981,"PreFalc", ifelse(Year<=1991, "Reintroduction", "Post-Reintro")),
				 BOF.falc.BeforeAfter = ifelse(Year<=1982,"PreFalc", "FalconsPresent"))


	AUC.for.Analysis$Index <- with(AUC.for.Analysis, Hmisc::impute(Index, mean))
	AUC.for.Analysis$Index.st <- arm::rescale(AUC.for.Analysis$Index)
	# AUC.for.Analysis$fiftyDay <- arm::rescale(with(AUC.for.Analysis, Hmisc::impute(fiftyDay, mean)))
	AUC.for.Analysis$nfalc <- with(AUC.for.Analysis, Hmisc::impute(nbirds, mean))
	AUC.for.Analysis$nfalc_nj <- arm::rescale(with(AUC.for.Analysis, Hmisc::impute(falc_per_hr, mean)))
	if(nrow(filter(AUC.for.Analysis, !is.na(aucRatio)))<2){
	  return(list("models" = NA, "AUC" =AUC.for.Analysis, "Species" = species ))}
	finalAICmodelSet <- list()
	#surveyModels <- list()
	
	
	
	
	finalAICmodelSet[["survey Global Additive"]] <- lm(aucRatio~ mean.danger + CV + Lat.st*Lon.st + LgSiteCount + n.sites, data =AUC.for.Analysis)
	finalAICmodelSet[["survey Global w interactions"]] <- lm(aucRatio~ CV + Lat.st*Lon.st + LgSiteCount + mean.danger*n.sites, data =AUC.for.Analysis)

	#falconModels <- list()
	# finalAICmodelSet[["Timing"]] <- lm(aucRatio~fiftyDay, data = AUC.for.Analysis)
	# finalAICmodelSet[['Timing by Snowmelt']] <- lm(aucRatio~fiftyDay * snow.sesa, data = AUC.for.Analysis)
	finalAICmodelSet[['Reintroduction']] = lm(aucRatio~BOF.falc.factor, data = AUC.for.Analysis)
	finalAICmodelSet[['Reintroduction with Year']] = lm(aucRatio~BOF.falc.factor* Yr.st, data = AUC.for.Analysis)
	finalAICmodelSet[['Pre/Post Reintroduction']] = lm(aucRatio~BOF.falc.BeforeAfter, data = AUC.for.Analysis)
	finalAICmodelSet[['Pre/Post Reintroduction with Year']] = lm(aucRatio~BOF.falc.BeforeAfter* Yr.st, data = AUC.for.Analysis)
	# finalAICmodelSet[['NJ_Falc_per_hr']] <- lm(aucRatio~nfalc_nj, data = AUC.for.Analysis)
	# finalAICmodelSet[['NJ_Falc_per_hr + n.sites']] <- lm(aucRatio~nfalc_nj+ n.sites, data = AUC.for.Analysis)
	
	# nullModels <- list()
	finalAICmodelSet[["No Trend"]]  <- lm(aucRatio~NULL , data=AUC.for.Analysis)
	finalAICmodelSet[["Year linear"]] <- lm(aucRatio~Yr.st, data = AUC.for.Analysis)
	finalAICmodelSet[["Year linearOFF"]] <- lm(aucObs~Yr.st + offset(aucExp), data = AUC.for.Analysis)
	
	finalAICmodelSet[["Log Year linear"]] <- glm(aucRatio~Yr.st, data = AUC.for.Analysis, family = gaussian(link = 'log'))
	finalAICmodelSet[['Quad Year']] <- lm(aucRatio~Yr.st + I(Yr.st**2), data = AUC.for.Analysis)
	# globalModels <- list()
	finalAICmodelSet[["globalModel"]] <- lm(aucRatio~mean.danger + CV + Lat.st*Lon.st + LgSiteCount + BOF.falc.factor+ 
                                      n.sites + Yr.st,  data = AUC.for.Analysis)#+Index+nfalc + +BoFfalcfiftyDay+
	finalAICmodelSet[['nsite by danger + year']] <-lm(aucRatio~mean.danger*n.sites + Yr.st, data = AUC.for.Analysis)
	finalAICmodelSet[["global Survey Additive + Year"]] <- lm(aucRatio~mean.danger + CV + Lat.st*Lon.st + LgSiteCount + n.sites+Yr.st,  data = AUC.for.Analysis)#+Index+nfalc + +BoFfalc
	finalAICmodelSet[["global Survey w Interactions + Year"]] <- lm(aucRatio~ CV + Lat.st*Lon.st + LgSiteCount + mean.danger*n.sites+Yr.st, data =AUC.for.Analysis)
  
	# finalAICmodelSet <- c(surveyModels, falconModels, sesaModels, nullModels, globalModels)
	
	# if(isTRUE(binIt)){
	#   finalAICmodelSet[which(names(finalAICmodelSet) %in% c("Mean Danger",
	#                                                         "Mean Danger+ Year",
	#                                                         'nsite by danger + year',
	#                                                         "Number of Sites Surveyed", 
	#                                                         'Number of Sites Surveyed + Year',
	#                                                         "Mean Danger by N Sites",
	#                                                         "CV of Date",
	#                                                         "Location",
	#                                                         "Large Site Counts"))] <- NULL
	#   
	#   finalAICmodelSet[["survey Global Additive"]] <- lm(aucRatio~  CV + Lat.st*Lon.st + LgSiteCount, data =AUC.for.Analysis)
	#   finalAICmodelSet[["globalModel"]] <- lm(aucRatio~ CV + Lat.st*Lon.st + LgSiteCount + BOF.falc.factor+ 
	#                                         Yr.st,  data = AUC.for.Analysis)
	#   finalAICmodelSet[["global Survey Additive + Year"]] <- lm(aucRatio~ CV + Lat.st*Lon.st + LgSiteCount + Yr.st,  data = AUC.for.Analysis)#+Index+nfalc + +BoFfalc
	#   finalAICmodelSet[["global Survey w Interactions + Year"]] <- lm(aucRatio~ CV + Lat.st*Lon.st + LgSiteCount + Yr.st, data =AUC.for.Analysis)
	# 
	# } else{
	  finalAICmodelSet[["Offset"]] <- lm(aucObs~offset(aucExp) + Yr.st, data = AUC.for.Analysis)
	  finalAICmodelSet[["Mean Danger"]] <- lm(aucRatio~mean.danger, data = AUC.for.Analysis)
	  finalAICmodelSet[["Mean Danger+ Year"]] <- lm(aucRatio~mean.danger + Yr.st, data = AUC.for.Analysis)
	  finalAICmodelSet[['Number of Sites Surveyed']] <- lm(aucRatio~n.sites, data=AUC.for.Analysis)
	  finalAICmodelSet[['Number of Sites Surveyed + Year']] <- lm(aucRatio~n.sites + Yr.st, data=AUC.for.Analysis)
	  finalAICmodelSet[["Mean Danger by N Sites"]] <- lm(aucRatio~mean.danger*n.sites, data=AUC.for.Analysis)
	  finalAICmodelSet[['CV of Date']] <- lm(aucRatio~CV, data = AUC.for.Analysis)
	  finalAICmodelSet[['Location']] <- lm(aucRatio~Lat.st*Lon.st, data=AUC.for.Analysis)
	  finalAICmodelSet[['Large Site Counts']] <- lm(aucRatio~LgSiteCount, data=AUC.for.Analysis)
	# }
	
	if(isTRUE(plotOut)){
		require(corrplot)
		dailySummary <- dat %>% group_by(DayNum, Year) %>% 
		  summarize(nsites = n()) %>% ungroup %>% tidyr::spread(Year, nsites, fill = NA) %>% tidyr::gather("Year", "nsites", 2:43) %>% 
		  mutate(Year = as.numeric(Year))


		
		
		dailySummaryDanger <- dat %>% group_by(DayNum, Year) %>% 
		  summarize(danger = mean(proportionDanger)) %>% ungroup %>% tidyr::spread(Year, danger, fill = NA) %>% tidyr::gather("Year", "danger", 2:43) %>% 
		  mutate(Year = as.numeric(Year))

    require(cowplot)
		# gridExtra::grid.arrange(
		heatmaps <- 
		plot_grid(
		  
		ggplot(data=dailySummary, aes(Year, DayNum),environment = environment()) + geom_tile(aes(fill=nsites)) +
		  labs(y = "Day of Year", fill = "Number\nof Sites", x="")+ scale_fill_continuous_tableau(),
		  #ggthemes::theme_few(),
		ggplot(data=dailySummaryDanger, aes(Year, DayNum),environment = environment()) + geom_tile(aes(fill=danger)) +
		  labs(y = "Day of Year", fill = "Mean\nSite\nDanger")+ scale_fill_continuous_tableau(), labels = "auto",
		  # ggthemes::theme_few()+ scale_fill_continuous_tableau(),
			nrow=2)
		heatmaps
		corrplot.mixed(cor(select(AUC.for.Analysis, mean.danger , CV , Lat.st,Lon.st , LgSiteCount , 
		                                      n.sites,BoFfalc , nfalc , Index,Yr.st))) #fiftyDay
		print(cbind(broom::tidy(finalAICmodelSet$`Year linear`), confint(finalAICmodelSet$`Year linear`, type = 'boot')))
		# bbmle::AICctab(finalAICmodelSet, weights =T,logLik=T, base=T)
	}else{heatmaps <- NULL}
	
	
	
	
	if(is.null(species)){species <- "SESA"}
	return(list("models" = finalAICmodelSet, "AUC" =AUC.for.Analysis, "Species" = species, "heatmaps" = heatmaps))
	
	}
	
	
prepareDat <- function(sesa.dat, danger.dat, datelimits = c(0.1, 0.9), species = NULL){
		newData <- sesa.dat %>% 
  			filter(!StateProvince %in% c("Newfoundland and Labrador","NL", "" ) & 
           # YearCollected != 2016 & 
           MonthCollected %in% c(7,8,9,10)) %>% mutate(Lat = round(DecimalLatitude, 2),
                                                       Lon = round(DecimalLongitude, 2)) %>% 
		  mutate(DecimalLatitude = round(DecimalLatitude,5),
		         DecimalLongitude = round(DecimalLongitude, 5))
  
		
		ncounts <- newData %>% #mutate(Lat = round(DecimalLatitude,6),
		                              # Lon = round(DecimalLongitude, 6))%>%
		  group_by(Locality, DecimalLatitude, DecimalLongitude) %>% summarise(n=n())
		

		# Clean up some errors
		newData[which(newData$Locality== "Sand Hills Beach Provincial Park"),][["DecimalLatitude"]] <- 43.53133
		newData[which(newData$Locality== "Sand Hills Beach Provincial Park"),][["DecimalLongitude"]] <-  -65.55322
		# newData[which(newData$Locality== "South Shore Malpeque Bay (sites 1 to 9)"),][["DecimalLongitude"]] <-  -63.67
		


		zeroObservations <- newData %>% dplyr::filter(NoObservations == "NoObs")
		birdObservations <- newData %>% dplyr::filter(NoObservations != "NoObs")

		danger.PRISM <- danger.dat %>% rename(area_km = IntArea, proportionDanger = danger) %>% 
		  mutate(Lat_full = round(Lat,5), Lon_full = round(Lon,5)) %>%
		filter(!is.na(proportionDanger)) %>% mutate(HighDanger = area_km * proportionDanger * 1000^2,
		                                                             IntertidalArea = area_km * 1000^2) %>% 
		  left_join(ncounts, by = c("Locality", "Lat_full" = "DecimalLatitude", "Lon_full" = "DecimalLongitude")) %>% 
		mutate(Lat = round(Lat_full, 2), Lon = round(Lon_full,2))



		dangerPooled <- danger.PRISM %>% 
		  # left_join(ncounts, by = c("Locality", "Lat_full" = "Lat", "Lon_full" = "Lon")) %>% 
		  group_by(Lat, Lon) %>% summarize(n_sites_combined = n(), 
		                                                              proportionDanger = weighted.mean(proportionDanger, n, na.rm=T),#mean(proportionDanger),
		                                                              IntertidalArea =  weighted.mean(IntertidalArea, n, na.rm=T),#mean(IntertidalArea),
		                                                              Locality = first(Locality),
		                                                              ID = first(ID),
		                                                              area_km = weighted.mean(area_km, n, na.rm=T), #mean(area_km),
		                                                              HighDanger = weighted.mean(HighDanger, n, na.rm=T),#mean(HighDanger),
		                                                              IntCells = weighted.mean(IntCells, n, na.rm=T),#mean(IntCells),
		                                                              utm = first(utm)
		                                                              ) %>% ungroup %>% 
		mutate(Site.Code = paste0(Lat,"_",Lon), Safety = 1-proportionDanger)



		spreadData <- birdObservations %>% group_by(GlobalUniqueIdentifier) %>% mutate(ObsCount = sum(ObservationCount, na.rm=T)) %>% 
		ungroup() %>% 
		dplyr::select(-ObservationCount) %>% rename(ObservationCount = ObsCount) %>% distinct() %>% 
		dplyr::select(YearCollected, MonthCollected, DayCollected, #Locality, 
		             Lat, Lon,
		             SurveyAreaIdentifier, SamplingEventIdentifier, CommonName, ObservationCount) %>% distinct() %>% 
		spread(CommonName,ObservationCount,fill = 0 ) 

		allCounts <- bind_rows(spreadData, dplyr::select(zeroObservations,YearCollected, MonthCollected, DayCollected, Lat, Lon,# Locality,
		                                             SurveyAreaIdentifier, SamplingEventIdentifier)) 
		allCounts[is.na(allCounts)] <- 0
		if(is_null(species)){
		allCounts.Zeros <- mutate(allCounts, SESA.Zeros =  `Semipalmated Sandpiper`+ `Unidentified Small Sandpiper ('peep')` +  `Calidris sp.`)#`Scolopacidae sp.`
		} else{
		  allCounts.Zeros <- allCounts
		  allCounts.Zeros$SESA.Zeros <- allCounts.Zeros[[species]]
		  # allCounts.Zeros <- mutate_(allCounts, SESA.Zeros = species)
		}
		pooledCounts <- allCounts.Zeros %>% group_by(YearCollected, MonthCollected, DayCollected, Lat, Lon) %>% 
		select(-SurveyAreaIdentifier, -SamplingEventIdentifier) %>% 
		summarize_all(funs(sum)) %>% ungroup #summarise_each

		allCounts.w.Danger <- left_join(pooledCounts, dangerPooled, by = c("Lat", "Lon")) 
		naDangerCounts <- allCounts.w.Danger[is.na(allCounts.w.Danger$proportionDanger),]	%>%
		 dplyr::select( -n_sites_combined, -proportionDanger,-IntertidalArea ,  -Locality  ,-ID,-area_km  ,   
		-HighDanger  ,     -IntCells   ,-utm,-Site.Code,-Safety) %>%
		rowwise() %>% 
		mutate(ID = dangerPooled$ID[which.min(sqrt((Lat-dangerPooled$Lat)**2 + (Lon - dangerPooled$Lon)**2))],
			dist = sqrt((Lat-dangerPooled$Lat[dangerPooled$ID == ID])**2 + (Lon - dangerPooled$Lon[dangerPooled$ID == ID])**2),
			Lat = dangerPooled$Lat[dangerPooled$ID == ID],
			Lon = dangerPooled$Lon[dangerPooled$ID == ID],
			Locality =  dangerPooled$Locality[dangerPooled$ID == ID]
			) %>% ungroup %>% filter(dist < 2)
		
		unique(naDangerCounts$dist)

		newCountsforAnalysis <- allCounts.w.Danger %>% 
		filter(!is.na(proportionDanger)) %>%
		bind_rows(naDangerCounts) %>%
		dplyr::select(YearCollected, MonthCollected, DayCollected, #SamplingEventIdentifier,
		                                                         Locality, 
		                                                         # SurveyAreaIdentifier, 
		                                                         ID, Lat, Lon, proportionDanger, IntCells, IntertidalArea, utm, area_km, HighDanger, SESA.Zeros )

		ACSSCounts <- newCountsforAnalysis %>% rename(Year = YearCollected, Month = MonthCollected, Day = DayCollected) %>% 
  			mutate(Date = as.Date(paste(Year, Month, Day,  sep = '/' ), format = "%Y/%m/%d" ),
         DayNum = as.numeric(format(Date , format = "%j")),
         Month = lubridate::month(Date),
         Week = DayNum %/% 7 + 1,
         FullWeek = as.numeric(format(Date, "%U")),
         Decade = (Year- 1900) %/% 10 ,
         Site.Code = paste0(Lat,"_",Lon),
         Site.ID = Site.Code,
         Site.Name = Locality,
         SiteName = Locality)  %>% filter(Month %in% c(7,8,9,10))


  		day.limits <- Hmisc::wtd.quantile(ACSSCounts$DayNum, weights = ACSSCounts$SESA.Zeros,probs =datelimits, na.rm = T)
  		print(day.limits)
  		print(lubridate::as_date(day.limits))
  		

  		ACSS_South_Danger <-
			  ACSSCounts %>%
			  filter(DayNum >= day.limits[1] & DayNum <= day.limits[2]) %>%
			  mutate(Chronology = paste0(Year, DayNum), Danger.cat = ifelse(proportionDanger>0.3, 'Low',  'High'),
			         YrWeek = paste0(Year, Week)) %>%
			  group_by(YrWeek) %>%
			  mutate(total.birds = sum(SESA.Zeros), est.birds = total.birds / n(),
			         diff.expected = SESA.Zeros - est.birds) %>%
			  group_by(Chronology, Lat, Lon) %>% 
			  ungroup() %>% filter(!is.na(proportionDanger) & proportionDanger!=0)

  		noBirdsObsed <- ACSS_South_Danger %>% group_by(Site.ID) %>%
  		  summarize(sumB = sum(SESA.Zeros, na.rm=T), n = n()) %>% ungroup %>% filter(sumB <= 0 )#& n>2)

		return(filter(ACSS_South_Danger, !Site.ID %in% noBirdsObsed$Site.ID))



	}