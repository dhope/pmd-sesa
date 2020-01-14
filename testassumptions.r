yearModels <- vector(mode = 'list', length = 14)

if(!exists('danger_all')) {danger_all <- readRDS("../Rscripts/DangerEstimatesUpdated/Danger_Estimates_all.rds") %>% 
  rename(danger = propD) %>% mutate(IntArea = Intertial / 1000**2, IntCells = IntArea ) }


test_assumption <- function(distD_, radius_, quantiles_dates, run_name, dat = acss_dat, danger_dat = danger_all)
{ lgonly_ <- TRUE
  danger <- danger_all %>% filter(distD == distD_ & radius == radius_)
  usearea <- ifelse(run_name == "Area Sorted PMD", TRUE, FALSE)

  clippeddat <- prepareDat(dat, danger, quantiles_dates)
  if(run_name == "No MAPT or GRAN") {
    clippeddat <- filter(clippeddat, proportionDanger != 0 &
           Site.ID != "45.72_-64.65" & Site.ID !="45.81_-64.5" )
    lgonly_ = F
  }
  if(run_name %in% c("Date_0_20","Date_80_100","No MAPT or GRAN)")){
    outlier_ = NULL
  }else{outlier_ = 1995}

  results_ <- calculateNewDataAnalysis(dat = clippeddat, 
    covars = covars, outlier = outlier_,plotOut = F, useArea = usearea, lgonly = lgonly_)
  mods_ <- results_$models
  yearModels <- cbind(broom::tidy(mods_$`Year linear`), confint(mods_$`Year linear`, type = 'boot')) %>% mutate(
  runName = run_name,
  rsq = summary(mods_$`Year linear`)$r.squared
  )
  return(yearModels)
}

assumptions_df <- tibble(distD_ = c(rep(150,6),300,450,50,rep(150,6)),
                        radius_  =c(rep(2500,9), 1000,5000, rep(2500,4)),
                        quantiles = list(c(0.1,0.90),c(0.20, 0.80),
                          c(0.05, 0.95),
                          c(0.025, 0.975),
                          c(0.005, 0.995),
                          c(0.0, 1.0),
                          c(0.1, 0.9),
                          c(0.1, 0.9),c(0.1, 0.9),c(0.1, 0.9),c(0.1, 0.9),c(0.0, 0.2),c(0.8, 1.0),c(0.1, 0.9),c(0.1, 0.9) ),
                        run_name = c("Baseline",
                                      "Dates_20_80",
                                      "Dates_5_95",
                                      "Dates_25_975",
                                      "Dates_1_99",
                                      "All_Dates",
                                      "Danger_300m",
                                      "Danger_450m",
                                      "Danger_50m",
                                      "Radius_1k",
                                      "Radius_5k",
                                      "Date_0_20",
                                      "Date_80_100",
                                      "Area Sorted PMD",
                                      "No MAPT or GRAN") )

# yearModels <- pmap_df(assumptions_df, test_assumption)

# baselineResults <- calculateNewDataAnalysis(dat = ACSS_Danger_150_10_90, covars = covars,plotOut = T)
# baselineModels <- baselineResults$models#$NJ_Falc_per_hr %>% summary
# yearModels[[1]] <- cbind(broom::tidy(baselineModels$`Year linear`), confint(baselineModels$`Year linear`, type = 'boot')) %>% mutate(
#   runName = "Baseline",
#   rsq = summary(baselineModels$`Year linear`)$r.squared
# )
# ACSS_Danger_150_20_80 <- prepareDat(acss_dat, danger_150, c(0.20, 0.80))
# expandedDatesResults60 <- calculateNewDataAnalysis(dat = ACSS_Danger_150_20_80, covars = covars, outlier = 1995,plotOut = T)
# expandedModels_60 <- expandedDatesResults60$models

# yearModels[[2]] <- cbind(broom::tidy(expandedModels_60$`Year linear`), confint(expandedModels_60$`Year linear`, type = 'boot')) %>% mutate(
#   runName = "Dates_20_80",
#   rsq = summary(expandedModels_60$`Year linear`)$r.squared
# )

# ACSS_Danger_150_5_95 <- prepareDat(acss_dat, danger_150, c(0.05, 0.95))
# expandedDatesResults90 <- calculateNewDataAnalysis(dat = ACSS_Danger_150_5_95, covars = covars, outlier = 1995,plotOut = T)
# expandedModels_90 <- expandedDatesResults90$models

# yearModels[[3]] <- cbind(broom::tidy(expandedModels_90$`Year linear`), confint(expandedModels_90$`Year linear`, type = 'boot')) %>% mutate(
#   runName = "Dates_5_95",
#   rsq = summary(expandedModels_90$`Year linear`)$r.squared
# )


# ACSS_Danger_150_2.5_97.5 <- prepareDat(acss_dat, danger_150, c(0.025, 0.975))
# expandedDatesResults <- calculateNewDataAnalysis(dat = ACSS_Danger_150_2.5_97.5, covars = covars, outlier = 1995,plotOut = T)
# expandedModels <- expandedDatesResults$models

# yearModels[[4]] <- cbind(broom::tidy(expandedModels$`Year linear`), confint(expandedModels$`Year linear`, type = 'boot')) %>% mutate(
#   runName = "Dates_25_975",
#   rsq = summary(expandedModels$`Year linear`)$r.squared
# )


# ACSS_Danger_150_99 <- prepareDat(acss_dat, danger_150, c(0.005, 0.995))
# expandedDatesResults_99 <- calculateNewDataAnalysis(dat = ACSS_Danger_150_99, covars = covars, outlier = 1995,plotOut = T)
# expandedModels_99 <- expandedDatesResults_99$models

# yearModels[[5]] <- cbind(broom::tidy(expandedModels_99$`Year linear`), confint(expandedModels_99$`Year linear`, type = 'boot')) %>% mutate(
#   runName = "Dates_1_99",
#   rsq = summary(expandedModels_99$`Year linear`)$r.squared
# )

# ACSS_Danger_150_all <- prepareDat(acss_dat, danger_150, c(0.0, 1.0))
# AllDatesResults <- calculateNewDataAnalysis(dat = ACSS_Danger_150_all, covars = covars, outlier = 1995,plotOut = T)
# AllDatesModels <- AllDatesResults$models

# yearModels[[6]] <- cbind(broom::tidy(AllDatesModels$`Year linear`), confint(AllDatesModels$`Year linear`, type = 'boot')) %>% mutate(
#   runName = "All_Dates",
#   rsq = summary(AllDatesModels$`Year linear`)$r.squared
# )


# danger_300 <- danger_all %>% filter(distD == 300 & radius == 2500)
#   #read.csv("./danger.r_Prism300.csv")

# ACSS_Danger_300_10_90 <- prepareDat(acss_dat, danger_300, c(0.1, 0.9))

# baselineResults_300 <- calculateNewDataAnalysis(dat = ACSS_Danger_300_10_90, covars = covars, outlier = 1995,plotOut = T)
# baselineModels_300 <- baselineResults_300$models

# yearModels[[7]] <- cbind(broom::tidy(baselineModels_300$`Year linear`), confint(baselineModels_300$`Year linear`, type = 'boot')) %>% mutate(
#   runName = "Danger_300m",
#   rsq = summary(baselineModels_300$`Year linear`)$r.squared
# )


# danger_450 <- danger_all %>% filter(distD == 450 & radius == 2500)#read.csv("./danger.r_Prism450.csv")

# ACSS_Danger_450_10_90 <- prepareDat(acss_dat, danger_450, c(0.1, 0.9))

# baselineResults_450 <- calculateNewDataAnalysis(dat = ACSS_Danger_450_10_90, covars = covars, outlier = 1995,plotOut = T)
# baselineModels_450 <- baselineResults_450$models

# yearModels[[8]] <- cbind(broom::tidy(baselineModels_450$`Year linear`), confint(baselineModels_450$`Year linear`, type = 'boot')) %>% mutate(
#   runName = "Danger_450m",
#   rsq = summary(baselineModels_450$`Year linear`)$r.squared
# )

# danger_50 <- danger_all %>% filter(distD == 50 & radius == 2500)##read.csv("./danger.r_Prism50.csv")

# ACSS_Danger_50_10_90 <- prepareDat(acss_dat, danger_50, c(0.1, 0.9))

# baselineResults_50 <- calculateNewDataAnalysis(dat = ACSS_Danger_50_10_90, covars = covars, outlier = 1995,plotOut = T)
# baselineModels_50 <- baselineResults_50$models

# yearModels[[9]] <- cbind(broom::tidy(baselineModels_50$`Year linear`), confint(baselineModels_50$`Year linear`, type = 'boot')) %>% mutate(
#   runName = "Danger_50m",
#   rsq = summary(baselineModels_50$`Year linear`)$r.squared
# )

# danger_1000 <- danger_all %>% filter(distD == 150 & radius == 1000)##read.csv("./danger.r_Prism50.csv")

# ACSS_Danger_1k_10_90 <- prepareDat(acss_dat, danger_1000, c(0.1, 0.9))

# baselineResults_1k <- calculateNewDataAnalysis(dat = ACSS_Danger_1k_10_90, covars = covars, outlier = 1995,plotOut = T,rad_ = 1000)
# baselineModels_1k <- baselineResults_1k$models

# yearModels[[10]] <- cbind(broom::tidy(baselineModels_1k$`Year linear`), confint(baselineModels_1k$`Year linear`, type = 'boot')) %>% mutate(
#   runName = "Radius_1k",
#   rsq = summary(baselineModels_1k$`Year linear`)$r.squared
# )


# danger_1000 <- danger_all %>% filter(distD == 150 & radius == 5000)##read.csv("./danger.r_Prism50.csv")

# ACSS_Danger_5k_10_90 <- prepareDat(acss_dat, danger_1000, c(0.1, 0.9))

# baselineResults_5k <- calculateNewDataAnalysis(dat = ACSS_Danger_5k_10_90, covars = covars, outlier = 1995,plotOut = T, rad_ = 5000)
# baselineModels_5k <- baselineResults_5k$models

# yearModels[[11]] <- cbind(broom::tidy(baselineModels_5k$`Year linear`), confint(baselineModels_5k$`Year linear`, type = 'boot')) %>% mutate(
#   runName = "Radius_5k",
#   rsq = summary(baselineModels_5k$`Year linear`)$r.squared
# )



# ACSS_Danger_150_early <- prepareDat(acss_dat, danger_150, c(0.0, 0.2))

# baselineResults_early <- calculateNewDataAnalysis(dat = ACSS_Danger_150_early, covars = covars, outlier = NULL,plotOut = T)
# baselineModels_early <- baselineResults_early$models

# yearModels[[12]] <- cbind(broom::tidy(baselineModels_early$`Year linear`), confint(baselineModels_early$`Year linear`, type = 'boot')) %>% mutate(
#   runName = "Date_0_20",
#   rsq = summary(baselineModels_early$`Year linear`)$r.squared
# )


# ACSS_Danger_150_late <- prepareDat(acss_dat, danger_150, c(0.8, 1.0))

# baselineResults_late <- calculateNewDataAnalysis(dat = ACSS_Danger_150_late, covars = covars, outlier = NULL,plotOut = T)
# baselineModels_late <- baselineResults_late$models

# yearModels[[13]] <- cbind(broom::tidy(baselineModels_late$`Year linear`), confint(baselineModels_late$`Year linear`, type = 'boot')) %>% mutate(
#   runName = "Date_80_100",
#   rsq = summary(baselineModels_late$`Year linear`)$r.squared
# )

# ACSS_South_Danger_forAnalysis_noGRAN <- ACSS_Danger_150_10_90 %>% 
#   filter(proportionDanger != 0 &
#            Site.ID != "45.72_-64.65" & Site.ID !="45.81_-64.5" )

# baselineResults_Area <- calculateNewDataAnalysis(dat = ACSS_Danger_150_10_90, covars = covars, outlier = 1995,plotOut = T,useArea = T)
# baselineModels_Area <- baselineResults_Area$models

# yearModels[[14]] <- cbind(broom::tidy(baselineModels_Area$`Year linear`), confint(baselineModels_Area$`Year linear`, type = 'boot')) %>% mutate(
#   runName = "Area Sorted PMD",
#   rsq = summary(baselineModels_Area$`Year linear`)$r.squared
# )


# # This should be the analysis sequence for using the mean counts and then bootstrapping the results.
# AUC.results_noLg <- calculateNewDataAnalysis(ACSS_South_Danger_forAnalysis_noGRAN, covars = covars, 
#                                              outlier = NULL, lgonly = F)

# # AUC.musthaveLg <- calculateDstats(data = filter(ACSS_South_Danger, proportionDanger !=0 & Year !=1995 &
# #                                                   Year %in% yrs.w.lg), seed = 565465, plotOut = T, 
# #                             transform = "None", average = T,useMice = F,calcTrends = F,modelType = "lm" )


# # ggplot(AUC.results_noLg$AUC, aes(Year, aucRatio)) + geom_point() + geom_smooth(method="lm")
# noLg <- AUC.results_noLg$models$`Year linear` %>% tidy %>% 
#   cbind(as.data.frame(confint(AUC.results_noLg$models$`Year linear`, method='boot'))) %>% 
#   mutate(runName = "No MAPT or GRAN")
