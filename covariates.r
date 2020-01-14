## Generate Covariates

source('hawkDat.r')

# 6. Bay of Fundy Breeding falcon population size

# Data entered directly from webpage of breeding pair abundance.
# https://www.canada.ca/en/environment-climate-change/services/species-risk-public-registry/cosewic-assessments-status-reports/peregrine-falcon-2017.html
falconsBoF <- data.frame(Year = c(1970,1975,1980,1985,1990,1995,2000,2005), 
                         territorialPairs = c(0,0,0,1,5,6,11,16),
     sitesOccupied = c(0,0,0,1,7,6,11,20)) %>% mutate(source = "COSEWIC")
falconsBoF_dickDekker <- tibble(Year = c(1989,1993,2001,2010), territorialPairs = c(1,5,11,27)) %>% 
  mutate(source = "Dekker pers.comm.")
falconsBoF <- bind_rows(falconsBoF_dickDekker, falconsBoF) %>% 
  arrange(Year) %>% select(Year, territorialPairs, source) %>% 
  distinct %>% right_join(tibble(Year=seq(1970,2016)), by = "Year")
falc.lm <- glm(territorialPairs ~ Year + I(Year^2), family = gaussian(link = 'log'),
               data = filter(falconsBoF, territorialPairs >0))
falc.predict <- broom::augment(falc.lm, newdata = data.frame(Year = seq(1985, 2016)) , 
                               type.predict = 'response', conf.level = 0.95) %>%  rename(
                                 BoFfalc = .fitted,se_fit = .se.fit) %>% mutate(
         falc.pres = "Yes") %>% bind_rows(tibble(Year=seq(1970, 1984), BoFfalc = 0,falc.pres = "No"))


# 8. Density dependence - Total population size either index or total counts

# http://www.ec.gc.ca/soc-sbc/graph-graph-eng.aspx?sY=2014&sL=e&sM=c&sB=SESA&sT=6e4d4a3a-ebdc-4b1e-add9-ff2c1965ccbe&sC=678fc51d-4c8a-49a3-b33e-a5924a5c374c&sI=b62b6b44-88f3-44f1-b4c1-7f8df724f808&sO=29223cc5-c2fa-4b4c-b1d9-b1d694bb0624
## Access December 2, 2016
SESA.index <- read.csv('../MasterFilesFromPaul/SESA_PopIndex2014.csv')


# 4-5 Snowmelt Data for SESA and Falcons

snow.falc <- read.csv("../Snowmelt/FalconSnowmeltPhenology.csv") %>% mutate(Year = year, snow.falc = yday.start)
snow.sesa <- read.csv("../Snowmelt/SESASnowmeltPhenology.csv") %>% mutate(Year = year, snow.sesa = yday.start)
snowmelt.both <- full_join(snow.falc, snow.sesa, by = "Year")  %>% dplyr::select(Year, snow.falc, snow.sesa)


covars = list("hawkYr50" = hawkYr50,"nj_hawk"= nj_falc , "falc.predict" = falc.predict, "SESA.index" = SESA.index, "snowmelt.both" = snowmelt.both)




