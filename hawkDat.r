## Hawk Data

require(tidyverse)
require(lubridate)
hawk <- read_csv("../MasterFilesFromPaul/HawkMountain.csv") %>% 
  select(Date, M, P) %>% mutate(Date_chr = Date,
                                Date = mdy(Date),
                                Month = month(Date),
                                Day = day(Date),
                                Year = year(Date),
                                DoY = yday(Date)) %>% filter(Month >2)%>% filter(DoY < 330 & DoY > 180) %>% 
  group_by(Year) %>% mutate(Yr.total = sum(P, na.rm=T)) %>% ungroup %>% 
  mutate(prop.total = P/Yr.total) %>% group_by(Year) %>% 
  mutate(cumProp = cumsum(prop.total),
         Ndays = n()) 

# ggplot(filter(hawk, Year == 2005),aes(DoY, cumProp)) + geom_point() +geom_line(aes(colour = Year))


hawkYr50 <- hawk %>% group_by(Year) %>% 
  summarize(fiftyDay = DoY[which.min(abs(cumProp - 0.5))],
            nextDay = DoY[which.min(abs(cumProp - 0.5))+1],
            prevDay = DoY[which.min(abs(cumProp - 0.5))-1],
            prop50 = cumProp[which(DoY == fiftyDay)],
            varDays = var(c(fiftyDay, nextDay, prevDay)),
            dev50 = prop50 - 0.5,
            ndays = n(),
            nbirds = sum(P),
            meanCount = mean(P))

# ggplot(hawk,aes(DoY, cumProp, colour=Ndays)) +geom_line() + facet_wrap(~Year) + geom_point() +
#   geom_hline(yintercept = 0.5, colour = 'red') + geom_vline(data=hawkYr50,aes(xintercept = fiftyDay))

# ggplot(hawkYr50, aes(Year, fiftyDay, colour = dev50)) + geom_point() + scale_colour_distiller()

# sesa_hawk <- left_join(AUC.results.w.bootstrap, hawkYr50, by = "Year")
# ggplot(sesa_hawk%>% filter(., Year %in% yrs.w.lg & Year !=1995), aes(fiftyDay, aucRatio)) + geom_point() + 
#   geom_smooth(method='lm')

# ggplot(hawkYr50, aes(Year, meanCount) )+ geom_point() + geom_smooth()




hawk_nj <- read_csv("../Datafiles/NJ_HW.csv") %>% 
  mutate(Year = year(Date),
         Month = month(Date),
         Day = day(Date),
         DoY = yday(Date)) 

nj_by_day <- hawk_nj %>% group_by(Date, Year, DoY) %>% filter(DoY <300) %>% 
  summarize(total_falc = sum(Count, na.rm=T),
            mean_falc = mean(Count, na.rm=T),
            nhrs = n()) %>% group_by(Year) %>% 
  mutate(yr.total = sum(mean_falc)) %>% 
  group_by(Year) %>%arrange(DoY) %>%  mutate(
  csum_falc = cumsum(mean_falc),
  cumProp = csum_falc / yr.total
  ) %>% ungroup

# ggplot(filter(nj_by_day, Year == 2005),aes(DoY, cumProp)) + geom_point() +geom_line(aes(colour = Year))

hawkYr50_nj <- nj_by_day %>% group_by(Year) %>% 
  summarize(fiftyDay = DoY[which.min(abs(cumProp - 0.5))],
            nextDay = DoY[which.min(abs(cumProp - 0.5))+1],
            prevDay = DoY[which.min(abs(cumProp - 0.5))-1],
            prop50 = cumProp[which(DoY == fiftyDay)],
            varDays = var(c(fiftyDay, nextDay, prevDay)),
            dev50 = prop50 - 0.5,
            ndays = n(),
            nbirds = sum(total_falc, na.rm=T),
            meanCount = mean(mean_falc, na.rm=T))
# ggplot(hawkYr50_nj, aes(Year, fiftyDay, colour = dev50)) + geom_point() + scale_colour_distiller()

yrSum <- hawk_nj %>% filter(DoY <=300& DoY >=246) %>% # & Hour < 8) %>% 
  group_by(Date) %>% mutate(dailyEffort = n()) %>% 
  group_by(Year) %>%
  summarize(ndays = length(unique(DoY)),
            nhours = n(),
            meanHrs = mean(dailyEffort, na.rm=T),
            nbirds = sum(Count, na.rm=T) ,
            firstday = min(DoY),
            lastday = max(DoY)) %>% ungroup %>% 
  mutate(falc_per_hr = nbirds / nhours) %>% filter(Year < 2016) 

nj_falc <- yrSum %>% select(Year, ndays, nhours, falc_per_hr, nbirds) %>% rename(nFalc_NJ=nbirds)

# yrSum <- hawk_nj %>% group_by(Date) %>% 
#   mutate(nhrs = n(),
#          falc_per_hr = mean(Count, na.rm=T)) %>% 
#   group_by(Year) %>% 
#   mutate(ndays = )
#   


# ggplot(hawk_nj, aes(Hour, Count, fill = Year)) + geom_col(position='stack')
# ggplot(hawk_nj, aes(DoY, Count, fill = Year)) + geom_col(position='stack')
#   
# ggplot(yrSum, aes(Year, falc_per_hr)) + geom_point() + geom_smooth()
# ggplot(yrSum, aes(Year, nbirds)) + geom_point() + geom_smooth()
# 
# ggplot(yrSum, aes(Year, firstday)) +geom_point()
# ggplot(yrSum, aes(Year, lastday)) +geom_point()
# 
# ggplot(yrSum, aes(Year, nhours)) +geom_point()
# ggplot(yrSum, aes(Year, meanHrs))  + geom_point()



# 
# ggplot(nj_falc, aes(Year, arm::rescale(falc_per_hr))) + 
#   geom_point() + geom_smooth(method='lm', se=F) +
#   geom_point(data = hawkYr50, aes(Year, arm::rescale(meanCount)), colour = 'red') +
#   geom_smooth(data = hawkYr50, aes(Year, arm::rescale(meanCount)), colour = 'red',method='lm', se=F) +
#   geom_point(data=baselineResults$AUC, aes(y=arm::rescale(aucRatio)), colour = 'green') +
#   geom_smooth(data=baselineResults$AUC, aes(y=arm::rescale(aucRatio)), colour = 'green', method='lm', se=F)+
#   geom_point(data=baselineResults$AUC, aes(y=arm::rescale(Index)), colour = 'blue') +
#   geom_smooth(data=baselineResults$AUC, aes(y=arm::rescale(Index)), colour = 'blue', method='lm', se=F)
# 




