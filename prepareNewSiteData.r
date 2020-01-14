require(tidyverse)
newData <- prepareDat(updatedacss_data, danger_150, c(0, 1))



reducedSites <- siteInfo #%>% group_by(Locality) %>% dplyr::summarize(Lat = mean(DecimalLatitude, na.rm=T),

require(sp)
dat <- reducedSites %>% ungroup %>%  mutate(y = Lat, x = Lon)  %>% filter(!is.na(y)) %>% ungroup
coordinates(dat) <- ~x+y

proj4string(dat) <- "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs "
