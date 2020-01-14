calculateSimulatedSDI <- function(dat, column.n, binned = FALSE,binsize = 0.1 ) {
 df.out1 <- dat %>% mutate_(SESA.Zeros = column.n) %>%
 mutate(
	IntertidalArea = area, 
	proportionDanger = danger, 
	DayNum = 5,
	Site.Code = siteid, Site.ID = siteid,  Site.Name=siteid, Lat=siteid, Lon=siteid, SiteName=siteid,
                  AreaDanger=1, Year = 2016,
                  HighDanger = 'Nope')
 # View(df.out1) #%>% ungroup %>% 
 df.out <- 
	calculateDstats(data = df.out1, seed = sample(1:6546541, 1, replace = T), average = T,binIt = binned, binSize = binsize,yrbinSize = 1, areaNumBins = 5000) %>%
	mutate(col = column.n)

	return(df.out)
}


resetPars <- function(){
  pars <- list()
  pars$rangeArea <- c(1,20) # in km2
  pars$rangeDanger <- c(0.1,1) # in proportion within 150m
  pars$nsites <- 30
  pars$nbirds <- 100000
  pars$Npar <- c(0.5, 1)
  pars$beta_shapes <- c(1,1)
  pars$seed <- 55462
  pars$dist <- 'uniform'
  pars$runs <- c("uniformBirds","normalBirds", "betaBirds", "areaMatchingBirds", "dangerMatchingBirds", "hurdleBirds","aggDanger",
"aggMid")
  pars$pbiased <- NA
  pars$biasD <- NA
  pars$Zeros <- FALSE
  pars$nzerosites <- 0
  pars$binned <- FALSE
  pars$binsize = 0.1
  pars$hurdleP <- 0.9
  pars$hurdleD <- 0.2
  pars <<- pars
}

resetPars()
simulation <- function(pars, returnALL = FALSE) {
  unlist(pars)
  set.seed(pars$seed)
  if(pars$dist == 'uniform') {
  site.info <- with(pars, tibble(siteid = seq(1, nsites), 
            danger = seq(rangeDanger[1], rangeDanger[2], length.out = nsites), 
            safety = 1-danger)) %>% mutate(
            area = map_dbl(danger, ~fulcalc(.x, TRUE) )) 
            #0.55 + 4.11 * safety + 3.15 * safety**2))
              #seq(rangeArea[2], rangeArea[1], length.out = nsites) ))
  # site.info$area <- site.info$area + rnorm(pars$nsites, 0, 1.5)
  }
  if(pars$dist == 'random') {
    site.info <- with(pars, tibble(siteid = seq(1,nsites),
                                   danger = runif(nsites, rangeDanger[1], rangeDanger[2])) ) %>% 
                        arrange(desc(danger)) %>% mutate(safety = 1-danger,
                                                         area =map_dbl(danger, ~fulcalc(.x, TRUE) ) )#0.55 + 4.11 * safety + 3.15 * safety**2))
                                                           #((1- danger)**2) * rangeArea[2]))# + rnorm(nsites,mean = 0, sd=0.5)) ))
    # site.info$area <- site.info$area + rnorm(pars$nsites, 0, 1.5)
    
  }
  if(pars$dist == 'biased') {
    pars$nbiasedSites <- round(pars$nsites * pars$pbiased, 0)# %>% round(0)
    pars$nunbiased <- pars$nsites - pars$nbiasedSites
    site.info <- with(pars,
                      tibble(siteid = seq(1,nunbiased),
                             danger = runif(nunbiased, rangeDanger[1], rangeDanger[2]))) %>% 
                        arrange(desc(danger)) %>% mutate(safety = 1-danger,
                                                         area = map_dbl(danger, ~fulcalc(.x, TRUE) )  )#0.55 + 4.11 * safety + 3.15 * safety**2))
    #area = ((1- danger)**2) * rangeArea[2]))# + rnorm(nsites,mean = 0, sd=0.5)) ))
    # site.info$area <- site.info$area + rnorm(pars$nsites, 0, 1.5)
    biasedSites <- with(pars,
                        tibble(siteid = seq((nunbiased+1), nsites),
                               danger = min(1,rnorm(nbiasedSites, biasD, 0.1))) %>% 
                          arrange(desc(danger)) %>% mutate(safety = 1-danger,
                                                           area = map_dbl(danger, ~fulcalc(.x, TRUE) )  ))#0.55 + 4.11 * safety + 3.15 * safety**2))
                            #area = ((1- danger)**2) * rangeArea[2]))# + rnorm(nsites,mean = 0, sd=0.5)) ))
    
    
    site.info <- bind_rows(site.info, biasedSites)
    # site.info$area <- site.info$area + rnorm(pars$nsites, 0, 1.5)
    
    
  }
  # site.info$area[site.info$area <=0] <- 0.01
  site.info <- mutate(site.info, area.safe = safety * area)
  nsites.h1 <- nrow(filter(site.info, danger < pars$hurdleD))
  count.dists <- with(pars,site.info %>% 
     mutate(
          uniformBirds = nbirds / nsites,
          dnormalBirds = dnorm(danger, Npar[1], Npar[2]),
          pnormalBirds = dnormalBirds / sum(dnormalBirds),
          normalBirds = pnormalBirds* nbirds,
          dbetaBirds = dbeta(danger, beta_shapes[1], beta_shapes[2]),
          pbetaBirds = dbetaBirds / sum(dbetaBirds),
          betaBirds  = pbetaBirds * nbirds,
          # lnormBirds = exp(rlnorm(2, 2))
          areaMatchingBirds = area / sum(area) * nbirds,
          
          dangerMatchingBirds = area.safe/ sum(area.safe) * nbirds,#(safety)/ sum(safety) * nbirds,
          hurdleBirds = ifelse(danger < hurdleD, hurdleP * nbirds /nsites.h1, (1-hurdleP) * nbirds / (nsites - nsites.h1) ),
          aggDanger = ifelse(danger < (1-hurdleD), hurdleP * nbirds /(nsites - nsites.h1), (hurdleP) * nbirds / (nsites.h1) ),
          aggMid = ifelse(danger < 0.6 & danger > 0.4, hurdleP * nbirds /nsites.h1, (1-hurdleP) * nbirds / (nsites - nsites.h1) )
          )
     # normBirds <- 10**(rlnorm(pars$nsites, 1, 0.5)) %>% -1
)
  if(isTRUE(pars$Zeros)) {
    count.dists2 <- 
    count.dists %>% 
      filter(danger > 0.7) %>% 
      sample_frac(pars$nzerosites) %>% 
      mutate(uniformBirds = 0, normalBirds = 0, betaBirds =0, areaMatchingBirds =0, dangerMatchingBirds =0) %>% 
      bind_rows(filter(count.dists, !siteid %in% .[['siteid']] ))
    count.dists <- count.dists2
  }
  res.df <- map_df(pars$runs, calculateSimulatedSDI, dat = count.dists,binned = pars$binned, binsize = pars$binsize)
  # detach(pars)
  if(isTRUE(returnALL)) {
    return(list("counts" = count.dists, "results" = res.df, "pars" = pars))
  } else{
  return(res.df) }
}


adjustNorm <- function(mn, sd, pars, returnAll = F) {
    pars$dist <- "uniform"
    pars$Npar <- c(mn, sd)
    res.df <- simulation(pars, returnAll) 
    if(isTRUE(returnAll)){
      res.df$results <-  res.df$results %>% mutate(par1 = mn, par2 = sd)
      res.df$counts <- res.df$counts %>% mutate(par1 = mn, par2 = sd) } else{
        res.df <- mutate(res.df, par1 = mn, par2 = sd)
      }
    return(res.df)
  }

adjustBeta <- function(par1, par2, pars, returnAll = F) {
    pars$beta_shapes <- c(par1, par2)
    res.df <- simulation(pars, returnAll) #%>% mutate(par1=par1, par2 = par2)
    if(isTRUE(returnAll)){
      res.df$results <-  res.df$results %>% mutate(par1 = par1, par2 = par2)
      res.df$counts <- res.df$counts %>% mutate(par1 = par1, par2 = par2) } else{
        res.df <- mutate(res.df, par1 = par1, par2 = par2)
      }
    
    return(res.df)
  }

  adjustHurdle <- function(hurdlePoint, hurdleProportion, pars=pars,returnAll = F){
    pars$dist <- "random"
    pars$hurdleD <- 1-hurdlePoint
    pars$hurdleP <- hurdleProportion
    res.df <- simulation(pars =  pars, returnAll) #%>% mutate(hurdlePoint, hurdleProportion)
    if(isTRUE(returnAll)){
      res.df$results <-  res.df$results %>% mutate(par1 = hurdlePoint, par2 = hurdleProportion)
      res.df$counts <- res.df$counts %>% mutate(par1 = hurdlePoint, par2 = hurdleProportion) } else{
        res.df <- mutate(res.df, par1 = hurdlePoint, par2 = hurdleProportion)
      }
      return(res.df)
    }

  randomSim <- function(seed, pars, rsim = 'random', runs = c("betaBirds")){
    pars$dist <- rsim
    pars$seed <- seed
    pars$runs <- runs
    pars$beta_shapes <- c(1,14)
    return(simulation(pars, F ))
  }

  randomSimP <- function(safetyPoint, safetyProp, seed, pars, rsim = 'random', runs = c("hurdleBirds")){
    pars$hurdleD <- 1-safetyPoint
    pars$nsites <- 100
    pars$hurdleP <- safetyProp
    pars$dist <- rsim
    pars$seed <- seed
    pars$runs <- runs
    # pars$beta_shapes <- c(1,14)
    return(simulation(pars, F ))
  }
  
  
  rsp_PMD <- function(S, P,seed=15641) randomSimP(safetyPoint = S,safetyProp = P, seed = seed, pars = pars) %>% .[['aucRatio']]


# p <- 
#   ggplot(df_res ,#%>% filter(SafetyPMD < 1 | PropPMD < 1), 
#          aes(1-r, SafetyPMD, group = seed)) + 
#     # geom_line( alpha = 0.2) +
#     # geom_line(aes(y=PropPMD), alpha = 0.2, colour = 'blue') +
#     stat_summary(aes(group = 1),geom='line', fun.y='mean') +
#     stat_summary(aes(x=r,y=PropPMD, group = 1),colour = 'blue',
#                  geom='line', fun.y='mean') +
#     scale_y_continuous(limits = c(0.19, 0.4)) +
#     # scale_x_continuous(limits = c(0.7, 1)) +
#     geom_vline(xintercept = 1-0.8) +
#     geom_vline(xintercept = 0.9) +
#     geom_hline(yintercept = mean(AUC.results$AUC$aucRatio),
#                linetype= 2) +
#       annotate(geom = "text",
#                x = 0.835,alpha= 1,
#                y = 0.4, colour = 'blue',
#                label = "Proportion of birds\nin 20% of safest sites") +
#   annotate(geom = "text",
#            x = 0.29,alpha= 1,
#            y = 0.4, #colour = 'blue',
#            label = "90% of birds found at a given\npercentages of safest sites") +
#   labs(x="", y ="PMD")



  # for (i in seq(0.05, 0.99, by = 0.01)){
  #   cat(i, "\t", rsp_PMD(i, 0.8), "\n")}
  # 
  # 
  # for (i in seq(0.05, 0.99, by = 0.01)){
  #   cat(i, "\t", rsp_PMD(0.8,i), "\n")}
  
  adjustSiteN <- function(nsites, pars){
    seeds <- seq(1,1000)
    pars$nsites <- nsites
    result.df <- map_df(seeds, randomSim, pars = pars) %>% mutate(nsites = nsites)
    return(result.df)
    
  }
    

  adjustBiasSites <- function(pbiased, meanBias, nsites=30, pars){
    pars$nsites <- nsites
    pars$pbiased <- pbiased
    pars$biasD <- meanBias
    result.df <- map_df(seeds, randomSim, pars = pars, rsim = 'biased') %>% mutate(nsites = nsites, pbiased = pbiased, meanBias = meanBias)
    return(result.df)
  }


    addemptySites <- function(nzerosites, pbiased, meanBias, pars) {
    pars$Zeros <- TRUE
    pars$nzerosites <- nzerosites
    result.df <- adjustBiasSites(pbiased = pbiased, meanBias=meanBias, pars = pars) %>% mutate(
      nzerosites = nzerosites  )
    return(result.df)
  }


    modifyHurdle <- function(hurdlePoint, hurdleProportion, pars=pars, nsims = 10){
    pars$hurdleD <- hurdlePoint
    pars$hurdleP <- hurdleProportion
    df.out <- map_df(1:nsims, randomSim, pars, runs = c('hurdleBirds')) %>% mutate(hurdlePoint, hurdleProportion)
    return(df.out)
  }

    b1 <- adjustBeta(1,14, pars, F) %>% filter(col=='betaBirds') %>% .[['aucRatio']]
    betaforSen <- function(var){adjustBeta(1,var, pars, F) %>% filter(col=='betaBirds') %>% .[['aucRatio']]}
    N_mean_forSen <- function(var){adjustNorm(var, 0.2, pars, F) %>% filter(col=='normalBirds') %>% .[['aucRatio']]}
    N_sd_forSen <- function(var){adjustNorm(0.1, var, pars, F) %>% filter(col=='normalBirds') %>% .[['aucRatio']]}
    HurdleLoc_forSen <- function(var){adjustHurdle(var,0.9 , pars, F) %>% filter(col=='hurdleBirds') %>% .[['aucRatio']]}
    HurdleProp_forSen <- function(var){adjustHurdle(0.15,var , pars, F) %>% filter(col=='hurdleBirds') %>% .[['aucRatio']]}
  calculateSensitivity <- function(var, df, func){
    a <- func(var)
    # a_d <- func(var-df*var)
    a_Plus_d <- func(var+(df*var))
    
    S <- ((a_Plus_d - a) / a) / df
    return(S)
  }