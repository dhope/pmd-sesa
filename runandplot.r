g_legend<-function(a.gplot){
  tmp <- ggplot_gtable(ggplot_build(a.gplot))
  leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box")
  legend <- tmp$grobs[[leg]]
  return(legend)}
require(cowplot)
runAndPlot<- function(runs, fun, pars, col, subsetVar1) {

results.ls <- map2(runs[,1], runs[,2], fun, pars = pars , returnAll =T) %>% purrr::transpose()
	Adjust.res <- do.call('rbind', results.ls$results)
	Adjust.counts <- do.call('rbind', results.ls$counts)
	d <-  col
	res_plt <- 
	filter(Adjust.res, col == d) %>% 
	  ggplot(aes(par1, aucRatio,colour = as.factor(par2)))+ geom_line() + 
	   theme(legend.position = 'none')
	
	
	histex <- 
	  filter(Adjust.counts, par1 %in% subsetVar1) %>% mutate_(Birds = col) %>%
	  ggplot( aes(1-danger,Birds , colour = as.factor(par2), group = par2)) + facet_grid(.~par1, scales='free_y') +
	  # geom_line()
	  geom_line( stat='identity', alpha = 1) +  theme(legend.position='none')
	
	ecdfplt <- Adjust.counts %>% mutate(safety = 1-danger) %>% 
	  group_by(par1, par2) %>% arrange(safety) %>%
	  mutate_(totalArea = "sum(area)",
	         totalBirds = paste0("sum(",col,")"),
	         cArea = "cumsum(area/totalArea)",
	         cBirds = paste0("cumsum(",col,"/totalBirds)")) %>% ungroup %>% 
	  filter(par1 %in% subsetVar1) %>%
	  ggplot( aes(safety, cBirds,  colour = as.factor(par2), group = par2)) + facet_grid(.~par1) +
	  # geom_line()
	  geom_line(  alpha = 1) +  theme(legend.position='none') +
	  geom_line(aes(y=cArea), colour = 'black')
	
	legendColours <- get_legend(res_plt +labs(colour = "") + theme(legend.position="bottom",legend.text = element_text(size = 8)))
	  #g_legend(res_plt+labs(colour = "")+
	                        #    theme(legend.position="bottom",legend.text = element_text(size = 8))) 
	
	
	gridExtra::grid.arrange(histex+xlab(""),ecdfplt, res_plt + geom_vline(xintercept = subsetVar1), nrow = 3)
	
	return(list("results" = results.ls, "resultsPlot" = res_plt, "histogram" = histex, "cumulative" = ecdfplt, "legend" = legendColours))

}