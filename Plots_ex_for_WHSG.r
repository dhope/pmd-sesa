resetPars()
test_sim <- 	simulation(pars =pars, returnALL = T)

stripaxis <- theme(axis.text = element_blank(), axis.ticks=element_blank()) 

unifPlot <- 
ggplot(test_sim$counts, aes(safety, uniformBirds)) + geom_line(colour = 'red') + 
  stripaxis + 
  labs(y="", x="")

areaMatchplt <- 
ggplot(test_sim$counts, aes(safety, areaMatchingBirds)) + geom_line(colour = 'red') + 
  stripaxis + 
  labs(y="", x="")
dangermatchplt <- 
ggplot(test_sim$counts, aes(safety, dangerMatchingBirds)) + geom_line(colour = 'red') + 
  stripaxis + 
  labs(y="", x="")
hurdleplt <- 
ggplot(test_sim$counts, aes(safety, hurdleBirds)) + geom_line(colour = 'red') + 
  stripaxis + 
  labs(y="", x="")

dangerhurdleplt <- 
ggplot(test_sim$counts, aes(safety, aggDanger)) + geom_line(colour = 'red') + 
  stripaxis + 
  labs(y="", x="")

midhurdleplt <- 
ggplot(test_sim$counts, aes(safety, aggMid)) + geom_line(colour = 'red') + 
  stripaxis + 
  labs(y="", x="")


explotsall <- 
plot_grid(dangerhurdleplt, unifPlot,midhurdleplt, areaMatchplt,
	 dangermatchplt, hurdleplt, nrow = 1)


