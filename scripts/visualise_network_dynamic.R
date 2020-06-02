library('network')
library(tsna)
library(here)
library(tidyverse)

head_network <- readRDS(here('cleaned_data', 'chicken_heads_network.rds'))
head_network%n%'net.obs.period'<-list(
  observations=list(c(1986,2017)),
  mode="discrete", 
  time.increment=1,
  time.unit="year")

plot(head_network)

tEdgeFormation(head_network) #the number of edges forming at each timepoint
plot(tEdgeFormation(head_network))
plot(tSnaStats(head_network,'components', end = 2017)) #Components in the network
plot(tSnaStats(head_network,'connectedness', end = 2017)) #Connectedness in the network
plot(tSnaStats(head_network,'gden', end = 2017)) #Density
plot(tSnaStats(head_network,'hierarchy', end = 2017)) #Hierarchy scores
