library(ndtv)
library(tidyverse)
library(here)

chicken_heads <-  read_rds(here('network_files', 'chicken_heads_network_format.rds'))

##Works fine for a single time slice
chicken_heads <- network.extract(chicken_heads, onset = 2017, terminus = 2018, trim.spells = T, retain.all.vertices = F)

render.d3movie(chicken_heads,
               usearrows = F, displaylabels = F,
               vertex.tooltip = paste("<b>Name:</b>", (chicken_heads %v% 'vertex.names') , "<br>"),
               edge.tooltip = paste("<b>Edge weight:</b>", (chicken_heads %e% "greatest" ) ),
               launchBrowser=F, filename="interactive_chicken_trade.html")

#Unable to generate anything meaningful for multiple
chicken_heads <- network.extract(chicken_heads, onset = 2015, terminus = 2018, trim.spells = T, retain.all.vertices = F)

render.d3movie(chicken_heads,
               usearrows = F, displaylabels = F,
               vertex.tooltip = paste("<b>Name:</b>", (chicken_heads %v% 'vertex.names') , "<br>"),
               edge.tooltip = paste("<b>Edge weight:</b>", (chicken_heads %e% "greatest" ) ),
               launchBrowser=F, filename="interactive_chicken_trade.html")
