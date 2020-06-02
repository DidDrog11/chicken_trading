library(ndtv)

chicken_heads <-  read_rds(here('network_files', 'chicken_heads_network_format.rds'))

chicken_heads <- network.extract(chicken_heads, onset = 2017, terminus = 2018, trim.spells = T, retain.all.vertices = F)

render.d3movie(chicken_heads,
               usearrows = F, displaylabels = F,
               vertex.tooltip = paste("<b>Name:</b>", (chicken_heads %v% 'vertex.names') , "<br>"),
               edge.tooltip = paste("<b>Edge weight:</b>", (chicken_heads %e% "greatest" ) ),
               launchBrowser=F, filename="interactive_chicken_trade.html")
