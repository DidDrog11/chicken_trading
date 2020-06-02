library(sna)
library(tidyverse)
library(here)
library(ndtv)

nodelist <- read.csv(here('cleaned_data', 'nodelist_heads.csv'), header = T)
edge_data <- read.csv(here('cleaned_data', 'edgelist_heads.csv'), header = T)
edgelist <- edge_data %>%
  rename('tail' = tail_vertex_number,
         'head' = head_vertex_number) %>%
  select(onset, terminus, tail, head, greatest)

node_name_1 <- unique(edge_data[c('tail_vertex_id', 'tail_vertex_number')]) %>%
  rename('country' = 1,
         'node_id' = 2) %>%
  as_tibble()
node_name_2 <- unique(edge_data[c('head_vertex_id', 'head_vertex_number')]) %>%
  rename('country' = 1,
         'node_id' = 2) %>%
  as_tibble()
node_name <- bind_rows(node_name_1, node_name_2) %>%
  distinct() %>%
  arrange(node_id)

write_rds(node_name, here('cleaned_data', 'complete_nodelist.rds'))


head(nodelist)
head(edgelist)
head(node_name)

chicken_trade_heads <-networkDynamic(edge.spells = edgelist,
                                     vertex.spells = nodelist,
                                     create.TEAs = T,
                                     end = 2017)
chicken_trade_heads %v% 'vertex.names' <- node_name$country
set.network.attribute(chicken_trade_heads, 'vertex.pid', 'vertex.names')
# There are some missing TEAs for some countries, not sure how important this is

chicken_trade_heads%n%'net.obs.period'<-list(
  observations=list(c(1986,2017)),
  mode="discrete", 
  time.increment=1,
  time.unit="year")
# Tells the network dynamics object what each unit of time is

write_rds(chicken_trade_heads, here('cleaned_data', 'chicken_heads_network.rds'))

list.edge.attributes(chicken_trade_heads)
list.vertex.attributes(chicken_trade_heads)

print(chicken_trade_heads)
as.data.frame(chicken_trade_heads)
plot(network.extract(chicken_trade_heads,at=1986))
plot(network.extract(chicken_trade_heads,at=2017))
plot(network.extract(chicken_trade_heads,onset=1987,terminus=2004))

plot.network(chicken_trade_heads)
write_rds(chicken_trade_heads, here('network_files', 'chicken_heads_network_format.rds'))

timeline(chicken_trade_heads, plot.edge.spells=FALSE)

saveVideo(render.animation(chicken_trade_heads, render.cache = 'none'), video.name = 'chicken_trade.mp4')

compute.animation(chicken_trade_heads,
                  animation.mode='MDSJ',verbose = FALSE)
render.d3movie(chicken_trade_heads)
