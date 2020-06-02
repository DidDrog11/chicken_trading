library(tidyverse)
library(igraph)
library(here)
library(ndtv)
nodelist_complete <- read_rds(here('cleaned_data', 'complete_nodelist.rds'))

production_heads <- read.csv(here('cleaned_data', 'production_heads_long.csv')) %>%
  select(country, year, heads, area, longitude, latitude, region_name, sub_region_name) %>%
  rename('node' = country,
         'onset' = year,
         'production' = heads,
         'producer_region_name' = region_name,
         'producer_sub_region_name' = sub_region_name)

write_rds(production_heads, here('cleaned_data', 'production_heads.rds'))

world_data <- read_rds(here('cleaned_data', 'world_data.RDS'))
edgelist_file <-  read_rds(here('cleaned_data', '1000_head_edgelist.rds'))

nodelist <- as.data.frame(sort(unique(c(edgelist_file$sender, edgelist_file$receiver)))) %>%
  rename('node' = 1) %>%
  mutate('tail_vertex_id' = node,
         'head_vertex_id' = node) %>%
  mutate(tail_vertex_number = 1:length(node),
         head_vertex_number = 1:length(node))

edgelist <- edgelist_file %>%
  mutate(terminus = year+1) %>%
  rename('onset' = year,
         'tail_vertex_id' = receiver,
         'head_vertex_id' = sender) %>%
  select(onset, terminus, tail_vertex_id, head_vertex_id, greatest, sender_region_name, sender_sub_region_name,
         receiver_region_name, receiver_sub_region_name) %>%
  left_join(., nodelist %>%
              select(tail_vertex_id, tail_vertex_number),
            by = 'tail_vertex_id') %>%
  left_join(., nodelist %>%
              select(head_vertex_id, head_vertex_number),
            by = 'head_vertex_id')
  

write.csv(edgelist, here('cleaned_data', 'edgelist_heads.csv'), row.names = F)

nodelist <-nodelist %>%
  select(node, tail_vertex_number) %>%
  rename(node_id = 'tail_vertex_number') %>%
  slice(rep(1:n(), each = 32)) %>% # Repeat out for each year in the matrix
  mutate(onset = rep(1986:2017, len = length(node)),
         terminus = rep(1987:2018, len = length(node))) %>%
  select(onset, terminus, node_id, node) %>%
  left_join(., production_heads, by = c('node', 'onset')) %>% # Join this with the production data taken from the FAO data
  rename('tail_vertex_id' = node) %>%
  semi_join(., edgelist, by = c('tail_vertex_id', 'onset')) %>% # Discard the production years without data in the trading matrix
  rename('node' = tail_vertex_id)

write.csv(nodelist, here('cleaned_data', 'nodelist_heads.csv'), row.names = F)

nodelist_igraph <- nodelist_complete %>%
  left_join(., production_heads %>%
              rename('country' = node) %>%
              select(country, area, longitude, latitude, producer_region_name, producer_sub_region_name),
            by = 'country') %>%
  distinct()
  

#Complete igraph
edgelist_igraph <- edgelist_file %>%
  select(sender, receiver, year, greatest)

graph <- graph_from_data_frame(edgelist_igraph, directed = T, vertices = nodelist_igraph)
graph <- set_edge_attr(graph, 'weight', value = edgelist_igraph$greatest)
graph


V(graph)$community <- V(graph)$producer_region_name
graph_92 <-  subgraph.edges(graph, E(graph)[year == 1992], delete.vertices = TRUE)

plot(graph_92, layout = layout_with_lgl(graph_92))


vertex_attr(graph, "label") <- V(graph)$name
vertex_attr(graph, "region") <- V(graph)

E(graph)       # The edges of the "graph" object
V(graph)       # The vertices of the "graph" object

plot.igraph(graph)

vshape <-  vector(length = vcount(graph))
vcolour <-  vector(length = vcount(graph))

africa <- which(edgelist_2017$sender_region_name == 'africa')
americas <- which(edgelist_2017$sender_region_name == 'americas')
asia <- which(edgelist_2017$sender_region_name == 'asia')
europe <- which(edgelist_2017$sender_region_name == 'europe')
oceania <- which(edgelist_2017$sender_region_name == 'oceania')

vshape[africa] <- 'square'
vshape[americas] <- 'rectangle'
vshape[asia] <- 'circle'
vshape[europe] <- 'csquare'
vshape[oceania] <- 'vrectangle'

#Various layouts can be used

