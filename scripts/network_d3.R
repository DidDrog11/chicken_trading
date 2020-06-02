library(networkD3)

edge_data <- read.csv(here('cleaned_data', 'edgelist_heads.csv'), header = T) %>%
  split(., .$onset)

edgelist_2010 <- as.data.frame(edge_data[['2010']]) %>%
  rename('tail' = tail_vertex_number,
         'head' = head_vertex_number) %>%
  mutate(tail = tail-1,
         head = head-1) %>%
  select(tail, head, tail_vertex_id, sender_region_name)

nodelist_2010 <- as.data.frame(edge_data[['2010']]) %>%
  subset(., !duplicated(.$tail_vertex_id)) %>%
  rename('idn' = tail_vertex_id)
  

forceNetwork(Links = edgelist_2010, Nodes = nodelist_2010, Source = 'tail', Target = 'head',
             NodeID = 'tail_vertex_id', Group = 'sender_region_name', linkWidth = 1, zoom = T, opacity = 0.8,
             charge = -300, width = 600, height = 400)

edgelist_igraph <- read.csv(here('cleaned_data', 'edgelist_heads.csv'))
nodelist_igraph <- read_rds(here('cleaned_data', 'complete_nodelist.rds'))
production_heads  <- read_rds(here('cleaned_data', 'production_heads.rds'))

nodelist_igraph <- nodelist_igraph %>%
  left_join(., production_heads %>%
              rename('country' = node) %>%
              select(country, area, longitude, latitude, producer_region_name, producer_sub_region_name),
            by = 'country') %>%
  distinct()

#Complete igraph
edgelist_igraph <- edgelist_igraph %>%
  rename('sender' = tail_vertex_id,
         'receiver' = head_vertex_id,
         'year' = onset) %>%
  select(sender, receiver, year, greatest)

graph <- graph_from_data_frame(edgelist_igraph, directed = T, vertices = nodelist_igraph)
graph <- set_edge_attr(graph, 'weight', value = edgelist_igraph$greatest)
graph <-  subgraph.edges(graph, E(graph)[year == 2010], delete.vertices = TRUE)
graph

walk_trap <- cluster_walktrap(graph, weights = E(graph)$weight, steps = 4,
                              merges = TRUE, modularity = TRUE, membership = TRUE)
members <- membership(walk_trap)

graph_d3 <- igraph_to_networkD3(graph, group = members)

force_2010 <- forceNetwork(Links = graph_d3$links, Nodes = graph_d3$nodes,
                           Source = 'source', Target = 'target', NodeID = 'name',
                           Group = 'group', linkWidth = 1, fontSize=24, zoom=T, legend=F,
                           opacity = 0.8, charge=-300, 
                           width = 1200, height = 1200)
saveNetwork(force_2010, "forcenetwork_2010.html", selfcontained = TRUE)
