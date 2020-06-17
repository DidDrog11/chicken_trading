nodelist <- read_rds(here::here("cleaned_data", "nodelist_igraph_heads.rds"))
edgelist <- read_rds(here::here("cleaned_data", "edgelist_igraph_heads.rds"))

graph <- graph_from_data_frame(edgelist, directed = T, vertices = nodelist)

vertex_attr(graph)
edge_attr(graph)

table(head_of(graph, E(graph)[[year == 2010]]))
graph_2010 <- subgraph.edges(graph, E(graph)[year == 2010], delete.vertices = TRUE)

unique(neighbors(graph, "united_states_of_america", mode = c("all"))) #for all years the USA trades with 122 countries
unique(neighbors(graph, "united_states_of_america", mode = c("out"))) #it net exports to 121 countries
unique(neighbors(graph, "united_states_of_america", mode = c("in"))) #it net imports from 34

farthest_vertices(graph_2010)
get_diameter(graph_2010)
betweenness_2010 <- betweenness(graph_2010, directed = T)
hist(betweenness_2010, breaks = 30)


# Egocentric graphs -------------------------------------------------------
#egocentric graphs show the connections to other nodes
india_trade_2010 <- make_ego_graph(graph_2010, diameter(graph_2010), nodes = "india", mode = c("all"))[[1]]
distances <- as.data.frame(t(distances(india_trade_2010, "india"))) %>%
  rownames_to_column() %>%
  rename("country" = 1,
         "distance" = 2)
distances$distance <- as.factor(distances$distance)

edgelist_coordinates <- as_edgelist(india_trade_2010) %>%
  as_tibble() %>%
  full_join(nodelist, by = c("V1" = "country")) %>%
  full_join(nodelist, by = c("V2" = "country")) %>%
  drop_na(V1, V2)

nodelist_india <- nodelist %>%
  filter(country %in% c(edgelist_coordinates$V1, edgelist_coordinates$V2)) %>%
  left_join(., distances, by = "country")

node_bounds_india <- nodelist_india %>%
  select(long, lat) %>%
  drop_na %>%
  sp::SpatialPointsDataFrame(coords = .[,1:2], data = .) %>%
  sp::bbox() * 1.1

india <- ggplot() +
  geom_polygon(data = map_world,
               aes(long, lat, group = group, fill = region), 
               show.legend = FALSE,
               alpha = 0.25,
               color = "white") +
  geom_segment(data = edgelist_coordinates, 
               aes(x = long.x, xend = long.y,
                   y = lat.x, yend = lat.y),
               arrow = arrow(length = unit(0.1, "inches"), # optional arrows
                             type = "closed"),
               size = 0.25,
               alpha = 0.5) +
  geom_point(data = nodelist_india,
             aes(long, lat, color = distance)) +
  theme_bw() +
  theme(axis.line = element_line(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        axis.ticks = element_blank(),
        axis.title = element_blank(),
        axis.text = element_blank()) +
  scale_colour_brewer(type = "qual", palette = "Dark2") +
  labs(title = "Indian Chicken Trade Network 2010",
       subtitle = "Polyconic projection",
       caption = "David Simons") +
  coord_map("polyconic",
            xlim = c(node_bounds_india["long", "min"],
                     node_bounds_india["long", "max"]),
            ylim = c(node_bounds_india["lat", "min"],
                     node_bounds_india["lat", "max"]))
