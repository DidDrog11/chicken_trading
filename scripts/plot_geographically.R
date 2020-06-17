library('tidyverse')
library('igraph')
library('ggmap')
library('ggplot2')
library('mapproj')

nodelist <- read_rds(here::here("cleaned_data", "nodelist_igraph_heads.rds")) %>%
  select(-longitude, -latitude)
edgelist <- read_rds(here::here("cleaned_data", "edgelist_igraph_heads.rds")) %>%
  filter(sender != "unspecified_area")

nodelist <- ggmap::mutate_geocode(nodelist, country) #Jordan and Togo need changing
nodelist <- nodelist %>%
  mutate(lon = ifelse(country == "jordan", 30.5852, lon),
         lat = ifelse(country == "jordan", 36.2384, lat),
         lon = ifelse(country == "togo", 8.6195, lon),
         lat = ifelse(country == "togo", 0.8248, lat),
         lon = ifelse(country == "ethiopia_pdr", 9.1450, lon),
         lat = ifelse(country == "ethiopia_pdr", 40.4897, lat)) %>%
  rename("long" = lon)

graph <- graph_from_data_frame(edgelist, directed = T, vertices = nodelist)
graph_2010 <-  subgraph.edges(graph, E(graph)[year == 2010], delete.vertices = TRUE)
europe_graph_2010 <-  induced_subgraph(graph, which(V(graph)$producer_region_name %in% "europe"))

node_bounds <- nodelist %>%
  select(long, lat) %>%
  drop_na %>%
  sp::SpatialPointsDataFrame(coords = .[,1:2], data = .) %>%
  sp::bbox() * 1.1

write_rds(nodelist, here::here("cleaned_data", "nodelist_igraph_heads.rds"))

# Global trade ------------------------------------------------------------

edgelist_coordinates <- as_edgelist(graph_2010) %>%
  as_tibble() %>%
  full_join(nodelist, by = c("V1" = "country")) %>%
  full_join(nodelist, by = c("V2" = "country")) %>%
  drop_na(V1, V2)
  
map_world <- map_data(map = "world") %>%
  filter(region != "Antarctica")

global_trade <- ggplot() +
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
  geom_point(data = nodelist,
             aes(long, lat), color = "red") +
  theme_bw() +
  theme(axis.line = element_line(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        axis.ticks = element_blank(),
        axis.title = element_blank(),
        axis.text = element_blank()) +
  labs(title = "Net Global Chicken Trade 2010",
       subtitle = "Polyconic projection",
       caption = "David Simons") +
  coord_map("polyconic",
            xlim = c(node_bounds["long", "min"],
                     node_bounds["long", "max"]),
            ylim = c(node_bounds["lat", "min"],
                     node_bounds["lat", "max"]))
# European trade ----------------------------------------------------------

edgelist_europe <- as_edgelist(europe_graph_2010) %>%
  as_tibble() %>%
  full_join(nodelist, by = c("V1" = "country")) %>%
  full_join(nodelist, by = c("V2" = "country")) %>%
  drop_na(V1, V2)

nodelist_europe <- nodelist %>%
  filter(producer_region_name == "europe")

node_bounds_europe <- nodelist_europe %>%
  select(long, lat) %>%
  drop_na %>%
  sp::SpatialPointsDataFrame(coords = .[,1:2], data = .) %>%
  sp::bbox() * 1.1

betweenness_europe <- stack(betweenness(europe_graph_2010, directed = T)) %>%
  rename("country" = 2,
         "betweenness" = 1)

nodelist_europe <- nodelist_europe %>%
  full_join(., betweenness_europe, by = "country")

europe_trade <- ggplot() +
  geom_polygon(data = map_world,
               aes(long, lat, group = group, fill = region), 
               show.legend = FALSE,
               alpha = 0.25,
               color = "white") +
  geom_segment(data = edgelist_europe, 
               aes(x = long.x, xend = long.y,
                   y = lat.x, yend = lat.y),
               arrow = arrow(length = unit(0.1, "inches"), # optional arrows
                             type = "closed"),
               size = 0.1,
               alpha = 0.5) +
  geom_point(data = nodelist_europe,
             aes(long, lat,
                 size = sqrt(betweenness)+1), color = "red") +
  theme_bw() +
  theme(axis.line = element_line(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        axis.ticks = element_blank(),
        axis.title = element_blank(),
        axis.text = element_blank()) +
  labs(title = "Net European Chicken Trade 2010",
       subtitle = "Polyconic projection",
       caption = "David Simons") +
  coord_map("polyconic",
            xlim = c(node_bounds_europe["long", "min"],
                     node_bounds_europe["long", "max"]),
            ylim = c(node_bounds_europe["lat", "min"],
                     node_bounds_europe["lat", "max"]))
