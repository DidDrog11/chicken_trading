library(tidyverse)
library(igraph)

nodelist <- read_rds(here::here("cleaned_data", "nodelist_igraph_heads.rds")) %>%
  mutate(producer_region_name = ifelse(country %in% c("belgiumluxembourg", "czechoslovakia", "faroe_islands", "greenland", "serbia_and_montenegro", "ussr", "yugoslav_sfr"),
                                       "europe", as.character(producer_region_name)),
         producer_region_name = ifelse(country %in% c("cote_divoire", "djibouti", "ethiopia_pdr", "sudan_former", "reunion"),
                                       "africa", as.character(producer_region_name)),
         producer_region_name = ifelse(country == "aruba", "americas", producer_region_name),
         producer_region_name = ifelse(country %in% c("british_indian_ocean_territory", "china_mainland", "maldives"), "asia",
                                       as.character(producer_region_name)),
         producer_region_name = ifelse(country == "unspecified_area", "unknown", as.character(producer_region_name)))
edgelist <- read_rds(here::here("cleaned_data", "edgelist_igraph_heads.rds"))

graph <- graph_from_data_frame(edgelist, directed = T, vertices = nodelist)

vertex_attr(graph)
edge_attr(graph[[2]])

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
  rename("origin" = 1,
         "destination" = 2)
  left_join(., wrld_simpl@data %>%
              select("ISO3", "LON", "LAT"))
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


# Edge density and assortativity ------------------------------------------------------------

edge_density(graph_2010)
values_region <-  as.numeric(factor(V(graph_2010)$producer_region_name))
observed_assortativity <- assortativity(graph_2010, values_region) #there is assortativity based on region
assortativity.degree(graph_2010, directed = T) #highly connected nodes do not preferrentially connect to other highly connected nodes.

results <- vector('list', 1000)
for(i in 1:1000){
  results[[i]] <- assortativity(graph_2010, sample(values_region))
}

ggplot(data =as.data.frame(unlist(results)), aes(x = unlist(results)))+
  geom_histogram()+
  geom_vline(xintercept = observed_assortativity)


# Community assessment in igraph ------------------------------------------

walktrap_2010 <- walktrap.community(graph_2010, weights = E(graph_2010)$greatest, merges = T, steps = 5, membership = T)
plot(walktrap_2010, graph_2010)
membership(walktrap_2010)
sizes(walktrap_2010)

infomap_community_2010 <- infomap.community(graph_2010, e.weights = E(graph_2010)$greatest)
plot(infomap_community_2010, graph_2010)
membership <- membership(infomap_community_2010)
sizes(infomap_community_2010)
graph_2010 <- set_vertex_attr(graph_2010, "community", value = c(membership))

graph_year <- vector("list", n_distinct(E(graph)$year))
names(graph_year) <- sort(unique(E(graph)$year))
walktrap_community <- vector("list", n_distinct(E(graph)$year))
names(walktrap_community) <- sort(unique(E(graph)$year))
membership <- vector("list", n_distinct(E(graph)$year))
names(membership) <- sort(unique(E(graph)$year))
country_communities <- vector("list", n_distinct(E(graph)$year))
names(country_communities) <- sort(unique(E(graph)$year))
largest_community <- vector("list", n_distinct(E(graph)$year))
names(largest_community) <- sort(unique(E(graph)$year))
sub_community <- vector("list", n_distinct(E(graph)$year))
names(sub_community) <- sort(unique(E(graph)$year))
sub_community_membership <- vector("list", n_distinct(E(graph)$year))
names(sub_community_membership) <- sort(unique(E(graph)$year))


for (i in names(graph_year)) {
  graph_year[[i]] <- subgraph.edges(graph, E(graph)[year == i], delete.vertices = TRUE)
  walktrap_community[[i]] <- walktrap.community(graph_year[[i]], weights = E(graph_year[[i]])$greatest, merges = T, steps = 5, membership = T)
  membership[[i]] <- membership(walktrap_community[[i]])
  graph_year[[i]] <- set_vertex_attr(graph_year[[i]], "community", value = c(membership[[i]]))
  country_communities[[i]] <- cbind(get.vertex.attribute(graph_year[[i]], "name"), get.vertex.attribute(graph_year[[i]], "community"))
  largest_community[[i]] <- induced_subgraph(graph_year[[i]], which(membership(walktrap_community[[i]]) ==  which.max(sizes(walktrap_community[[i]]))))
  sub_community[[i]] <- walktrap.community(largest_community[[i]], weights = E(largest_community[[i]])$greatest, merges = T, steps = 5, membership = T)
  sub_community_membership[[i]] <- membership(sub_community[[i]])
}

communities <- plyr::ldply(country_communities, data.frame) %>%
  rename("year" = 1,
         "country" = 2,
         "community" = 3) %>%
  pivot_wider(names_from = year, values_from = community)
communities


largest_community[["2010"]] <- induced_subgraph(graph_year[["2010"]], which(membership(walktrap_community[["2010"]]) ==  which.max(sizes(walktrap_community[["2010"]]))))
sub_community[["2010"]] <- walktrap.community(largest_community[["2010"]], weights = E(largest_community[["2010"]])$greatest, merges = T, steps = 5, membership = T)
sub_community_membership[["2014"]] <- membership(sub_community[["2010"]])
graph_year[["2010"]] <- set_vertex_attr(graph_year[["2010"]], "sub_community", value = c(sub_community_membership[["2010"]]))
