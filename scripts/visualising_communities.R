library('linkcomm')

linkcomm_edgelist <-  read_rds(here('cleaned_data', '1000_head_edgelist.rds')) %>%
  select(sender, receiver, year, greatest) %>%
  split(.$year) %>%
  lapply(., function(x) x[(names(x) %in% c('sender', 'receiver', 'greatest'))])


# Unweighted --------------------------------------------------------------

lc_2010_unweighted <- as.data.frame(linkcomm_edgelist[['2010']]) %>%
  select(sender, receiver)

lc_2010_unweighted <- getLinkCommunities(lc_2010_unweighted, directed = T, hcmethod = 'single')
lc_2010_unweighted$nodeclusters
lc_2010_unweighted$numclusters

plot(lc_2010_unweighted, type = "graph", layout = layout.fruchterman.reingold)
plot(lc_2010_unweighted, type = "graph", layout = layout.fruchterman.reingold, shownodesin = 3)
plot(lc_2010_unweighted, type = "graph", layout = layout.fruchterman.reingold, shownodesin = 4) #The members can be displayed for specific communities
plot(lc_2010_unweighted, type = "members") #Plot displaying community membership

nested_communities_uwt <- getAllNestedComm(lc_2010_unweighted) %>%
  names() %>%
  as.numeric()

nested_community_plots <- vector('list', length(nested_communities_uwt))
for (i in nested_communities_uwt) {
  nested_community_plots[[i]] <- getNestedHierarchies(lc_2010_unweighted, clusid = i)
}
nested_community_plots_uwt <- compact(nested_community_plots) #This generates nested communities of the trading network for 2010

cluster_relatedness_uwt <- getClusterRelatedness(lc_2010_unweighted, hcmethod = "ward.D")
cluster_dendrogram_uwt <- cutDendrogramAt(cluster_relatedness_uwt, cutat = 1) #A dendrogram can be produced that can be set to produce different clusters based on the cut

mc <- meta.communities(lc_2010_unweighted, hcmethod = "ward.D", deepSplit = 0) #If metacommunities are preferred

cc <- getCommunityCentrality(lc_2010_unweighted) #The centrality of the nodes within the clusters can be calculated

cm <- getCommunityConnectedness(lc_2010_unweighted, conn = "modularity") #Clusters 1,2,22,30 and 39 display the greatest modularity which is the inverse of connectedness
plot(lc_2010_unweighted, type = "commsumm", summary = "modularity")
plot(lc_2010_unweighted, type = "graph", layout = "spencer.circle")

greatest_modularity_uwt <- getNodesIn(lc_2010_unweighted, clusterids = c(1,2,22,30,39))
get.shared.nodes(lc_2010_unweighted, comms = c(3,4))

plot(lc_2010_unweighted, type = 'graph', layout = 'spencer.circle', vsize = 5, ewidth = 1, vshape = 'pie')

# Weighted inclusion ------------------------------------------------------
lc_2010_weighted <- as.data.frame(linkcomm_edgelist[['2010']])

lc_2010_weighted <- getLinkCommunities(lc_2010_weighted, directed = T, hcmethod = 'single')
lc_2010_weighted$nodeclusters
lc_2010_weighted$numclusters #When incorporating weighting there are fewer numbers of clusters

plot(lc_2010_weighted, type = "graph", layout = layout.fruchterman.reingold)
plot(lc_2010_weighted, type = "graph", layout = layout.fruchterman.reingold, shownodesin = 3)
plot(lc_2010_weighted, type = "graph", layout = layout.fruchterman.reingold, shownodesin = 2) #The members can be displayed for specific communities
plot(lc_2010_weighted, type = "members") #Plot displaying community membership


nested_communities_wt <- getAllNestedComm(lc_2010_weighted) %>%
  names() %>%
  as.numeric()

nested_community_plots_wt <- vector('list', length(nested_communities_wt))
for (i in nested_communities_wt) {
  nested_community_plots_wt[[i]] <- getNestedHierarchies(lc_2010_weighted, clusid = i)
}
nested_community_plots_wt <- compact(nested_community_plots) #This generates nested communities of the trading network for 2010

cluster_relatedness <- getClusterRelatedness(lc_2010_weighted, hcmethod = "ward.D")
cluster_dendrogram <- cutDendrogramAt(cluster_relatedness, cutat = 1) #A dendrogram can be produced that can be set to produce different clusters based on the cut

mc <- meta.communities(lc_2010_weighted, hcmethod = "ward.D", deepSplit = 0) #If metacommunities are preferred

cc <- getCommunityCentrality(lc_2010_weighted) #The centrality of the nodes within the clusters can be calculated

cm <- getCommunityConnectedness(lc_2010_weighted, conn = "modularity") #Clusters 1,2,4,and 5 display the greatest modularity which is the inverse of connectedness
plot(lc_2010_weighted, type = "commsumm", summary = "modularity")
plot(lc_2010_weighted, type = "graph", layout = "spencer.circle")

greatest_modularity <- getNodesIn(lc_2010_weighted, clusterids = c(1,2,4,5))
get.shared.nodes(lc_2010_weighted, comms = c(3,4))

plot(lc_2010_weighted, type = 'graph', layout = 'spencer.circle', vsize = 5, ewidth = 1, vshape = 'pie')



# OCG Clusters ------------------------------------------------------------

oc <- as.data.frame(linkcomm_edgelist[['2010']]) %>%
  select(sender, receiver)
oc$sender <- str_sub(oc$sender, end = 20)
oc$receiver <- str_sub(oc$receiver, end =  20)

oc <- getOCG.clusters(oc)
plot(oc, type = "graph", shownodesin = 7, scale.vertices = 0.1)