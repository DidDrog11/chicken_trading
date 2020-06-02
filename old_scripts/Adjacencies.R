#What I'd like to do is create an adjacency matrix of trade between countries for these years so I will make edge lists for a years worth of trade
#I'm sure there's a much more elegant way of doing this but I thought it would be quicker to just do this

# Adjacencies -------------------------------------------------------------
EdgeList_1986 <- matrix(data = 0, nrow = 2, ncol = 2)
Importers_1986 <- Chicken_trade_year[["1986"]][["Reporter Country Code"]]
Exporters_1986 <- Chicken_trade_year[["1986"]][["Partner Country Code"]]
EdgeList_1986 <- matrix(data = c(Importers_1986, Exporters_1986), ncol = 2)
Adjacency_1986 <- get.adjacency(graph.edgelist(EdgeList_1986, directed = T))

EdgeList_1987 <- matrix(data = 0, nrow = 2, ncol = 2)
Importers_1987 <- Chicken_trade_year[["1987"]][["Reporter Country Code"]]
Exporters_1987 <- Chicken_trade_year[["1987"]][["Partner Country Code"]]
EdgeList_1987 <- matrix(data = c(Importers_1987, Exporters_1987), ncol = 2)
Adjacency_1987 <- get.adjacency(graph.edgelist(EdgeList_1987, directed = T))

EdgeList_1988 <- matrix(data = 0, nrow = 2, ncol = 2)
Importers_1988 <- Chicken_trade_year[["1988"]][["Reporter Country Code"]]
Exporters_1988 <- Chicken_trade_year[["1988"]][["Partner Country Code"]]
EdgeList_1988 <- matrix(data = c(Importers_1988, Exporters_1988), ncol = 2)
Adjacency_1988 <- get.adjacency(graph.edgelist(EdgeList_1988, directed = T))

EdgeList_1989 <- matrix(data = 0, nrow = 2, ncol = 2)
Importers_1989 <- Chicken_trade_year[["1989"]][["Reporter Country Code"]]
Exporters_1989 <- Chicken_trade_year[["1989"]][["Partner Country Code"]]
EdgeList_1989 <- matrix(data = c(Importers_1989, Exporters_1989), ncol = 2)
Adjacency_1989 <- get.adjacency(graph.edgelist(EdgeList_1989, directed = T))

EdgeList_1990 <- matrix(data = 0, nrow = 2, ncol = 2)
Importers_1990 <- Chicken_trade_year[["1990"]][["Reporter Country Code"]]
Exporters_1990 <- Chicken_trade_year[["1990"]][["Partner Country Code"]]
EdgeList_1990 <- matrix(data = c(Importers_1990, Exporters_1990), ncol = 2)
Adjacency_1990 <- get.adjacency(graph.edgelist(EdgeList_1990, directed = T))

EdgeList_1991 <- matrix(data = 0, nrow = 2, ncol = 2)
Importers_1991 <- Chicken_trade_year[["1991"]][["Reporter Country Code"]]
Exporters_1991 <- Chicken_trade_year[["1991"]][["Partner Country Code"]]
EdgeList_1991 <- matrix(data = c(Importers_1991, Exporters_1991), ncol = 2)
Adjacency_1991 <- get.adjacency(graph.edgelist(EdgeList_1991, directed = T))

EdgeList_1992 <- matrix(data = 0, nrow = 2, ncol = 2)
Importers_1992 <- Chicken_trade_year[["1992"]][["Reporter Country Code"]]
Exporters_1992 <- Chicken_trade_year[["1992"]][["Partner Country Code"]]
EdgeList_1992 <- matrix(data = c(Importers_1992, Exporters_1992), ncol = 2)
Adjacency_1992 <- get.adjacency(graph.edgelist(EdgeList_1992, directed = T))

EdgeList_1993 <- matrix(data = 0, nrow = 2, ncol = 2)
Importers_1993 <- Chicken_trade_year[["1993"]][["Reporter Country Code"]]
Exporters_1993 <- Chicken_trade_year[["1993"]][["Partner Country Code"]]
EdgeList_1993 <- matrix(data = c(Importers_1993, Exporters_1993), ncol = 2)
Adjacency_1993 <- get.adjacency(graph.edgelist(EdgeList_1993, directed = T))

EdgeList_1994 <- matrix(data = 0, nrow = 2, ncol = 2)
Importers_1994 <- Chicken_trade_year[["1994"]][["Reporter Country Code"]]
Exporters_1994 <- Chicken_trade_year[["1994"]][["Partner Country Code"]]
EdgeList_1994 <- matrix(data = c(Importers_1994, Exporters_1994), ncol = 2)
Adjacency_1994 <- get.adjacency(graph.edgelist(EdgeList_1994, directed = T))

EdgeList_1995 <- matrix(data = 0, nrow = 2, ncol = 2)
Importers_1995 <- Chicken_trade_year[["1995"]][["Reporter Country Code"]]
Exporters_1995 <- Chicken_trade_year[["1995"]][["Partner Country Code"]]
EdgeList_1995 <- matrix(data = c(Importers_1995, Exporters_1995), ncol = 2)
Adjacency_1995 <- get.adjacency(graph.edgelist(EdgeList_1995, directed = T))

EdgeList_1996 <- matrix(data = 0, nrow = 2, ncol = 2)
Importers_1996 <- Chicken_trade_year[["1996"]][["Reporter Country Code"]]
Exporters_1996 <- Chicken_trade_year[["1996"]][["Partner Country Code"]]
EdgeList_1996 <- matrix(data = c(Importers_1996, Exporters_1996), ncol = 2)
Adjacency_1996 <- get.adjacency(graph.edgelist(EdgeList_1996, directed = T))

EdgeList_1997 <- matrix(data = 0, nrow = 2, ncol = 2)
Importers_1997 <- Chicken_trade_year[["1997"]][["Reporter Country Code"]]
Exporters_1997 <- Chicken_trade_year[["1997"]][["Partner Country Code"]]
EdgeList_1997 <- matrix(data = c(Importers_1997, Exporters_1997), ncol = 2)
Adjacency_1997 <- get.adjacency(graph.edgelist(EdgeList_1997, directed = T))

EdgeList_1998 <- matrix(data = 0, nrow = 2, ncol = 2)
Importers_1998 <- Chicken_trade_year[["1998"]][["Reporter Country Code"]]
Exporters_1998 <- Chicken_trade_year[["1998"]][["Partner Country Code"]]
EdgeList_1998 <- matrix(data = c(Importers_1998, Exporters_1998), ncol = 2)
Adjacency_1998 <- get.adjacency(graph.edgelist(EdgeList_1998, directed = T))

EdgeList_1999 <- matrix(data = 0, nrow = 2, ncol = 2)
Importers_1999 <- Chicken_trade_year[["1999"]][["Reporter Country Code"]]
Exporters_1999 <- Chicken_trade_year[["1999"]][["Partner Country Code"]]
EdgeList_1999 <- matrix(data = c(Importers_1999, Exporters_1999), ncol = 2)
Adjacency_1999 <- get.adjacency(graph.edgelist(EdgeList_1999, directed = T))

EdgeList_2000 <- matrix(data = 0, nrow = 2, ncol = 2)
Importers_2000 <- Chicken_trade_year[["2000"]][["Reporter Country Code"]]
Exporters_2000 <- Chicken_trade_year[["2000"]][["Partner Country Code"]]
EdgeList_2000 <- matrix(data = c(Importers_2000, Exporters_2000), ncol = 2)
Adjacency_2000 <- get.adjacency(graph.edgelist(EdgeList_2000, directed = T))

EdgeList_2001 <- matrix(data = 0, nrow = 2, ncol = 2)
Importers_2001 <- Chicken_trade_year[["2001"]][["Reporter Country Code"]]
Exporters_2001 <- Chicken_trade_year[["2001"]][["Partner Country Code"]]
EdgeList_2001 <- matrix(data = c(Importers_2001, Exporters_2001), ncol = 2)
Adjacency_2001 <- get.adjacency(graph.edgelist(EdgeList_2001, directed = T))

EdgeList_2002 <- matrix(data = 0, nrow = 2, ncol = 2)
Importers_2002 <- Chicken_trade_year[["2002"]][["Reporter Country Code"]]
Exporters_2002 <- Chicken_trade_year[["2002"]][["Partner Country Code"]]
EdgeList_2002 <- matrix(data = c(Importers_2002, Exporters_2002), ncol = 2)
Adjacency_2002 <- get.adjacency(graph.edgelist(EdgeList_2002, directed = T))

EdgeList_2003 <- matrix(data = 0, nrow = 2, ncol = 2)
Importers_2003 <- Chicken_trade_year[["2003"]][["Reporter Country Code"]]
Exporters_2003 <- Chicken_trade_year[["2003"]][["Partner Country Code"]]
EdgeList_2003 <- matrix(data = c(Importers_2003, Exporters_2003), ncol = 2)
Adjacency_2003 <- get.adjacency(graph.edgelist(EdgeList_2003, directed = T))

EdgeList_2004 <- matrix(data = 0, nrow = 2, ncol = 2)
Importers_2004 <- Chicken_trade_year[["2004"]][["Reporter Country Code"]]
Exporters_2004 <- Chicken_trade_year[["2004"]][["Partner Country Code"]]
EdgeList_2004 <- matrix(data = c(Importers_2004, Exporters_2004), ncol = 2)
Adjacency_2004 <- get.adjacency(graph.edgelist(EdgeList_2004, directed = T))

EdgeList_2005 <- matrix(data = 0, nrow = 2, ncol = 2)
Importers_2005 <- Chicken_trade_year[["2005"]][["Reporter Country Code"]]
Exporters_2005 <- Chicken_trade_year[["2005"]][["Partner Country Code"]]
EdgeList_2005 <- matrix(data = c(Importers_2005, Exporters_2005), ncol = 2)
Adjacency_2005 <- get.adjacency(graph.edgelist(EdgeList_2005, directed = T))

EdgeList_2006 <- matrix(data = 0, nrow = 2, ncol = 2)
Importers_2006 <- Chicken_trade_year[["2006"]][["Reporter Country Code"]]
Exporters_2006 <- Chicken_trade_year[["2006"]][["Partner Country Code"]]
EdgeList_2006 <- matrix(data = c(Importers_2006, Exporters_2006), ncol = 2)
Adjacency_2006 <- get.adjacency(graph.edgelist(EdgeList_2006, directed = T))

EdgeList_2007 <- matrix(data = 0, nrow = 2, ncol = 2)
Importers_2007 <- Chicken_trade_year[["2007"]][["Reporter Country Code"]]
Exporters_2007 <- Chicken_trade_year[["2007"]][["Partner Country Code"]]
EdgeList_2007 <- matrix(data = c(Importers_2007, Exporters_2007), ncol = 2)
Adjacency_2007 <- get.adjacency(graph.edgelist(EdgeList_2007, directed = T))

EdgeList_2008 <- matrix(data = 0, nrow = 2, ncol = 2)
Importers_2008 <- Chicken_trade_year[["2008"]][["Reporter Country Code"]]
Exporters_2008 <- Chicken_trade_year[["2008"]][["Partner Country Code"]]
EdgeList_2008 <- matrix(data = c(Importers_2008, Exporters_2008), ncol = 2)
Adjacency_2008 <- get.adjacency(graph.edgelist(EdgeList_2008, directed = T))

EdgeList_2009 <- matrix(data = 0, nrow = 2, ncol = 2)
Importers_2009 <- Chicken_trade_year[["2009"]][["Reporter Country Code"]]
Exporters_2009 <- Chicken_trade_year[["2009"]][["Partner Country Code"]]
EdgeList_2009 <- matrix(data = c(Importers_2009, Exporters_2009), ncol = 2)
Adjacency_2009 <- get.adjacency(graph.edgelist(EdgeList_2009, directed = T))

EdgeList_2010 <- matrix(data = 0, nrow = 2, ncol = 2)
Importers_2010 <- Chicken_trade_year[["2010"]][["Reporter Country Code"]]
Exporters_2010 <- Chicken_trade_year[["2010"]][["Partner Country Code"]]
EdgeList_2010 <- matrix(data = c(Importers_2010, Exporters_2010), ncol = 2)
Adjacency_2010 <- get.adjacency(graph.edgelist(EdgeList_2010, directed = T))

EdgeList_2011 <- matrix(data = 0, nrow = 2, ncol = 2)
Importers_2011 <- Chicken_trade_year[["2011"]][["Reporter Country Code"]]
Exporters_2011 <- Chicken_trade_year[["2011"]][["Partner Country Code"]]
EdgeList_2011 <- matrix(data = c(Importers_2011, Exporters_2011), ncol = 2)
Adjacency_2011 <- get.adjacency(graph.edgelist(EdgeList_2011, directed = T))

EdgeList_2012 <- matrix(data = 0, nrow = 2, ncol = 2)
Importers_2012 <- Chicken_trade_year[["2012"]][["Reporter Country Code"]]
Exporters_2012 <- Chicken_trade_year[["2012"]][["Partner Country Code"]]
EdgeList_2012 <- matrix(data = c(Importers_2012, Exporters_2012), ncol = 2)
Adjacency_2012 <- get.adjacency(graph.edgelist(EdgeList_2012, directed = T))

EdgeList_2013 <- matrix(data = 0, nrow = 2, ncol = 2)
Importers_2013 <- Chicken_trade_year[["2013"]][["Reporter Country Code"]]
Exporters_2013 <- Chicken_trade_year[["2013"]][["Partner Country Code"]]
EdgeList_2013 <- matrix(data = c(Importers_2013, Exporters_2013), ncol = 2)
Adjacency_2013 <- get.adjacency(graph.edgelist(EdgeList_2013, directed = T))

EdgeList_2014 <- matrix(data = 0, nrow = 2, ncol = 2)
Importers_2014 <- Chicken_trade_year[["2014"]][["Reporter Country Code"]]
Exporters_2014 <- Chicken_trade_year[["2014"]][["Partner Country Code"]]
EdgeList_2014 <- matrix(data = c(Importers_2014, Exporters_2014), ncol = 2)
Adjacency_2014 <- get.adjacency(graph.edgelist(EdgeList_2014, directed = T))

EdgeList_2015 <- matrix(data = 0, nrow = 2, ncol = 2)
Importers_2015 <- Chicken_trade_year[["2015"]][["Reporter Country Code"]]
Exporters_2015 <- Chicken_trade_year[["2015"]][["Partner Country Code"]]
EdgeList_2015 <- matrix(data = c(Importers_2015, Exporters_2015), ncol = 2)
Adjacency_2015 <- get.adjacency(graph.edgelist(EdgeList_2015, directed = T))

EdgeList_2016 <- matrix(data = 0, nrow = 2, ncol = 2)
Importers_2016 <- Chicken_trade_year[["2016"]][["Reporter Country Code"]]
Exporters_2016 <- Chicken_trade_year[["2016"]][["Partner Country Code"]]
EdgeList_2016 <- matrix(data = c(Importers_2016, Exporters_2016), ncol = 2)
Adjacency_2016 <- get.adjacency(graph.edgelist(EdgeList_2016, directed = T))

EdgeList_2017 <- matrix(data = 0, nrow = 2, ncol = 2)
Importers_2017 <- Chicken_trade_year[["2017"]][["Reporter Country Code"]]
Exporters_2017 <- Chicken_trade_year[["2017"]][["Partner Country Code"]]
EdgeList_2017 <- matrix(data = c(Importers_2017, Exporters_2017), ncol = 2)
Adjacency_2017 <- get.adjacency(graph.edgelist(EdgeList_2017, directed = T))

Adjacency_List <- list(Adjacency_1986, Adjacency_1987, Adjacency_1988, Adjacency_1989, Adjacency_1990, Adjacency_1991, Adjacency_1992,
                       Adjacency_1993, Adjacency_1994, Adjacency_1995, Adjacency_1996, Adjacency_1997, Adjacency_1998, Adjacency_1999,
                       Adjacency_2000, Adjacency_2001, Adjacency_2002, Adjacency_2003, Adjacency_2004, Adjacency_2005, Adjacency_2006,
                       Adjacency_2007, Adjacency_2008, Adjacency_2009, Adjacency_2010, Adjacency_2011, Adjacency_2012, Adjacency_2013,
                       Adjacency_2014, Adjacency_2015, Adjacency_2016, Adjacency_2017)

# Adjacencies done --------------------------------------------------------
#POC with 1986 data
Adjacency_matrix_1986 <- as_adjacency_matrix(Adjacency_graph_1986)
Adjacency_graph_1986 <- graph.adjacency(Adjacency_List[[1]], mode = 'directed')
Graph_1986 <- graph_from_adjacency_matrix(Adjacency_matrix_1986)
degree_1986 <- degree(Adjacency_graph_1986)
#No difference between degree or strength as currently unweighted
strength_1986 <- graph.strength(Adjacency_graph_1986)

hist(degree_1986, xlab = 'Degree', ylab = 'Frequency', main = 'Trade Network')
CCDplot(x = degree_1986, Variable = 'Degree')
#The network is heavily skewed to small degree with some, few, highly connected sites

#Now to calculate the betweenness. The extent to which a given node lies on a path between two other
betweenness_1986 <- betweenness(Adjacency_graph_1986, directed = T, weight = NA)
plot(betweenness_1986, xlab = 'Binary', ylab = 'Weighted')

#The closeness assesses how close a node is to all other nodes. 
closeness_1986 <- igraph::closeness(Adjacency_graph_1986, mode = 'all', weights = NA, normalized = T)

#Looking across degree, betweenness
pairs(cbind(degree_1986, betweeneness_1986, closeness_1986))

vColour <- rep('blue', nrow(Adjacency_List[[1]]))

#The five highest weighted degree nodes are coloured from black to yellow, others remain blue
vColour[rev(order(degree_1986))][1] <- 'black'
vColour[rev(order(degree_1986))][2] <- 'darkred'
vColour[rev(order(degree_1986))][3] <- 'red'
vColour[rev(order(degree_1986))][4] <- 'orange'
vColour[rev(order(degree_1986))][5] <- 'yellow'

par(mfrow = c(2,2), mar = c(0,0,1,0))
#gplot.target(dat = Adjacency_matrix_1986, degree_1986, gmode = 'graph', vertex.cex = 1, vertex.side = 100, vertex.border = vColour,
#             vertex.col = vColour, edge.col = 'lightgray', main = 'Strength')
#gplot.target(Adjacency_graph_1986, betweeneness_1986, gmode = 'graph', vertex.cex = 1, vertex.side = 100, vertex.border = vColour,
#             vertex.col = vColour, edge.col = 'lightgray', main = 'Betweenness')
#gplot.target(Adjacency_graph_1986, closeness_1986, gmode = 'graph', vertex.cex = 1, vertex.side = 100, vertex.border = vColour,
#             vertex.col = vColour, edge.col = 'lightgray', main = 'Closeness')
#Some difficulty here as there is an error in the code some difficulty with edgelist

tkplot(Graph_1986)
plot.igraph(Graph_1986)





