EdgeList_to_AdjMat = function(EdgeList, Directed = F, Weighted = F){
  NodeList <- sort(unique(c(EdgeList[,1], EdgeList[,2])))
  AdjMat <- matrix(nrow = length(NodeList), ncol = length(NodeList))
  AdjMat[] <- 0
  for (i in 1:nrow(EdgeList)){
    Endpoint1 <- EdgeList[i,1]
    Endpoint2 <- EdgeList[i,2]
    if (Weighted == F | ncol(EdgeList) == 2){
      W <- 1
    } else {
      W <- EdgeList[i,3]
    }
    AdjMat[Endpoint1, Endpoint2] <- W
    if (Directed == F){
      AdjMat[Endpoint2, Endpoint1] <- W
    }
  }
  return(AdjMat)
}
