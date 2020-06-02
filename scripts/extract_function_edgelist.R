## To extract a particular ITEM
## "1000Head", "Tonnes", "1000USD"
Extract_EdgeList <- function(AllEdgeLists,Item){
  if(Item=="1000Head"){ EdgeList <- AllEdgeLists[,c("Sender","Receiver","Year","1000Head_IMP","1000Head_EXP")] }
  if(Item=="1000USD" ){ EdgeList <- AllEdgeLists[,c("Sender","Receiver","Year","1000USD_IMP" ,"1000USD_EXP" )] }
  if(Item=="Tonnes"  ){ EdgeList <- AllEdgeLists[,c("Sender","Receiver","Year","tonnes_IMP"  ,"tonnes_EXP"  )] }
  names(EdgeList)[4:5] <- c("Import","Export")
  EdgeList <- EdgeList[EdgeList$"Import">0 | EdgeList$"Export">0 , ] 
  EdgeList
}
