rm(list=ls())

library(dplyr)
library(tidyselect)
library("janitor")
library(stringr)
library('tidyverse')

source(here('scripts', 'data_cleaning_functions.R'))


detailed_trading <- read_csv('cleaned_data/detailed_trading.csv')

Db <- as.data.frame(detailed_trading)

table(Db$element)
table(Db$unit)

Db$year <- as.numeric(Db$year)
Db$value <- as.numeric(Db$value)

Db$Trans <- rep(0,nrow(Db))
Db$Trans[Db$element %in% c("import_quantity","import_value")] <- "Import"
Db$Trans[Db$element %in% c("export_quantity","export_value")] <- "Export"

vImp <- Db$Trans=="Import"
vExp <- Db$Trans=="Export"
Edges <- rbind(
  Db[vImp,c("partner_countries","reporter_countries","year")] , 
  Db[vExp,c("reporter_countries","partner_countries","year")]
)

pEd <- paste(Edges[,1],Edges[,2],Edges[,3])
pEd <- unique(pEd)

AllEdgeLists <- as.data.frame( str_split_fixed(pEd," ",3) )
names(AllEdgeLists) <- c("Sender","Receiver","Year")
AllEdgeLists$Year <- as.numeric(as.character(AllEdgeLists$Year))
AllEdgeLists$"1000Head_IMP" <- rep(0,nrow(AllEdgeLists))
AllEdgeLists$"1000Head_EXP" <- rep(0,nrow(AllEdgeLists))
AllEdgeLists$"1000USD_IMP"  <- rep(0,nrow(AllEdgeLists))
AllEdgeLists$"1000USD_EXP"  <- rep(0,nrow(AllEdgeLists))
AllEdgeLists$"tonnes_IMP"   <- rep(0,nrow(AllEdgeLists))
AllEdgeLists$"tonnes_EXP"   <- rep(0,nrow(AllEdgeLists))

for(i in 1:nrow(AllEdgeLists)){
  
  Send <- AllEdgeLists$Sender[i]
  Rec  <- AllEdgeLists$Receiver[i]
  Year <- AllEdgeLists$Year[i]
  
  iExp <- which( Db$reporter_countries==Send & Db$partner_countries==Rec  & Db$Trans=="Export")
  iImp <- which( Db$reporter_countries==Rec  & Db$partner_countries==Send & Db$Trans=="Import")
  
  ExpHead <- iExp[ Db[iExp,"year"]==Year & Db[iExp,"unit"]=="1000_head" ]
  ExpUSD  <- iExp[ Db[iExp,"year"]==Year & Db[iExp,"unit"]=="1000_us$" ]
  ExpTon  <- iExp[ Db[iExp,"year"]==Year & Db[iExp,"unit"]=="tonnes" ]
  
  ImpHead <- iImp[ Db[iImp,"year"]==Year & Db[iImp,"unit"]=="1000_head" ]
  ImpUSD  <- iImp[ Db[iImp,"year"]==Year & Db[iImp,"unit"]=="1000_us$" ]
  ImpTon  <- iImp[ Db[iImp,"year"]==Year & Db[iImp,"unit"]=="tonnes" ]
  
  if( length(ImpHead)>0 ){ AllEdgeLists$"1000Head_IMP"[i] <- sum(Db$value[ImpHead]) }
  if( length(ExpHead)>0 ){ AllEdgeLists$"1000Head_EXP"[i] <- sum(Db$value[ExpHead]) }
  
  if( length(ImpUSD)>0  ){ AllEdgeLists$"1000USD_IMP"[i]  <- sum(Db$value[ImpUSD])  }
  if( length(ExpUSD)>0  ){ AllEdgeLists$"1000USD_EXP"[i]  <- sum(Db$value[ExpUSD])  }
  
  if( length(ImpTon)>0  ){ AllEdgeLists$"tonnes_IMP"[i]   <- sum(Db$value[ImpTon])  }
  if( length(ExpTon)>0  ){ AllEdgeLists$"tonnes_EXP"[i]   <- sum(Db$value[ExpTon])  }
  
}

write_csv(AllEdgeLists, 'cleaned_data/all_edge_lists.csv')
write_rds(AllEdgeLists, 'cleaned_data/all_edge_lists.rds')
