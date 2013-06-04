#' Test data set with 50 nodes, through imputation

source("http://www.haptonstahl.org/R/Decruft/Decruft.R")

source("rough/functions.R")

nets <- readRDS("rough/test50_data_artificial.rds")
n <- vcount(nets[[1]])
k <- length(nets)
ids <- sort(V(nets[[1]])$name)

x.list <- lapply(nets, IgraphToDyadTable)
dm.table <- DyadMultiTable(x.list, zero.to.na=FALSE)
#' ImputeDyad(v1=1, v2=6, dmt=dm.table, g.list=nets, n=200)
#' system.time(one.imputed.dyad <- ImputeDyad(v1=1, v2=6, dmt=dm.table, g.list=nets, n=200))

#' #####  Inpute entire DyadMultiTable  #####
dm.table.imputed <- dm.table
# dm.table.imputed <- readRDS("rough/dm_table_imputed.rds")

#' #####  May need to do this in batches, saving as you go  #####
range.to.do <- 1:10

pb <- txtProgressBar()
for(i in range.to.do) {
  dm.table.imputed[i, -c(1:2)] <- ImputeDyad(v1=dm.table$v1[i],
                                             v2=dm.table$v2[i],
                                             dmt=dm.table,
                                             g.list=nets,
                                             n=500)
  setTxtProgressBar(pb, value=(i-min(range.to.do)+1)/length(range.to.do))
}
close(pb)
saveRDS(dm.table.imputed, "rough/test50_dm_table_imputed.rds")
#' head(dm.table.imputed)
#' tail(dm.table.imputed)
