library(dils)
# setwd("C:/github/network-analytics/ChineseLeaders")

###################
##  Import data  ##
###################
chinese.edgelist <- readRDS("networks/chinese_all_networks_edgelist.rds")
chinese.edgelist <- chinese.edgelist[order(chinese.edgelist[,1], chinese.edgelist[,2]),]
n <- sqrt(nrow(chinese.edgelist)); if(n %% 1 != 0) warning("Incorrect number of rows")
constituent.names <- names(chinese.edgelist)[3:ncol(chinese.edgelist)]

chinese.dils <- GenerateDilsNetwork(chinese.edgelist,
                                    subsample=1e5,
                                    n.subsamples=1e3,
                                    ignore.cols=1:2, 
                                    progress.bar=TRUE)
full.edgelist <- cbind(chinese.edgelist, dils=chinese.dils$dils)

#save(n, constituent.names, full.edgelist, file="analysis with dils/0_waypoint_1.RData")
##  Restart R and continue from here  ##
library(dils)
load("analysis with dils/0_waypoint_1.RData")

########################
##  Generate igraphs  ##
########################
adj.dils <- AdjacencyFromEdgelist(full.edgelist[,c("from", "to", "dils")], check.full=FALSE)
g.dils <- graph.adjacency(adj.dils$adjacency, mode="undirected", weighted=TRUE)
constituent.graphs <- lapply(constituent.names, function(v) {
  cat("Generating network with", v, "\n")
  return( graph.adjacency(AdjacencyFromEdgelist(full.edgelist[,c("from", "to", v)], FALSE)$adjacency,
                          mode="undirected", weighted=TRUE) )
})
names(constituent.graphs) <- constituent.names
# save(g.dils, constituent.graphs, file="analysis with dils/0_waypoint_graphs.RData")
load("analysis with dils/0_waypoint_graphs.RData")

evcent.dils <- evcent(g.dils)
hist(evcent.dils$vector, 2000, ylim=c(0,200), main="DILS Eigenvector Centrality")

########################################
##  Measure relative informativeness  ##
########################################
rel.info.dils <-   MeasureNetworkInformation(g.dils)

rel.info.constituent <- sapply(constituent.graphs, MeasureNetworkInformation)
# save(rel.info.dils, rel.info.constituent, file="analysis with dils/0_waypoint_rel_info.RData")

rel.info.ratios <- rel.info.dils / rel.info.constituent
all(rel.info.ratios > .98)
median(rel.info.ratios)

###########################
###  Who is important?  ###
###########################
uuids <- read.table("networks/chinese_age_network_names.txt", stringsAsFactors=FALSE)[,1]

all.officials <- readRDS("1_output_raw_china.rds")$officials

node.info <- data.frame(uuid=uuids, 
                        node.id=1:length(uuids),
                        name=sapply(uuids, function(u) all.officials$bio_name[u==all.officials$id]),
                        ev.dils=evcent.dils$vector,
                        stringsAsFactors=FALSE)
row.names(node.info) <- NULL
head(node.info)

# Calculate evcent for each constituent network
for(i in 1:length(constituent.graphs)) {
  cat("Generating network with", constituent.names[i], "\n")
  ev <- evcent(constituent.graphs[[i]])$vector
  node.info <- cbind(node.info, ev)
  names(node.info)[ncol(node.info)] <- paste("ev.", constituent.names[i], sep="")
}
head(node.info)

# Rank-order correlation
rank.cor <- sapply(constituent.names, function(name) {
  scores <- node.info[,paste("ev.", name, sep="")]
  if(sd(scores)==0) return(0)
  else return( cor(node.info$ev.dils, scores, method="spearman") )
})
sort(rank.cor, decreasing=TRUE)

top.20.dils <- node.info$name[sort(node.info$ev.dils, index.return=TRUE, decreasing=TRUE)$ix[1:20]]
top.20.age <- node.info$name[sort(node.info$ev.age, index.return=TRUE, decreasing=TRUE)$ix[1:20]]
top.20.executive <- node.info$name[sort(node.info$ev.executive, index.return=TRUE, decreasing=TRUE)$ix[1:20]]

top.20 <- data.frame(dils=top.20.dils,
                     age=top.20.age,
                     executive=top.20.executive,
                     stringsAsFactors=FALSE)
write.table(top.20, file="analysis with dils/top20.csv",
            sep=",",
            row.names=FALSE)
