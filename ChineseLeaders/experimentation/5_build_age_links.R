#####  Build links for officials within 5 years in age  #####
# Uses output from MapReduce job performed on SRH laptop Ubuntu VM

library(igraph)

# Read cleaned China data scraped from http://www.chinavitae.com
china <- readRDS("china_data_cleaned.rds")
n <- nrow(china$officials)
max.workunit <- n * n

# Read output from MapReduce job
age.links <- data.frame(workunit=as.integer(read.delim("age_links_output_only_1s.kv")[,1]))
head(age.links)

# check validity of workunit numbers
bad.workunits <- age.links$workunit[which(age.links$workunit < 1 || age.links$workunit > max.workunit)]
if( length(bad.workunits) > 0 ) {
  warning("There are ", length(bad.workunits), " bad workunits.")
} else {
  cat("There are no bad workunits.\n")
}

# recover from and to node id
age.links$from <- ifelse(age.links$workunit %% n == 0, age.links$workunit %/% n - 1L, age.links$workunit %/% n)
age.links$to <- ifelse(age.links$workunit %% n == 0, n, age.links$workunit %% n)
# check for links to self
if( any(age.links$from == age.links$to) ) {
  cat("There are", sum(age.links$from == age.links$to), "links to self.\n")
  age.links <- age.links[-which(age.links$from == age.links$to),]
  if( !any(age.links$from == age.links$to) ) cat("These links have been removed.\n")
}
# remove links duplicated by switching direction (this is a nondirected network)
duplicate.links <- numeric(0)
for( i in 1:(nrow(age.links)-1) ) {
  duplicate.links <- c(duplicate.links, 
                       which(age.links$from[(i+1):nrow(age.links)] == age.links$to[i] & age.links$to[(i+1):nrow(age.links)] == age.links$from[i]) + i)
  if( i %% 10 == 0 ) cat("Row", i, "checked.\n")
}
cat("Identified", length(duplicate.links), "duplicate links.\n")
age.links <- age.links[-duplicate.links,c("from", "to")]
cat("Duplicate links removed.\n")

cat("A total of", nrow(age.links), "links have been identified.\n")
head(age.links)

age.graph.names <- china$officials[,c("bio_name")]

# Useful subgraph
subgraph.size <- 25
subgraph.indices <- head(which(age.graph.names != ""), subgraph.size)

#####  Using igraph  #####
age.graph <- graph.empty(directed=FALSE) + vertices(sort(unique(c(age.links$from, age.links$to)))-1)
## From here on, nodes are indexed {0, 1, 2,..., n-1}
for(i in 1:nrow(age.links)) {
  age.graph[age.links$from[i], age.links$to[i]] <- 1
}

# For details see 
#   ?plot.igraph
#   ?igraph.plotting
plot(age.graph, 
     layout=layout.lgl(age.graph), 
     vertex.size=1, 
     vertex.label=NA)
plot(age.graph, 
     layout=layout.drl(age.graph), 
     vertex.size=1, 
     vertex.label=NA)

# subgraph
age.sub.graph <- induced.subgraph(age.graph, vids=subgraph.indices-1)
age.sub.graph.names <- age.graph.names[subgraph.indices]

par(mai=c(0,0,0,0),
    omi=c(.25,.25,.25,.25))

layout.auto(age.sub.graph)
layout.random(age.sub.graph)
layout.circle(age.sub.graph)
layout.sphere(age.sub.graph)
layout.fruchterman.reingold(age.sub.graph)
layout.kamada.kawai(age.sub.graph)
layout.spring(age.sub.graph)
layout.reingold.tilford(age.sub.graph)
layout.fruchterman.reingold.grid(age.sub.graph)
layout.lgl(age.sub.graph)
layout.graphopt(age.sub.graph)
layout.mds(age.sub.graph)
layout.svd(age.sub.graph)

plot(age.sub.graph, 
     vertex.label=age.sub.graph.names,
     vertex.label.cex=.5,
     frame=TRUE,
     layout=layout.circle(age.sub.graph),
     mark.groups=sapply(1:5, function(g) which(g == as.numeric(cut(china$officials$birthyear[subgraph.indices], breaks=5)))))

#####  Using network  #####
library(network)
age.network <- network(x=age.links,
                       directed=FALSE)
network.vertex.names(age.network) <- age.graph.names

plot(age.network)

age.sub.network <- get.inducedSubgraph(age.network,
                                       v=subgraph.indices)
network.vertex.names(age.sub.network)
plot(age.sub.network, label=network.vertex.names(age.sub.network))

# export age network

# adjacency: each row and each column is a node; 1 means edge exists between row and column nodes, 0 OTW
write.table(as.matrix.network(age.network, matrix.type="adjacency"), 
            file="chinese_age_network_adjacency.tsv", 
            sep="\t",
            quote=FALSE,
            col.names=FALSE,
            row.names=FALSE)
# incidence: each row is a node, each column is an edge; 1 means node is park of edge, 0 OTW
# write.table(as.matrix.network(age.network, matrix.type="incidence"), 
#             file="chinese_age_network_incidence.tsv", 
#             sep="\t",
#             quote=FALSE,
#             col.names=FALSE,
#             row.names=FALSE)
# edgelist: each row is an edge, two columns list ids of nodes in the edge
write.table(as.matrix.network(age.network, matrix.type="edgelist"), 
            file="chinese_age_network_edgelist.tsv", 
            sep="\t",
            quote=FALSE,
            col.names=FALSE,
            row.names=FALSE)
writeLines(text=network.vertex.names(age.network),
           con=file("chinese_age_network_names.txt"))

# adjacency: each row and each column is a node; 1 means edge exists between row and column nodes, 0 OTW
write.table(as.matrix.network(age.sub.network, matrix.type="adjacency"), 
            file="chinese_age_subnetwork_adjacency.tsv", 
            sep="\t",
            quote=FALSE,
            col.names=FALSE,
            row.names=FALSE)
# incidence: each row is a node, each column is an edge; 1 means node is park of edge, 0 OTW
# write.table(as.matrix.network(age.sub.network, matrix.type="incidence"), 
#             file="chinese_age_network_incidence.tsv", 
#             sep="\t",
#             quote=FALSE,
#             col.names=FALSE,
#             row.names=FALSE)
# edgelist: each row is an edge, two columns list ids of nodes in the edge
write.table(as.matrix.network(age.sub.network, matrix.type="edgelist"), 
            file="chinese_age_subnetwork_edgelist.tsv", 
            sep="\t",
            quote=FALSE,
            col.names=FALSE,
            row.names=FALSE)
writeLines(text=network.vertex.names(age.sub.network),
           con=file("chinese_age_subnetwork_names.txt"))
