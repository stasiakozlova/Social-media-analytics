library(vkR)
library(igraph)
library(tkrplot)
vkOAuth(7643106)
setAccessToken("a29a977849e2955112e74473e4eaa62a8542f42dba369ff0baa0045ad5eb2572d56f4194c681b89703e97")

members <- getGroupsMembersExecute('supcolorc')
members_network <- getArbitraryNetwork(members)

g <- graph_from_edgelist(as.matrix(members_network), directed = FALSE)
tkplot(g)

edges <- nrow(members_network)
edges

connected_components <- components(g)$no
connected_components

vertex_giant_component <- max(components(g)$csize) / sum(components(g)$csize) 
vertex_giant_component

density_without_isolates <- 2 * edges / (vcount(g) * (vcount(g) - 1))
density_without_isolates

modularity <- modularity(g, components(g)$membership)
modularity

clusters <- cluster_louvain(g)
length(clusters)

mean_geodesics <- mean_distance(g) 
mean_geodesics

diameter <- diameter(g)
diameter

mean_degree <- mean(degree(g))
mean_degree

write.csv()

write.csv(members_network, "members_network.csv", sep = "/t")

x <- rnorm(members_network)
ks.test(x, "pnorm")

centr_degree(g)
closeness(g)
betweenness(g)