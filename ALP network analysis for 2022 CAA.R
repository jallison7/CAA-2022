

setwd("C:/Users/jra64/Documents/GitHub/ALP-Network-analysis")

library(magrittr)
library(igraph)
library(ggnetwork)
library(dplyr)

#establishes a cutoff value for binarizing the networks; similarities less than the cutoff are replaced with 0; 
#cutoff = 0 does nothing, 
#cutoff <- .5


# network based on gray ware refired colors
gray.ware.original <-  read.csv(file = "Gray ware refired colors.csv")
gray.ware.refired.colors <- read.csv(file = "Gray ware refired colors.csv", row.names = 1)


gray.ware.refired.colors <- gray.ware.refired.colors[1:25,2:10]
m <- prop.table(as.matrix(gray.ware.refired.colors),1)

d <- dist(m, method = "manhattan") %>% as.matrix()

# convert to 1 as most similar
d <- 1 - (d / 2)
# round
d <- round(d,2)
cutoff <- quantile(d,.8)
dCut <- d
dCut[dCut <cutoff] <- 0
g <- graph_from_adjacency_matrix(adjmatrix = dCut, mode = "undirected",
                                weighted = T, diag = F)

g

# E(g)$weight
plot(g)
gray_ware_refired_betweenness <- as.data.frame(betweenness(g))
gray_ware_refired_betweenness_normalized <- as.data.frame(betweenness(g, normalized = TRUE))
gray_ware_refired_eigen <- as.data.frame(eigen_centrality(g, weights = E(g)$weight)$vector)
gray_ware_refired_degree <- as.data.frame(degree(g))

plotdf = ggnetwork(g) %>%
  left_join(gray.ware.original %>% rename(name = Site_and_Locus)) %>%
  select(x,y,xend,yend,name,Settlement_Cluster, weight) %>%
  distinct_all()

ggplot(plotdf, aes(x = x, y = y, label = name)) +
  geom_edges(aes(xend = xend, yend = yend), color = "darkgray") +
  geom_nodes(aes(fill = Settlement_Cluster), size = 8,  shape = 21) +
  scale_fill_manual("Settlement Cluster", values = c("Blue Mesa" = "cornflowerblue", "Sacred Ridge" = "black", "Eastern" = "cyan4", "North-Central" = "firebrick3", "Western" = "darkorchid1")) +
  theme_blank()


# if you want to look at the plot interactively
library(plotly)
plotly::ggplotly()

# network based on white ware design elements

design.elements.original <-  read.csv(file = "design elements.csv")
design.elements <- read.csv(file = "design elements.csv", row.names = 1)


design.elements <- design.elements[1:22,2:10]
design_m <- prop.table(as.matrix(design.elements),1)

design_d <- dist(design_m, method = "manhattan") %>% as.matrix()

# convert to 1 as most similar
design_d <- 1 - (design_d / 2)
# round
design_d <- round(design_d,2)
cutoff <- quantile(design_d,.8)
design_dCut <- design_d
design_dCut[design_dCut <cutoff] <- 0
design_g <- graph_from_adjacency_matrix(adjmatrix = design_dCut, mode = "undirected",
                                 weighted = T, diag = F)



#basic plot of the network
plot(design_g)

#calculate centrality measures
design_element_betweenness <- as.data.frame(betweenness(design_g))
design_element_betweenness_normalized <- as.data.frame(betweenness(design_g, normalized = TRUE))
design_element_eigen <- as.data.frame(eigen_centrality(design_g, weights = E(design_g)$weight)$vector)
design_element_degree <- as.data.frame(degree(design_g))

#plot using ggplot
plotdf = ggnetwork(design_g) %>%
  left_join(design.elements.original %>% rename(name = Site_and_Locus)) %>%
  select(x,y,xend,yend,name,Settlement_Cluster, weight) %>%
  distinct_all()

ggplot(plotdf, aes(x = x, y = y, label = name)) +
  geom_edges(aes(xend = xend, yend = yend), color = "darkgray") +
  geom_nodes(aes(fill = Settlement_Cluster), size = 8,  shape = 21) +
  scale_fill_manual("Settlement Cluster", values = c("Blue Mesa" = "cornflowerblue", "Sacred Ridge" = "black", "Eastern" = "cyan4", "North-Central" = "firebrick3", "Western" = "darkorchid1")) +
  theme_blank()


# to look at the plot interactively

plotly::ggplotly()

#network based on simlarity in faunal assemblages

faunal.original <-  read.csv(file = "faunal types.csv")
faunal <- read.csv(file = "faunal types.csv", row.names = 1)


faunal <- faunal[1:20,2:9]
faunal_m <- prop.table(as.matrix(faunal),1)

faunal_d <- dist(faunal_m, method = "manhattan") %>% as.matrix()

# convert to 1 as most similar
faunal_d <- 1 - (faunal_d / 2)
# round
faunal_d <- round(faunal_d,2)
cutoff <- quantile(faunal_d,.8)
faunal_dCut <- faunal_d
faunal_dCut[faunal_dCut <cutoff] <- 0
faunal_g <- graph_from_adjacency_matrix(adjmatrix = faunal_dCut, mode = "undirected",
                                        weighted = T, diag = F)




plot(faunal_g)

#calculate centrality measures
faunal_betweenness <- as.data.frame(betweenness(faunal_g))
faunal_betweenness_normalized <- as.data.frame(betweenness(faunal_g, normalized = TRUE))
faunal_eigen <- as.data.frame(eigen_centrality(faunal_g, weights = E(faunal_g)$weight)$vector)
faunal_degree <- as.data.frame(degree(faunal_g))

plotdf = ggnetwork(faunal_g) %>%
  left_join(faunal.original %>% rename(name = Site_and_Locus)) %>%
  select(x,y,xend,yend,name,Settlement_Cluster, weight) %>%
  distinct_all()

ggplot(plotdf, aes(x = x, y = y, label = name)) +
  geom_edges(aes(xend = xend, yend = yend), color = "darkgray") +
  geom_nodes(aes(fill = Settlement_Cluster), size = 8,  shape = 21) +
  scale_fill_manual("Settlement Cluster", values = c("Blue Mesa" = "cornflowerblue", "Sacred Ridge" = "black", "Eastern" = "cyan4", "North-Central" = "firebrick3", "Western" = "darkorchid1")) +
  theme_blank()


#  to look at the plot interactively

plotly::ggplotly()

#network based on simlarity in overall artifact assemblages

artifact.original <-  read.csv(file = "artifact types.csv")
artifact <- read.csv(file = "artifact types.csv", row.names = 1)


artifact <- artifact[1:36,2:16]
artifact_m <- prop.table(as.matrix(artifact),1)

artifact_d <- dist(artifact_m, method = "manhattan") %>% as.matrix()

# convert to 1 as most similar
artifact_d <- 1 - (artifact_d / 2)
# round
artifact_d <- round(artifact_d,2)
cutoff <- quantile(artifact_d,.8)
artifact_dCut <- artifact_d
artifact_dCut[artifact_dCut <cutoff] <- 0
artifact_g <- graph_from_adjacency_matrix(adjmatrix = artifact_dCut, mode = "undirected",
                                        weighted = T, diag = F)




plot(artifact_g)

#calculate centrality measures
artifact_betweenness <- as.data.frame(betweenness(artifact_g))
artifact_betweenness_normalized <- as.data.frame(betweenness(artifact_g, normalized = TRUE))
artifact_eigen <- as.data.frame(eigen_centrality(artifact_g, weights = E(artifact_g)$weight)$vector)
artifact_degree <- as.data.frame(degree(artifact_g))

plotdf = ggnetwork(artifact_g) %>%
  left_join(artifact.original %>% rename(name = Site_and_Locus)) %>%
  select(x,y,xend,yend,name,Settlement_Cluster, weight) %>%
  distinct_all()

ggplot(plotdf, aes(x = x, y = y, label = name)) +
  geom_edges(aes(xend = xend, yend = yend), color = "darkgray") +
  geom_nodes(aes(fill = Settlement_Cluster), size = 8,  shape = 21) +
  scale_fill_manual("Settlement Cluster", values = c("Blue Mesa" = "cornflowerblue", "Sacred Ridge" = "black", "Eastern" = "cyan4", "North-Central" = "firebrick3", "Western" = "darkorchid1")) +
  theme_blank()


#  to look at the plot interactively

plotly::ggplotly()

#combine the centrality tables from each data type

gray_ware_refired_centrality <- cbind(gray_ware_refired_betweenness_normalized, gray_ware_refired_degree, gray_ware_refired_eigen)
design_element_centrality <- cbind(design_element_betweenness_normalized, design_element_degree, design_element_eigen)
faunal_centrality <- cbind(faunal_betweenness_normalized, faunal_degree, faunal_eigen)
artifact_centrality <- cbind(artifact_betweenness_normalized, artifact_degree, artifact_eigen)

#save the centrality tables as .csv files

write.csv(gray_ware_refired_centrality, "gray ware refired centrality.csv")
write.csv(design_element_centrality, "design element centrality.csv")
write.csv(faunal_centrality, "faunal centrality.csv")
write.csv(artifact_centrality, "artifact centrality.csv")

