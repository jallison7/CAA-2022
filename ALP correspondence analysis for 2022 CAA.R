
packages_used <- c("ca","tidyverse", "ggrepel", "plotly")
load_packages <- function(pack) {
  if (!is.element(pack, installed.packages()[, 1]))
    install.packages(pack, dep = TRUE)
  require(pack, character.only = TRUE)
}
lapply(packages_used, load_packages)

setwd("C:/Users/jra64/Documents/GitHub/ALP-Network-analysis")

gray.ware.refired.colors <- read.csv(file = "Gray ware refired colors.csv", row.names = 1)


# saves column with cluster membership separately, the drops it as well as the total row and column from the data file
settlement_cluster <- gray.ware.refired.colors$Settlement_Cluster[1:25]
gray.ware.refired.colors <- gray.ware.refired.colors[1:25, 2:10]



options(digits=2,scipen=5)

corranlysis <- ca(gray.ware.refired.colors)
corranlysis

summary(corranlysis)


plot.ca(corranlysis,c(1,2), map = "rowprincipal")
plot.ca(corranlysis,c(1,2), map = "colprincipal")
plot.ca(corranlysis,c(1,2), map = "symmetric")

plot.ca(corranlysis,c(1,3))

plot.ca(corranlysis,c(2,3))

# organize and transform the data from the correspondence analysis to get a format that will work with ggplot()
#extract the standard row and column coordinates
standrowcoords <- as.data.frame(corranlysis$rowcoord)
standcolcoords <- as.data.frame(corranlysis$colcoord)

#extract the singular values
sv <- corranlysis$sv

#find the eigenvalues and percent variance for each axis
eigenvalues <- sv^2
variance <- 100 * prop.table(eigenvalues)

variance


#find the principal row coordinates
principalrowcoords <- standrowcoords
for (i in 1:nrow((standrowcoords))) {
  for (j in 1:ncol(standrowcoords)) {
    principalrowcoords[i,j] <- standrowcoords[i,j] * sv[j]
  }
}

#find the principal column coordinates
principalcolcoords <- standcolcoords
for (i in 1:nrow((standcolcoords))) {
  for (j in 1:ncol(standcolcoords)) {
    principalcolcoords[i,j] <- standcolcoords[i,j] * sv[j]
  }
}


#labels
rowlabs <- rownames(gray.ware.refired.colors)
collabs <- colnames(gray.ware.refired.colors) %>%
 str_remove("X")

axis1lab <- paste0("Axis 1 (", round(variance[1], 1), "%)")
axis2lab <- paste0("Axis 2 (", round(variance[2], 1), "%)")
axis3lab <- paste0("Axis 3 (", round(variance[3], 1), "%)")

principalrowcoords <- cbind(rowlabs, settlement_cluster, principalrowcoords)
fill_by_cluster <- c("Blue Mesa" = "cornflowerblue", "Sacred Ridge" = "black", "Eastern" = "cyan4", "North-Central" = "firebrick3", "Western" = "darkorchid1")

#symmetric plot of axes 1 and 2
 gray_plot_1_2 <- ggplot() +
  geom_point(data = principalrowcoords, aes(x = Dim1, y = Dim2, fill = settlement_cluster), size = 8, shape = 21 ) +
  geom_point(data = principalcolcoords, aes(x = Dim1, y = Dim2), size = 2, color = "red") +
  theme_bw() +
  coord_fixed(ratio = 1, xlim = c(-.75, .8), ylim = c(-.75, .5)) +
  geom_hline(yintercept = 0) +
  geom_vline(xintercept = 0) +
  #geom_text_repel(data = principalrowcoords, aes(x = Dim1, y = Dim2), label = rowlabs, color = "blue", nudge_y = .08) +
  geom_text_repel(data = principalcolcoords, aes(x = Dim1, y = Dim2), label = collabs, color = "red", nudge_y = .02, size = 4) +
  scale_fill_manual("Settlement Cluster", values = fill_by_cluster) +
  labs(x = axis1lab, y = axis2lab)

gray_plot_1_2 <- plotly::ggplotly(gray_plot_1_2)

gray_plot_1_2

#white ware design elements

design_elements <- read.csv(file = "design elements.csv", row.names = 1)

# saves column with cluster membership separately, the drops it as well as the total row and column from the data file
settlement_cluster <- design_elements$Settlement_Cluster[1:22]
design_elements <- design_elements[1:22,2:10]



options(digits=2,scipen=5)

corranlysis <- ca(design_elements)
corranlysis

summary(corranlysis)


plot.ca(corranlysis,c(1,2), map = "rowprincipal")
plot.ca(corranlysis,c(1,2), map = "colprincipal")
plot.ca(corranlysis,c(1,2), map = "symmetric")

plot.ca(corranlysis,c(1,3))

plot.ca(corranlysis,c(2,3))

# organize and transform the data from the correspondence analysis to get a format that will work with ggplot()
#extract the standard row and column coordinates
standrowcoords <- as.data.frame(corranlysis$rowcoord)
standcolcoords <- as.data.frame(corranlysis$colcoord)

#extract the singular values
sv <- corranlysis$sv

#find the eigenvalues and percent variance for each axis
eigenvalues <- sv^2
variance <- 100 * prop.table(eigenvalues)

variance


#find the principal row coordinates
principalrowcoords <- standrowcoords
for (i in 1:nrow((standrowcoords))) {
  for (j in 1:ncol(standrowcoords)) {
    principalrowcoords[i,j] <- standrowcoords[i,j] * sv[j]
  }
}

#find the principal column coordinates
principalcolcoords <- standcolcoords
for (i in 1:nrow((standcolcoords))) {
  for (j in 1:ncol(standcolcoords)) {
    principalcolcoords[i,j] <- standcolcoords[i,j] * sv[j]
  }
}


#labels
rowlabs <- rownames(design_elements)
collabs <- colnames(design_elements)

axis1lab <- paste0("Axis 1 (", round(variance[1], 1), "%)")
axis2lab <- paste0("Axis 2 (", round(variance[2], 1), "%)")
axis3lab <- paste0("Axis 3 (", round(variance[3], 1), "%)")

principalrowcoords <- cbind(rowlabs, settlement_cluster, principalrowcoords)
fill_by_cluster <- c("Blue Mesa" = "cornflowerblue", "Sacred Ridge" = "black", "Eastern" = "cyan4", "North-Central" = "firebrick3", "Western" = "darkorchid1")

#symmetric plot of axes 1 and 2
design_elements_plot_1_2 <- ggplot() +
  geom_point(data = principalrowcoords, aes(x = Dim1, y = Dim2, fill = settlement_cluster), size = 8, shape = 21 ) +
  geom_point(data = principalcolcoords, aes(x = Dim1, y = Dim2), size = 2, color = "red") +
  theme_bw() +
  #coord_fixed(ratio = 1, xlim = c(-.75, .8), ylim = c(-.75, .5)) +
  geom_hline(yintercept = 0) +
  geom_vline(xintercept = 0) +
  #geom_text_repel(data = principalrowcoords, aes(x = Dim1, y = Dim2), label = rowlabs, color = "blue", nudge_y = .08) +
  geom_text_repel(data = principalcolcoords, aes(x = Dim1, y = Dim2), label = collabs, color = "red", nudge_y = .02, size = 4) +
  scale_fill_manual("Settlement Cluster", values = fill_by_cluster) +
  labs(x = axis1lab, y = axis2lab)

design_elements_plot_1_2

#design_elements_plot_1_2 <- plotly::ggplotly(artifact_plot_1_2)

#design_elements_plot_1_2

#faunal data

faunal <- read.csv(file = "faunal types.csv", row.names = 1)

# saves column with cluster membership separately, the drops it as well as the total row and column from the data file
settlement_cluster <- faunal$Settlement_Cluster[1:20]
faunal <- faunal[1:20,2:9]



options(digits=2,scipen=5)

corranlysis <- ca(faunal)
corranlysis

summary(corranlysis)


plot.ca(corranlysis,c(1,2), map = "rowprincipal")
plot.ca(corranlysis,c(1,2), map = "colprincipal")
plot.ca(corranlysis,c(1,2), map = "symmetric")

plot.ca(corranlysis,c(1,3))

plot.ca(corranlysis,c(2,3))

# organize and transform the data from the correspondence analysis to get a format that will work with ggplot()
#extract the standard row and column coordinates
standrowcoords <- as.data.frame(corranlysis$rowcoord)
standcolcoords <- as.data.frame(corranlysis$colcoord)

#extract the singular values
sv <- corranlysis$sv

#find the eigenvalues and percent variance for each axis
eigenvalues <- sv^2
variance <- 100 * prop.table(eigenvalues)

variance


#find the principal row coordinates
principalrowcoords <- standrowcoords
for (i in 1:nrow((standrowcoords))) {
  for (j in 1:ncol(standrowcoords)) {
    principalrowcoords[i,j] <- standrowcoords[i,j] * sv[j]
  }
}

#find the principal column coordinates
principalcolcoords <- standcolcoords
for (i in 1:nrow((standcolcoords))) {
  for (j in 1:ncol(standcolcoords)) {
    principalcolcoords[i,j] <- standcolcoords[i,j] * sv[j]
  }
}


#labels
rowlabs <- rownames(faunal)
collabs <- colnames(faunal)

axis1lab <- paste0("Axis 1 (", round(variance[1], 1), "%)")
axis2lab <- paste0("Axis 2 (", round(variance[2], 1), "%)")
axis3lab <- paste0("Axis 3 (", round(variance[3], 1), "%)")

principalrowcoords <- cbind(rowlabs, settlement_cluster, principalrowcoords)
fill_by_cluster <- c("Blue Mesa" = "cornflowerblue", "Sacred Ridge" = "black", "Eastern" = "cyan4", "North-Central" = "firebrick3", "Western" = "darkorchid1")

#symmetric plot of axes 1 and 2
faunal_plot_1_2 <- ggplot() +
  geom_point(data = principalrowcoords, aes(x = Dim1, y = Dim2, fill = settlement_cluster), size = 8, shape = 21 ) +
  geom_point(data = principalcolcoords, aes(x = Dim1, y = Dim2), size = 2, color = "red") +
  theme_bw() +
  #coord_fixed(ratio = 1, xlim = c(-.75, .8), ylim = c(-.75, .5)) +
  geom_hline(yintercept = 0) +
  geom_vline(xintercept = 0) +
  #geom_text_repel(data = principalrowcoords, aes(x = Dim1, y = Dim2), label = rowlabs, color = "blue", nudge_y = .08) +
  geom_text_repel(data = principalcolcoords, aes(x = Dim1, y = Dim2), label = collabs, color = "red", nudge_y = .02, size = 4) +
  scale_fill_manual("Settlement Cluster", values = fill_by_cluster) +
  labs(x = axis1lab, y = axis2lab)

faunal_plot_1_2

#faunal_plot_1_2 <- plotly::ggplotly(artifact_plot_1_2)

#faunal_plot_1_2

#artifact assemblage data

artifact <- read.csv(file = "artifact types.csv", row.names = 1)

# saves column with cluster membership separately, the drops it as well as the total row and column from the data file
settlement_cluster <- artifact$Settlement_Cluster[1:36]
artifact <- artifact[1:36,2:16]



options(digits=2,scipen=5)

corranlysis <- ca(artifact)
corranlysis

summary(corranlysis)


plot.ca(corranlysis,c(1,2), map = "rowprincipal")
plot.ca(corranlysis,c(1,2), map = "colprincipal")
plot.ca(corranlysis,c(1,2), map = "symmetric")

plot.ca(corranlysis,c(1,3))

plot.ca(corranlysis,c(2,3))

# organize and transform the data from the correspondence analysis to get a format that will work with ggplot()
#extract the standard row and column coordinates
standrowcoords <- as.data.frame(corranlysis$rowcoord)
standcolcoords <- as.data.frame(corranlysis$colcoord)

#extract the singular values
sv <- corranlysis$sv

#find the eigenvalues and percent variance for each axis
eigenvalues <- sv^2
variance <- 100 * prop.table(eigenvalues)

variance


#find the principal row coordinates
principalrowcoords <- standrowcoords
for (i in 1:nrow((standrowcoords))) {
  for (j in 1:ncol(standrowcoords)) {
    principalrowcoords[i,j] <- standrowcoords[i,j] * sv[j]
  }
}

#find the principal column coordinates
principalcolcoords <- standcolcoords
for (i in 1:nrow((standcolcoords))) {
  for (j in 1:ncol(standcolcoords)) {
    principalcolcoords[i,j] <- standcolcoords[i,j] * sv[j]
  }
}


#labels
rowlabs <- rownames(artifact)
collabs <- colnames(artifact)

axis1lab <- paste0("Axis 1 (", round(variance[1], 1), "%)")
axis2lab <- paste0("Axis 2 (", round(variance[2], 1), "%)")
axis3lab <- paste0("Axis 3 (", round(variance[3], 1), "%)")

principalrowcoords <- cbind(rowlabs, settlement_cluster, principalrowcoords)
fill_by_cluster <- c("Blue Mesa" = "cornflowerblue", "Sacred Ridge" = "black", "Eastern" = "cyan4", "North-Central" = "firebrick3", "Western" = "darkorchid1")

#symmetric plot of axes 1 and 2
artifact_plot_1_2 <- ggplot() +
  geom_point(data = principalrowcoords, aes(x = Dim1, y = Dim2, fill = settlement_cluster), size = 8, shape = 21 ) +
  geom_point(data = principalcolcoords, aes(x = Dim1, y = Dim2), size = 2, color = "red") +
  theme_bw() +
  #coord_fixed(ratio = 1, xlim = c(-.75, .8), ylim = c(-.75, .5)) +
  geom_hline(yintercept = 0) +
  geom_vline(xintercept = 0) +
 # geom_text_repel(data = principalrowcoords, aes(x = Dim1, y = Dim2), label = rowlabs, color = "blue", nudge_y = .08) +
  geom_text_repel(data = principalcolcoords, aes(x = Dim1, y = Dim2), label = collabs, color = "red", nudge_y = .02, size = 4) +
  scale_fill_manual("Settlement Cluster", values = fill_by_cluster) +
  labs(x = axis1lab, y = axis2lab)

artifact_plot_1_2

artifact_plot_1_2 <- plotly::ggplotly(artifact_plot_1_2)

artifact_plot_1_2


