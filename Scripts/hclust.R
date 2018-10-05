#libraries
library(proxy) 
library(dendextend)
library(dplyr)
library(circlize)
library(factoextra)
library(NbClust)
library(pvclust)
library(cluster)
library(fpc)
library(clusterSim)

#repoduciblitiy 
set.seed(92718)

#dissimilarity-distance matrix, correlation based because data is binary
#and produced better results than Jaccard and Gower distance which also common in binary data distance matrices
dist_mat <- proxy::dist(veri, by_rows = TRUE, method = "Pearson")

#hierarchical clustering, clustering method "ward.D2", previously called "agnes"
veri_clus <- hcut(dist_mat, k = 6, hc_func = "hclust", hc_method = "average", stand=TRUE, graph=TRUE)


#dendrogam and cluster graphics
#red frames
hclusters <- hclust(dist_mat, method = "complete")
plot(hclusters )

#cuttree & dendrograms
plotcut <- cutree(hclusters, k = 6)
plot(hclusters)
rect.hclust(hclusters , k = 6)
abline(h = 2.36, col = 'red')

suppressPackageStartupMessages(library(dendextend))
avg_dend_obj <- as.dendrogram(hclusters)
avg_col_dend <- color_branches(avg_dend_obj, k = 6)
plot(avg_col_dend)

suppressPackageStartupMessages(library(dplyr))
vericl <- mutate(veri, cluster = plotcut)
count(vericl,cluster)

par(mar = rep(0,4))
circlize_dendrogram(avg_col_dend, legend)

#Optimum number of cluster

# Elbow method
fviz_nbclust(veri, hcut, method = "wss") +
  geom_vline(xintercept = 6, linetype = 2)+
  labs(subtitle = "Elbow method")
# Silhouette method
fviz_nbclust(veri, hcut, method = "silhouette")+
  labs(subtitle = "Silhouette method")
# Gap statistic
# nboot = 500 to keep the function speedy. It's better to raise the number but comp limitations
# Use verbose = FALSE to hide computing progression.
set.seed(123)
fviz_nbclust(veri, hcut,  method = "gap_stat", nboot = 500)+
  labs(subtitle = "Gap statistic method")
#only frey, mcclain, cindex, sihouette and dunn can be computed
noc <- NbClust(dist_mat, min.nc = 3, max.nc = 9, 
               method = "averga", index = "dunn", alphaBeale = 0.005)
noc$Best.nc

##Dunn index,optimum number of cluster is 6

#Cluster Validation

#Internal

#Silhouette statistics
fviz_silhouette(veri_clus)
plot(silhouette(plotcut, dist_mat))
sil <- veri_clus$silinfo$widths[, 1:3]
# Objects with negative silhouette
neg_sil_index <- which(sil[, 'sil_width'] < 0)
sil[neg_sil_index, , drop = FALSE]

##Comment: we might correct the observation's clusters but we don't because ... and we aim to improve by updates

#Dunn index = 0.35 - will improve as sample grows and data updated by new inputs
clusterstat <- cluster.stats(d=dist_mat, plotcut, silhouette = TRUE)
clusterstat$dunn

##Comment: as the sample grows, algorithms better learns and cluster, the data will be updated and the clustering validity will be improve as the data updated

#External(Cross-validation), confusion matrix

clusvalid <- cbind(veri, clusterNum = veri_clus$cluster)
clusvalid$label <- cluster_labels
table(clusvalid$clusterNum, clusvalid$label)

# Compute cluster stats

labelo <- as.numeric(as.factor(clusvalid$label))
clust_stats <- cluster.stats(d = dist_mat, 
                             labelo, clusvalid$clusterNum)
# Corrected Rand index
clust_stats$corrected.rand

#   Z1 Z2 Z3 Z4 Z5 Z6
# 1 67 11  2  6  1  3
# 2 47  1  2 41 14  0
# 3 12 13 11  5 22  6
# 4 26  1  0 24  0  0
# 5 53  0  0 33  0  0
# 6 20 12  1 36  2  8


#Conclusion: 

#H0: there isn't a single gaussian distribution in the data
#so we ddecided to apply hclust given the data as binary and k=6 as a priori info
#hclust results are evaluated in both ways, internal and external,
#Rand index used to decide the appropriate method for clustering 
#ward.D2 clust method is used and as a dissimilarity method Pearson is chosen among
#correlation based methods Pearson and distance based methods, Jaccard and Gower
#to check the optimal number f cluster data frey, mcclain, cindex, sihouette and dunn
#Among the indices, 2,4 and 10 clusters suggested and we decide to move on to confirmation after displaying that dunn give the 6 as the optimal number of clusters
#and to confirm that the clusters are valid, we examined the Dunn index and silhouette 

#Suggestions
#the questions between 1-4 needs to be updated and more distinctive questions is needed
#the position and combination of the already existed questions can be considered

#summary statistics of each cluster


desc <- cluster.Description(veri, veri_clus$cluster, sdType="sample", precission=2)
desc[,,1]

#1 - arithmetic mean
#2 - standard deviation
#3 - median
#4 - median absolute deviation (mad)
#5 - mode 
