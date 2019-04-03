install.packages("ISLR")
library(ISLR)
data(College)
View(College)
library(clValid)
library(plotrix)

College_x <- College[,-1]
#K-Means Clustering
College_x_scaled <- scale(College_x, center = TRUE, scale = TRUE)

#Q1-1
# Evaluating the cluster validity measures k=2~10
College_cvalid <- clValid(College_x_scaled, 2:10, clMethods = "kmeans", 
                        validation = c("internal", "stability"))
summary(College_cvalid)


# Perform K-Means Clustering with the best K determined by Silhouette
College_kmc <- kmeans(College_x_scaled,3)

str(College_kmc) 
College_kmc$centers
College_kmc$size
College_kmc$cluster

# Compare the cluster info. and class labels
real_class <- wine_class
kmc_cluster <- wine_kmc$cluster
table(real_class, kmc_cluster)

# Compare each cluster for KMC
cluster_kmc <- data.frame(wine_x_scaled, clusterID = as.factor(wine_kmc$cluster))
kmc_summary <- data.frame()

for (i in 1:(ncol(cluster_kmc)-1)){
  kmc_summary = rbind(kmc_summary, 
                      tapply(cluster_kmc[,i], cluster_kmc$clusterID, mean))
}

colnames(kmc_summary) <- paste("cluster", c(1:3))
rownames(kmc_summary) <- colnames(wine_x)
kmc_summary  ##same as centroid!!
