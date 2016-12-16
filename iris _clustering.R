iris<-read.csv("Iris.csv")
###########CLUSTERING WITH IRIS

library(clValid)
library(factoextra)
str(iris)
iris<-iris[,-1]


####step 1 getting cluster tendency

data<-scale(iris[,-5])
get_clust_tendency(data,n=50,gradient =list(low="yellow",high="red"))
###n is the no of selected sample
### this gives Hopkin's statistic which is <0,5, we can conclude that it is clusterable


#####Step 2 getting optimal number of clusters

fviz_nbclust(data,kmeans,method = "wss")

####we can replace kmeans with other methods such as clara and pam (hard clustering types)
#### ooops optimal number of cluster is 3
#### We can determine no of clusters for K-means clustering using this
#### WSS is the elbow method showing 3 the optimal no of clusters


##### choosing the right algorithm for clustering
which_algo<-clValid(data,nClust = 2:6,
                    clMethods = c("kmeans","clara","model","hierarchical"),
                    validation = "internal")

summary(which_algo)
#### K-means seems to be best for IRIS data
kmean_iris<-kmeans(data,3,nstart=20)
fviz_cluster(kmean_iris,data=data,frame.type = "convex")+
  scale_color_brewer(palette = "Set2")+
  scale_fill_brewer(palette = "Set2") +
  theme_minimal()

#### visualise clusters
####validating clusters now using silhoutte( we can use dunn's index also)
silhou<-silhouette(kmean_iris$cluster,dist =dist(data))



fviz_silhouette(silhou, print.summary = FALSE) +
  scale_fill_brewer(palette = "Spectral") +
  scale_color_brewer(palette = "Spectral") +
  theme_minimal()+
  theme(axis.text.x = element_blank(), axis.ticks.x = element_blank())
####seems like they are well clustered







