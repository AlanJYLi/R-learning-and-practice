data = iris
data[,1:4] = scale(data[,1:4], center=TRUE, scale=TRUE) # scale the features

k_set = c(2,3,4,5,6,7,8,9,10)  # k values

# use the enumeration approach to explore the best combination of predictors
compare_table = matrix(nrow=9, ncol=12)  
# store the total distance from each point 
# to the cluster center of different models
compare_table[,1] = k_set  # k values for kmeans model
colnames(compare_table) = c('k',"sl+sw","sl+pl","sl+pw","sw+pl","sw+pw","pl+pw", 
                            "sl+sw+pl","sl+pl+pw","sl+sw+pw","sw+pl+pw","sl+sw+pl+pw")
for (i in k_set){
  kmeans_model = kmeans(data[,c(1,2)], centers=i, iter.max=1000, 
                        nstart=5, algorithm="Hartigan-Wong")
  compare_table[i-1,2] = round(kmeans_model[["tot.withinss"]],3)
  kmeans_model = kmeans(data[,c(1,3)], centers=i, iter.max=1000, 
                        nstart=5, algorithm="Hartigan-Wong")
  compare_table[i-1,3] = round(kmeans_model[["tot.withinss"]],3)
  kmeans_model = kmeans(data[,c(1,4)], centers=i, iter.max=1000, 
                        nstart=5, algorithm="Hartigan-Wong")
  compare_table[i-1,4] = round(kmeans_model[["tot.withinss"]],3)
  kmeans_model = kmeans(data[,c(2,3)], centers=i, iter.max=1000, 
                        nstart=5, algorithm="Hartigan-Wong")
  compare_table[i-1,5] = round(kmeans_model[["tot.withinss"]],3)
  kmeans_model = kmeans(data[,c(2,4)], centers=i, iter.max=1000, 
                        nstart=5, algorithm="Hartigan-Wong")
  compare_table[i-1,6] = round(kmeans_model[["tot.withinss"]],3)
  kmeans_model = kmeans(data[,c(3,4)], centers=i, iter.max=1000, 
                        nstart=5, algorithm="Hartigan-Wong")
  compare_table[i-1,7] = round(kmeans_model[["tot.withinss"]],3)
  kmeans_model = kmeans(data[,c(1,2,3)], centers=i, iter.max=1000, 
                        nstart=5, algorithm="Hartigan-Wong")
  compare_table[i-1,8] = round(kmeans_model[["tot.withinss"]],3)
  kmeans_model = kmeans(data[,c(1,3,4)], centers=i, iter.max=1000, 
                        nstart=5, algorithm="Hartigan-Wong")
  compare_table[i-1,9] = round(kmeans_model[["tot.withinss"]],3)
  kmeans_model = kmeans(data[,c(1,2,4)], centers=i, iter.max=1000, 
                        nstart=5, algorithm="Hartigan-Wong")
  compare_table[i-1,10] = round(kmeans_model[["tot.withinss"]],3)
  kmeans_model = kmeans(data[,c(2,3,4)], centers=i, iter.max=1000, 
                        nstart=5, algorithm="Hartigan-Wong")
  compare_table[i-1,11] = round(kmeans_model[["tot.withinss"]],3)
  kmeans_model = kmeans(data[,c(1,2,3,4)], centers=i, iter.max=1000, 
                        nstart=5, algorithm="Hartigan-Wong")
  compare_table[i-1,12] = round(kmeans_model[["tot.withinss"]],3)
}


# Elbow method
plot(compare_table[,1], compare_table[,2], 
     type="n", 
     xlab="k clusters", 
     ylab="sum of in-cluster distance", 
     ylim=c(0,200))# build the plot
for (i in seq(2,12)){  # draw lines
  lines(compare_table[,1], compare_table[,i], col=i, lty=i-1)
}
legend('topright',colnames(compare_table)[2:12],lty=1:11,col=2:12,cex=0.6)


kmeans_model = kmeans(data[,c(3,4)], centers=3, iter.max=1000, 
                      nstart=5, algorithm="Hartigan-Wong")
cat("group size:\n");kmeans_model["size"]

data[,6] = kmeans_model["cluster"]
plot(data[,3], data[,4], 
     type="n", 
     xlab="Petal.Length", 
     ylab="Petal.Width")# build the plot
points(data[data["cluster"]==1,3],data[data["cluster"]==1,4],col="red")
points(data[data["cluster"]==2,3],data[data["cluster"]==2,4],col="blue")
points(data[data["cluster"]==3,3],data[data["cluster"]==3,4],col="black")
legend("topleft",c("group1","group2","group3"),pch=1,col=c("red","blue","black"))




library(cluster)
library(fpc)
kmeans_k_3 = kmeans(data[,c(3,4)], centers=3, iter.max=1000, 
                    nstart=5, algorithm="Hartigan-Wong")
# kmeans model for k=3
km_stats1 = cluster.stats(d=dist(data[,3:4],method="minkowski"),
                          kmeans_k_3[["cluster"]],silhouette=TRUE)
# compute "silhouette"
cat("slihouette for k=3:\n")
km_stats1$avg.silwidth  # average slihouette value for all points
km_stats1$clus.avg.silwidths  # average slihouette value for different clusters

kmeans_k_2 = kmeans(data[,c(3,4)], centers=2, iter.max=1000, 
                    nstart=5, algorithm="Hartigan-Wong")
# kmeans model for k=2
km_stats2 = cluster.stats(d=dist(data[,3:4],method="minkowski"),
                          kmeans_k_2[["cluster"]],silhouette=TRUE)
# compute "silhouette"
cat("slihouette for k=2:\n")
km_stats2$avg.silwidth  # average slihouette value for all points
km_stats2$clus.avg.silwidths  # average slihouette value for different clusters