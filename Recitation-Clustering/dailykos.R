#reading csv file

dailykos1<-read.csv("dailykos.csv")

#calculating distances between rows
distances<-dist(dailykos1,method = "euclidean")

#hierarrchial clustering
clusterDailykos = hclust(distances, #distance variable
                         method = "ward.D2"#method to be used, here wards minimumvariance is used
                         ) 
plot(clusterDailykos)

clusterGroups<-cutree(clusterDailykos,k=7)
dailykosGroup1 = dailykos[clusterGroups == 1,]
dailykosGroup2 = dailykos[clusterGroups == 2,]
dailykosGroup3 = dailykos[clusterGroups == 3,]
dailykosGroup4 = dailykos[clusterGroups == 4,]
dailykosGroup5 = dailykos[clusterGroups == 5,]
dailykosGroup6 = dailykos[clusterGroups == 6,]
dailykosGroup7 = dailykos[clusterGroups == 7,]


tail(sort(colMeans(dailykosGroup1)))
tail(sort(colMeans(dailykosGroup2)))
tail(sort(colMeans(dailykosGroup3)))
tail(sort(colMeans(dailykosGroup4)))
tail(sort(colMeans(dailykosGroup5)))
tail(sort(colMeans(dailykosGroup6)))
tail(sort(colMeans(dailykosGroup7)))


set.seed(1000)
kmeansCluster = kmeans(x = dailykos, centers = 7)
dailykoskGroup1 = dailykos[kmeansCluster$cluster == 1,]
dailykoskGroup2 = dailykos[kmeansCluster$cluster == 2,]
dailykoskGroup3 = dailykos[kmeansCluster$cluster == 3,]
dailykoskGroup4 = dailykos[kmeansCluster$cluster == 4,]
dailykoskGroup5 = dailykos[kmeansCluster$cluster == 5,]
dailykoskGroup6 = dailykos[kmeansCluster$cluster == 6,]
dailykoskGroup7 = dailykos[kmeansCluster$cluster == 7,]

tail(sort(colMeans(dailykoskGroup1)))
tail(sort(colMeans(dailykoskGroup2)))
tail(sort(colMeans(dailykoskGroup3)))
tail(sort(colMeans(dailykoskGroup4)))
tail(sort(colMeans(dailykoskGroup5)))
tail(sort(colMeans(dailykoskGroup6)))
tail(sort(colMeans(dailykoskGroup7)))



