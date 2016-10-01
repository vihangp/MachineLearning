library(caret)

airlinesCluster = read.csv("AirlinesCluster.csv")

preproc = preProcess(airlinesCluster)
airlinesNorm = predict(preproc, airlinesCluster)

distances = dist(x = airlinesNorm, method = "euclidean")
clust = hclust(d = distances, method = "ward.D")
plot(clust)

clusterGroups = cutree(clust, k = 5)
airlinesGroup1 = airlinesCluster[clusterGroups == 1,]

which.max(tapply(airlinesCluster$Balance, clusterGroups, mean))
which.max(tapply(airlinesCluster$QualMiles, clusterGroups, mean))
which.max(tapply(airlinesCluster$BonusMiles, clusterGroups, mean))
which.max(tapply(airlinesCluster$BonusTrans, clusterGroups, mean))
which.max(tapply(airlinesCluster$FlightMiles, clusterGroups, mean))
which.max(tapply(airlinesCluster$FlightTrans, clusterGroups, mean))
which.max(tapply(airlinesCluster$DaysSinceEnroll, clusterGroups, mean))

set.seed(88)
kcluster = kmeans(x = airlinesNorm, iter.max = 1000, centers = 5)
table(kcluster$cluster)