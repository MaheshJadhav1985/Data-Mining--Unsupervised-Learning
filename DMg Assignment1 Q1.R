##Data Mining: Assignment - 1 Question - 1 Aitlines Data Set

setwd("D:/ISB/24-DataMining/Assignment-1")
input <- read.csv("EastWestAirlinesCluster-csv.csv",header=TRUE)

dim(input)
mydata <- input[,2:12]
normalized_data <- scale(mydata)

## Hierarchical Clustering
d <- dist(normalized_data, method = "euclidean")
fit <- hclust(d, method="ward.D")
plot(fit)
rect.hclust(fit, k=3, border="red")

groups <- cutree(fit, k=3)
cluster_id <- groups
input2 <- cbind(cluster_id,mydata)
write.csv(input2,"HCluster-Output 3.csv")

##########################################################################################
##########################################################################################

#random sample of 95% of records 
sampled_mydata = mydata[sample(1:nrow(mydata),round(nrow(mydata)*0.95),replace = F),]
dim(sampled_mydata)

normalized_data1 <- scale(sampled_mydata)
d1 <- dist(normalized_data1, method = "euclidean")
fit1 <- hclust(d1, method="ward.D") 
plot(fit1) 
rect.hclust(fit1, k=3, border="red") 

groups1 <- cutree(fit1, k=3)
cluster_id1 <- groups1
input1 <- cbind(cluster_id1,sampled_mydata)
write.csv(input1,"HCluster-Output sampled 3.csv")

##########################################################################################
##########################################################################################

## K-means clustering
fit <- kmeans(normalized_data, 3) # 3 cluster solution
fit

mydata<- data.frame(fit$cluster,mydata) # append cluster membership
dim(mydata)

write.csv(mydata,"Kmeans-Output 3.csv")

#################################################################################################

# Determine number of clusters
wss <- (nrow(normalized_data))*sum(apply(normalized_data,2,var))
for (i in 2:30) wss[i] <- sum(kmeans(normalized_data, 
                                     centers=i)$withinss)
plot(1:30, wss, type="b", xlab="Number of Clusters",
     ylab="Within groups sum of squares")