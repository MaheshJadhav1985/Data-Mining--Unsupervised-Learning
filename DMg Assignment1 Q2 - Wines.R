## Data Mining: Assignment - 1 Question - 2. Wine's Data Set

setwd("D:/ISB/24-DataMining/Assignment-1")
wine_data <- read.csv("wines.csv",header = T)

dim(wine_data)
wine_data1 <- wine_data[,2:14]
head(wine_data1)

pcaObj<-princomp(wine_data1, cor = TRUE, scores = TRUE, covmat = NULL)
summary(pcaObj)

loadings(pcaObj)
pcaObj$loadings

pcaObj$scores

plot(pcaObj)
biplot(pcaObj)

####################################################################################################

normailzed_wines <- scale(wine_data1)
pcaObj1 <-princomp(normailzed_wines, cor = TRUE, scores = TRUE, covmat = NULL)
summary(pcaObj1)

#######################################################################################################

##Dendrogram for top 5 principle components
PC_scores <- pcaObj$scores[,1:2]
d1 <- dist(PC_scores, method = "euclidean") 
fit1 <- hclust(d1, method="ward.D")     
plot(fit1,sub = "Principle Components Dendrogram")
rect.hclust(fit1, k=3, border="red") 

groups1 <- cutree(fit1, k=3) 
cluster_id1 <- groups1
Wine_PC_cluster <- cbind(cluster_id1,wine_data1)
write.csv(Wine_PC_cluster,"Wines PC Cluster.csv")

##Dendrogram for all the chemical combiations
normailzed_wines <- scale(wine_data1)
d2 <- dist(normailzed_wines, method = "euclidean") 
fit2 <- hclust(d2, method="ward.D")     
plot(fit2,sub = "All Wines Dendrogram") 
rect.hclust(fit2, k=3, border="red")

groups2 <- cutree(fit2, k=3)
cluster_id2 <- groups2
Wine_All_cluster <- cbind(cluster_id2,wine_data1)
write.csv(Wine_PC_cluster,"Wines All Cluster.csv")

#################################################################################################

##Correlations between columns
x <- data.frame(cor(wine_data1))
View(x)
pairs(~Alcohol+Malic_Acid+Ash+Ash_Alcalinity+Magnesium+Total_Phenols, 
      data = wine_data1, main="Simple Scatterplot Matrix")