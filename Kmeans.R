
## Menginstall Library

library(cluster)
library(factoextra)
library(ggplot2)

## Input Data

library(readxl)
data <- read_excel("C:/Users/Rzyns/Desktop/Portofolio/K-Means/data.xlsx")
View(data)
str(data)
summary(data)

## Melakukan Uji Multikolinieritas

install.packages('corrplot')
library(corrplot)
corrmatrix <- cor(data)
corrmatrix
corrplot(corrmatrix, method = 'number')

#Klaster 2
Clustering2=kmeans(data,centers=2,nstart=25, iter.max = 25)
Clustering2
fviz_cluster(Clustering2, geom = "point", data = data)+ggtitle("k=2")
fviz_cluster(Clustering2, data = data)+ggtitle("Pembagian 2 Klaster")
final=data.frame(data, Clustering2$cluster, Clustering2$iter)
print(final)
View(final)

#Klaster 3
Clustering3=kmeans(data,centers=3,nstart=25, iter.max = 25)
Clustering3
fviz_cluster(Clustering3, geom = "point", data = data)+ggtitle("k=3")
fviz_cluster(Clustering3, data = data)+ggtitle("Pembagian 3 Klaster")
final3=data.frame(data, Clustering3$cluster, Clustering3$iter)
print(final3)
View(final3)

#Klaster 4
Clustering4=kmeans(data,centers=4,nstart=25, iter.max = 25)
Clustering4
fviz_cluster(Clustering4, geom = "point", data = data)+ggtitle("k=4")
fviz_cluster(Clustering4, data = data)+ggtitle("Pembagian 4 Klaster")
final4=data.frame(data, Clustering4$cluster, Clustering4$iter)
print(final4)
View(final4)

## Silhouette Index
df <- data[-c(1)]
silhouette_score <- function(k){
  km <- kmeans(df, centers = k, nstart=25)
  ss <- silhouette(km$cluster, dist(df))
  mean(ss[, 3])
}
k <- 2:4
avg_sil <- sapply(k, silhouette_score)
avg_sil
plot(k, type='b', avg_sil, xlab='Number of clusters', ylab='Average Silhouette Scores', frame=FALSE)
fviz_nbclust(data, kmeans, method='silhouette')


