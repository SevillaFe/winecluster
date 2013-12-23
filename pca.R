#pca.R - PCA with the output of wineclustering.R

library(Matrix) #must be installed
forpca = wine_data_norm
A = svd(forpca[,2:14]) #we only care about the columns with actual data
A$d = Diagonal(A$d, n=13) #to turn the singular values into a diagonal matrix
Ak = A$u[,1:3]%*%A$d[1:3,1:3]%*%t(A$v)[1:3,] #rebuilding the matrix with k=3 approximation
pcaplot = cbind(data.frame(Ak[,1]),data.frame(Ak[,2]),forpca$class) #so we can plot
colnames(pcaplot) = c("col1","col2", "class")
pcaplot$class<-cut(pcaplot$class, c(.5,1.5,2.5,3.5,4.5,5.5,6.5), right=FALSE, labels=c(
  "Class 1", "Class 2", "Class 3", "Cluster 1 Centroid", "Cluster 2 Centroid", "Cluster 3 Centroid")) #for coloring
qplot(col1, col2, data=pcaplot, color=class, alpha = I(2/3), size=I(10), main="Principal Component Analysis Using
      Rank-k=3 Approximation of Normalized Wine Data")

#we do the same as above, but by cluster instead of class
forpca = wine_data_norm
forpca[which(forpca$cluster == 2),15] = 200 #swap cluster 2 and 3 to match class
forpca[which(forpca$cluster == 3),15] = 2
forpca[which(forpca$cluster == 200),15] = 3
forpca$class = forpca$cluster
forpca = rbind(forpca[1:14],k1,k2,k3)
forpca[179,]$class = 4 #we're giving new classes to our centroids so we can label them
forpca[180,]$class = 6
forpca[181,]$class = 5
A = svd(forpca[,2:14]) #we only care about the columns with actual data
A$d = Diagonal(A$d, n=13)
Ak = A$u[,1:3]%*%A$d[1:3,1:3]%*%t(A$v)[1:3,]
pcaplot = cbind(data.frame(Ak[,1]),data.frame(Ak[,2]),forpca$class)
colnames(pcaplot) = c("col1","col2", "class")
pcaplot$class<-cut(pcaplot$class, c(.5,1.5,2.5,3.5,4.5,5.5,6.5), right=FALSE, labels=c(
  "Cluster 1", "Cluster 2", "Cluster 3", "Cluster 1 Centroid", "Cluster 2 Centroid", "Cluster 3 Centroid")) #for coloring
qplot(col1, col2, data=pcaplot, color=class, alpha = I(2/3), size=I(10), main="Principal Component Analysis Using
      Rank-k=3 Approximation of Normalized Wine Data")

#finally, highlighting the misclassified samples
forpca = wine_data_norm
forpca[which(forpca$cluster == 2),15] = 200 #swap cluster 2 and 3 to match class
forpca[which(forpca$cluster == 3),15] = 2
forpca[which(forpca$cluster == 200),15] = 3
forpca[which(forpca$class != forpca$cluster),]$cluster = 7 #this marks the misclassifed samples
forpca$class = forpca$cluster
forpca = rbind(forpca[1:14],k1,k2,k3)
forpca[179,]$class = 4 #we're giving new classes to our centroids so we can label them
forpca[180,]$class = 6
forpca[181,]$class = 5
A = svd(forpca[,2:14]) #we only care about the columns with actual data
A$d = Diagonal(A$d, n=13)
Ak = A$u[,1:3]%*%A$d[1:3,1:3]%*%t(A$v)[1:3,]
pcaplot = cbind(data.frame(Ak[,1]),data.frame(Ak[,2]),forpca$class)
colnames(pcaplot) = c("col1","col2", "class")
pcaplot$class<-cut(pcaplot$class, c(.5,1.5,2.5,3.5,4.5,5.5,6.5,7.5), right=FALSE, labels=c(
  "Cluster 1", "Cluster 2", "Cluster 3", "Cluster 1 Centroid", "Cluster 2 Centroid", "Cluster 3 Centroid", "Incorrectly Clustered")) #for coloring
qplot(col1, col2, data=pcaplot, color=class, alpha = I(2/3), size=I(10), main="Principal Component Analysis Using
      Rank-k=3 Approximation of Normalized Wine Data")