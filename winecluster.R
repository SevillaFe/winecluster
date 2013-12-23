#winecluster.R
#Custom k-means algorithm for data at http://archive.ics.uci.edu/ml/datasets/Wine
#Different functionality is commented out
#We used this code for both predictive model creating and whole-data clustering

results = matrix(nrow=100,ncol=3) #a matrix of our different clustering results
for (i in 1:100) { #we comment out the for loop when we are running with a seed we pick
  #wine_data <- read.csv(file="wine_data_train3.csv",head=TRUE,sep=",") #for training
  wine_data <-read.csv(file="winedata.csv", head=TRUE, sep=",") #for whole set
  #randomize
  wine_data = wine_data[sample(nrow(wine_data)),]
  
  #a few different normalization/standardization options:
  #scale just the ash, alcalinity of ash, hue, and proanthocyanin attributes
  #wine_data_norm = wine_data
  #wine_data_norm$ash = scale(wine_data$ash)
  #wine_data_norm$alcal = scale(wine_data$alcal)
  #wine_data_norm$proant = scale(wine_data$proant)
  #wine_data_norm$hue = scale(wine_data$hue)
  #wine_data_norm = apply(wine_data,2,function (x) (x-min(x)) / (max(x) - min(x) ) ) #then normalized
  
  #merely normalize by min/max
  wine_data_norm = apply(wine_data,2,function (x) (x-min(x)) / (max(x) - min(x) ) )
  
  #scale the whole thing (make each attribute mean 0 with a standard deviation of 1)
  #wine_data_norm = scale(wine_data)
  #wine_data_norm = apply(wine_data,2,function (x) (x-min(x)) / (max(x) - min(x) ) )
  
  #and everyone's favorite, do nothing:
  #wine_data_norm = wine_data
  
  #after normalizing/standardizing
  #convert into a data frame, and fix the erroneously normalized/scaled/etc class field to be 1,2, or 3
  wine_data_norm = as.data.frame(wine_data_norm)
  wine_data_norm$class = wine_data$class
  
  #get three random centroids.
  #Was easy to copy rows to keep the names matching. There is probably a better way to deal with this.
  #We didn't want a random value that was necessarily in the data set,
  #so after copying a random row we make all relevant attributes random
  set.seed(15) #i can be a number if we aren't using the for loop. this lets us get our best result from
               #the brute-force for loop.
  k1 = wine_data_norm[sample(length(wine_data_norm[,1]),1),]
  k2 = wine_data_norm[sample(length(wine_data_norm[,1]),1),]
  k3 = wine_data_norm[sample(length(wine_data_norm[,1]),1),]
  
  #note that these random values range from 0 to 1, which is slightly incorrect for
  #non-normalized data. But since we ended up sticking to normalized data most of the time
  #we decided it wasn't worth it to fix this.
  k1[2:14] = sapply(k1[2:14], function(x) runif(1,0.0,1.0))
  k2[2:14] = sapply(k2[2:14], function(x) runif(1,0.0,1.0))
  k3[2:14] = sapply(k3[2:14], function(x) runif(1,0.0,1.0))
  
  #two options for distance, euclidean and arithmetic:
  #vectors of euclidean distances, note we leave out the class
  k1dist = apply(wine_data_norm[,2:14],1, function(x) sqrt(sum((x - k1[2:14])^2)))
  k2dist = apply(wine_data_norm[,2:14],1, function(x) sqrt(sum((x - k2[2:14])^2)))
  k3dist = apply(wine_data_norm[,2:14],1, function(x) sqrt(sum((x - k3[2:14])^2)))
  
  #vectors of manhattan distance
  #k1dist = apply(wine_data_norm[,2:14],1, function(x) sum(abs(x-k1[2:14])))
  #k2dist = apply(wine_data_norm[,2:14],1, function(x) sum(abs(x-k2[2:14])))
  #k3dist = apply(wine_data_norm[,2:14],1, function(x) sum(abs(x-k3[2:14])))
  
  dists = cbind(k1dist,k2dist,k3dist)
  
  #we compare the distances, adding a new column "cluster" to wine_data_norm
  #cluster contains the closer cluster (the lesser distance)
  wine_data_norm$cluster = apply(dists,1, function(x) which(x == min(x)))
  
  #we want "old" centroids, for comparing in the main kmeans loop. When the new centroids match the old centroids
  #kmeans stops.
  k1old = k1
  k2old = k2
  k3old = k3
  
  #compute new centroids for each cluster
  k1[2:14] = apply(subset(wine_data_norm[2:14],wine_data_norm$cluster == 1),2, function(col) mean(col))
  k2[2:14] = apply(subset(wine_data_norm[2:14],wine_data_norm$cluster == 2),2, function(col) mean(col))
  k3[2:14] = apply(subset(wine_data_norm[2:14],wine_data_norm$cluster == 3),2, function(col) mean(col))
  
  #if a cluster is empty, we want to make its centroid the point with the greatest SSE
  class1samples = subset(wine_data_norm, wine_data_norm$cluster == 1)
  class2samples = subset(wine_data_norm, wine_data_norm$cluster == 2)
  class3samples = subset(wine_data_norm, wine_data_norm$cluster == 3)
  
  #we have to find every sample's SSE
  class1samples$sse = apply(class1samples[,2:14],1, function(x) sum( (x-k1[2:14])^2 ))
  class2samples$sse = apply(class2samples[,2:14],1, function(x) sum( (x-k2[2:14])^2 ))
  class3samples$sse = apply(class3samples[,2:14],1, function(x) sum( (x-k3[2:14])^2 ))
  sse = rbind(class1samples,class2samples,class3samples)
  sse = sse[order(sse$sse),] #sort by sse
  
  #if a centroid is not a number, make that centroid the point with the highest SSE
  #we then remove the centroid from our sse list so we don't reuse it if more than one
  #centroid is null.
  if(any(is.na(k1))) {
    #print(c("EMPTY",k1[1,]))
    k1 = sse[(length(sse[,1])),]
    k1 = k1[,-16]
    sse = sse[-length(sse[,1]),]
    #print(c("FIXED",k1[1,]))
  }
  if(any(is.na(k2))) {
    #print(c("EMPTY",k2[1,]))
    k2 = sse[(length(sse[,1])),]
    k2 = k2[,-16]
    sse = sse[-length(sse[,1]),]
    #print(c("FIXED",k2[1,]))
  }
  if(any(is.na(k3))) {
    #print(c("EMPTY",k3[1,]))
    k3 = sse[(length(sse[,1])),]
    k3 = k3[,-16]
    sse = sse[-length(sse[,1]),]
    #print(c("FIXED",k3[1,]))
  }
  
  finaltotalerror = 99999 #for scope purposes
  
  #now we loop until the previous iteration's centroids match (meaning centroids haven't moved)
  #while (!all(k1==k1old) || !all(k2==k2old) || !all(k3=k3old) ){ kept giving errors
  #this is still probably good enough to work in most instances
  while ( !(sum(k1^2)==sum(k1old^2) && sum(k2^2)==sum(k2old^2) && sum(k3^2)==sum(k3old^2)) ){
    
    #new distances euclidean
    k1dist = apply(wine_data_norm[,2:14],1, function(x) sqrt(sum((x - k1[2:14])^2)))
    k2dist = apply(wine_data_norm[,2:14],1, function(x) sqrt(sum((x - k2[2:14])^2)))
    k3dist = apply(wine_data_norm[,2:14],1, function(x) sqrt(sum((x - k3[2:14])^2)))
    
    #manhattan
    #k1dist = apply(wine_data_norm[,2:14],1, function(x) sum(abs(x-k1[2:14])))
    #k2dist = apply(wine_data_norm[,2:14],1, function(x) sum(abs(x-k2[2:14])))
    #k3dist = apply(wine_data_norm[,2:14],1, function(x) sum(abs(x-k3[2:14])))
    
    dists = cbind(k1dist,k2dist,k3dist)
    
    #assign points to a cluster
    wine_data_norm$cluster = apply(dists,1, function(x) which(x == min(x)))
    
    #make new clusters
    k1old = k1
    k2old = k2
    k3old = k3
    
    #compute new centroids for each cluster
    k1[2:14] = apply(subset(wine_data_norm[2:14],wine_data_norm$cluster == 1),2, function(col) mean(col))
    k2[2:14] = apply(subset(wine_data_norm[2:14],wine_data_norm$cluster == 2),2, function(col) mean(col))
    k3[2:14] = apply(subset(wine_data_norm[2:14],wine_data_norm$cluster == 3),2, function(col) mean(col))
    
    #if a cluster is empty, we want to make its centroid the point with the greatest SSE
    class1samples = subset(wine_data_norm, wine_data_norm$cluster == 1)
    class2samples = subset(wine_data_norm, wine_data_norm$cluster == 2)
    class3samples = subset(wine_data_norm, wine_data_norm$cluster == 3)
    
    #we have to find every sample's SSE
    class1samples$sse = apply(class1samples[,2:14],1, function(x) sum( (x-k1[2:14])^2 ))
    class2samples$sse = apply(class2samples[,2:14],1, function(x) sum( (x-k2[2:14])^2 ))
    class3samples$sse = apply(class3samples[,2:14],1, function(x) sum( (x-k3[2:14])^2 ))
    sse = rbind(class1samples,class2samples,class3samples)
    sse = sse[order(sse$sse),] #sort by sse
    
    #if a centroid is not a number, make that centroid the point with the highest SSE
    #we then remove the centroid from our sse list so we don't reuse it if more than one
    #centroid is null.
    if(any(is.na(k1))) {
      #print(c("EMPTY",k1[1,]))
      k1 = sse[(length(sse[,1])),]
      k1 = k1[,-16]
      sse = sse[-length(sse[,1]),]
      #print(c("FIXED",k1[1,]))
    }
    if(any(is.na(k2))) {
      #print(c("EMPTY",k2[1,]))
      k2 = sse[(length(sse[,1])),]
      k2 = k2[,-16]
      sse = sse[-length(sse[,1]),]
      #print(c("FIXED",k2[1,]))
    }
    if(any(is.na(k3))) {
      #print(c("EMPTY",k3[1,]))
      k3 = sse[(length(sse[,1])),]
      k3 = k3[,-16]
      sse = sse[-length(sse[,1]),]
      #print(c("FIXED",k3[1,]))
    }
    
    #We want to measure the quality of our clustering. One way is SSE:
    #print(c("Current SSE = ",sum(sse$sse)))
    
    #Another way we're calling total error classes. It is the total number of misclasified examples.
    #Note that this measurement isn't perfect, and that it will give strange results
    #when clusters aren't relatively homogenous. But it is likely quite useful for comparing
    #different clusterings.
    k1subset = subset(wine_data_norm, wine_data_norm$cluster == 1)
    k2subset = subset(wine_data_norm, wine_data_norm$cluster == 2)
    k3subset = subset(wine_data_norm, wine_data_norm$cluster == 3)
    #we compute total error by taking the mean of each cluster's class (which is usually a number
    #close to the majority of the cluster's classes) and then rounding this (to get the actual
    #value of the cluster's class), and then comparing each sample's class to the majority
    #class of the cluster found by rounding. There is probably a better way to do this.
    #then we count up the number of mistakes
    totalerror = 0
    totalerror = totalerror + length(which(k1subset$class != round(mean(k1subset$class))))
    totalerror = totalerror + length(which(k2subset$class != round(mean(k2subset$class))))
    totalerror = totalerror + length(which(k3subset$class != round(mean(k3subset$class))))
    #print(c("Total Errors = ", totalerror))
    finaltotalerror = totalerror
  }
  #print(k1)
  #print(k2)
  #print(k3)
  #print(c("Mean Class Cluster 1 = ", mean(subset(wine_data_norm, wine_data_norm$cluster == 1)$class)))
  #print(c("Mean Class Cluster 2 = ", mean(subset(wine_data_norm, wine_data_norm$cluster == 2)$class)))
  #print(c("Mean Class Cluster 3 = ", mean(subset(wine_data_norm, wine_data_norm$cluster == 3)$class)))
  print(c("Final SSE = ",sum(sse$sse)))
  print(c("Final Total Errors = ", finaltotalerror))
  #store the results, so we can reference them later to find a good seed
  results[i,1] = i
  results[i,2] = sum(sse$sse)
  results[i,3] = finaltotalerror
}

