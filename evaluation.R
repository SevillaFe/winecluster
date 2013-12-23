#evaluation.R - code for analysis and plotting of predictive wine data clustering model
#run a clustering first with winecluster.R, some of that output is referenced here.

#plots to test for normal distribution
par(mfrow=c(2,4))
qqnorm(wine_data$alc, main="Normal Q-Q Plot of Alcohol");qqline(wine_data$alc)
qqnorm(wine_data$malic, main="Normal Q-Q Plot of Malic Acid");qqline(wine_data$malic)
qqnorm(wine_data$ash, main="Normal Q-Q Plot of Ash");qqline(wine_data$ash)
qqnorm(wine_data$alcal, main="Normal Q-Q Plot of Alcalinity of Ash");qqline(wine_data$alcal)
qqnorm(wine_data$mag, main="Normal Q-Q Plot of Magnesium");qqline(wine_data$mag)
qqnorm(wine_data$totphen, main="Normal Q-Q Plot of Total Phenols");qqline(wine_data$totphen)
qqnorm(wine_data$flav, main="Normal Q-Q Plot of Flavanoids");qqline(wine_data$flav)
qqnorm(wine_data$nonflav, main="Normal Q-Q Plot of Nonflavanoid Phenols");qqline(wine_data$nonflav)
par(mfrow=c(2,3))
qqnorm(wine_data$proant, main="Normal Q-Q Plot of Proanthocyanins");qqline(wine_data$proant)
qqnorm(wine_data$color, main="Normal Q-Q Plot of Color Intensity");qqline(wine_data$color)
qqnorm(wine_data$hue, main="Normal Q-Q Plot of Hue");qqline(wine_data$hue)
qqnorm(wine_data$ods, main="Normal Q-Q Plot of OD280/OD315 \n of Diluted Wines");qqline(wine_data$ods)
qqnorm(wine_data$proline, main="Normal Q-Q Plot of Proline");qqline(wine_data$proline)

wine_data_test <- read.csv(file="wine_data_test.csv",head=TRUE,sep=",")
wine_data_train <- read.csv(file="wine_data_train.csv",head=TRUE,sep=",")

#we now have the issue that we wish to apply the same normalization to the test data
#that we did to training data. This means that we need to use the min and the max from the training
#data, which means we sadly need too much code here:
wine_data_test_norm = wine_data_test
wine_data_test_norm$alc = (wine_data_test_norm$alc - min(wine_data_train$alc) ) / 
  (max(wine_data_train$alc) - min(wine_data_train$alc))
wine_data_test_norm$malic = (wine_data_test_norm$malic - min(wine_data_train$malic) ) / 
  (max(wine_data_train$malic) - min(wine_data_train$malic))
wine_data_test_norm$ash = (wine_data_test_norm$ash - min(wine_data_train$ash) ) / 
  (max(wine_data_train$ash) - min(wine_data_train$ash))
wine_data_test_norm$alcal = (wine_data_test_norm$alcal - min(wine_data_train$alcal) ) / 
  (max(wine_data_train$alcal) - min(wine_data_train$alcal))
wine_data_test_norm$mag = (wine_data_test_norm$mag - min(wine_data_train$mag) ) / 
  (max(wine_data_train$mag) - min(wine_data_train$mag))
wine_data_test_norm$totphen = (wine_data_test_norm$totphen - min(wine_data_train$totphen) ) / 
  (max(wine_data_train$totphen) - min(wine_data_train$totphen))
wine_data_test_norm$flav = (wine_data_test_norm$flav - min(wine_data_train$flav) ) / 
  (max(wine_data_train$flav) - min(wine_data_train$flav))
wine_data_test_norm$nonflav = (wine_data_test_norm$nonflav - min(wine_data_train$nonflav) ) / 
  (max(wine_data_train$nonflav) - min(wine_data_train$nonflav))
wine_data_test_norm$proant = (wine_data_test_norm$proant - min(wine_data_train$proant) ) / 
  (max(wine_data_train$proant) - min(wine_data_train$proant))
wine_data_test_norm$color = (wine_data_test_norm$color - min(wine_data_train$color) ) / 
  (max(wine_data_train$color) - min(wine_data_train$color))
wine_data_test_norm$hue = (wine_data_test_norm$hue - min(wine_data_train$hue) ) / 
  (max(wine_data_train$hue) - min(wine_data_train$hue))
wine_data_test_norm$ods = (wine_data_test_norm$ods - min(wine_data_train$ods) ) / 
  (max(wine_data_train$ods) - min(wine_data_train$ods))
wine_data_test_norm$proline = (wine_data_test_norm$proline - min(wine_data_train$proline) ) / 
  (max(wine_data_train$proline) - min(wine_data_train$proline))

#now we determine which centroid each test case goes in. Our centroids are from our
#run of the clustering with the training data
k1dist = apply(wine_data_test_norm[,2:14],1, function(x) sqrt(sum((x - k1[2:14])^2)))
k2dist = apply(wine_data_test_norm[,2:14],1, function(x) sqrt(sum((x - k2[2:14])^2)))
k3dist = apply(wine_data_test_norm[,2:14],1, function(x) sqrt(sum((x - k3[2:14])^2)))
dists = cbind(k1dist,k2dist,k3dist)

#we compare the distances, adding anew column "cluster" to wine_data_test_norm
#cluster contains the closer cluster (the lesser distance)
wine_data_test_norm$cluster = apply(dists,1, function(x) which(x == min(x)))

#now we calculate the errors by splitting and finding the SSE
class1samples = subset(wine_data_test_norm, wine_data_test_norm$cluster == 1)
class2samples = subset(wine_data_test_norm, wine_data_test_norm$cluster == 2)
class3samples = subset(wine_data_test_norm, wine_data_test_norm$cluster == 3)
class1samples$sse = apply(class1samples[,2:14],1, function(x) sum( (x-k2[2:14])^2 ))
class2samples$sse = apply(class2samples[,2:14],1, function(x) sum( (x-k2[2:14])^2 ))
class3samples$sse = apply(class3samples[,2:14],1, function(x) sum( (x-k2[2:14])^2 ))
sse = rbind(class1samples,class2samples,class3samples)
print(c("Test Data SSE = ",sum(sse$sse)))

#and our count of errors:
totalerror = 0
totalerror = totalerror + length(which(k1subset$class != round(mean(k1subset$class))))
totalerror = totalerror + length(which(k2subset$class != round(mean(k2subset$class))))
totalerror = totalerror + length(which(k3subset$class != round(mean(k3subset$class))))
print(c("Total Errors = ", totalerror))

#jitter plot of distance from centroid colored by class
ssetrain = sse
ssetrain$cluster = jitter(ssetrain$cluster)
ssetrain$class<-cut(sse$class, c(.5,1.5,2.5,3.5), right=FALSE, labels=c(1:3)) #for better coloring
jitplot = qplot(cluster, sse, data = ssetrain, color=class, alpha = I(2/3),  size = I(10))
jitplot + coord_cartesian(ylim=c(0, 1.25)) + scale_y_continuous(breaks=seq(0, 1.25, .25)) + 
  scale_x_continuous(breaks=seq(1,3,1)) + xlab("Cluster") + ylab("Euclidean Distance from Centroid") + 
  ggtitle("Distance of Samples from Their Closest Cluster Centroid, Colored by Class")

#A Jitter Plot of the Test Data
ssetest = sse
ssetest$cluster = jitter(ssetest$cluster)
ssetest$class<-cut(sse$class, c(.5,1.5,2.5,3.5), right=FALSE, labels=c("1 - Test", "2 - Test", "3 - Test")) #for better coloring
jitplot = qplot(cluster, sse, data = ssetest, color=class, alpha = I(2/3),  size = I(10))
jitplot + coord_cartesian(ylim=c(0, 3.5)) + scale_y_continuous(breaks=seq(0, 3.5, .25)) + 
  scale_x_continuous(breaks=seq(1,3,1)) + xlab("Cluster") + ylab("Euclidean Distance from Centroid") + 
  ggtitle("Distance of Test Samples from Their Closest Cluster Centroid, Colored by Class")

#now lets overlay our testing and training data on a jitter plot. ssetrain is from the training data
ssetrain$class<-cut(sse$class, c(.5,1.5,2.5,3.5), right=FALSE, labels=c("1 - Train", "2 - Train", "3 - Train")) #for better coloring
sseall = rbind(ssetrain, ssetest)
sseall$cluster = jitter(sseall$cluster, factor = .3, amount = .2)
jitplot = qplot(cluster, sse, data = sseall, color=class, alpha = I(2/3),  size = I(10))
jitplot + coord_cartesian(ylim=c(0, 3.5)) + scale_y_continuous(breaks=seq(0, 3.5, .25)) + 
  scale_x_continuous(breaks=seq(1,3,1)) + xlab("Cluster") + ylab("Euclidean Distance from Centroid") + 
  ggtitle("Distance of Samples from Their Closest Cluster Centroid, Colored by Class")