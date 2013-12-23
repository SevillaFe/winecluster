#traintestsplit.R
#data for reading in wine data, and then creating a test set and a training set
#as well as verifying that the subsets have an even distribution of the 3 classes
wine_data <- read.csv(file="winedata.csv",head=TRUE,sep=",") #read the data

#rerun this next block of code to get new sets and then show the distribution of the classes in a plot
#there's probably a better way to this in R but we don't know it
wine_data = wine_data[sample(nrow(wine_data)),] #randomize order of rows
wine_data_test = wine_data[1:59,]
wine_data_train = wine_data[60:178,]
class1 = c(sum(wine_data$class == 1) / length(wine_data[,1]), 
           sum(wine_data_train$class == 1) / length(wine_data_train[,1]), 
           sum(wine_data_test$class == 1) / length(wine_data_test[,1]))
class2 = c(sum(wine_data$class == 2) / length(wine_data[,2]), 
           sum(wine_data_train$class == 2) / length(wine_data_train[,2]), 
           sum(wine_data_test$class == 2) / length(wine_data_test[,2]))
class3 = c(sum(wine_data$class == 3) / length(wine_data[,3]), 
           sum(wine_data_train$class == 3) / length(wine_data_train[,3]), 
           sum(wine_data_test$class == 3) / length(wine_data_test[,3]))
classplot <- data.frame(class1,class2,class3)
classplot <- classplot * 100
barplot(as.matrix(classplot), main="Distribution of Classes in Complete Data, Training Data, and Test Data", 
        ylab= "Percentage of Samples of Given Class",   beside=TRUE, col=rainbow(3))
legend(9,40, c("Complete Data (n=178)","Training Data (n=119)","Test Data (n=59)"), cex=0.6, bty="n", fill=rainbow(3));

#We run the above code until we get a good split, then we save the files
write.csv(wine_data_test, "wine_data_test.csv", row.names=FALSE)
write.csv(wine_data_train, "wine_data_train.csv", row.names=FALSE)