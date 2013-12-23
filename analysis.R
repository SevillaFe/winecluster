#analysis.R
#some plots and other analysis from the a clustering produced by winecluster. 
#winecluster.R should be run first, this uses some of that output

#jitter plot of distance from centroid colored by class
ssetrain = sse
ssetrain$cluster = jitter(ssetrain$cluster)
ssetrain$class<-cut(sse$class, c(.5,1.5,2.5,3.5), right=FALSE, labels=c(1:3)) #for better coloring
jitplot = qplot(cluster, sse, data = ssetrain, color=class, alpha = I(2/3),  size = I(10))
jitplot + coord_cartesian(ylim=c(0, 1.25)) + scale_y_continuous(breaks=seq(0, 1.25, .25)) + 
  scale_x_continuous(breaks=seq(1,3,1)) + xlab("Cluster") + ylab("Euclidean Distance from Centroid") + 
  ggtitle("Distance of Samples from Their Closest Cluster Centroid, Colored by Class")

#to generate some interesting box and whisker plots we have to relabel things, using some tricks
classes = wine_data_norm
clusters = wine_data_norm

#from the jitter plot we see we have to swap clusters so they match class numbers
#this swapping will make our box and whisker plots easier to read.
#this is swapping 2 and 3
clusters[which(clusters$cluster == 2),15] = 200
clusters[which(clusters$cluster == 3),15] = 2
clusters[which(clusters$cluster == 200),15] = 3

clusters$class = clusters$cluster + .5 #we use a clever numbering trick to label our groups in a nice order
all = rbind(classes,clusters)
all$class = cut(all$class, c(.9, 1.1, 1.6, 2.2, 2.8, 3.1, 3.6), right=FALSE, 
                labels=c("class=1", "cluster=1", "class=2", "cluster=2","class=3", "cluster=3") )

#now a plotting storm of box and whiskers
alc = qplot(class, alc, data=all, geom="boxplot", ylab="Alcohol, Normalized", xlab = "Class/Cluster", 
      main="Box/Whisker Plots of Normalized Alcohol Values, \n Grouped by Original Class and Cluster Membership")
malic = qplot(class, malic, data=all, geom="boxplot", ylab="Malic Acid, Normalized", xlab = "Class/Cluster", 
            main="Box/Whisker Plots of Normalized Malic Acid Values, \n Grouped by Original Class and Cluster Membership")
ash = qplot(class, ash, data=all, geom="boxplot", ylab="Ash, Normalized", xlab = "Class/Cluster", 
            main="Box/Whisker Plots of Normalized Ash Values, \n Grouped by Original Class and Cluster Membership")
alcal = qplot(class, alcal, data=all, geom="boxplot", ylab="Alcalinity of Ash, Normalized", xlab = "Class/Cluster", 
            main="Box/Whisker Plots of Normalized Alcalinity of Ash Values, \n Grouped by Original Class and Cluster Membership")
mag = qplot(class, mag, data=all, geom="boxplot", ylab="Magnesium, Normalized", xlab = "Class/Cluster", 
            main="Box/Whisker Plots of Normalized Magnesium Values, \n Grouped by Original Class and Cluster Membership")
totphen = qplot(class, totphen, data=all, geom="boxplot", ylab="Total Phenols, Normalized", xlab = "Class/Cluster", 
            main="Box/Whisker Plots of Normalized Total Phenol Values, \n Grouped by Original Class and Cluster Membership")
flav = qplot(class, flav, data=all, geom="boxplot", ylab="Flavanoids, Normalized", xlab = "Class/Cluster", 
            main="Box/Whisker Plots of Normalized Flavanoid Values, \n Grouped by Original Class and Cluster Membership")
nonflav = qplot(class, nonflav, data=all, geom="boxplot", ylab="Non-Flavanoid Phenols, \nNormalized", xlab = "Class/Cluster", 
            main="Box/Whisker Plots of Normalized Non-Flavanoid Phenol Values, \n Grouped by Original Class and Cluster Membership")
proant = qplot(class, proant, data=all, geom="boxplot", ylab="Proanthocyanins, Normalized", xlab = "Class/Cluster", 
            main="Box/Whisker Plots of Normalized Proanthocyanin Values, \n Grouped by Original Class and Cluster Membership")
color = qplot(class, color, data=all, geom="boxplot", ylab="Color Intensity, Normalized", xlab = "Class/Cluster", 
            main="Box/Whisker Plots of Normalized Color Intensity Values, \n Grouped by Original Class and Cluster Membership")
hue = qplot(class, hue, data=all, geom="boxplot", ylab="Hue, Normalized", xlab = "Class/Cluster", 
            main="Box/Whisker Plots of Normalized Hue Values, \n Grouped by Original Class and Cluster Membership")
ods = qplot(class, ods, data=all, geom="boxplot", ylab="OD280/OD315, Normalized", xlab = "Class/Cluster", 
            main="Box/Whisker Plots of Normalized OD280/OD315 Values, \n Grouped by Original Class and Cluster Membership")
proline = qplot(class, proline, data=all, geom="boxplot", ylab="Proline, Normalized", xlab = "Class/Cluster", 
            main="Box/Whisker Plots of Normalized Proline Values, \n Grouped by Original Class and Cluster Membership")

#multiplot is from the web, defined below
multiplot(alc, malic, ash, alcal, mag, totphen, flav, nonflav, cols=2)
multiplot(proant, color, hue, ods, proline, cols=2)

#alcohol vs. flavanoids
clusterplot = wine_data_norm
clusterplot$class<-cut(sse$class, c(.5,1.5,2.5,3.5), right=FALSE, labels=c(1:3)) #for coloring
clusterplot = qplot(alc, flav, data = clusterplot, color=class, alpha = I(1/2), size = I(10))
clusterplot + xlab("Alcohol, Normalized") + ylab("Flavanoids, Normalized") + 
  ggtitle("Alcohol vs. Flavanoids,\nColored by Class")

# Multiple plot function, from http://www.cookbook-r.com/Graphs/Multiple_graphs_on_one_page_(ggplot2)/
#
# ggplot objects can be passed in ..., or to plotlist (as a list of ggplot objects)
# - cols:   Number of columns in layout
# - layout: A matrix specifying the layout. If present, 'cols' is ignored.
#
# If the layout is something like matrix(c(1,2,3,3), nrow=2, byrow=TRUE),
# then plot 1 will go in the upper left, 2 will go in the upper right, and
# 3 will go all the way across the bottom.
#
multiplot <- function(..., plotlist=NULL, file, cols=1, layout=NULL) {
  require(grid)
  
  # Make a list from the ... arguments and plotlist
  plots <- c(list(...), plotlist)
  
  numPlots = length(plots)
  
  # If layout is NULL, then use 'cols' to determine layout
  if (is.null(layout)) {
    # Make the panel
    # ncol: Number of columns of plots
    # nrow: Number of rows needed, calculated from # of cols
    layout <- matrix(seq(1, cols * ceiling(numPlots/cols)),
                     ncol = cols, nrow = ceiling(numPlots/cols))
  }
  
  if (numPlots==1) {
    print(plots[[1]])
    
  } else {
    # Set up the page
    grid.newpage()
    pushViewport(viewport(layout = grid.layout(nrow(layout), ncol(layout))))
    
    # Make each plot, in the correct location
    for (i in 1:numPlots) {
      # Get the i,j matrix positions of the regions that contain this subplot
      matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))
      
      print(plots[[i]], vp = viewport(layout.pos.row = matchidx$row,
                                      layout.pos.col = matchidx$col))
    }
  }
}