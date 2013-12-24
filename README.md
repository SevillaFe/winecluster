Winecluster
======

Code from an assignment for Pradeep Ravikumar's Intro to Business Data Analytics class in UT Austin's MS in Business Analytics program.

A k-means clustering of the wine data set from UC Irvine's Machine Learning Repository. The dataset is 13 chemical attributes of wines taken from three different grape cultivars. The dataset is available at http://archive.ics.uci.edu/ml/datasets/Wine .

winecluster.R is the k-means code

traintestsplit.R is for splitting the complete dataset into a training set and test set

evaluation.R measures a training set clustering's predictive abilites by running test set samples through it, and generates associated plots

analysis.R generates  plots showing the accuracy of a clustering

pca.R uses principal component analysis to generate a 2-d visualization of a clustering

See http://www.michaelwsherman.com/projects/winecluster/index.html for more information, a full analysis of the dataset, and examples of the generated plots.
