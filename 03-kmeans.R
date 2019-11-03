##############################################################################
## Expand on Distance and now apply Kmeans
## Learning goals:
## - Kmeans applications
## - Evaluate cluster solutions
## - hands on with Kmeans

## resources:
## awesome  https://uc-r.github.io/kmeans_clustering
## https://en.wikibooks.org/wiki/Data_Mining_Algorithms_In_R/Clustering/K-Means
## page16: https://core.ac.uk/download/pdf/27210461.pdf
##############################################################################


options(stringsAsFactors = FALSE)

## load the packages
library(tidyverse)
library(cluster)
library(factoextra)

## load a dataset
## Data dictionary below
# https://stat.ethz.ch/R-manual/R-devel/library/datasets/html/USJudgeRatings.html
data(USJudgeRatings)
head(USJudgeRatings)
judges = USJudgeRatings   ## keeps things easier to type


## as always, EDA!
glimpse(judges)
summary(judges)
skimr::skim(judges)

## scaling is our friend
j <- scale(judges)
summary(j)

## kmeans
## set 3 clusters, recommended to do about 25 intial starts
## we do this to avoid local optimization on the wss used, we want to globally
## find the best solution (why try 1 random set of points when we can try many?)
k <- kmeans(j, centers = 3, iter.max = 25, nstart = 25)
class(k)
is.list(k)
names(k)


## from factoextra - we can make nicer ggplot2 plots
fviz_cluster(k, data=j)

## inspect what we have for data
k$cluster
k$centers
table(k$cluster)
k$size

## information about the algorithm and convergence
k$tot.withinss
k$withinss
k$iter
k$ifault

### EXERCISE: 
###       1.  Read in the Wine Excel dataset for Mod3
###       2.  explore the data to get a sense
###       3.  Scale the data to be z-score
###       4.  apply kmeans clustering with K of 5
###       5.  Plot the cluster solution
###       6.  Determine the number of records in each cluser
###       7.  Review the cluster properties (HINT:  cluster centers)
wine <- read_xlsx("dataset/Wine.xlsx")

glimpse(wine)
summary(wine)
skimr::skim(wine)
colnames(wine) <- gsub(" ", "_", colnames(wine))

wine_s <- scale(wine)
summary(wine_s)

wine_k <- kmeans(wine_s, centers = 5, iter.max = 25, nstart = 25)

fviz_cluster(wine_k, data=wine_s)

wine_k$size

wine_k$centers

############################## Cluster Evaluation/Optimization
## we have to manually set the K, but can we use eval metrics to attempt to optimize K?

############ METHOD 1:  WSS

## remember we can extract the total withinss
## & refresher
fviz_cluster(k, data = j)

## for each K, lets evaluate the distance 
## build our own function!
## fit kmeans given the k argument, return tot wss
k_wss = function(k) {
  km = kmeans(j, k, nstart=25, iter=25)
  kwss = km$tot.withinss
  return(kwss)
}


## use purrr to map and analyze
## we try to use the "elbow" method, something we will use in other techniques
x <- 1:15
wss_vals <- map_dbl(x, k_wss)
plot(x, wss_vals, type="b", main = "WSS Judges K Eval")

## another way you could think about it



### Exercise:   Use the WSS method 
##  Use the included USArrests dataset
##  HINT: a = USArrests
## using total wss as the metric, how many clusters would you recommend using?
a <-  USArrests
az <- scale(a) 
az_withinss <- map_dbl(1:30, function(k) {
  a_model <- kmeans(x = az, centers = k, nstart=25, iter=25)
  a_model$tot.withinss
})

az_df <- data.frame(
  k = 1:30,
  tot_withinss = az_withinss
)
az_df
plot(1:30, az_withinss, type="b", main = "WSS Judges K Eval")

# Here we use k=4

############ METHOD 2:  Silhoutte evaluation

## silhoutte plots
## when we group points, how similar are the points to those in the cluster and others
## For every point, we want the score to be close to 1
## zero means the point is at the boundary
## negative values means that we may not have the best clustering (though it can happen)
plot(silhouette(k$cluster, dist = dist(j)), col = 1:3)



## we can also extract the data fi we really need to
## you could roll your own eval
## https://medium.com/codesmart/r-series-k-means-clustering-silhouette-794774b46586
s <- silhouette(k$cluster, dist = dist(j))
s
mean(s[, 3])
plot(silhouette(k$cluster, dist = dist(j)))

## but we dont have to, factoextra to the rescue
fviz_nbclust(j, kmeans, method = "silhouette")

############ Putting it all together Exercise:
##    Using the same wine dataset from earlier
##    how many clusters would you recommend?
##    up to you in how you determine K, everything can be used
##    plot your final cluster solution
##    how many records are in each cluster
##    quick review of the cluster profiles
fviz_cluster(wine_k, data=wine_s)

## wss function
k_wss <- function(k) {
  km <- kmeans(wine_s, k, nstart = 25, iter = 25)
  kwss <- km$tot.withinss
  return(kwss)
}

x = 1:20
wine_stats <- map_dbl(x, k_wss)
plot(x, wine_stats, type="b", main="Wine - WSS")


# silhouette
wine_sil <- silhouette(wine_k$cluster, dist = dist(wine_s))
wine_sil
plot(wine_sil, col =1:5)

fviz_nbclust(wine_s, kmeans, method = "silhouette", k.max = 20)

# lets use a solution of 2
fviz_cluster(kmeans(wine_s, 2, 25, 25))


