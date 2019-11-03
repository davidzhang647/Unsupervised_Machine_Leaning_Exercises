##############################################################################
## Distance and Hierarchical Clustering - hclust
## Learning goals:
## - generate distance matrices 
## - different forms/application
## - hierarchical clustering (hclust)
## - hclust evaluation and visuzation

## Notes
## categorical/dummy and numeric distances?
## fuse them (add, or weighted add)
## https://www.rdocumentation.org/packages/analogue/versions/0.17-3/topics/fuse
##############################################################################

options(stringsAsFactors = FALSE)

## setup
library(dplyr)
library(ggplot2)
library(readxl)
# install.packages("dummies")
library(dummies)    ## easier to dummy encode categorical data
# install.packages("philentropy")
library(philentropy)   ## expanded distance calculations
# install.packages("skimr")
library(skimr)     ## easy summary statistics
# install.packages("cluster")
library(cluster)    ## datasets and utilities for clustering, along with some algos
# install.packages("factoextra")
library(factoextra)    ## clustering visualization utilities
# install.packages("dendextend")
library(dendextend)     ## for working with dendrograms
library(devtools)     ## source a function from the web


## a comment on the packages
## tidyverse = awesome tools, 
## dummies - makes model.matrix easier
## philentropy - extends dist to add a whole host of others
## skimr = easy EDA
## cluster = some datasets, some methods, but helper ultilities and metrics



### really basic example to see this in action
dat = data.frame(x = floor(runif(2, 0, 10)), 
                 y = floor(runif(2, 0, 10)))
dat


## simple 1 way distance
dist(dat$x)
dist(dat$y)

## distance for the records -- euclidian by default
dist(dat)

## other approaches
dist(dat, method= "manhattan")
dist(dat, method= "canberra")

## there are other approaches, but I really dont see these come up that often
## that said, nothing stopping you from looping parameters to assess what works the best
## more on that later

## we can also use the philentropy package which gives us others
## 100% compatitble with the dist 
## has a nice feature where you can specify an id variable so it will be excluded
distance(dat) == dist(dat)

## for more, getDistMethods()
distance(dat, method = "cosine")

################################ ok, enough with a simple  dataset
############ let's start small


## bring in the canoncial cars dataset
cars = mtcars
head(cars)



## Exercise:   a quick explore, thinking about our goal
##             to calc distances and cluster/segment our data

## lets keep just the variables that have a decent distribution
cars2 <- cars %>% select(mpg, disp:qsec)
glimpse(cars2)
skimr::skim(cars2)

## create a euclidian distance matrix using base R to start
dist_e <- dist(cars2)
dist_e
class(dist_e)
plot(dist_e)
dist_e_matrix <- as.matrix(dist_e)
dist_e_matrix[1:5, 1:5]

## however, we can plot it with external packages
## factoextra
fviz_dist(dist_e)

## EXERCISE:  generate a distance matrix using the manhattan method
## ?dist
dist_m <- dist(cars2, method = "manhattan")
dist_m
dist_m_matrix <- as.matrix(dist_m)
dist_m_matrix[1:5, 1:5]
fviz_dist(dist_m)

## remember scale and z-scores from 760? (and basic stats?)
## scaling is almost always necessary
## with variables on different units of measurement, large variances will drive
## the distance
## ?scale = z score (0/1)
cars2_z <- scale(cars2)
skim(as.data.frame(cars2_z))

## Exercise:  which car is the most similar (shortest distance) from 
##            Porsche 914-2 using the scaled distances/euclidan
## HINT:      convert to matrix, and filter/sort. 
dist_e2 <- dist(cars2_z)
dist_e2_mat <- as.matrix(dist_e2)
p_pairs <- dist_e_matrix["Porsche 914-2", ]
sort(p_pairs)


################################ hclust

## we don't have to hunt manually for pairs
## we can use the power of R to start to cluster our data
## hclust is built in and needs a distance matrix
## three linkage methods we will explore, complete, single, and average
## complete is default
h1 <- hclust(dist_e2)
class(h1)
names(h1)
plot(h1)
fviz_dend(h1)

## above is from scaled data, euclidian, and complete linkage
## there are SO MANY other combinations we can try!!
## more on that later

## the goal is to put these into groups or clusters
## remember the cut technique?
# ?cutree
## control via k (clusters), or h, height (for later)
cutree(h1, k = 5)
clus <- cutree(h1, k=5)
table(clus)


## needless to say, we can add the cluster assignments back to the 
## ORIGINAL dataset easily
## why = the "sort" of the rows never changed, we are passing data objects
##       around, so we can align (cbind) the data
cars$cluster <- clus


################################ Profiling - tell a story from the data!
############   do the segments tell a story that has business value?
############   - identify trends/patterns that are core to the segment
############   Data analytics is iterative.  A return to EDA

## simple boxplot
boxplot(mpg ~ cluster, data = cars)

## remember looping through plots?
for (i in 1:ncol(cars)) {
  boxplot(cars[, i] ~ clus, main = names(cars)[i])
}

## we could also try to group by and skim
## with can use skim to do summaries, or
## use the tidyverse to work with the output to help with profiling
cars %>% 
  group_by(cluster) %>% 
  skim()

cars %>% 
  group_by(cluster) %>% 
  skim_to_wide() %>% 
  arrange() %>% 
  print(n=100)


################################ Attempts to optimize/explore
############  Coding lets us explore our decisions and the solution 
############   sets more easily

## bring in a function to help with distance on hclust
## function is hclust_eval
devtools::source_gist("441822ea54d7fa5e17c4772ffe70dfb1")

## quick look at the function
hclust_eval

## function takes an hclust object and extracts data to explore
## iterations and height
hstats <- hclust_eval(h1)

## make a dataframe from the list
## remember purrr and map functions?
hstats_df <- map_dfr(hstats, function(x) x)


## finally, the plot that we are looking for
plot(hstats_df$n_clust,
     hstats_df$h_dist,
     type = "b",
     main = "Distance by Cluster")


## What we just did:
## because everything is an object of sorts in R
## use the hclust algo to help guide the stats
## for each entry, get the distance and clusters
## 1 cluster is the entire dataset

## need to balance the number of segments with cluster size
## and practicality of the biz problem
# table(cutree(h1, k=10))



################################ exploration methods
############  Above we only used euclidian distance and complete linkage
############  Below is for getting you to think about how we can 
############  leverage what we have learned in 760 to expand our 
############  consideration space

### we can combine multiple settings
### its just a data blending exercise to ensure
### we get the metrics we need for the plots
### above emphasizes that we can at least do it manually
### But we shoudl consider evaluating the process
### both visually and via plots we can generate for "optimization"
### as it relates to our business problem
### This means that some solutions may make more sense within the context
### of problem.


################################ categorical data
############  euclidian is not appropriate, but can use jaccard

## go back to the cars dataset, but keep the just the "categorical"
cars$cluster <- NULL
View(cars)
## use the dummies package to encode 0/1 for some
## ?dummy.data.frame
cars_cat <- cars %>% 
  select(cyl, vs:carb)
glimpse(cars_cat)

cars_cat2 <- dummy.data.frame(cars_cat,
                              names = c("cyl",
                                        "gear",
                                        "carb"))
glimpse(cars_cat2)

## the distance is binary
## binary = jaccard
dist_j <- dist(cars_cat2, method="binary")
h2 <- hclust(dist_j, method = "average")
plot(h2)
fviz_dend(h2)
