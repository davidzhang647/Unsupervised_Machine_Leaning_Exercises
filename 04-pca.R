##############################################################################
## Dimension Reduction 1: Principal Components Analysis
## Learning goals:
## - application of PCA in R
## - data considerations
##
##############################################################################

## set the options
options(stringsAsFactors=FALSE)
options(digits=3)

## load the packages
library(tidyverse)
library(factoextra)
library(skimr)

## new packages
install.packages("corrplot")
library("corrplot")



## lets start with that same judges dataset
## ordinarily, we are dealing with larger datsets
judges = USJudgeRatings

## of course, we should explore
glimpse(judges)
skim(judges)

###################################### Correlation Review

## generate the correlation matrix
jc <- cor(judges)
class(jc)
jc

## lets visualize the matrix
corrplot(jc)
corrplot(jc, type = "upper", method = "number")
corrplot(jc,
         type = "upper",
         method = "color",
         diag = F,
         addCoef.col = "white")

###################################### PCA


## fit the mode PCA model
p <- prcomp(judges, center = T, scale. = T)

## EXERCISE:  load the diamonds dataset on Qtools
##            do you have to do any data cleaning?
##            EXCLUDE the price variable, we will use it later
##            review the correlation matrix
##            fit a PCA model and ensure to center/scale the data
##            review the variance captured
##            review the rotations/loadings
diamonds <- read_csv("dataset/diamonds.csv")
diamonds_int <- diamonds

glimpse(diamonds)
skim(diamonds)

diamonds <- diamonds %>% 
  select_if(is.numeric) %>% 
  select(-X1, -price)

dc <- cor(diamonds)

corrplot(dc,
         type = "upper",
         method = "color",
         diag = F,
         addCoef.col = "white")

## explore pca
diamonds_PCA <- prcomp(diamonds, center = T, scale. = T)
class(diamonds_PCA)
is.list(diamonds_PCA)
names(diamonds_PCA)
summary(diamonds_PCA)
diamonds_PCA$rotation

######################################## coming back to kmeans

## remember kmeans?
k <- kmeans(scale(judges), 5, 25, 25)
fviz_cluster(k, scale(judges))

######################################## EXPLORE/VIZ

## back to the judges
## the screeplot to review the variance captured
fviz_screeplot(p)
fviz_screeplot(p, addlabels = T, ylim = c(0, 100))

## another approach is eigenvalues > 0
get_eigenvalue(p)

## how do the variables contribute 
fviz_pca_contrib(p, choice = "var")
fviz_pca_contrib(p, choice = "var", axes = 2)

## variable contribution plot on two dimensions
## https://blog.bioturing.com/2018/06/18/how-to-read-pca-biplots-and-scree-plots/
fviz_pca_var(p, col.var = "contrib",
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             repl = T)

fviz_pca_ind(p, repl = T)

## TODO: review rotations, negative values 

## we can combine it all together
## https://blog.bioturing.com/2018/06/18/how-to-read-pca-biplots-and-scree-plots/
fviz_pca_biplot(p, col.var = "red",
                col.ind = "grey",
                repl = T)

## EXERCISE:  diamonds pca:
##            from the previous PCA we applied to diamonds
##            fit the scree plot, get a first pass on # PCs
##            what variables contribute the most to
##            PC1 and PC2
fviz_screeplot(diamonds_PCA, addlabels = T, ylim = c(0, 100))
get_eigenvalue(diamonds_PCA)

fviz_pca_contrib(diamonds_PCA, choice = "var")
fviz_pca_contrib(diamonds_PCA, choice = "var", axes = 2)

######################################## extract as new features

## apply the features -- could use new data, or the original as I do below
j_pcs <- predict(p, newdata = judges)
class(j_pcs)
j_pcs <- as.data.frame(j_pcs)
head(j_pcs)

## the main point for ML - how many to keep?
fviz_screeplot(p)
j_pca <- j_pcs[, 1:2]


## EXERCISE:  for the diamonds PCA, 
##            get the PC values for each record/PC
##            extract the PCs you think should be used
fviz_screeplot(diamonds_PCA)
get_eigenvalue(diamonds_PCA)
dia_pcs <- predict(diamonds_PCA, newdata = diamonds)
dia_pcs <- as.data.frame(dia_pcs)
dia_pc <- dia_pcs[, 1:3]
head(dia_pc)
nrow(dia_pc)

######################################## downstream ML

## ok, I have been talking about how this all comes together
## first lets put the diamonds pcs back
dia_model <- cbind(dia_pc, price = diamonds_int$price)

## add back on price
mod <- lm(price ~ ., data = dia_model)
summary(mod)


## EXERCISE:   fit a linear regression using price and the PCs



