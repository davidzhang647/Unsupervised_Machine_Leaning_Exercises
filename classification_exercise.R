## set the options
options(stringsAsFactors=FALSE)
options(digits=3)

## load packages
library(tidyverse)
library(factoextra)
library(skimr)
library(corrplot)
library(caret)

## load datasets
train <- read_csv("dataset/college-train.csv")
test <- read_csv("dataset/college-test.csv")

glimpse(train)
skim(train)

train$Private <- ifelse(train$Private=="Yes", 1, 0)
train$Private <- as.factor(train$Private)
glimpse(train)

##
train_s <- train[, -c(1,2)]

k <- kmeans(scale(train_s), 2, 25, 25)
fviz_cluster(k, scale(train_s))


##

## lets visualize the matrix
dc <- cor(train_s)

corrplot(dc,
         type = "upper",
         method = "color",
         diag = F,
         addCoef.col = "white")

college_PCA <- prcomp(train_s, center = T, scale. = T)


fviz_screeplot(college_PCA, addlabels = T)

get_eigenvalue(college_PCA)

## how do the variables contribute 
fviz_pca_contrib(college_PCA, choice = "var")
fviz_pca_contrib(college_PCA, choice = "var", axes = 2)

##
j_pcs <- predict(college_PCA, newdata = train_s)
j_pcs <- as.data.frame(j_pcs)
head(j_pcs)

j_pc2 <- j_pcs[, 1:2]
j_pc6 <- j_pcs[, 1:6]

dia_model2 <- cbind(j_pc2, Private = train$Private)
dia_model6 <- cbind(j_pc6, Private = train$Private)

## add back on Private
mod2 <- glm(Private ~ ., data = dia_model2, family = binomial(link = "logit"))
mod6 <- glm(Private ~ ., data = dia_model6, family = binomial(link = "logit"))

summary(mod2)
summary(mod6)

##
test_s <- test[, -1]

t_pcs <- predict(college_PCA, newdata = test_s)
t_pcs <- as.data.frame(t_pcs)
head(t_pcs)

t_pc2 <- t_pcs[, 1:2]
t_pc6 <- t_pcs[, 1:6]


test2 <- predict(mod2, t_pc2, type = "response")
test2
test6 <- predict(mod6, t_pc6, type = "response")
test6

th <- 0.5

Private2 <- ifelse(test2 > 0.5, "Yes", "No")
table(Private2)
Private6 <- ifelse(test6 > 0.5, "Yes", "No")
table(Private6)

sub2 <- as.data.frame(cbind(test$College, Private = Private2))
sub6 <- as.data.frame(cbind(test$College, Private = Private6))

write_csv(sub2, path = "dataset/sub2.csv")
write_csv(sub6, path = "dataset/sub6.csv")
