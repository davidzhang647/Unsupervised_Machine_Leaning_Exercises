options(stringsAsFactors=FALSE)
options(digits = 3)

## load our libraries
suppressPackageStartupMessages(library(tidyverse))
library(arules)
library(arulesViz)

## we are going to use a built-in dataset from arules
## but transformed to get the mechanics of building a dataset of transactions
## from the shape of a real world dataset
g = read_csv("dataset/groceries.csv")
head(g)


## QUICK EXERCISE:
## how many unique transactions and items?
length(unique(g$tid))  # 9835 unique transactions
length(unique(g$item)) # 169 unique items

## get the data into transaction format
## ?read.transactions
## Bring in from a file
tr <- read.transactions("dataset/groceries.csv",
                        format="single",
                        header = TRUE,
                        sep=",",
                        cols = c("tid", "item"),
                        rm.duplicates = TRUE) 


## what do we have
class(tr)
summary(tr)



## there is another way to bring the data in if we have it 
## I think going from files is easier, but it may mean that
## we have to preprocess in R, save out the file, and then read back in
## but below is a process that I have used many years ago
## must be a dataframe, not a tibble!
g2 <- as.data.frame(g)
g3 <- split(g2[, "item"], g2[, "tid"])
tr_alt <- as(g3, "transactions")
class(tr_alt)

## I am going to use the original
## you can also have the items in 1 row per dataframe
## but the items are in the column, with a separator identifying the items
## this format is basket


## There are all sorts of operators that we can apply now that our 
## dataset is in the transactions class

## inspect 5 rules
inspect(head(tr, 5))

## we can even sample the data
sample(tr, 1)

## say we wanted to plot the distribution of items per transaction
summary(tr)
hist(size(tr))


## item frequency plotting
itemf <- itemFrequency(tr)
head(itemf)
head(itemFrequency(tr, type = "absolute"))
itemFrequencyPlot(tr)
itemFrequencyPlot(tr, topN = 30)
itemFrequencyPlot(tr, topN = 30, horiz = TRUE)


## EXERCISE:  Take 1 minute, what does this mean for us as we think about
##            the settings we discussed for the algorithm and how it works?



## lets fit our first assoc rules model!
## leaving confidence low to demonstrate rule evaluation
rules <- apriori(tr,
                 parameter = list(supp = 0.003,
                                  conf = 0.2,
                                  minlen = 2,
                                  target = "rules"))
class(rules)
summary(rules)

## Exercise: think about the following questions
## 1:  how many rules were created
## 2:  what is the minimum number of times a rule was seen in our database?
## 3:  how many transactions do we have again?
## 4:  what is the range of lift values
## 5:  what is the average rule size

# 1. 2245 rules
# 2. 30
# 3. 9835 
# 4. 0.8076 - 11.4214
# 5. 3.002


## lets print out the first 5 rules
inspect(head(rules, 5))


## EXERCISE:
## calculate the support for the first rule by hand, does it match?
46/9835

## we can sort the rules
inspect(head(sort(rules, decreasing = TRUE, by = "count")))

## we can filter the rules
inspect(head(subset(rules, rhs %in% 'whole milk')))
inspect(head(sort(subset(rules, rhs %in% 'whole milk'),
                  decreasing = TRUE, by = "count")))

## histogram of rule sizes
hist(size(rules))


## Exercise 
## 1.  sort the rules ascending by lift - print out the first 5
## 2.  find the top 10 rules (sorted descending by count) where baby food
##     is in the RHS
# 1.
inspect(head(sort(rules, decreasing = FALSE, by = "lift"), 5))
# 2.
inspect(head(sort(subset(rules, rhs %in% 'baby food'),
                  decreasing = TRUE, by = "count")), 10)


##  We can add other interest measures
##  we do this by calculating the measure, and then cbinding it
## to the QUALITY of our rules
## ?interestMeasure
## chisquare - test of independence between LHS and RHS, p < .05 is depdenence
rule_chisq <- interestMeasure(rules,
                              measure = "chisquare",
                              transactions = tr,
                              significance = T)
quality(rules) <- cbind(quality(rules), rule_chisq)
inspect(head(rules))

## Rule pruning
## mentioned that rules be symmetric, but based on the algorithm
## rules can also contain items that may not add value for interest measures
##  --- a more general rule with the same or higher confidence exists
rr <- rules[is.redundant(rules)]
length(rr)
length(rules)
rules_purned <- rules[!is.redundant(rules)]
length(rules_purned)

## Rule pruning 2
## Above and beyond subsetting rules based on the business problem
## (e.g. subsetting rules based on frequency or lift/confidence)
## you can also run the algorithm to be driven by one side or the other
rules2 <- apriori(tr,
                  parameter = list(supp = 0.003,
                                   conf = 0.5,
                                   minlen = 2,
                                   target = "rules"),
                  appearance = list(default ="lhs",
                                    rhs = "yogurt"))


## Visualize the rules
## we have done some basic plots of the items, but lets explore what else
## is possible

## Lets keep the rules where the count is > 300
top_rules <- rules[quality(rules)$count > 300]



## interactive plot with ploytly for confidence, support and lift
plot(top_rules)
plot(top_rules, engine = "plotly")

## last but not least, exporting rules
tr_af <- as(top_rules, "data.frame")
View(tr_af)


## can use the tidyverse to parse the rules into columns 
## tidyr, separate, clean strings, not create, but 


