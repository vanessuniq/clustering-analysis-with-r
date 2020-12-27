# UMGC SuMMER 2020
# DATA 630
# ASSIGNMENT 5: Clustering Analysis of Vowel Dataset
# Written by Vanessa Fotso

# 1. Load  packages
library("cluster")
library(gridExtra)
library(factoextra)
library(animation)

# 2. Read the CSV file. 
vowel <- read.csv("vowel.csv", head= TRUE, sep = ",")

# 3. Data exploration

# data preview
head(vowel)
# data summary
summary(vowel)

# class distribution
table(vowel$Class)

# check for missing values
vowel[!complete.cases(vowel),]
nrow(vowel[!complete.cases(vowel),])
#apply(vowel, 1, function(vowel) sum(is.na(vowel)))
apply(vowel, 2, function(vowel) sum(is.na(vowel)))

# 4. Data Preprocessing
# create new copy of the vowel dataset
newvowel <- vowel

# remove the categorical variables from the copy
newvowel$Class <- NULL
newvowel$TrainTest <- NULL
newvowel$Sex <- NULL
newvowel$SpeakerNumber <- NULL

# inspect the new dataset
head(newvowel)
plot(newvowel, col= rep(1:3, each=10), pch= 19)

# scare variables to normalize the data
newvowel[1:10]<-scale(newvowel[1:10])
summary(newvowel)

# 5. clustering
# seed the data to ensure that the results are rproduceble
set.seed(1234)

# run k-means algorithm with animation for k = 4
kmeans.ani(newvowel, 4)
kc4 <- kmeans(newvowel, 4)
print(kc4)

# checking the results component
kc4$centers
kc4$totss
kc4$iter
kc4$betweenss
kc4$tot.withinss

# Cross-tabulation - cluster to sex evaluation
table(vowel$Sex, kc4$cluster)




# running k means with k = 11
kc11<-kmeans(newvowel, 11)
print(kc11)

# checking the results component
kc11$centers
kc11$totss
kc11$iter
kc11$betweenss
kc11$tot.withinss

# Cross-tabulation - cluster to class evaluation
table(vowel$Class, kc11$cluster)

# clusters plot for k = 4 and k = 11 using a PCA algorithm to reduce the dimensions
p4 <- fviz_cluster(kc4, geom = "point", data = newvowel) + ggtitle("k = 4")
p11 <- fviz_cluster(kc11, geom = "point", data = newvowel) + ggtitle("k = 11")
grid.arrange(p4, p11, nrow = 2)

# Finding the best k value
fviz_nbclust(newvowel, kmeans, method = "wss")

# plot cluster for optimal k =5
kc5 <- kmeans(newvowel, 5)
fviz_cluster(kc5, data = newvowel)
table(vowel$Sex, kc5$cluster)
table(vowel$Class, kc5$cluster)

# End Script