# Import necessary packages
library(dplyr)
library(ggplot2)
library(caret)

# Read Data
df <- read.csv("/Users/meghanajoshi/Desktop/NEBD/OnlineNewsPopularity.csv", header = T);
df

# Data Pre-Processing

df <- df[, -c(1,2)]
df <- df[df$n_tokens_content != 0,]
df <- df[df$n_non_stop_words != 1042,]
df <- subset(df,select = -c(n_non_stop_words))
summary(df)
dim(df) 

# Feature Engineering
df$shares_class <- ifelse(df$shares > 1400, 'Popular', 'Unpopular')
df
dim(df) 

# Data Visualization

# Visualize the feature of different day of week 
ggplot(df, aes(x = weekday_is_monday)) + geom_bar() + ggtitle("Monday")
ggplot(df, aes(x = weekday_is_tuesday)) + geom_bar() + ggtitle("Tuesday")
ggplot(df, aes(x = weekday_is_wednesday)) + geom_bar() + ggtitle("Wednesday")
ggplot(df, aes(x = weekday_is_thursday)) + geom_bar() + ggtitle("Thursday")
ggplot(df, aes(x = weekday_is_friday)) + geom_bar() + ggtitle("Friday")
ggplot(df, aes(x = weekday_is_saturday)) + geom_bar() + ggtitle("Saturday")
ggplot(df, aes(x = weekday_is_sunday)) + geom_bar() + ggtitle("Sunday")

# Normalization 

for (i in 1:ncol(df)) {
  if (i != ncol(df)) {
    if (min(df[,i]) > 0) {
      df[,i] <- log(df[,i])
      print(names(df)[i])
    }
  }
}

# Scaling
# Scale all columns except shares and shares_class
df[, -c(60,61)] <- scale(df[, -c(60,61)])
df

# Model Building

X1 = subset(df, select = -c(shares, shares_class))
X1
y1 = subset(cluster1, select = c(shares_class))
y1

y1$shares_class <- ifelse(y1$shares_class == "Popular", 1, 0)
y1

df_new  <- cbind(X1, y1)
df_new

# Splitting the data into training and testing set
set.seed(123)
trainIndex <- createDataPartition(df_new$shares_class, p = 0.7, list = FALSE)
train <- df_new[trainIndex,]
test <- df_new[-trainIndex,]


# Classification Tree
library(rpart)
library(rpart.plot)

tree <- rpart(shares_class ~ ., data = train, method = "class")
tree

pred <- predict(tree, test, type = "class")
pred

mean(pred == as.factor(test$shares_class))

# Confusion Matrix
cm <- table(test$shares_class, pred)
cm

# Precision
precision <- cm[1,1] / (cm[1,1] + cm[2,1])
precision

# Recall
recall <- cm[1,1] / (cm[1,1] + cm[1,2])
recall

# F1 Score
f1 <- 2 * (precision * recall) / (precision + recall)
f1

plotcp(tree)

# Random Forests with Cross Validation

## (1) Define the packages that will be needed
packages <- c('dplyr', 'ggplot2', 'caret')

## (2) Install them if not yet installed
installed_packages <- packages %in% rownames(installed.packages())
if (any(installed_packages == FALSE)) {
  install.packages(packages[!installed_packages])
}

## (3) Load the packages into R session
invisible(lapply(packages, library, character.only = TRUE))

## Set seed for reproducibility
set.seed(123)

## Define repeated cross validation with 5 folds and three repeats
repeat_cv <- trainControl(method='repeatedcv', number=10, repeats=3)

## Split the data so that we use 70% of it for training
train_index <- createDataPartition(y = df_new$shares_class, p=0.7, list=FALSE)

## Subset the data
training_set <- df_new[train_index, ]
testing_set <- df_new[-train_index, ]

model <- train(shares_class ~., data = training_set,
               method = "",
               trControl = train_control)

# SVC

library(e1071)
svm_model <- svm(shares_class ~ ., data = train, kernel = "linear", type = 'C-classification')
summary(svm_model)
# Predicting the test set results
y_pred <- predict(svm_model, test)
y_pred

# Making the confusion matrix
cm <- table(test$shares_class, y_pred)

# Accuracy
accuracy <- (cm[1,1] + cm[2,2]) / sum(cm)
accuracy
precision <- cm[1,1] / (cm[1,1] + cm[2,1])
precision


### Naive Bayes

# Fitting Naive Bayes to the Training set
library(e1071)
classifier <- naiveBayes(shares_class ~ ., data = train)

# Predicting the Test set results
y_pred <- predict(classifier, test)
# Making the Confusion Matrix
cm <- table(test$shares_class, y_pred)

# Accuracy
accuracy <- (cm[1,1] + cm[2,2]) / sum(cm)
accuracy

#Precision
precision <- cm[1,1] / (cm[1,1] + cm[2,1])
precision

# Recall
recall <- cm[1,1] / (cm[1,1] + cm[1,2])
recall

# F1 Score
f1 <- 2 * (precision * recall) / (precision + recall)
f1

### KNN

# Fitting K-NN to the Training set
library(class)
classifier <- knn(train[,-58], test[,-58], train[,58], k = 10)
classifier

cm <- table(test[,58], classifier)
cm

accuracy <- (cm[1,1] + cm[2,2]) / sum(cm)
accuracy


### Adaboost
library(e1071)
library(adabag)

adaboost <- boosting(shares_class~., data=train, boos=TRUE, mfinal=10)
adaboost

# Predict the test data
pred <- predict(adaboost, test)
pred$class

# Confusion Matrix
cm <- table(test$shares_class, pred$class)

# Accuracy
accuracy <- (cm[1,1] + cm[2,2]) / sum(cm)
accuracy

#Precision
precision <- cm[1,1] / (cm[1,1] + cm[2,1])
precision

# Recall
recall <- cm[1,1] / (cm[1,1] + cm[1,2])
recall

# F1 Score
f1 <- 2 * (precision * recall) / (precision + recall)
f1



----- CLUSTERING ----
  
 # Read csv named OnlineNewsPopularity into dataframe
df <- read.csv('/Users/meghanajoshi/Desktop/NEBD/OnlineNewsPopularity.csv', header = T);
df
dim(df) # [1] 39644    61
df <- df[, -c(1,2)]
df
dim(df)

df <- df[df$n_tokens_content != 0,]
df

df <- df[df$n_non_stop_words != 1042,]
df

df <- subset(df,select = -c(n_non_stop_words))
df
original_data <- df


# describe the dataframe
summary(df)
# number of rows and columns in the dataframe
dim(df) # [1] 38462    58


# New columns called shares_class which is 'Popular' if the number of shares is greater than 1400, otherwise it is 'Unpopular' 
df$shares_class <- ifelse(df$shares > 1400, 'Popular', 'Unpopular')
df
dim(df) # [1] 38462    59


# Print Skewness and Kurtosis of shares
library(moments)
# Skewness
skewness(df$shares)
# Kurtosis
kurtosis(df$shares)

# Applying log transform if the value of shares is greater than 0
# df$shares <- ifelse(df$shares > 0, log(df$shares), df$shares)

for (i in 1:ncol(df)) {
  if (i != ncol(df)) {
    if (min(df[,i]) > 0) {
      df[,i] <- log(df[,i])
      print(names(df)[i])
    }
  }
}


dim(df) # [1] 38462    59

# Scaling
# Scale all columns except shares and shares_class
df[, -c(60,61)] <- scale(df[, -c(60,61)])
df
summary(df)
dim(df)

# import pca and kmeans
install.packages("pracma")
install.packages("cluster")
install.packages("kmeans")
library(pracma)
df

# Create a new df with all data except last two columns
df1 <- subset(df, select = -c(shares, shares_class))
df1
dim(df1)

# Perform t-sne on the data df1 with two components and 300 iterations
tsne <- Rtsne(df1, dims = 2, max_iter = 300)

#Print first 5 rows of the t-sne output
head(tsne$Y)

# Plot the first two dimensions of the t-sne output

plot(tsne$Y[,1], tsne$Y[,2], pch = 19, cex = 0.5, main = "t-SNE", xlab = "t-SNE 1", ylab = "t-SNE 2")

# KMeans
kmeans <- kmeans(tsne$Y, centers = 5, nstart = 20)
kmeans


# Create a scatter plot that visualizes the clusters 
# Plot the clusters
plot(tsne$Y, col = kmeans$cluster, pch = 19, cex = 0.5, main = "t-SNE", xlab = "t-SNE 1", ylab = "t-SNE 2")
# Plot the centroids
points(kmeans$centers, col = "red", pch = 16, cex = 2)
# Plot the cluster labels
text(kmeans$centers, labels = 1:5, col = "black", cex = 1.5)

# fussing the cluster data into the dataframe
df$cluster <- kmeans$cluster

# extrating individual cluster from the data

# cluster 1
cluster1 <- df[df$cluster == 1,]
dim(cluster1)

# cluster 2
cluster2 <- df[df$cluster == 2,]
dim(cluster2)

# cluster 3
cluster3 <- df[df$cluster == 3,]
dim(cluster3)

# cluster 4
cluster4 <- df[df$cluster == 4,]
dim(cluster4)

# cluster 5
cluster5 <- df[df$cluster == 5,]
dim(cluster5)


# Make a new dataframe X1 which has all columns from cluster1 except for shares, shares_class and cluster
X1 = subset(cluster1, select = -c(shares, shares_class, cluster))
X1
y1 = subset(cluster1, select = c(shares_class))
y1


X2 = subset(cluster2, select = -c(shares, shares_class, cluster))
X2
y2 = subset(cluster2, select = c(shares_class))
y2

X3 = subset(cluster3, select = -c(shares, shares_class, cluster))
X3
y3 = subset(cluster3, select = c(shares_class))
y3

X4 = subset(cluster4, select = -c(shares, shares_class, cluster))
X4
y4 = subset(cluster4, select = c(shares_class))
y4

X5 = subset(cluster5, select = -c(shares, shares_class, cluster))
X5
y5 = subset(cluster5, select = c(shares_class))


## 

y1$shares_class <- ifelse(y1$shares_class == "Popular", 1, 0)
y1

df_new  <- cbind(X1, y1)
df_new

# Split the data into train and test data with 0.7 train and 0.3 test
set.seed(123)
train_index <- createDataPartition(y = df_new$shares_class, p = 0.7, list = FALSE)
train <- df_new[train_index, ]
test <- df_new[-train_index, ]

library(e1071)
classifier <- naiveBayes(shares_class ~ ., data = train)

# Predicting the Test set results
y_pred <- predict(classifier, test)
# Making the Confusion Matrix
cm <- table(test$shares_class, y_pred)

# Accuracy
accuracy <- (cm[1,1] + cm[2,2]) / sum(cm)
accuracy

# Cluster 2

y2$shares_class <- ifelse(y2$shares_class == "Popular", 1, 0)
y2

df_new  <- cbind(X2, y2)
df_new

# Split the data into train and test data with 0.7 train and 0.3 test
set.seed(123)
train_index <- createDataPartition(y = df_new$shares_class, p = 0.7, list = FALSE)
train <- df_new[train_index, ]
test <- df_new[-train_index, ]

library(e1071)
classifier <- naiveBayes(shares_class ~ ., data = train)

# Predicting the Test set results
y_pred <- predict(classifier, test)
# Making the Confusion Matrix
cm <- table(test$shares_class, y_pred)

# Accuracy
accuracy <- (cm[1,1] + cm[2,2]) / sum(cm)
accuracy

# Cluster 3
y3$shares_class <- ifelse(y3$shares_class == "Popular", 1, 0)
y3

df_new  <- cbind(X3, y3)
df_new

# Split the data into train and test data with 0.7 train and 0.3 test
set.seed(123)
train_index <- createDataPartition(y = df_new$shares_class, p = 0.7, list = FALSE)
train <- df_new[train_index, ]
test <- df_new[-train_index, ]

library(e1071)
classifier <- naiveBayes(shares_class ~ ., data = train)

# Predicting the Test set results
y_pred <- predict(classifier, test)
# Making the Confusion Matrix
cm <- table(test$shares_class, y_pred)

# Accuracy
accuracy <- (cm[1,1] + cm[2,2]) / sum(cm)
accuracy

# Cluster 4

y4$shares_class <- ifelse(y4$shares_class == "Popular", 1, 0)
y4

df_new  <- cbind(X4, y4)
df_new

# Split the data into train and test data with 0.7 train and 0.3 test
set.seed(123)
train_index <- createDataPartition(y = df_new$shares_class, p = 0.7, list = FALSE)
train <- df_new[train_index, ]
test <- df_new[-train_index, ]

library(e1071)
classifier <- naiveBayes(shares_class ~ ., data = train)

# Predicting the Test set results
y_pred <- predict(classifier, test)
# Making the Confusion Matrix
cm <- table(test$shares_class, y_pred)

# Accuracy
accuracy <- (cm[1,1] + cm[2,2]) / sum(cm)
accuracy


# Cluster 5

y5$shares_class <- ifelse(y5$shares_class == "Popular", 1, 0)
y5

df_new  <- cbind(X5, y5)
df_new

# Split the data into train and test data with 0.7 train and 0.3 test
set.seed(123)
train_index <- createDataPartition(y = df_new$shares_class, p = 0.7, list = FALSE)
train <- df_new[train_index, ]
test <- df_new[-train_index, ]

library(e1071)
classifier <- naiveBayes(shares_class ~ ., data = train)

# Predicting the Test set results
y_pred <- predict(classifier, test)
# Making the Confusion Matrix
cm <- table(test$shares_class, y_pred)

# Accuracy
accuracy <- (cm[1,1] + cm[2,2]) / sum(cm)
accuracy


### Adaboost

## 1

y1$shares_class <- ifelse(y1$shares_class == "Popular", 1, 0)
y1

df_new  <- cbind(X1, y1)
df_new

# Split the data into train and test data with 0.7 train and 0.3 test
set.seed(123)
train_index <- createDataPartition(y = df_new$shares_class, p = 0.7, list = FALSE)
train <- df_new[train_index, ]
test <- df_new[-train_index, ]

library(e1071)
library(adabag)

adaboost <- boosting(shares_class~., data=train, boos=TRUE, mfinal=10)
adaboost

# Predict the test data
pred <- predict(adaboost, test)
pred$class

# Confusion Matrix
cm <- table(test$shares_class, pred$class)

# Accuracy
accuracy <- (cm[1,1] + cm[2,2]) / sum(cm)
accuracy


## Cluster 2

y2$shares_class <- ifelse(y2$shares_class == "Popular", 1, 0)
y2

df_new  <- cbind(X2, y2)
df_new

# Split the data into train and test data with 0.7 train and 0.3 test
set.seed(123)
train_index <- createDataPartition(y = df_new$shares_class, p = 0.7, list = FALSE)
train <- df_new[train_index, ]
test <- df_new[-train_index, ]

library(e1071)
library(adabag)

adaboost <- boosting(shares_class~., data=train, boos=TRUE, mfinal=10)
adaboost

# Predict the test data
pred <- predict(adaboost, test)
pred$class

# Confusion Matrix
cm <- table(test$shares_class, pred$class)

# Accuracy
accuracy <- (cm[1,1] + cm[2,2]) / sum(cm)
accuracy


## Cluster 3

y4$shares_class <- ifelse(y4$shares_class == "Popular", 1, 0)
y4

df_new  <- cbind(X4, y4)
df_new

# Split the data into train and test data with 0.7 train and 0.3 test
set.seed(123)
train_index <- createDataPartition(y = df_new$shares_class, p = 0.7, list = FALSE)
train <- df_new[train_index, ]
test <- df_new[-train_index, ]

library(e1071)
library(adabag)

adaboost <- boosting(shares_class~., data=train, boos=TRUE, mfinal=10)
adaboost

# Predict the test data
pred <- predict(adaboost, test)
pred$class

# Confusion Matrix
cm <- table(test$shares_class, pred$class)

# Accuracy
accuracy <- (cm[1,1] + cm[2,2]) / sum(cm)
accuracy


## Cluster 4

y4$shares_class <- ifelse(y4$shares_class == "Popular", 1, 0)
y4

df_new  <- cbind(X4, y4)
df_new

# Split the data into train and test data with 0.7 train and 0.3 test
set.seed(123)
train_index <- createDataPartition(y = df_new$shares_class, p = 0.7, list = FALSE)
train <- df_new[train_index, ]
test <- df_new[-train_index, ]

library(e1071)
library(adabag)

adaboost <- boosting(shares_class~., data=train, boos=TRUE, mfinal=10)
adaboost

# Predict the test data
pred <- predict(adaboost, test)
pred$class

# Confusion Matrix
cm <- table(test$shares_class, pred$class)

# Accuracy
accuracy <- (cm[1,1] + cm[2,2]) / sum(cm)
accuracy



## Cluster 5

y5$shares_class <- ifelse(y5$shares_class == "Popular", 1, 0)
y5

df_new  <- cbind(X5, y5)
df_new

# Split the data into train and test data with 0.7 train and 0.3 test
set.seed(123)
train_index <- createDataPartition(y = df_new$shares_class, p = 0.7, list = FALSE)
train <- df_new[train_index, ]
test <- df_new[-train_index, ]

library(e1071)
library(adabag)

adaboost <- boosting(shares_class~., data=train, boos=TRUE, mfinal=10)
adaboost

# Predict the test data
pred <- predict(adaboost, test)
pred$class

# Confusion Matrix
cm <- table(test$shares_class, pred$class)

# Accuracy
accuracy <- (cm[1,1] + cm[2,2]) / sum(cm)
accuracy