
#Install class package
#install.packages('class')
# Load class package
library(e1071)
library(caTools)
library(class)
library(caret)

#Cluster 1 KNN
df_new  <- cbind(X1, y1)

set.seed(123)
train_index <- createDataPartition(y = df_new$shares_class, p = 0.7, list = FALSE)
train <- df_new[train_index, ]
test <- df_new[-train_index, ]

classifier_knn1 <- knn(train = train[,1:57],
                       test = test[,1:57],
                       cl = train$shares_class,
                       k = 1)
#classifier_knn1

# Confusion Matrix
confusionMatrix(table(classifier_knn1, test$shares_class))



#Cluster 2 KNN
df_new  <- cbind(X2, y2)

set.seed(123)
train_index <- createDataPartition(y = df_new$shares_class, p = 0.7, list = FALSE)
train <- df_new[train_index, ]
test <- df_new[-train_index, ]

classifier_knn2 <- knn(train = train[,1:57],
                       test = test[,1:57],
                       cl = train$shares_class,
                       k = 1)
#classifier_knn1

# Confusion Matrix
confusionMatrix(table(classifier_knn2, test$shares_class))



#Cluster 3 KNN
df_new  <- cbind(X3, y3)

set.seed(123)
train_index <- createDataPartition(y = df_new$shares_class, p = 0.7, list = FALSE)
train <- df_new[train_index, ]
test <- df_new[-train_index, ]

classifier_knn3 <- knn(train = train[,1:57],
                       test = test[,1:57],
                       cl = train$shares_class,
                       k = 1)
#classifier_knn3

# Confusion Matrix
confusionMatrix(table(classifier_knn3, test$shares_class))



#Cluster 4 KNN
df_new  <- cbind(X4, y4)

set.seed(123)
train_index <- createDataPartition(y = df_new$shares_class, p = 0.7, list = FALSE)
train <- df_new[train_index, ]
test <- df_new[-train_index, ]

classifier_knn4 <- knn(train = train[,1:57],
                       test = test[,1:57],
                       cl = train$shares_class,
                       k = 1)
#classifier_knn4

# Confusion Matrix
confusionMatrix(table(classifier_knn4, test$shares_class))



#Cluster 5 KNN
df_new  <- cbind(X5, y5)

set.seed(123)
train_index <- createDataPartition(y = df_new$shares_class, p = 0.7, list = FALSE)
train <- df_new[train_index, ]
test <- df_new[-train_index, ]

classifier_knn5 <- knn(train = train[,1:57],
                       test = test[,1:57],
                       cl = train$shares_class,
                       k = 1)
#classifier_knn5

# Confusion Matrix
confusionMatrix(table(classifier_knn5, test$shares_class))



#KNN for whole data
df_knn  <- df

set.seed(123)
train_index <- createDataPartition(y = df_knn$shares_class, p = 0.7, list = FALSE)
train <- df_knn[train_index, ]
test <- df_knn[-train_index, ]

classifier_knn <- knn(train = train[,1:57],
                      test = test[,1:57],
                      cl = train$shares_class,
                      k = 1)
#classifier_knn

# Confusion Matrix
confusionMatrix(table(classifier_knn, test$shares_class))

