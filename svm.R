#install.packages('e1071')
library(e1071)
library(caret)

#Cluster 1 SVM
df_new  <- cbind(X1, y1)

set.seed(123)
train_index <- createDataPartition(y = df_new$shares_class, p = 0.7, list = FALSE)
train <- df_new[train_index, ]
test <- df_new[-train_index, ]

svmClassifier1 = svm(formula = shares_class ~ .,
                     data = train,
                     type = 'C-classification',
                     kernel = 'radial',
                     cost=1)

preds1 <- predict(svmClassifier1, test[,1:57])

confusionMatrix(table(preds1, test[,58]))


#Cluster 2 SVM
df_new  <- cbind(X2, y2)

set.seed(123)
train_index <- createDataPartition(y = df_new$shares_class, p = 0.7, list = FALSE)
train <- df_new[train_index, ]
test <- df_new[-train_index, ]

svmClassifier2 = svm(formula = shares_class ~ .,
                     data = train,
                     type = 'C-classification',
                     kernel = 'radial',
                     cost=1)

preds2 <- predict(svmClassifier2, test[,1:57])

confusionMatrix(table(preds2, test[,58]))


#Cluster 3 SVM
df_new  <- cbind(X3, y3)

set.seed(123)
train_index <- createDataPartition(y = df_new$shares_class, p = 0.7, list = FALSE)
train <- df_new[train_index, ]
test <- df_new[-train_index, ]

svmClassifier3 = svm(formula = shares_class ~ .,
                     data = train,
                     type = 'C-classification',
                     kernel = 'radial',
                     cost=1)

preds3 <- predict(svmClassifier3, test[,1:57])

confusionMatrix(table(preds3, test[,58]))



#Cluster 4 SVM
df_new  <- cbind(X4, y4)

set.seed(123)
train_index <- createDataPartition(y = df_new$shares_class, p = 0.7, list = FALSE)
train <- df_new[train_index, ]
test <- df_new[-train_index, ]

svmClassifier4 = svm(formula = shares_class ~ .,
                     data = train,
                     type = 'C-classification',
                     kernel = 'radial',
                     cost=1)

preds4 <- predict(svmClassifier4, test[,1:57])

confusionMatrix(table(preds4, test[,58]))



#Cluster 5 SVM
df_new  <- cbind(X5, y5)

set.seed(123)
train_index <- createDataPartition(y = df_new$shares_class, p = 0.7, list = FALSE)
train <- df_new[train_index, ]
test <- df_new[-train_index, ]

svmClassifier5 = svm(formula = shares_class ~ .,
                     data = train,
                     type = 'C-classification',
                     kernel = 'radial',
                     cost=1)

preds5 <- predict(svmClassifier5, test[,1:57])

confusionMatrix(table(preds5, test[,58]))
