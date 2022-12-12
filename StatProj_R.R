# Import Required Libraries
install.packages("languageserver")
install.packages("survival")
install.packages("lattice")
install.packages("ggplot2")
install.packages("Hmisc")
install.packages("DataExplorer")
install.packages("caret")
install.packages("randomForest")
install.packages("quantable")
install.packages("anamoly")
install.packages("moments")
install.packages("caret")
install.packages("M3C")
install.packages("Rtsne")
install.packages("devtools")
install.packages("infotheo")
devtools::install_github("drsimonj/twidlr")
library(twidlr)
library(Rtsne)
library(caret)


# Read csv named OnlineNewsPopularity into dataframe
df <- read.csv('/Users/meghanajoshi/Desktop/NEBD/OnlineNewsPopularity.csv', header = T);
df
dim(df) # [1] 39644    61


# Drop the two non - preditive (url and timedelta) attributes. They do not contribute anything to the model.
df <- df[, -c(1,2)]
df
dim(df)


# Remove noise from n_tokens_content. those equals to 0
df <- df[df$n_tokens_content != 0,]
df


# Comment - Visualizing the n_non_stop_words data field shows that the present of a record with 1042 value, 
# futher observation of that data shows that it belongs to entertainment which is not actually. It belongs to world news or others.
# this particluar also contains 0 on a lot of attributes. This record is classifed as a noise and will be remove
df <- df[df$n_non_stop_words != 1042,]
df


# Remove column 'n_non_stop_words' from df
df <- subset(df,select = -c(n_non_stop_words))
df


# make a copy of the data and call it original_data
original_data <- df

# describe the dataframe
summary(df)
# number of rows and columns in the dataframe
dim(df) # [1] 38462    58


# create a new columns called shares_class which is 'Popular' if shares>1400 and 'Unpopular' if shares<=1400
df$shares_class <- ifelse(df$shares > 1400, 'Popular', 'Unpopular')
df
dim(df) # [1] 38462    59


# The given data doesn't have a normal distribution. A log transformation will be carried out to transform the full data to 
# have a normal distribution as close as possible
# Evaluating the effects of normal distribution on the shares
# Print Skewness and Kurtosis of shares
library(moments)
# Skewness
skewness(df$shares)
# Kurtosis
kurtosis(df$shares)

# Applying log transform if the value of shares is greater than 0
# df$shares <- ifelse(df$shares > 0, log(df$shares), df$shares)


# This code applies a log transformation to the columns in the data DataFrame, excluding the last column. The log transformation is only applied to columns that have no 0 values, as taking the log of 0 results in an error. This transformation can be useful for normalizing the data and potentially improving the performance of machine learning models.
# Apply log transformation to all columns except the last one if the column has no 0 values, print column name that is transformed

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

# This code performs principal component analysis (PCA) on the data DataFrame, keeping only the two principal components with the highest variance.  Fit pca on df excluding last two columns
# pca <- princomp(df1, cor = TRUE, scores = TRUE)

# Perform t-sne on the data df1 with two components and 300 iterations
tsne <- Rtsne(df1, dims = 2, max_iter = 300)

#Print first 5 rows of the t-sne output
head(tsne$Y)

# Plot the first two dimensions of the t-sne output

plot(tsne$Y[,1], tsne$Y[,2], pch = 19, cex = 0.5, main = "t-SNE", xlab = "t-SNE 1", ylab = "t-SNE 2")

# The KMeans algorithm is a clustering method that divides a dataset into a specified number of clusters. In this case, the code initializes the KMeans algorithm with the 'k-means++' parameter for the initial cluster centroids, and specifies the number of clusters to be 5. The algorithm is then trained on the reduced_tsne data, and predictions are made on the same dataset. These predictions can be used to determine which cluster each data point belongs to.
# Perform kmeans on the t-sne output with 5 clusters


kmeans <- kmeans(tsne$Y, centers = 5, nstart = 20)
kmeans


# Predict the cluster for each data point using twidlr
# predictions <- predict(as.data.frame(kmeans), as.data.frame(tsne$Y))

# Create a scatter plot that visualizes the clusters that were predicted by the KMeans model. The cluster centroids are plotted as red "x" markers, and the cluster labels are plotted as black text on a white background. The data points are plotted as a scatter plot, with the color of each point indicating which cluster it belongs to. 

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
y5

--- Fitting random forest on the date ---

--- CLuster 1 ---
# Fit a classification tree to the data with X1 and y1
# install.packages("rpart")
y1$shares_class <- ifelse(y1$shares_class == "Popular", 1, 0)
y1

df_new  <- cbind(X1, y1)
df_new

# Split the data into train and test data with 0.7 train and 0.3 test
set.seed(123)
train_index <- createDataPartition(y = df_new$shares_class, p = 0.7, list = FALSE)
train <- df_new[train_index, ]
test <- df_new[-train_index, ]

# Fit a classification tree to the data with X1 and y1
# install.packages("rpart")
library(rpart)
tree <- rpart(shares_class ~ ., data = train, method = "class")
tree

# Plot the tree
# install.packages("rpart.plot")
library(rpart.plot)
rpart.plot(tree)

# Predict the test data
pred <- predict(tree, test, type = "class")
pred

# Confusion matrix
table(pred, as.factor(test$shares_class))

# Accuracy
mean(pred == as.factor(test$shares_class))

--- Cluster 2 ---
# Fit a classification tree to the data with X1 and y1
# install.packages("rpart")
y2$shares_class <- ifelse(y2$shares_class == "Popular", 1, 0)
y2

df_new  <- cbind(X2, y2)
df_new

# Split the data into train and test data with 0.7 train and 0.3 test
set.seed(123)
train_index <- createDataPartition(y = df_new$shares_class, p = 0.7, list = FALSE)
train <- df_new[train_index, ]
test <- df_new[-train_index, ]

# Fit a classification tree to the data with X1 and y1
# install.packages("rpart")
library(rpart)
tree <- rpart(shares_class ~ ., data = train, method = "class")
tree

# Plot the tree
# install.packages("rpart.plot")
library(rpart.plot)
rpart.plot(tree)

# Predict the test data
pred <- predict(tree, test, type = "class")
pred

# Confusion matrix
table(pred, as.factor(test$shares_class))

# Accuracy
mean(pred == as.factor(test$shares_class))


--- Cluster 3 ---

# Fit a classification tree to the data with X1 and y1
# install.packages("rpart")
y3$shares_class <- ifelse(y3$shares_class == "Popular", 1, 0)
y3

df_new  <- cbind(X3, y3)
df_new

# Split the data into train and test data with 0.7 train and 0.3 test
set.seed(123)
train_index <- createDataPartition(y = df_new$shares_class, p = 0.7, list = FALSE)
train <- df_new[train_index, ]
test <- df_new[-train_index, ]

# Fit a classification tree to the data with X1 and y1
# install.packages("rpart")
library(rpart)
tree <- rpart(shares_class ~ ., data = train, method = "class")
tree

# Plot the tree
# install.packages("rpart.plot")
library(rpart.plot)
rpart.plot(tree)

# Predict the test data
pred <- predict(tree, test, type = "class")
pred

# Confusion matrix
table(pred, as.factor(test$shares_class))

# Accuracy
mean(pred == as.factor(test$shares_class))

--- Cluster 4 ---

# Fit a classification tree to the data with X1 and y1
# install.packages("rpart")
y1$shares_class <- ifelse(y1$shares_class == "Popular", 1, 0)
y1

df_new  <- cbind(X1, y1)
df_new

# Split the data into train and test data with 0.7 train and 0.3 test
set.seed(123)
train_index <- createDataPartition(y = df_new$shares_class, p = 0.7, list = FALSE)
train <- df_new[train_index, ]
test <- df_new[-train_index, ]

# Fit a classification tree to the data with X1 and y1
# install.packages("rpart")
library(rpart)
tree <- rpart(shares_class ~ ., data = train, method = "class")
tree

# Plot the tree
# install.packages("rpart.plot")
library(rpart.plot)
rpart.plot(tree)

# Predict the test data
pred <- predict(tree, test, type = "class")
pred

# Confusion matrix
table(pred, as.factor(test$shares_class))

# Accuracy
mean(pred == as.factor(test$shares_class))z

--- Cluster 5 ---


--- PCA ---


# Run PCA on X1 with 5 components, 10 components, 20 components and 30 components

pca1 <- prcomp(X1, scale = TRUE, center = TRUE)
pca1
dim(pca1$x)
dim(y1)

# select first 5 components

pca1 <- prcomp(X1, scale = TRUE, center = TRUE, rank. = 5)
pca1
dim(pca1$x)

# pca for 10 components
pca2 <- prcomp(X1, scale = TRUE, center = TRUE)
pca2


# Label encode y1 as 1 for popular and 0 for unpopular

y1$shares_class <- ifelse(y1$shares_class == "Popular", 1, 0)
y1


# Join pca1$x and y1
pca1_data <- cbind(pca1$x, y1)
#Split pca1_data into train and test data with 0.2 test size
set.seed(123)
train_index <- createDataPartition(pca1_data$shares_class, p = 0.8, list = FALSE)
train <- pca1_data[train_index,]
test <- pca1_data[-train_index,]
dim(train)
dim(test)

# Fit a classification tree on train data
library(rpart)
set.seed(123)
tree1 <- rpart(shares_class ~ ., data = train, method = "class")
tree1

# Predict on test data
pred1 <- predict(tree1, test, type = "class")
pred1

# Accuracy
mean(pred1 == test$shares_class)


# Confusion matrix
confusionMatrix(pred1, as.factor(test$shares_class))

# Plot the tree
plot(tree1)
text(tree1, pretty = 0)


--- PCA 2 ---

pca2 <- prcomp(X2, scale = TRUE, center = TRUE)
pca2
dim(pca2$x)
dim(y2)

# Label encode y1 as 1 for popular and 0 for unpopular

y2$shares_class <- ifelse(y2$shares_class == "Popular", 1, 0)
y2

# Join pca1$x and y1
pca2_data <- cbind(pca2$x, y2)
#Split pca1_data into train and test data with 0.2 test size
set.seed(123)
train_index <- createDataPartition(pca2_data$shares_class, p = 0.8, list = FALSE)
train <- pca2_data[train_index,]
test <- pca2_data[-train_index,]
dim(train)
dim(test)

# Fit a classification tree on train data
library(rpart)
set.seed(123)
tree2 <- rpart(shares_class ~ ., data = train, method = "class")
tree2

# Predict on test data
pred2 <- predict(tree2, test, type = "class")
pred2

# Accuracy
mean(pred2 == test$shares_class)


# Confusion matrix
confusionMatrix(pred2, as.factor(test$shares_class))

# Plot the tree
plot(tree2)
text(tree2, pretty = 0)

--- PCA 3 ---

pca3 <- prcomp(X3, scale = TRUE, center = TRUE)
pca3
dim(pca3$x)
dim(y3)

# Label encode y1 as 1 for popular and 0 for unpopular

y3$shares_class <- ifelse(y3$shares_class == "Popular", 1, 0)
y3

# Join pca1$x and y1
pca3_data <- cbind(pca3$x, y3)
#Split pca1_data into train and test data with 0.2 test size
set.seed(123)
train_index <- createDataPartition(pca3_data$shares_class, p = 0.8, list = FALSE)
train <- pca3_data[train_index,]
test <- pca3_data[-train_index,]
dim(train)
dim(test)

# Fit a classification tree on train data
library(rpart)
set.seed(123)
tree3 <- rpart(shares_class ~ ., data = train, method = "class")
tree3

# Predict on test data
pred3 <- predict(tree3, test, type = "class")
pred3

# Accuracy
mean(pred3 == test$shares_class)


# Confusion matrix
confusionMatrix(pred3, as.factor(test$shares_class))

# Plot the tree
plot(tree3)
text(tree3, pretty = 0)

--- PCA 4 ---


pca4 <- prcomp(X4, scale = TRUE, center = TRUE)
pca4
dim(pca4$x)
dim(y4)

# Label encode y1 as 1 for popular and 0 for unpopular

y4$shares_class <- ifelse(y4$shares_class == "Popular", 1, 0)
y4

# Join pca1$x and y1
pca4_data <- cbind(pca4$x, y4)
#Split pca1_data into train and test data with 0.2 test size
set.seed(123)
train_index <- createDataPartition(pca4_data$shares_class, p = 0.8, list = FALSE)
train <- pca4_data[train_index,]
test <- pca4_data[-train_index,]
dim(train)
dim(test)

# Fit a classification tree on train data
library(rpart)
set.seed(123)
tree4 <- rpart(shares_class ~ ., data = train, method = "class")
tree4

# Predict on test data
pred4 <- predict(tree4, test, type = "class")
pred4

# Accuracy
mean(pred4 == test$shares_class)


# Confusion matrix
confusionMatrix(pred4, as.factor(test$shares_class))

# Plot the tree
plot(tree4)
text(tree4, pretty = 0)



--- PCA 5 ---



pca5 <- prcomp(X5, scale = TRUE, center = TRUE)
pca5
dim(pca5$x)
dim(y5)

# Label encode y1 as 1 for popular and 0 for unpopular

y5$shares_class <- ifelse(y5$shares_class == "Popular", 1, 0)
y5

# Join pca1$x and y1
pca5_data <- cbind(pca5$x, y5)
#Split pca1_data into train and test data with 0.2 test size
set.seed(123)
train_index <- createDataPartition(pca5_data$shares_class, p = 0.8, list = FALSE)
train <- pca5_data[train_index,]
test <- pca5_data[-train_index,]
dim(train)
dim(test)

# Fit a classification tree on train data
library(rpart)
set.seed(123)
tree5 <- rpart(shares_class ~ ., data = train, method = "class")
tree5

# Predict on test data
pred5 <- predict(tree5, test, type = "class")
pred5

# Accuracy
mean(pred5 == test$shares_class)


# Confusion matrix
confusionMatrix(pred5, as.factor(test$shares_class))

# Plot the tree
plot(tree5)
text(tree5, pretty = 0)
