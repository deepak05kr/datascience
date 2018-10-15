library(caret)
library(kernlab)
library(dplyr)
library(readr)
library(ggplot2)
library(gridExtra)
library(knitr)  # Markdown
library(RColorBrewer)
library(ElemStatLearn)
library(foreign)  # For reading and writing data stored by statistical packages such as Minitab,S,SAS,SPSS
library(tree)
library(maptree)
library(gridBase)
library(e1071)
library(rpart)
library(ROCR)
library(knitr)
library(caTools)
library(kernlab)
library(h2o)
h2o.init(nthreads = 4)

CUSTOM_COLORS_PLOT <- colorRampPalette(brewer.pal(10,"Set3"))

#Loading Data

mnist_train_Data <- read.csv("mnist_train.csv",stringsAsFactors = FALSE)
mnist_test_DATA <- read.csv("mnist_test.csv",stringsAsFactors=FALSE)

#Understanding Dimensions
dim(mnist_train_Data)
#num of rows
nrow(mnist_train_Data)
#num of columns
ncol(mnist_train_Data)
#column names
colnames(mnist_train_Data)
#Structure of the dataset
str(mnist_train_Data)
#Exploring the data
summary(mnist_train_Data)
#data type check
sapply(mnist_train_Data[1,],class)
#checking missing value
sapply(mnist_train_Data, function(x) sum(is.na(x)))
#printing first few rows
head(mnist_train_Data)

#Understanding Dimensions
dim(mnist_test_DATA)
#num of rows
nrow(mnist_test_DATA)
#num of columns
ncol(mnist_test_DATA)
#column names
colnames(mnist_test_DATA)
#Structure of the dataset
str(mnist_test_DATA)
#Exploring the data
summary(mnist_test_DATA)
#data type check
sapply(mnist_test_DATA[1,],class)
#checking missing value
sapply(mnist_test_DATA, function(x) sum(is.na(x)))
#printing first few rows
head(mnist_test_DATA)
#number of missing values check
sum(is.na(mnist_train_Data))
sum(is.na(mnist_test_DATA))

#Transformation. Transform Label as Factor (Categorical) and Change Column Names (TESTING data set)
scale(mnist_train_Data,center=TRUE,scale=TRUE)
scale(mnist_test_DATA,center=TRUE,scale=TRUE)

mnist_train_Data[, 1] <- as.factor(mnist_train_Data[, 1])  # As Category
colnames(mnist_train_Data) <- c("Y", paste("X.", 1:784, sep = ""))
class(mnist_train_Data[, 1])
levels(mnist_train_Data[,1])
sapply(mnist_train_Data[1,],class)

#Transformation. Transform Label as Factor (Categorical) and Change Column Names (TESTING data set)
mnist_test_DATA[, 1] <- as.factor(mnist_test_DATA[, 1])  # As Category
colnames(mnist_test_DATA) <- c("Y", paste("X.", 1:784, sep = ""))
class(mnist_test_DATA[, 1])
levels(mnist_test_DATA[,1])
sapply(mnist_test_DATA[1,],class)

#Total Number of Digits (Training Set)
resTable <- table(mnist_train_Data$Y)
par(mfrow = c(1, 1))
par(mar = c(5, 4, 4, 2) + 0.1)  # increase y-axis margin.
plot <- plot(mnist_train_Data$Y, col = CUSTOM_COLORS_PLOT(10), main = "Total Number of Digits (Training Set)", 
             ylim = c(0, 1500), ylab = "Examples Number")
text(x = plot, y = resTable + 50, labels = resTable, cex = 0.75)


par(mfrow = c(1, 1))
percentage <- round(resTable/sum(resTable) * 100)
labels <- paste0(row.names(resTable), " (", percentage, "%) ")  # add percents to labels
pie(resTable, labels = labels, col = CUSTOM_COLORS_PLOT(10), main = "Total Number of Digits (Training Set)")


#Total Number of Digits (Testing Set)
resTable <- table(mnist_test_DATA$Y)
par(mfrow = c(1, 1))
par(mar = c(5, 4, 4, 2) + 0.1)  # increase y-axis margin.
plot <- plot(mnist_test_DATA$Y, col = CUSTOM_COLORS_PLOT(10), main = "Total Number of Digits (Testing Set)", 
             ylim = c(0, 400), ylab = "Examples Number")
text(x = plot, y = resTable + 20, labels = resTable, cex = 0.75)

par(mfrow = c(1, 1))
percentage <- round(resTable/sum(resTable) * 100)
labels <- paste0(row.names(resTable), " (", percentage, "%) ")  # add percents to labels
pie(resTable, labels = labels, col = CUSTOM_COLORS_PLOT(10), main = "Total Number of Digits (Training Set)")

# Splitting the data between train and test

set.seed(100)
indices = sample.split(mnist_train_Data$Y, SplitRatio = 0.7) 
train = mnist_train_Data[indices,]
test = mnist_train_Data[!(indices),]

#  Model Building

#--------------------------------------------------------------------
#  Linear model - SVM  at Cost(C) = 1
#####################################################################

# Model with C =1
model_1<- ksvm(train$Y ~ ., data = train,scale = TRUE,C=1)

# Predicting the model results 
evaluate_1<- predict(model_1, test)

# Confusion Matrix - Finding accuracy, Sensitivity and specificity
confusionMatrix(evaluate_1, test$Y)

#--------------------------------------------------------------------
#  Linear model - SVM  at Cost(C) = 10
#####################################################################

# Model with C =10
model_2<- ksvm(train$Y ~ ., data = train,scale = TRUE,C=10)

# Predicting the model results 
evaluate_2<- predict(model_2, test)

# Confusion Matrix - Finding accuracy, Sensitivity and specificity
confusionMatrix(evaluate_2, test$Y)

error.rate.svm <- sum(train$Y != evaluate_2)/nrow(test)
print(paste0("Accuary (Precision): ", 1 - error.rate.svm))
errors <- as.vector(which(test$Y != evaluate_2))
print(paste0("Error Numbers: ", length(errors)))


# Hyperparameter tuning and Cross Validation  - Linear - SVM 
# We will use the train function from caret package to perform crossvalidation

trainControl <- trainControl(method="cv", number=5)
# Number - Number of folds 
# Method - cross validation

set.seed(100)

# making a grid of C values. 
grid <- expand.grid(C=seq(1, 5, by=1))

# Performing 5-fold cross validation
#fit.svm <- train(train$Y~., data=train, method="svmLinear", metric="Accuracy", tuneGrid=grid, trControl=trainControl)

# Printing cross validation result
#print(fit.svm)

# Plotting "fit.svm" results
#plot(fit.svm)

#####################################################################
#Using Linear Kernel
#####################################################################

Model_linear <- ksvm(train$Y~ ., data = train, scale = FALSE, kernel = "vanilladot")
Eval_linear<- predict(Model_linear, test)

#confusion matrix - Linear Kernel
confusionMatrix(Eval_linear,test$Y)

#Using RBF Kernel
Model_RBF <- ksvm(train$Y~ ., data = train, scale = FALSE, kernel = "rbfdot")
Eval_RBF<- predict(Model_RBF, test)

#confusion matrix - RBF Kernel
confusionMatrix(Eval_RBF,test$Y)

error.rate.svm <- sum(train$Y != evaluate_2)/nrow(test)
print(paste0("Accuary (Precision): ", 1 - error.rate.svm))
errors <- as.vector(which(test$Y != evaluate_2))
print(paste0("Error Numbers: ", length(errors)))


############   Hyperparameter tuning and Cross Validation #####################

# We will use the train function from caret package to perform Cross Validation. 

#traincontrol function Controls the computational nuances of the train function.
# i.e. method =  CV means  Cross Validation.
#      Number = 2 implies Number of folds in CV.

trainControl <- trainControl(method="cv", number=5)


# Metric <- "Accuracy" implies our Evaluation metric is Accuracy.

#Expand.grid functions takes set of hyperparameters, that we shall pass to our model.

set.seed(7)
grid <- expand.grid(.sigma=c(0.025, 0.05), .C=c(0.1,0.5,1,2) )


#train function takes Target ~ Prediction, Data, Method = Algorithm
#Metric = Type of metric, tuneGrid = Grid of Parameters,
# trcontrol = Our traincontrol method.

#fit.svm <- train(train$Y~., data=train, method="svmRadial", metric="Accuracy", tuneGrid=grid, trControl=trainControl)

#print(fit.svm)

#plot(fit.svm)

###############################################################################

# Valdiating the model after cross validation on test data

#evaluate_linear_test<- predict(fit.svm, mnist_test_DATA)
#confusionMatrix(evaluate_linear_test, mnist_test_DATA$Y)

error.rate.svm <- sum(train$Y != evaluate_2)/nrow(test)
print(paste0("Accuary (Precision): ", 1 - error.rate.svm))
errors <- as.vector(which(test$Y != evaluate_2))
print(paste0("Error Numbers: ", length(errors)))

