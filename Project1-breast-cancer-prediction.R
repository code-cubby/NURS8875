# Load required libraries
library(e1071) 
library(class)

# assumes data file is in the working directory
# Load the data (replace 'data.csv' with your data file's name)
data <- read.csv("breast-cancer-wisconsin.data", header = FALSE)
names(data) <- c('id' ,'clump_thickness', 'uniform_cell_size', 'uniform_cell_shape','marginal_adhesion', 'single_epithelial_size', 'bare_nuclei','bland_chromatin', 'normal_nucleoli', 'mitoses', 'class')

# Check the structure of the dataset
str(data)

#recode missing
data$bare_nuclei[which(data$bare_nuclei == "?")] <- -99999
data <- data[,-1]

# recheck the structure of the dataset
str(data)


# Assuming that all columns are numeric
# Convert 'class' to a factor 
data$bare_nuclei <- as.numeric(data$bare_nuclei)
data$class <- as.factor(data$class)

# Split the dataset into a training set and a test set
set.seed(123)
trainIndex <- sample(1:nrow(data), nrow(data)*0.8)
trainSet <- data[trainIndex, ]
testSet <- data[-trainIndex, ]

# K-Nearest Neighbors (KNN)
knn_pred <- knn(train = trainSet[,-10], test = testSet[,-10], cl = trainSet[,10], k=3)

# Support Vector Machine (SVM)
svm_model <- svm(class ~ ., data = trainSet, kernel = 'radial', gamma = 0.1, cost = 10)
svm_pred <- predict(svm_model, testSet[,-10])

# Evaluate the performance
knn_cm <- table(testSet[,10], knn_pred)
svm_cm <- table(testSet[,10], svm_pred)

print("Confusion Matrix for KNN:")
print(knn_cm)

print("Confusion Matrix for SVM:")
print(svm_cm)

# Accuracy
knn_accuracy <- sum(diag(knn_cm)) / sum(knn_cm)
svm_accuracy <- sum(diag(svm_cm)) / sum(svm_cm)

print(paste("Accuracy for KNN: ", round(knn_accuracy, 4)))
print(paste("Accuracy for SVM: ", round(svm_accuracy, 4)))

