# Load necessary libraries
library(kernlab)
library(caret)

# Load the dataset
cs <- read.csv("CaseStudy.csv")

#summary  
str(cs)
head(cs)

#removing variables - omitting dates except for `submitteddate`
remove <- c("personid", "appyear", "schoolsel_chr", "major1", "major2", "minor", 
            "essay1length", "essay2length", "essay3length", "signupdate", "starteddate")
cs_cleaned <- cs[, !(names(cs) %in% remove)]
str(cs_cleaned) 

#replace zeros in 'submitteddate' with -1 for not submitted
cs_cleaned$submitteddate[cs_cleaned$submitteddate == 0] <- -1
cs_cleaned$submitteddate <- as.numeric(cs_cleaned$submitteddate)

#convert to factors
cs_cleaned$major1group <- as.factor(cs_cleaned$major1group)  
cs_cleaned$major2group <- as.factor(cs_cleaned$major2group) 
cs_cleaned$minorgroup <- as.factor(cs_cleaned$minorgroup) 
cs_cleaned$appdeadline <- as.factor(cs_cleaned$appdeadline)
cs_cleaned$completedadm <- as.factor(cs_cleaned$completedadm)

cs_cleaned$undergrad_uni[is.na(cs_cleaned$undergrad_uni)] <- 6
cs_cleaned$undergrad_uni <- as.factor(cs_cleaned$undergrad_uni) 

str(cs_cleaned)
#undergrad_uni is a factor w/ 1 level of 6 or NA so omitting

cs_cleaned <- cs_cleaned[,-c(7)]
str(cs_cleaned)

#caret library

#One-Hot Encoding for `major1group`, `major2group`, and `minorgroup` (no ordinal relationship)
#Using model.matrix for one-hot encoding (without intercept)
major1group_encoded <- model.matrix(~ major1group - 1, data = cs_cleaned)
major2group_encoded <- model.matrix(~ major2group - 1, data = cs_cleaned)
minorgroup_encoded  <- model.matrix(~ minorgroup - 1, data = cs_cleaned)

#Combine the encoded columns back into the dataset
cs_cleaned <- cbind(cs_cleaned, major1group_encoded, major2group_encoded, minorgroup_encoded)

#Remove the original `major1group`, `major2group`, and `minorgroup` columns
cs_cleaned <- cs_cleaned[ , !(names(cs_cleaned) %in% c("major1group", "major2group", "minorgroup"))]

#Label Encoding for `appdeadline` (ordinal)
#assign numeric values in the order they appear
cs_cleaned$appdeadline <- as.numeric(factor(cs_cleaned$appdeadline, 
                                            levels = c("August", "September", "October", "January", "March"), 
                                            ordered = TRUE))

str(cs_cleaned)

# CLEANING UP NA VALUES
# Check the number of NA values in each column
colSums(is.na(cs_cleaned))
head(cs_cleaned$schoolsel, 70)

library(ggplot2)

# Bar plot for school selection (as a factor)
ggplot(cs_cleaned, aes(x = as.factor(schoolsel))) +
  geom_bar(fill = "skyblue", color = "black") +
  labs(x = "Application Deadline", y = "Count", title = "Distribution of School Selection") +
  theme_minimal()

str(cs_cleaned)

#NA for school selection will be labeled as 0
cs_cleaned$schoolsel[is.na(cs_cleaned$schoolsel)] <- 0
cs_cleaned$schoolsel <- as.factor(cs_cleaned$schoolsel)

str(cs_cleaned)

colSums(is.na(cs_cleaned))
#no more NAs = can proceed to SVM

##### SVM ANALYSIS 

# Load necessary libraries
library(kernlab)  # for SVM
library(caret)

# Set the seed for reproducibility
set.seed(1947)

# Split data into 80% training and 20% testing sets
train_index <- createDataPartition(cs_cleaned$completedadm, p = 0.8, list = FALSE)
train_data <- cs_cleaned[train_index, ]
test_data  <- cs_cleaned[-train_index, ]

# Train an SVM model using a linear kernel
svm_linear <- ksvm(completedadm ~ ., data = train_data, kernel = "vanilladot")

# Output model summary
print(svm_linear)

# Make predictions on the test dataset
svm_linear_predictions <- predict(svm_linear, test_data)

# Display the first few predictions
head(svm_linear_predictions)

# Evaluate the model using a confusion matrix
conf_matrix_linear <- confusionMatrix(svm_linear_predictions, test_data$completedadm)

# Print the confusion matrix
print(conf_matrix_linear)

# Train an SVM model using the RBF kernel
svm_rbf <- ksvm(completedadm ~ ., data = train_data, kernel = "rbfdot")

# Make predictions on the test dataset using the RBF kernel
svm_rbf_predictions <- predict(svm_rbf, test_data)

# Evaluate the RBF kernel model
conf_matrix_rbf <- confusionMatrix(svm_rbf_predictions, test_data$completedadm)

# Print the confusion matrix for the RBF model
print(conf_matrix_rbf)

# Define a range of cost values for tuning
cost_values <- c(1, seq(from = 5, to = 10, by = 5))

# Perform cross-validation with different cost values
accuracy_values <- sapply(cost_values, function(c) {
  model <- ksvm(completedadm ~ ., data = train_data, kernel = "rbfdot", C = c)
  predictions <- predict(model, test_data)
  cm <- confusionMatrix(predictions, test_data$completedadm)
  return(cm$overall["Accuracy"])
})

# Plot the accuracy vs cost
plot(cost_values, accuracy_values, type = "b", xlab = "Cost", ylab = "Accuracy", main = "SVM Accuracy for Different Cost Values")
