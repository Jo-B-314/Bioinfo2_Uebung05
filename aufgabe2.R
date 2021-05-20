library(randomForest)
library(caret)
library(readr)
library(caTools)
library(ggplot2)

#we read the data out of the table which is in the same directory as our script
data <- read.table("Methotrexate_drug_response_binarized_and_gene_expression.txt")
#we set a seed (which has to be 42 because we are computer scientists :D)
set.seed(42)
#a) we split our data so we have 80% in our training set and 20% in the test set
sample = sample.split(data, SplitRatio = 0.8)
training = subset(data, sample == TRUE)
test  = subset(data, sample == FALSE)
#b) 5 fold cross validation
train_control <- trainControl(method = "cv", number = 5)
model <- train(Methotrexate ~ .,  data = training, trControl = train_control,
               method = "rf", metric = "roc")
print(model)
# Train the model
for (mtry in 1:70) {
  rf <- randomForest(formula = Methotrexate ~., data = training, mtry = mtry,
               trControl = train_control, metric = "roc")
}
# Summarize the results
print(rf)
