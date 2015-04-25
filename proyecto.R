
# First of all, I call all the libraries we are going to need to make de proyect:
  
# Data input-output library:
library(gdata)  

# Machine Learning libraries:
library(caret)
library(rpart)
library(klaR)
library(Hmisc)

# Plotting library:
library(rattle)

# I inicialize the data-frames with the data imported from the csv files
plm_testing = read.csv("pml-testing.csv")  # read csv file 
plm_training = read.csv("pml-training.csv")  # read csv file

# I create a partition using the training data to divide it between training and testing data
# I will use this division to first train diferent models with the training data and then testing them
# using the testing data. I think we can use 70% of the data for training and 30% of the data for testing, because
# we have enought data (more than 19K rows)
inTrain = createDataPartition(y = plm_training$X, p=0.7, list=FALSE)
training = plm_training[inTrain,]
testing = plm_training[-inTrain,]
dim(training)
dim(testing)

# Now it's time to inspectionate all the variables we can use to select the variables we really want to use.
names(training)

# We can see also that there is no relationship between the names and the classes:
qplot(user_name, classe, data=training)
# For each user name we find almost the same amount of different classes:
training_carlitos = training[training$user_name=="carlitos", ]
#training_carlitos$classe

# Now I started to plot some relationships between all the variables that I want to use, avoiding those ones that are
# empty or with missing values. Here I put some plotting examples I have done during this process:
featurePlot(x=training[, c("gyros_belt_x", "gyros_belt_y", "gyros_belt_z")], 
            y = training$classe, 
            plot = "pairs")
featurePlot(x=training[, c("accel_belt_x", "accel_belt_y", "accel_belt_z")], 
            y = training$classe, 
            plot = "pairs")
featurePlot(x=training[, c("magnet_belt_x", "magnet_belt_y", "magnet_belt_z")], 
            y = training$classe, 
            plot = "pairs")


# Finally I created a training dataset (training4) removing the variables with missing values and variables that are
# not useful in our analysis, like the date and time:
training4 = training[, c(37:45, 46:49, 60:68, 84:86, 113:121, 122:124, 151:159, 160)]

# We create another training dataset (training5) from the last one removin the variables that are strongly correlated.
# For that, I first calculate the matrix of correlation between the variables and then I calculate the mean of the
# correlation between each variable with all the other variables.
dim(training4)
variables4 = training4[, c(1:46)]
cor_variables = cor(variables4)
mean_cor = c(1:46)
for (i in 1:46){
  mean_cor[i] = mean(cor_variables[i,])
}
# Finally we eliminate the variables with high correlation (variables 4, 7, 23, 30, 34, 40, 42) to create training5
training5 = training4[, c(-4, -7, -23, -30, -34, -40, -42)]


# Now we can start to use some prediction models using caret package, using both the training4 and training5 
# datasets, and then test them using the testing partition to check their performance.
# First of all, we fit a model using prediction with trees:
#modelFit3 = train(classe ~ ., method="rpart", data=training4)
#modelFit3v2 = train(classe ~ ., method="rpart", data=training5)
# We draw the decission tree:
#fancyRpartPlot(modelFit3v2$finalModel)

# We fit models using prediction with linear discriminant analysis:
#modelFit5 = train(classe ~ ., method="lda", data=training4)
#modelFit5v2 = train(classe ~ ., method="lda", data=training5)

# I fit models using prediction with naive bayes model:
#modelFit6 = train(classe ~ ., method="nb", data=training4)
#modelFit6v2 = train(classe ~ ., method="nb", data=training5)


# And finally I fit the model using prediction with bagging:
modelFit8 = train(classe ~ ., method="treebag", data=training4)



# Now we can test our diferent models using the testing data:
#pred3 = predict(modelFit3, newdata=testing)
#pred3v2 = predict(modelFit3v2, newdata=testing)

#pred5 = predict(modelFit5, newdata=testing)
#pred5v2 = predict(modelFit5v2, newdata=testing)

#pred6 = predict(modelFit6, newdata=testing)
#pred6v2 = predict(modelFit6v2, newdata=testing)

pred8 = predict(modelFit8, newdata=testing)



# We evaluate our models using the confusion matrix. For each prediction we show the accuracy of the model and
# the out of sample error:
#result3 = confusionMatrix(pred3, testing$classe) # Accuracy : 0.48 - Out of sample error = 0.52
#result3v2 = confusionMatrix(pred3v2, testing$classe) # Accuracy : 0.48- Out of sample error = 0.52

#result5 = confusionMatrix(pred5, testing$classe) # Accuracy : 0.66 - Out of sample error = 0.33
#result5v2 = confusionMatrix(pred5v2, testing$classe) # Accuracy : 0.59  - Out of sample error = 0.41

#result6 = confusionMatrix(pred6, testing$classe) # Accuracy : 0.74 - Out of sample error = 0.26
#result6v2 = confusionMatrix(pred6v2, testing$classe) # Accuracy : 0.71 - Out of sample error = 0.29

# This bagging model has actually the best accuracy, so that is the reason we decide to use it for the final answer
result8 = confusionMatrix(pred8, testing$classe) # Accuracy : 0.96 - Out of sample error = 0.04



# Now I use the best of our model to test the 20 original examples to test (original plm-testing data)
pred_final = predict(modelFit8, newdata=plm_testing)
pred_final


# Now we fit a model again using our best model and all the original training data (without splitting it),
# and without eliminating variables with high correlation. It seems to work better if we only eliminate
# the variables with missing values.
training4_final = plm_training[, c(37:45, 46:49, 60:68, 84:86, 113:121, 122:124, 151:159, 160)]
modelFit8_final = train(classe ~ ., method="treebag", data=training4_final)
pred_final_v2 = predict(modelFit8_final, newdata=plm_testing)

# And these are the predictions we are going to send as the best of our models:
pred_final_v2

