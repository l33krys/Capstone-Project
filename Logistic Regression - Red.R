# Steps for Logistic Regression (Red Wine Data Set)

#-----Step 0: Source and import data for logistic regression-----

# Source Wrangling R script
red <- capstone_wrangling_color("red")

#-----Step 1: Create training and testing data sets-----------------

# Split data using sample.split function
# install.packages("caTools")
# library(caTools)
capstone_training <- function(dfcol, dataframe) {
  set.seed(123456)
  split <- sample.split(dfcol, SplitRatio = .67)
  training.set <- subset(dataframe, split == TRUE)
  return(training.set)
}

capstone_testing <- function(dfcol, dataframe) {
  set.seed(123456)
  split <- sample.split(dfcol, SplitRatio = .67)
  testing.set <- subset(dataframe, split == FALSE)
  return(testing.set)
}

# Create training and testing sets
redTrain <- capstone_training(red$binary, red)
redTest <- capstone_testing(red$binary, red)


#-----Step 2: Create glm model-----

# Identify baseline for train set. % of observations with high quality wine.
redTrain.Baseline <- table(redTrain$binary)

# glm with all variables
redTrain.glm.all <- glm(binary ~ . , data = redTrain, family = binomial)
summary(redTrain.glm.all)
# glm with top 4 highly significant variables (manual forward stepwise - added one variable at a time and removed when AIC increased)
redTrain.glm.top4 <- glm(binary ~ volatile.acidity + total.sulfur.dioxide + sulphates + alcohol, data = redTrain, family = binomial)
summary(redTrain.glm)
# glm with stepAIC function from MASS package. Slight variation in model.
# install.packages("MASS")
# library(MASS)
# stepAIC model selection by AIC
redTrain.glm.sw <- stepAIC(redTrain.glm.all, trace = FALSE)
summary(redTrain.glm.sw)


# Use glm model with specific significant variables to run predictions
predict.redTrain <- predict(redTrain.glm.sw, type = "response")
summary(predict.redTrain)
# Identify average probablity of binary variable on training set. 0 = 37.8%; 1 = 67.1%
tapply(predict.redTrain, redTrain$binary, mean)


#-----Step 3: Calculate metrics based on model chosen-----

# Set threshold value to 50%; no preference on either rate at this point. Check confusion matrix; accuracy = 75%
table(redTrain$binary, predict.redTrain > 0.50)
# Adjust threshold after identify max cut off point; accuracy = 75%
table(redTrain$binary, predict.redTrain > 0.4844531)
# Calculate accuracy rate for train set using TP + TN / Total observations


# Install and load package ROCR
    # install.packages("ROCR")
    # library(ROCR)
# To plot an ROC, create a prediction model to standardize format
ROCRpred.redTrain <- prediction(predict.redTrain, redTrain$binary)
ROCRperf.redTrain <- performance(ROCRpred.redTrain, "tpr", "fpr")
# Plot ROCR
plot(ROCRperf.redTrain, colorsize = TRUE, print.cutoffs.at=seq(0,1,0.1), text.adj = c(-.2,1.7))
# Identify AUC = 81%
as.numeric(performance(ROCRpred.redTrain, "auc")@y.values)
# Find optimal threshold value = 0.4844531
SensSpec.redTrain <- performance(ROCRpred.redTrain,  "sens", "spec")

#ROCR performance of sensitivity and specificity
Sens.ROCRpred.redTrain <- performance(ROCRpred.redTrain,  measure="sens", x.measure="cutoff")
Spec.ROCRpred.redTrain <- performance(ROCRpred.redTrain,  measure="spec", x.measure="cutoff")

# Plot sensitivity and specificity rates to find where they intersect as best tradeoff between rates
plot(Sens.ROCRpred.redTrain, type="l", col="red",xlab="",ylab="")
par(new=TRUE)
plot(Spec.ROCRpred.redTrain, type="l", col="blue", xlab="Probability cutoff (threshold)",ylab="Sensitivity/Specificity")


#-----Apply testing set to model-----

# Testing data set already created in earlier steps
# Use glm model from training set to apply to testing set
predict.redTest <- predict(redTrain.glm.sw, type = "response", newdata = redTest)
summary(predict.redTest)
# Identify average probablity of binary variable on training set. 0 = 36.5%; 1 = 66.5%
tapply(predict.redTest, redTest$binary, mean)

#-----Plot ROCR and identify AUC-----

# Set threshold value to 50% as was done with train set; accuracy = 74%
table(redTest$binary, predict.redTest > 0.5)
# Adjust threshold after identify max cut off point; accuracy = 76%
table(redTest$binary, predict.redTest > 0.463297)
# Calculate accuracy rate of test set
# (TP + TN) / Total observations

# To plot an ROC, create a prediction model to standardize format
ROCRpred.redTest <- prediction(predict.redTest, redTest$binary)
ROCRperf.redTest <- performance(ROCRpred.redTest, "tpr", "fpr")
# Plot ROCR
plot(ROCRperf.redTrain, colorsize = TRUE, print.cutoffs.at=seq(0,1,0.1), text.adj = c(-.2,1.7))

ROCRpred.redTest <- prediction(predict.redTest, redTest$binary)
# Identify AUC = 82%
as.numeric(performance(ROCRpred.redTest, "auc")@y.values)
# Find optimal threshold value for test set = 0.463297
SensSpec.redTest <- performance(ROCRpred.redTest,  "sens", "spec")

#ROCR performance of sensitivity and specificity
Sens.ROCRpred.redTest <- performance(ROCRpred.redTest,  measure="sens", x.measure="cutoff")
Spec.ROCRpred.redTest <- performance(ROCRpred.redTest,  measure="spec", x.measure="cutoff")

# Plot sensitivity and specificity rates to find where they intersect as best tradeoff between rates
plot(Sens.ROCRpred.redTest, type="l", col="red",xlab="",ylab="")
par(new=TRUE)
plot(Spec.ROCRpred.redTest, type="l", col="blue", xlab="Probability cutoff (threshold)",ylab="Sensitivity/Specificity")
