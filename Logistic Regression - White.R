# Steps for Logistic Regression (White Wine Data Set)

#-----Step 0: Source and import data for logistic regression-----

# Source Wrangling R script
white <- capstone_wrangling_color("white")

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
whiteTrain <- capstone_training(white$binary, white)
whiteTest <- capstone_testing(white$binary, white)


#-----Step 2: Create glm model-----

# Identify baseline for train set. % of observations with high quality wine.
whiteTrain.Baseline <- table(whiteTrain$binary)

# glm with all variables
whiteTrain.glm.all <- glm(binary ~ . , data = whiteTrain, family = binomial)
summary(whiteTrain.glm.all)
# glm with top 4 highly significant variables (manual forward stepwise - added one variable at a time and removed when AIC increased)
whiteTrain.glm.top4 <- glm(binary ~ volatile.acidity + total.sulfur.dioxide + sulphates + alcohol, data = whiteTrain, family = binomial)
summary(whiteTrain.glm)
# glm with stepAIC function from MASS package. Slight variation in model.
# install.packages("MASS")
# library(MASS)
# stepAIC model selection by AIC
whiteTrain.glm.sw <- stepAIC(whiteTrain.glm.all, trace = FALSE)
summary(whiteTrain.glm.sw)


# Use glm model with specific significant variables to run predictions
predict.whiteTrain <- predict(whiteTrain.glm.sw, type = "response")
summary(predict.whiteTrain)
# Identify average probablity of binary variable on training set. 0 = 38.3%; 1 = 66.7%
tapply(predict.whiteTrain, whiteTrain$binary, mean)


#-----Step 3: Calculate metrics based on model chosen-----

# Set threshold value to 50%; no preference on either rate at this point. Check confusion matrix; accuracy = 75%
table(whiteTrain$binary, predict.whiteTrain > 0.50)
# Adjust threshold after identify max cut off point; accuracy = 75%
table(whiteTrain$binary, predict.whiteTrain > 0.6367076)
# Calculate accuracy rate for train set using TP + TN / Total observations


# Install and load package ROCR
# install.packages("ROCR")
# library(ROCR)
# To plot an ROC, create a prediction model to standardize format
ROCRpred.whiteTrain <- prediction(predict.whiteTrain, whiteTrain$binary)
ROCRperf.whiteTrain <- performance(ROCRpred.whiteTrain, "tpr", "fpr")
# Plot ROCR
plot(ROCRperf.whiteTrain, colorsize = TRUE, print.cutoffs.at=seq(0,1,0.1), text.adj = c(-.2,1.7))
# Identify AUC = 80%
as.numeric(performance(ROCRpred.whiteTrain, "auc")@y.values)
# Find optimal threshold value = 0.6367076
SensSpec.whiteTrain <- performance(ROCRpred.whiteTrain,  "sens", "spec")

#ROCR performance of sensitivity and specificity
Sens.ROCRpred.whiteTrain <- performance(ROCRpred.whiteTrain,  measure="sens", x.measure="cutoff")
Spec.ROCRpred.whiteTrain <- performance(ROCRpred.whiteTrain,  measure="spec", x.measure="cutoff")

# Plot sensitivity and specificity rates to find where they intersect as best tradeoff between rates
plot(Sens.ROCRpred.whiteTrain, type="l", col="red",xlab="",ylab="")
par(new=TRUE)
plot(Spec.ROCRpred.whiteTrain, type="l", col="blue", xlab="Probability cutoff (threshold)",ylab="Sensitivity/Specificity")


#-----Apply testing set to model-----

# Testing data set already created in earlier steps
# Use glm model from training set to apply to testing set
predict.whiteTest <- predict(whiteTrain.glm.sw, type = "response", newdata = whiteTest)
summary(predict.whiteTest)
# Identify average probablity of binary variable on training set. 0 = 50%; 1 = 75%
tapply(predict.whiteTest, whiteTest$binary, mean)

#-----Plot ROCR and identify AUC-----

# Set threshold value to 50% as was done with train set; accuracy = 74%
table(whiteTest$binary, predict.whiteTest > 0.5)
# Adjust threshold after identify max cut off point; accuracy = 76%
table(whiteTest$binary, predict.whiteTest > 0.463297)
# Calculate accuracy rate of test set
# (TP + TN) / Total observations

# To plot an ROC, create a prediction model to standardize format
ROCRpred.whiteTest <- prediction(predict.whiteTest, whiteTest$binary)
ROCRperf.whiteTest <- performance(ROCRpred.whiteTest, "tpr", "fpr")
# Plot ROCR
plot(ROCRperf.whiteTrain, colorsize = TRUE, print.cutoffs.at=seq(0,1,0.1), text.adj = c(-.2,1.7))

# Identify AUC = 82%
as.numeric(performance(ROCRpred.whiteTest, "auc")@y.values)
# Find optimal threshold value for test set = 0.463297
SensSpec.whiteTest <- performance(ROCRpred.whiteTest,  "sens", "spec")

#ROCR performance of sensitivity and specificity
Sens.ROCRpred.whiteTest <- performance(ROCRpred.whiteTest,  measure="sens", x.measure="cutoff")
Spec.ROCRpred.whiteTest <- performance(ROCRpred.whiteTest,  measure="spec", x.measure="cutoff")

# Plot sensitivity and specificity rates to find where they intersect as best tradeoff between rates
plot(Sens.ROCRpred.whiteTest, type="l", col="red",xlab="",ylab="")
par(new=TRUE)
plot(Spec.ROCRpred.whiteTest, type="l", col="blue", xlab="Probability cutoff (threshold)",ylab="Sensitivity/Specificity")
