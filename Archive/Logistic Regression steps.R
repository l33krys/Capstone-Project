# Steps for Logistic Regression

# Source Wrangling R script
red <- capstone_wrangling_alt("red")
# Split data into training and testing sets using caTools
redTrain <- capstone_training_2(red$binary, red)
redTest <- capstone_testing_2(red$binary, red)
# Identify baseline for train set. % of observations with high quality wine.
redTrain.Baseline <- table(redTrain$binary)
# glm with all variables
redTrain.glm.all <- glm(binary ~ . , data = redTrain, family = binomial)
# glm with top 4 highly significant variables
redTrain.glm.top4 <- glm(binary ~ volatile.acidity + total.sulfur.dioxide + sulphates + alcohol, data = redTrain, family = binomial)
summary(redTrain.glm)
# glm with stepAIC function from MASS package. Slight variation in model.
# install.packages("MASS")
# library(MASS)
redTrain.glm.step <- glm(binary ~ . , family = binomial, data = redTrain)
redTrain.glm.sw <- stepAIC(redTrain.glm.step, trace = FALSE)
summary(redTrain.glm.sw)
# Use glm model with specific significant variables to run predictions
predict.redTrain <- predict(redTrain.glm.sw, type = "response")
summary(predict.redTrain)
# Identify average probablity of binary variable on training set. 0 = 38.3%; 1 = 66.7%
tapply(predict.redTrain, redTrain$binary, mean)
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
CP.redTrain <-SensSpec.redTrain@alpha.values[[1]][which.max(SensSpec.redTrain@x.values[[1]]+SensSpec.redTrain@y.values[[1]])]

#ROCR performance of sensitivity and specificity
Sens.ROCRpred.redTrain <- performance(ROCRpred.redTrain,  measure="sens", x.measure="cutoff")
Spec.ROCRpred.redTrain <- performance(ROCRpred.redTrain,  measure="spec", x.measure="cutoff")

# Plot sensitivity and specificity rates to find where they intersect as best tradeoff between rates
plot(Sens.ROCRpred.redTrain, type="l", col="red",xlab="",ylab="")
par(new=TRUE)
plot(Spec.ROCRpred.redTrain, type="l", col="blue", xlab="Probability cutoff (threshold)",ylab="Sensitivity/Specificity")
abline(v = CP.redTrain, col = "black", lty = 3)#add a line indicating the suggested 'optimal' cutoff value differing from the visually expected one


# TESTING SET STEPS
# Testing data set already created in earlier steps
# Use glm model from training set to apply to testing set
predict.redTest <- predict(redTrain.glm.sw, type = "response", newdata = redTest)
# Set threshold value to 50% as was done with train set; accuracy = 74%
table(redTest$binary, predict.redTest > 0.4844531)
# Adjust threshold after identify max cut off point; accuracy = 76%
table(redTest$binary, predict.redTest > 0.463297)
# Calculate accuracy rate of test set
# (TP + TN) / Total observations
ROCRpred.redTest <- prediction(predict.redTest, redTest$binary)
# Identify AUC = 82%
as.numeric(performance(ROCRpred.redTest, "auc")@y.values)
# Find optimal threshold value for test set = 0.463297
SensSpec.redTest <- performance(ROCRpred.redTest,  "sens", "spec")
CP.redTest <-SensSpec.redTest@alpha.values[[1]][which.max(SensSpec.redTest@x.values[[1]]+SensSpec.redTest@y.values[[1]])]
