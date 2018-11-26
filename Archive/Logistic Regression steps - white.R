# Steps for Logistic Regression - white wine data set

# Source Wrangling R script
white <- capstone_wrangling_alt("white")
# Split data into training and testing sets using caTools
whiteTrain <- capstone_training_2(white$binary, white)
whiteTest <- capstone_testing_2(white$binary, white)
# Identify baseline for train set. % of observations with high quality wine.
whiteTrain.Baseline <- table(whiteTrain$binary)
# glm with all variables
# glm with top 4 highly significant variables
whiteTrain.glm.top4 <- glm(binary ~ volatile.acidity + total.sulfur.dioxide + sulphates + alcohol, data = whiteTrain, family = binomial)
summary(whiteTrain.glm)
# glm with stepAIC function from MASS package. Slight variation in model.
# install.packages("MASS")
# library(MASS)
whiteTrain.glm.step <- glm(binary ~ . , family = binomial, data = whiteTrain)
whiteTrain.glm.sw <- stepAIC(whiteTrain.glm.step, trace = FALSE)
summary(whiteTrain.glm.sw)
# Use glm model with specific significant variables to run predictions
predict.whiteTrain <- predict(whiteTrain.glm.sw, type = "response")
summary(predict.whiteTrain)
# Identify average probablity of binary variable on training set. 0 = 38.3%; 1 = 66.7%
tapply(predict.whiteTrain, whiteTrain$binary, mean)
# Set threshold value to 50%; no preference on either rate at this point. Check confusion matrix; accuracy = 75%
table(whiteTrain$binary, predict.whiteTrain > 0.50)
# Adjust threshold after identify max cut off point; accuracy = 73%
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
CP.whiteTrain <-SensSpec.whiteTrain@alpha.values[[1]][which.max(SensSpec.whiteTrain@x.values[[1]]+SensSpec.whiteTrain@y.values[[1]])]

#ROCR performance of sensitivity and specificity
Sens.ROCRpred.whiteTrain <- performance(ROCRpred.whiteTrain,  measure="sens", x.measure="cutoff")
Spec.ROCRpred.whiteTrain <- performance(ROCRpred.whiteTrain,  measure="spec", x.measure="cutoff")

# Plot sensitivity and specificity rates to find where they intersect as best tradeoff between rates
plot(Sens.ROCRpred.whiteTrain, type="l", col="red",xlab="",ylab="")
par(new=TRUE)
plot(Spec.ROCRpred.whiteTrain, type="l", col="blue", xlab="Probability cutoff (threshold)",ylab="Sensitivity/Specificity")
abline(v = CP.whiteTrain, col = "black", lty = 3)#add a line indicating the suggested 'optimal' cutoff value differing from the visually expected one


# TESTING SET STEPS
# Testing data set already created in earlier steps
# Use glm model from training set to apply to testing set
predict.whiteTest <- predict(whiteTrain.glm.sw, type = "response", newdata = whiteTest)
# Set threshold value to 50% as was done with train set; accuracy = 76%
table(whiteTest$binary, predict.whiteTest > .5)
# Adjust threshold after identify max cut off point; accuracy = 72%
table(whiteTest$binary, predict.whiteTest > 0.6684387)
# Calculate accuracy rate of test set
# (TP + TN) / Total observations
ROCRpred.whiteTest <- prediction(predict.whiteTest, whiteTest$binary)
# Identify AUC = 80%
as.numeric(performance(ROCRpred.whiteTest, "auc")@y.values)
# Find optimal threshold value for test set = 0.6684387
SensSpec.whiteTest <- performance(ROCRpred.whiteTest,  "sens", "spec")
CP.whiteTest <-SensSpec.whiteTest@alpha.values[[1]][which.max(SensSpec.whiteTest@x.values[[1]]+SensSpec.whiteTest@y.values[[1]])]
