#----------------Capstone Logistic Regression------------------

# Functions below analyze UCI Machine Learning Repository - Wine Quality Data Set to identify trends and anomalies

#----------------Step 0: Source Capstone Project Data EDA R script---------

# Import source code from Capstone - Wrangling and run all functions to prepare data set for analysis
capstone_wrangling_rw <- function() {
  source("~/Documents/Data Science Intro/Capstone Project/Capstone - Wrangling 102618.R")
  L1 <- import_data_2()
  L2 <- add_col_1(L1)
  L3 <- combo_data(L2)
  L4 <- reorder_col(L3)
  L5 <- binary_quality(L4)
  L6 <- factor.binary(L5)
}

# Import source code from Capstone - Wrangling and run all functions to prepare data set for analysis. Specify color to import red or white data set.
capstone_wrangling <- function(x) { # Indicate which wine color data set in quotations
  source("~/Documents/Data Science Intro/Capstone Project/Capstone - Wrangling 102618.R")
  L1 <- import_data_3(x)
  L2 <- add_col_2(L1, x)
  L3 <- reorder_col(L2)
  L4 <- binary_quality(L3)
  L5 <- factor.binary(L4)
}

# Alternate. Import source code from Capstone - Wrangling and run all functions to prepare data set for analysis. Specify color to import red or white data set.
capstone_wrangling_alt <- function(x) { # Indicate which wine color data set in quotations
  source("~/Documents/Data Science Intro/Capstone Project/Capstone - Wrangling 102618.R")
  L1 <- import_data_3(x)
  L2 <- binary_quality(L1)
  L3 <- factor.binary(L2)
  L4 <- select(L3, -quality)
}

#----------------Step 1: Create training and testing data sets-----------------

# Split data set into 70% training and 30% testing
capstone_training <- function(df) {
  red.rows <- as.numeric(count(df))
  training.max <- as.numeric(round(count(df)*.7))
  testing.min <- as.numeric(count(df) - training.max)
  training <- wine[1:training.max, ]
  return(training)
}

capstone_testing <- function(df) {
  red.rows <- as.numeric(count(df))
  training.max <- as.numeric(round(count(df)*.7))
#  testing.min <- as.numeric(count(df) - training.max)
  test <- wine[(training.max + 1):red.rows, ]
  return(test)
}

# Split data using sample.split function
# install.packages("caTools")
# library(caTools)
capstone_training_2 <- function(dfcol, dataframe) {
  set.seed(123456)
  split <- sample.split(dfcol, SplitRatio = .67)
  training.set <- subset(dataframe, split == TRUE)
  return(training.set)
}

capstone_testing_2 <- function(dfcol, dataframe) {
  set.seed(123456)
  split <- sample.split(dfcol, SplitRatio = .67)
  testing.set <- subset(dataframe, split == FALSE)
  return(testing.set)
}

#-----------------Step #: Identify baseline model for logistic regression


#-----------------Step 2: Apply logistic regression model to data and decide which variables to keep in model

# glm formula to apply all variables
# biquality.train <- glm(formula = binary ~ fixed.acidity + volatile.acidity + citric.acid + residual.sugar + chlorides + free.sulfur.dioxide + total.sulfur.dioxide + density + pH + sulphates + alcohol, data = training.set, family = binomial)
# summary(biquality.train)

# Apply glm to top 4 highly correlated variables

# biquality.train <- glm(formula = binary ~ volatile.acidity + total.sulfur.dioxide + sulphates + alcohol, data = training.set, family = binomial)
# summary(biquality.train)


#-----------------Step 3: Determine threshold value using ROCR

# Use threshold value .50
