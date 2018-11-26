#-------------Capstone Data Wrangling---------------------

# Data sets for this capstone project are from UCI Machine Learning Reposity - Wine Quality Data Set
# Functions are specific to this capstone project's data sets but can be tweaked for use in other projects

#------------Import Data Sets-----------------------------

# Function that imports Wine Quality Data Set by indicating "red" OR "white"
import_data_color <- function(x) { # Put color in quotation marks
  if (x == "red") {
    red <- read.csv(url("https://archive.ics.uci.edu/ml/machine-learning-databases/wine-quality/winequality-red.csv"), sep = ";")
    return(red)
  } else if (x == "white") {
    white <- read.csv(url("https://archive.ics.uci.edu/ml/machine-learning-databases/wine-quality/winequality-white.csv"), sep = ";")
    return(white)
  } else {
    return("Specify red or white data set")
  }
}

#------------Add a binary wine quality column for regression analysis--------------

# Add binary quality column, rating of 6 and over is 1 (good) and 5 and under is 0 (bad).
binary_quality <- function(df) {
  df$binary <- df$quality
  df$binary[df$quality>=6] <- 1
  df$binary[df$quality<=5] <- 0
  df
}

#-----------Source and perform wrangling functions-------------------

# Import source code from Capstone - Wrangling and run all functions to prepare data set for analysis. Specify color to import red or white data set.
capstone_wrangling_color <- function(x) { # Indicate which wine color data set in quotations
  source("C:/Users/krlee/Documents/Krystle/Capstone - Wrangling - FINAL.R")
  L1 <- import_data_color(x)
  L2 <- binary_quality(L1)
  # L3 <- factor.binary(L2)
  L3 <- select(L2, -quality)
}
