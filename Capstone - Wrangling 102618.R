#-------------Capstone Data Wrangling---------------------

# Data sets for this capstone project are from UCI Machine Learning Reposity - Wine Quality Data Set
# Functions are specific to this capstone project's data sets but can be tweaked for use in other projects

#------------Import Data Sets-----------------------------

# 3 Function variations:

# Import both red and white wine data sets into R and combine them into a list
# (1) Generic function to import red and white wine data sets by specifying csv files 
import_data_1 <- function(x, y) { # Use quotations when indicating csv files
  red <- read.csv(url(x), sep = ";")
  white <- read.csv(url(y), sep = ";")
  L <- list(red.df = red, white.df = white)
  return(L)
}

# (2) Function that imports Wine Quality Data Sets without being specified
import_data_2 <- function() {
  red <- read.csv(url("https://archive.ics.uci.edu/ml/machine-learning-databases/wine-quality/winequality-red.csv"), sep = ";")
  white <- read.csv(url("https://archive.ics.uci.edu/ml/machine-learning-databases/wine-quality/winequality-white.csv"), sep = ";")
  L <- list(red.df = red, white.df = white)
    return(L)
}

# (3) Function that imports Wine Quality Data Set by indicating "red" OR "white". Use this option if analyzing wine data sets separately.
import_data_3 <- function(x) { # Put color in quotation marks
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

#-------------Add column to data sets----------------------

# 2 Function variations:

# Add column in each data set to identify red or white using dplyr
add_col_1 <- function(L) {
  library(dplyr)
  red_clean <- mutate(L$red, "color" = "red")
  white_clean <- mutate(L$white, "color" = "white")
  Lclean <- list(red_clean.df = red_clean, white_clean.df = white_clean)
    return(Lclean)
}

# Add column to identify wine color. Use this option if analyzing wine data sets separately.
add_col_2 <- function(df, x) { # Put color in quotations
  library(dplyr)
  color <- x
  wine.color <- mutate(df, "color" = color)
  return(wine.color)
}

#-------------Combine data sets----------------------

# Combine data sets into one data frame for easier analysis. Exclude function if analyzing data sets separately.
combo_data <- function(Lclean) {
  red_white.df <- bind_rows(Lclean$red, Lclean$white)
  return(red_white.df)
}

#-------------Reorder columns-------------------------

# Reorder columns to have the new wine color column become the first column
reorder_col <- function(L) {
  red_white_clean.df <- select(L, c(ncol(L), 1:(ncol(L)-1)))
  return(red_white_clean.df)
}

#------------Add a binary wine quality column for regression analysis--------------

# Add binary quality column, rating of 6 and over is 1 (good) and 5 and under is 0 (bad).
binary_quality <- function(df) {
  df$binary <- df$quality
  df$binary[df$quality>=6] <- 1
  df$binary[df$quality<=5] <- 0
  df
}