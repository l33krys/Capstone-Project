# Import 2 data sets into R and create a list by passing URL through function
# optional: import_data <- function(x, y) {
#   red <- read.csv(url(x), sep = ";")
#   white <- read.csv(url(y), sep = ";")
#   L <- list(red.df = red, white.df = white)
#   return(L)
# }

import_data <- function() {
  red <- read.csv(url("https://archive.ics.uci.edu/ml/machine-learning-databases/wine-quality/winequality-red.csv"), sep = ";")
  white <- read.csv(url("https://archive.ics.uci.edu/ml/machine-learning-databases/wine-quality/winequality-white.csv"), sep = ";")
  L <- list(red.df = red, white.df = white)
    return(L)
}

# Add column in each data set to identify red or white using dplyr
add_col <- function(L) {
  library(dplyr)
  red_clean <- mutate(L$red, "color" = "red")
  white_clean <- mutate(L$white, "color" = "white")
  Lclean <- list(red_clean.df = red_clean, white_clean.df = white_clean)
    return(Lclean)
}

# Combine data sets
combo_data <- function(Lclean) {
  red_white.df <- bind_rows(Lclean$red, Lclean$white)
  return(red_white.df)
}

# Reorder columns to have last column become first column
reorder_col <- function(L) {
  red_white_clean.df <- select(L, c(ncol(L), 1:(ncol(L)-1)))
  return(red_white_clean.df)
}

# Add binary quality column, rating of 6 and over is 1 and 5 and under is 0.
binary_quality <- function(df) {
  df$binary <- df$quality
  df$binary[df$quality>=6] <- 1
  df$binary[df$quality<=5] <- 0
  df
}