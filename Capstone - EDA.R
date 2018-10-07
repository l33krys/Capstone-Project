# Import source code from Capstone - Wrangling
capstone_wrangling <- function() {
  source("~/Documents/Data Science Intro/Capstone Project/Capstone - Wrangling.R")
  L1 <- import_data()
  L2 <- add_col(L1)
  L3 <- combo_data(L2)
  L4 <- reorder_col(L3)
  L5 <- binary_quality(L4)
}

# After data wrangling, start exploratory data analysis using ggplot, plyr
# install.packages("ggplot2")
library(ggplot2)
library(plyr)

# Explore data using basic functions in R
# str(L5)
# head(L5, n = 10)
# summary(L5)
# View(L5)

# Plot column data into a histogram and identify by wine color
capstone_histogram <- function(df, dfcol, xlabel) {
  print(ggplot(df, aes(x = dfcol)) + 
    geom_histogram(binwidth = .5) + 
    facet_grid(. ~ color) +
    xlab(xlabel))
}

# Plot all columns into individual histograms
capstone_all_histograms <- function(df) {
  columns <- colnames(df)
  # Start function at column 2 since column 1 is text
  for (i in 2:length(columns)) {
    print(ggplot(df, aes(x = df[,i])) +
      geom_histogram(binwidth = .5) +
      xlab(columns[i]) +
      facet_grid(. ~ color))
  }
}

# Plot column data into a scatterplot by wine quality rating and in separate charts by wine color
capstone_scatter <- function(df, y_value, title, xlabel, ylabel) {
  capstone_scatter <- ggplot(df, aes(x = factor(df$quality), y = y_value)) + 
    geom_point(alpha = .2) + 
    facet_grid(. ~ color) +
    ggtitle(title) +
    xlab(xlabel) +
    ylab(ylabel)
  return(capstone_scatter)
}

# Plot all columns into individual scatter plots by wine quality rating
capstone_all_scatter <- function(df) {
  columns <- colnames(df)
  for (i in 1:length(columns)) {
    print(ggplot(df, aes(x = factor(df$quality), y = df[,i])) +
      geom_point(alpha = .2) +
      geom_jitter() +
      xlab("quality") +
      ylab(columns[i]) +
      facet_grid(. ~ color))
  }
}

# Plot column data into a scatterplot and boxplots by wine quality rating and in separate charts by wine color
capstone_scatter_boxplot <- function(df, y_value, title, xlabel, ylabel) {
  print(ggplot(df, aes(x = factor(df$quality), y = y_value)) + 
    geom_point(alpha = .2) +
    geom_jitter() +
    geom_boxplot() +
    facet_grid(. ~ color) +
    ggtitle(title) +
    xlab(xlabel) +
    ylab(ylabel))
}

# Plot all columns into individual scatter plots and boxplots by wine quality rating
capstone_all_scatter_boxplot <- function(df) {
  columns <- colnames(df)
  for (i in 1:length(columns)) {
    print(ggplot(df, aes(x = factor(df$quality), y = df[,i])) +
            geom_point(alpha = .2) +
            geom_jitter() +
            geom_boxplot() +
            xlab("quality") +
            ylab(columns[i]) +
            facet_grid(. ~ color))
  }
}

# Create frequency table to show how many of each wine quality rating
# qual_freq_percent <- mutate(qual_freq, "percent" = paste(round((100 * freq/sum(freq)), 0), "%"))

freq_percent <- function(df, col) {
  qual_freq <- count(df, col) # col format 'column name'
  freq_col <- mutate(qual_freq, "percent" = paste(round((100 * freq/sum(freq)), 0), "%"))
  View(freq_col)
}

# Function to show frequencies for all variable values
freq_percent_all <- function(df) {
  columns <- colnames(df)
  for (i in 1:length(columns)) {
    qual_freq <- count(df, colnames(df[i]))
    freq_col <- mutate(qual_freq, "percent" = paste(round((100 * freq/sum(freq)), 0), "%"))
     print(freq_col)
  }
}

# Function to identify number of unique values in each column
unique_variable <- function(dfcol) {
  num_unique_variable <- length(unique(dfcol))
  return(num_unique_variable)
}

# Function to identify number of unique values in each column
all_unique_variable <- function(df) {
  columns <- colnames(df)
  for (i in 1:length(columns)) {
    num_all_unique_variable <- length(unique(df[,i]))
    cat(columns[i], num_all_unique_variable, sep = "\n")
  }
}

# chi-square test with quality rating
chi.sq <- function(df) {
  columns <- colnames(df)
  for (i in 2:12) {
    test.result <- chisq.test(table(df[,13], df[,i]))
    cat(columns[i]," p value: ",test.result$p.value,"\n")
  }
}

# Identify 1%, 5%, 10%, 90%, 99%, and 100% range
quantile.all <- function(df) {
  columns <- colnames(df)
  for (i in 2:length(columns)) {
    quantile.variable <- quantile(df[,i], probs = c(.01, .05, .1, .90, .99, 1))
    tab <- data.frame("variable" = columns[i], quantile.variable)
    print(tab)
}
}

# Calculate correlation coefficient table for all values
cor.single <- function(df) {
  columns <- colnames(df)
  for (i in 2:length(columns)) { # Start at column 2 since column 1 is color
    cor.df <- cor(L5[,13], df[,i])
    cat(columns[i], "corr efficient:  ", cor.df, "\n")
  }
}