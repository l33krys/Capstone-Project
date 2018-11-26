#----------------Capstone Exploratory Data Analysis------------------

# Functions below analyze UCI Machine Learning Repository - Wine Quality Data Set to identify trends and anomalies

#----------------Source Capstone Project Data Wrangling R script---------

# Import source code from Capstone - Wrangling and run all functions to prepare data set for analysis. Specify color to import red or white data set.
capstone_wrangling_color <- function(x) { # Indicate which wine color data set in quotations
  source("C:/Users/krlee/Documents/Krystle/Capstone - Wrangling - FINAL.R")
  L1 <- import_data_color(x)
  L2 <- binary_quality(L1)
  # L3 <- factor.binary(L2)
  L3 <- select(L2, -quality)
}

#---------------Install packages needed for analysis----------------

# Install ggplot, plyr
# install.packages("ggplot2") # Commented out to prevent reinstalling with each source
library(ggplot2)
library(plyr)

#--------------Standard functions to get an overview of data-------------------

# Explore data using basic functions in R (Commented out to prevent standard functions from running with each source)
# str()
# head(, n = 10)
# summary()
# View()

#--------------ggplot functions------------------------

# Plots are separated by wine color. Functions that run for all attributes exclude categorical variable. (Wine color col)

# Plot bar chart of quality
capstone_bar <- function(df, dfcol, xlabel, ylabel) {
  print(ggplot(df, aes(x = factor(dfcol))) + 
    geom_bar(width = .5) + 
    xlab(xlabel) +
    ylab(ylabel))
}

# Plot  histograms
capstone_histograms <- function(df, dfcol) {
  print(ggplot(df, aes(x = dfcol)) +
            geom_histogram(binwidth = (max(dfcol) - min(dfcol)) / 10) +
            xlab("test"))
  }

# Plot ALL histograms
capstone_all_histograms <- function(df) {
  columns <- colnames(df)
  # Start function at column 2 since column 1 is text
  for (i in 1:(length(columns)-1)) {
    binwidth.10 <- (max(df$columns[i]) - min(df$columns[i])) / 10
    print(ggplot(df, aes(x = df[,i])) +
      geom_histogram(binwidth = .5) +
      xlab(columns[i]))
  }
}

# Plot INDIVIDUAL scatterplots by wine quality rating
capstone_scatter <- function(df, y_value, title, xlabel, ylabel) {
  capstone_scatter <- ggplot(df, aes(x = factor(df$binary), y = y_value)) + 
    geom_point(alpha = .2) + 
    ggtitle(title) +
    xlab(xlabel) +
    ylab(ylabel)
  return(capstone_scatter)
}

# Plot ALL scatter plots by wine quality rating
capstone_all_scatter <- function(df) {
  columns <- colnames(df)
  for (i in 1:length(columns)) {
    print(ggplot(df, aes(x = factor(df$binary), y = df[,i])) +
      geom_point(alpha = .2) +
      geom_jitter() +
      xlab("binary") +
      ylab(columns[i]))
  }
}

# Plot INDIVIDUAL scatterplot and boxplots by wine quality rating
capstone_scatter_boxplot <- function(df, y_value, xlabel, ylabel) {
  print(ggplot(df, aes(x = factor(df$binary), y = y_value)) + 
    geom_boxplot(width = .25) +
    xlab(xlabel) +
    ylab(ylabel))
}

# Plot ALL boxplots by wine quality rating
capstone_all_boxplots <- function(df) {
  columns <- colnames(df)
  for (i in 1:(length(columns)-1)) {
    print(ggplot(df, aes(x = factor(df$binary), y = df[,i])) +
      geom_boxplot(width = .25) +
      xlab("quality") +
      ylab(columns[i]))
  }
}

#--------------Frequency and count functions-------------------------

# Create frequency table to show how many of each wine quality rating
freq.percent <- function(df, x) {
  qual.freq <- count(df, x) # put actual column name in quotation marks ie. 'column name'
  freq.col <- mutate(qual.freq, "percent" = paste(round((100 * freq/sum(freq)), 0), "%"))
  View(freq.col)
}

# Function to show frequencies for all variable values
freq.percent.all <- function(df) {
  columns <- colnames(df)
  for (i in 1:length(columns)) {
    qual.freq <- count(df, colnames(df[i]))
    freq.col <- mutate(qual.freq, "percent" = paste(round((100 * freq/sum(freq)), 0), "%"))
     print(freq.col)
  }
}

# Function to identify number of unique values in each column
unique.all <- function(df) {
  columns <- colnames(df)
  unique.df <- data.frame(matrix(ncol = 2, nrow = 12))
  for (i in 1:12) {
    unique_variable <- length(unique(df[,i]))
    unique.df[i, 2] <- unique_variable
    unique.df[i, 1] <- columns[i]
  }
  colnames(unique.df) <- c("variable", "unique.variables")
  unique.df <- arrange(unique.df, desc(unique.variables))
  View(unique.df)
  return(unique.df)
}

#--------------chi-square test----------------------

# chi-square test with quality rating
chi.sq <- function(df) {
  columns <- colnames(df)
  pvalue.df <- data.frame(matrix(ncol = 2, nrow = 11))
  for (i in 1:11) {
    test.result <- chisq.test(table(df[,12], df[,i]))
    pvalue.df[i, 2] <- test.result$p.value
    pvalue.df[i, 1] <- columns[i]
  }
  colnames(pvalue.df) <- c("variable", "pvalue")
  pvalue.df <- arrange(pvalue.df, desc(pvalue))
  View(pvalue.df)
  return(pvalue.df)
}

#-------------correlation function------------------

# Calculate correlation coefficient table for all values
cor.all <- function(df) {
  columns <- colnames(df)
  cor.df <- data.frame(matrix(ncol = 2, nrow = 11))
  for (i in 1:11) {
    cor.test <- cor(df[,12], df[,i])
    cor.df[i, 2] <- cor.test
    cor.df[i, 1] <- columns[i]
    #cat(columns[i], "corr efficient:  ", cor.df, "\n")
  }
  colnames(cor.df) <- c("variable", "corr.efficient")
  cor.df <- arrange(cor.df, desc(corr.efficient))
  View(cor.df)
  return(cor.df)
}

#------------quantile function to determine if outliers should be removed------------

# Identify 1%, 5%, 10%, 90%, 99%, and 100% range
quantile.all <- function(df) {
  columns <- colnames(df)
  quant.df <- data.frame(matrix(ncol = 6, nrow = 12))
  probability <- c(.01, .05, .1, .90, .99, 1)
  for (i in 1:length(columns)) {
    quantile.variable <- quantile(df[,i], probs = probability)
    quant.df[i, ] <- quantile.variable
  }
  quant.df <- cbind(columns, quant.df)
  names(quant.df) <- c("variable", ".01", ".05", ".1", ".90", ".99", "1")
  print(quant.df)
}