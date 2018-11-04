#----------------Capstone Exploratory Data Analysis------------------

# Functions below analyze UCI Machine Learning Repository - Wine Quality Data Set to identify trends and anomalies

#----------------Source Capstone Project Data Wrangling R script---------

# Import source code from Capstone - Wrangling and run all functions to prepare data set for analysis
capstone_wrangling_rw <- function() {
  source("~/Documents/Data Science Intro/Capstone Project/Capstone - Wrangling 102618.R")
  L1 <- import_data_2()
  L2 <- add_col_1(L1)
  L3 <- combo_data(L2)
  L4 <- reorder_col(L3)
  L5 <- binary_quality(L4)
}

# Import source code from Capstone - Wrangling - red and run all functions to prepare data set for analysis
capstone_wrangling <- function(x) { # Indicate which wine color data set in quotations
  source("~/Documents/Data Science Intro/Capstone Project/Capstone - Wrangling 102618.R")
  L1 <- import_data_3(x)
  L2 <- add_col_2(L1, x)
  L3 <- reorder_col(L2)
  L4 <- binary_quality(L3)
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

# Plot INDIVIDUAL histograms
capstone_histogram <- function(df, dfcol, xlabel) {
  print(ggplot(df, aes(x = dfcol)) + 
    geom_histogram(binwidth = .5) + 
    facet_grid(. ~ color) +
    xlab(xlabel))
}

# Plot ALL histograms
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

# Plot INDIVIDUAL scatterplots by wine quality rating
capstone_scatter <- function(df, y_value, title, xlabel, ylabel) {
  capstone_scatter <- ggplot(df, aes(x = factor(df$binary), y = y_value)) + 
    geom_point(alpha = .2) + 
    facet_grid(. ~ color) +
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
      ylab(columns[i]) +
      facet_grid(. ~ color))
  }
}

# Plot INDIVIDUAL scatterplot and boxplots by wine quality rating
capstone_scatter_boxplot <- function(df, y_value, title, xlabel, ylabel) {
  print(ggplot(df, aes(x = factor(df$binary), y = y_value)) + 
  #  geom_point(alpha = .2) +
  #  geom_jitter() +
    geom_boxplot(width = .25, outlier.color = "Red") +
    facet_grid(. ~ color) +
    ggtitle(title) +
    xlab(xlabel) +
    ylab(ylabel))
}

# Plot ALL scatter plots and boxplots by wine quality rating
capstone_all_scatter_boxplot <- function(df) {
  columns <- colnames(df)
  for (i in 1:length(columns)) {
    print(ggplot(df, aes(x = factor(df$binary), y = df[,i])) +
          #  geom_point(alpha = .2) +
          #  geom_jitter() +
            geom_boxplot(width = .25, outlier.color = "Red") +
            xlab("binary value") +
            ylab(columns[i]) +
            facet_grid(. ~ color))
  }
}

#--------------Frequency and count functions-------------------------

# Create frequency table to show how many of each wine quality rating
# qual_freq_percent <- mutate(qual_freq, "percent" = paste(round((100 * freq/sum(freq)), 0), "%"))

freq_percent <- function(df, x) {
  qual_freq <- count(df, x) # col format 'column name'
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

#--------------chi-square test----------------------

# chi-square test with quality rating
chi.sq <- function(df) {
  columns <- colnames(df)
  # pvalue.vec <- numeric()
  pvalue.df <- data.frame(matrix(ncol = 2, nrow = 11))
  for (i in 2:12) {
    test.result <- chisq.test(table(df[,13], df[,i]))
   # pvalue.vec[i - 1] <- test.result$p.value
    pvalue.df[i - 1, 2] <- test.result$p.value
    pvalue.df[i - 1, 1] <- columns[i]
  }
  # names(pvalue.vec) <- columns[2:12]
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
  for (i in 2:12) { # Start at column 2 since column 1 is color
    cor.test <- cor(df[,13], df[,i])
    cor.df[i - 1, 2] <- cor.test
    cor.df[i - 1, 1] <- columns[i]
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
  for (i in 2:length(columns)) {
    quantile.variable <- quantile(df[,i], probs = c(.01, .05, .1, .90, .99, 1))
    tab <- data.frame("variable" = columns[i], quantile.variable)
    print(tab)
  }
}
