# I Install/Load required packages
if (!require("pacman")) install.packages("pacman")
pacman::p_load(tidyverse, rlang, Hmisc, lubridate, corrplot)

# -----------------------------------------------------------------------------

# II Check/Set working directory
getwd()

# Set working directory to your desktop
# setwd("C:/Users/You/Desktop")
setwd("C:/Users/SimonaPaskaleva/Desktop")
# Make sure to download the data files to your desktop

# -----------------------------------------------------------------------------

# III Load data into R
# 1 SECOM dataset
# Use underscores for functions and datasets and points for variables
# Separator/delimiter is a space, decimal separator is a point
# Missing values are displayed as NaN
secom_data <- read.table("secom.data", 
                         header = FALSE, 
                         sep = " ", 
                         dec = ".", 
                         na.strings = "NaN")
# Check that data is imported correctly
tibble(secom_data)

# 2 Labels
# Separator/delimitor is a space, no decimal separator
secom_labels <- read.table("secom_labels.data", 
                           header = FALSE, 
                           sep = " ", 
                           quote ="'\"")
# Check that data is imported correctly
tibble(secom_labels)

# Rename column names of secom.labels dataset in order to merge the two datasets
secom_labels_renamed <- secom_labels %>%
  rename(label = 1, date = 2)

# Rename column names of secom.data dataset to display feature
secom_data_renamed <- secom_data %>% 
  set_names(~ str_replace_all(
    colnames(secom_data), "V", "feature")) 

# 3 Merge secom dataset with the labels data set
df1 <- data_frame(secom_data_renamed)
df2 <- data_frame(secom_labels_renamed)
secom_merged <- data_frame(bind_cols(df2, df1))
tibble(secom_merged)

# Convert date from character variable to datetime variable
secom <- secom_merged %>%
  mutate(date = dmy_hms(date))
tibble(secom)
View(secom)

# ----------------------------------------------------------------------------- 

# IV Descriptive analysis
# 1 Calculate variance of each feature
# Function to calculate the variance of a single feature
feature_var <- function(x) {
  value.var <- var(x, na.rm = TRUE)
  return(value.var)
}
feature_var(secom$feature10)

# Function to calculate the variance of all features
feature_var_all <- function(x) {
  column1 <- colnames(secom[3:592])
  value_var_all <- data_frame(column1, apply(x, 2, var, na.rm = T)) %>%
    rename(feature = 1, variance = 2)
  return(value_var_all)
}
# Select only the feature columns 3:592
data_hist <- data_frame(feature_var_all(secom[3:592]))

# Create histogram of variances/volatility
hist_var <- data_hist %>%
  ggplot(aes(variance))+
  geom_histogram(col = "black",
                 fill = "navyblue",
                 alpha = 0.7)+
  labs(title="Histogram of Feature Volatility",
       x = "Feature Variance",
       y = "Frequency")
print(hist_var)

# Majority of features have relatively low volatility
# Check number of features with variance of zero
data_hist %>%
  filter(variance == 0) %>%
  count()
# 116 features with variance of zero

# -----------------------------------------------------------------------------

# 2 Missing values
# 2.1 Check for missing values in each column
col.missing <- colSums(is.na(secom))
missing.cols <- names(which(col.missing > 0))
num.missing.cols <- col.missing[missing.cols]
percent.missing.cols <- round(num.missing.cols / nrow(secom) * 100, 2)

# 2.2 Create a table of missing values by column
missing_data_cols <- data.frame(column = missing.cols, 
                                missing_values = num.missing.cols, 
                                percent_missing = percent.missing.cols)
missing_table_cols <- missing_data_cols[order(missing_data_cols$missing_values, decreasing = TRUE), ]
print(head(missing_table_cols,200))

# 2.3 Create a histogram of percentages of missing values
library(ggplot2)
hist_missing_values <- 
  ggplot(data = data.frame(percent_missing_cols), aes(x = percent_missing_cols)) +
  geom_histogram(binwidth = 1, 
                 color = "black", 
                 fill = "navyblue", 
                 alpha = 0.7) +
  labs(title = "Histogram of Percentages of Missing Values per Column", 
       x = "% Missing Values", 
       y = "Count")
print(hist_missing_values)

# -----------------------------------------------------------------------------

# 3 Frequency distribution of target values
# 3.1 Isolate the target variable 'label' to create a frequency table of target variable

target_freq_table <- data.frame(label = names(table(secom$label)), frequency = as.vector(table(secom$label)))
target_freq_table$label <- ifelse(target_freq_table$label == -1, "Pass", "Fail")

# 3.2 Create a bar chart of the frequency distribution of target variable
bar_target_freq <- 
ggplot(target_freq_table, aes(x = label, y = frequency, fill = label)) +
  geom_bar(stat = "identity")  +
  xlab("Label") +
  ylab("Frequency") +
  ggtitle("Frequency Distribution of Target Variable") +
  theme_bw() +
  scale_fill_manual(values = c("navyblue", "navyblue"))
print(bar_target_freq)

# -----------------------------------------------------------------------------
# 4 Correlation matrix
# Calculate correlations of all features as a matrix of the type 590:590

# 4.1 Check for variables with constant values 
# Reason: if one variable has constant values (variance = 0) then the correlation coefficient will be 0
data_hist %>%
  filter(variance == 0) %>%
  count()
# 116 features with a variance of zero
# Create character vector of features with a variance of 0
feature_var_0 <- data_hist %>%
  filter(variance == 0) %>%
  select(feature) %>% 
  pull(feature)
# pull() extracts the filtered values into a character vector
# Check the class to verify that it is a character vector
class(feature_var_0)

# Remove features with a variance of 0
feature_corr <- secom %>%
  select(-all_of(feature_var_0))
# Verify that the specified features (e.g. feature6) have been removed
colnames(feature_corr)

# 4.2 Remove features with over 50% missing values
# Reason: 
# Create character vector of features to exclude
feature_miss_50 <- missing_table_cols %>%
  filter(percent_missing >= 50) %>%
  select(column) %>%
  pull(column)

# Remove features with over 50% missing values
feature_corr2 <- feature_corr %>%
  select(-all_of(feature_miss_50))

# Verify that the specified features (e.g. feature158) have been removed
colnames(feature_corr2)

# WIP!!! 4.3 Calculate correlation coefficients and exclude correlations under the treshhold of 0.05
feature_corr_all <- function(x, p = 0.05) {
  corr_mat <- rcorr(as.matrix(x))
  # Extract correlation coefficients
  df_r <- corr_mat$r %>%
    as.data.frame() %>% 
    rownames_to_column("features1") %>%
    as_tibble() %>% 
    pivot_longer(cols = -features1, names_to = "features2", 
                 values_to = "corr")
  # Extract p-values
  df_p <- corr_mat$P %>%
    as.data.frame() %>% 
    rownames_to_column("features1") %>%
    as_tibble() %>% 
    pivot_longer(cols = -features1, names_to = "features2", 
                 values_to = "p.val")
  # Merge correlation coefficients and p-values into one table
  df_merge <- df_r %>%
    left_join(df_p) 
  
  # Remove the NA values (removes perfect correlation of feature with itself)
  df_merge <- na.omit(df_merge) 
  
  # Select significant correlation coefficients
  corr_mat_sig <- subset(df_merge, p.val < p) 
  corr_mat_sign_n <- corr_mat_sig %>%
    nrow() %>%
    print()
  # 38980 significant correlation coefficients
  
  # Number of perfect correlations
  corr_mat_per_n <- corr_mat_sig %>%
    filter(corr == 1 | corr == -1) %>%
    nrow()
  # 14 perfect correlation coefficients of 1/-1 between different features
  
  # Filter for strong correlations
  corr_mat2 <- corr_mat_sig %>%
    select(-p.val) %>%
    filter(corr >= 0.8 | corr <= -0.8)
  
  corr_mat2 <- reshape2::acast(corr_mat2, features1~features2, value.var="corr")
  feature_correlogram <- corrplot(corr_mat2, tl.cex = 0.01)
}

feature_corr_all(feature_corr2[3:448])

# V Split data into training and test set

# VI Descriptive analysis of training data
# 1 Identify duplicates
# 2 Identify missing values
