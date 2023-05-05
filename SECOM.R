# I Install/Load required packages --------------------------------------------
if (!require("pacman")) install.packages("pacman")
pacman::p_load(tidyverse, rlang, Hmisc, lubridate, corrplot, janitor)

# II Check/Set working directory ----------------------------------------------
getwd()

# Set working directory to your desktop
# setwd("C:/Users/You/Desktop")
setwd("C:/Users/SimonaPaskaleva/Desktop")
# Make sure to download the data files to your desktop

# III Load data into R --------------------------------------------------------

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

# IV Descriptive analysis------------------------------------------------------ 

# 1 Calculate volatility of each feature ----------------------------------------
# 1.1 Calculate variance
# Function to calculate the variance of a single feature
feature_var <- function(x) {
  value.var <- var(x, na.rm = TRUE)
  return(value.var)
}
feature_var(secom$feature10)

# Function to calculate the variance of all features
feature_var_all <- function(x) {
  column1 <- colnames(x)
  value_var_all <- data_frame(column1, apply(x, 2, var, na.rm = T)) %>%
    rename(feature = 1, variance = 2)
  return(value_var_all)
}
feature_var_all(secom)
# Select only the feature columns 
data_hist_var <- data_frame(feature_var_all(secom[,!names(secom) %in% c("label","date")]))

# Create histogram of volatility
hist.var <- data_hist_var %>%
  ggplot(aes(variance))+
  geom_histogram(col = "black",
                 fill = "navyblue",
                 alpha = 0.7)+
  labs(title="Histogram of Feature Volatility",
       x = "Feature Variance",
       y = "Frequency")+
  theme_bw()
print(hist.var)

# Majority of features have relatively low volatility
# Check number of features with variance of zero
data_hist_var %>%
  filter(variance == 0) %>%
  count()
# 116 features with variance of 0

# 1.2 Calculate coefficient of variance
# Function to calculate the coefficient
CV <- function(x) {
  (sd(x, na.rm = T)/mean(x, na.rm = T))*100 
}
# Apllying the CV function to the whole dataset
feature_cv_all <- function(x) {
  column1 <- colnames(x)
  value_coef_var <- data_frame(column1, apply(x, 2, CV)) %>%
    rename(feature = 1, coef_var = 2)
  return(value_coef_var)
}
data_hist_cv <- feature_cv_all(secom[,!names(secom) %in% c("label","date")])

# Histogram of the coefficients of variance
hist.cv <- data_hist_cv %>%
  ggplot(aes(coef_var))+
  geom_histogram(col = "black",
                 fill = "navyblue",
                 alpha = 0.7)+
  labs(title="Histogram of Feature Volatility",
       x = "Coefficient of variance",
       y = "Frequency")+
  theme_bw()
print(hist.cv)
summary(data_hist_cv)

# Zoom in
hist.cv.zoom <- data_hist_cv %>%
  filter(coef_var > -quantile(data_hist_cv$coef_var, 0.75, na.rm = T) &
           coef_var < quantile(data_hist_cv$coef_var, 0.75, na.rm = T)) %>%
  ggplot(aes(coef_var))+
  geom_histogram(col = "black",
                 fill = "navyblue",
                 alpha = 0.7)+
  labs(title="Histogram of Feature Volatility",
       x = "Coefficient of variance",
       y = "Frequency")+
  theme_bw()
print(hist.cv.zoom)

# 2 Missing values ------------------------------------------------------------
# 2.1 Check for missing values in each column
missing_cols <- function(x) {
  cols_missing <- data_frame(feature = colnames(x),
                             missing_values = colSums(is.na(x)),
                             percent_missing = round((missing_values / nrow(x))*100,2)) %>%
    filter(missing_values > 0) %>%
    arrange(desc(percent_missing)) %>%
    print()
}
missing_cols(secom)
# 538 features with missing values

missing_cols(secom) %>%
  filter(percent_missing > 50) %>%
  nrow()
# 28 features with over 50% missing values

# 2.2 Check for missing values in each row
missing_rows <- function(x) {
  row_missing <- data_frame(row_id = seq.int(nrow(x)),
                            missing_values = rowSums(is.na(x)),
                            percent_missing = round((missing_values/length(x))*100,2)) %>%
    print()
}
missing_rows(secom)
# There are no complete rows

missing_rows(secom) %>%
  filter(percent_missing <= 10) %>%
  nrow()
# 1533 rows have <= 10% missing values

# 2.3 Create a histogram of percentages of missing values in columns
hist.missing.cols <- 
  ggplot(missing_cols(secom), aes(percent_missing)) +
  geom_histogram(binwidth = 1, 
                 color = "black", 
                 fill = "navyblue", 
                 alpha = 0.7) +
  labs(title = "Histogram of Percentages of Missing Values per Column", 
       x = "% Missing Values", 
       y = "Count") +
  theme_bw()
print(hist.missing.cols)

# 2.4 Create a histogram of percentages of missing values in rows
hist.missing.rows <- 
  ggplot(missing_rows(secom), aes(percent_missing)) +
  geom_histogram(binwidth = 1, 
                 color = "black", 
                 fill = "navyblue", 
                 alpha = 0.7) +
  labs(title = "Histogram of Percentages of Missing Values per Row", 
       x = "% Missing Values", 
       y = "Count") +
  theme_bw()
print(hist.missing.rows)

# 3 Frequency distribution of target values -----------------------------------
# 3.1 Isolate the target variable 'label' to create a frequency table of target variable
target_freq <- function(x, col1) {
  col1 <- enquo(col1)
  target_freq_table <- x %>%
    select(!!col1) %>%
    group_by(x[1]) %>%
    mutate(frequency = n(),
           percentage = (frequency/nrow(x))*100,
           label = ifelse(!!col1 == -1 , "Pass", "Fail")) %>%
    distinct()
  as_tibble(target_freq_table)
}
target_freq(secom, label)

# 3.2 Create a bar chart of the frequency distribution of target variable
bar.target.freq <- 
ggplot(target_freq(secom, label), aes(x = label, y = frequency, fill = label))+
  geom_bar(stat = "identity", 
           color = "black",
           alpha = 0.7)+
  labs(title = "Frequency Distribution of Target Variable",
       x = "Label",
       y = "Frequency")+
  theme_bw() +
  scale_fill_manual(values = c("darkred", "darkgreen"))
print(bar.target.freq)

# 4 Correlation matrix --------------------------------------------------------
# Calculate correlations of all features as a matrix of the type 590:590

# 4.1 and 4.2 NOT NEEDED as of yet
# 4.1 Check for variables with constant values 
# Reason: if one variable has constant values (variance = 0) then the correlation coefficient will be 0
# 116 features with a variance of zero
# Create character vector of features with a variance of 0
# feature_var_0 <- data_hist_var %>%
#  filter(variance == 0) %>%
#  select(feature) %>% 
#  pull(feature)
# pull() extracts the filtered values into a character vector
# Check the class to verify that it is a character vector
# class(feature_var_0)

# Remove features with a variance of 0
# feature_corr <- secom %>%
#  select(-all_of(feature_var_0))
# Verify that the specified features (e.g. feature6) have been removed
# colnames(feature_corr)

# 4.2 Remove features with over 50% missing values
# Create character vector of features to exclude
# feature_miss_50 <- missing_cols(secom) %>%
#  filter(percent_missing >= 50) %>%
#  select(column) %>%
#  pull(column)

# Remove features with over 50% missing values
#feature_corr2 <- feature_corr %>%
#  select(-all_of(feature_miss_50))

# Verify that the specified features (e.g. feature158) have been removed
#colnames(feature_corr2)

# 4.3 Calculate correlation coefficients 
# To exclude features with variance = 0 and missing values > 50% use feature_corr2
feature_corr_all <- function(x) {
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
  p = 0.05
  corr_mat_sig <- subset(df_merge, p.val < p)
  corr_mat_sig_n <- corr_mat_sig %>%
    nrow()
  # 38980 significant correlation coefficients of features, including variance = 0
  # and missing values > 50%
  # 42632 significant correlation coefficients of all features
  
  # Number of perfect correlations
  corr_mat_per_n <- corr_mat_sig %>%
    filter(corr == 1 | corr == -1) %>%
    nrow()
  # 14 perfect correlation coefficients between different features including variance = 0
  # and missing values > 50%
  # 30 perfect correlation coefficients of all features
  
  # Filter for strong correlations
  corr_mat_str_n <- corr_mat_sig %>%
    select(-p.val) %>%
    filter(corr >= 0.8 | corr <= -0.8) %>%
    nrow()
  # 1190 strong correlation coefficients between different features including variance = 0
  # and missing values > 50%
  # 2292 strong correlation coefficients of all features
  
  # Reshape dataframe into matrix
  # Matrix not useful at this time
  # corr_mat2 <- reshape2::acast(corr_mat2, features1~features2, value.var="corr")
  # feature_correlogram <- corrplot(corr_mat2, tl.cex = 0.01)
  
  # Calculate correlation coefficients
  corr_coef_pos <- corr_mat_sig %>%
    filter(corr >= 0) %>%
    arrange(corr) %>%
    mutate(row_id = row_number())
  corr_pos_graph <- ggplot(corr_coef_pos,aes(x = (row_id/nrow(corr_coef_pos))*100, y = corr)) +
    geom_line(size = 1,
              color = "navyblue",
              alpha = 0.7)+
    labs(title = "Positive Correlation coefficients per Feature Pair",
         x = "% of Feature Pairs",
         y = "Correlation coefficient") +
    theme_bw()
  print(corr_pos_graph)
  
  corr_coef_neg <- corr_mat_sig %>%
    filter(corr <= 0) %>%
    arrange(desc(corr)) %>%
    mutate(row_id = row_number())
  corr_neg_graph <- ggplot(corr_coef_neg,aes(x = (row_id/nrow(corr_coef_neg))*100, y = corr)) +
    geom_line(size = 1,
              color = "navyblue",
              alpha = 0.7) +
    labs(title = "Negative Correlation coefficients per Feature Pair",
         x = "% of Feature Pairs",
         y = "Correlation coefficient") +
    theme_bw()
  print(corr_neg_graph)
  return_list <- list(signifcant = corr_mat_sig_n, perfect = corr_mat_per_n, strong = corr_mat_str_n)
  return(return_list)
}
feature_corr_all(secom[3:592])

# 5 Duplicates ----------------------------------------------------------------
# 5.1 Duplicate columns
dup_cols_main <- secom[duplicated(as.list(secom))]
dup_cols_main
# 104 duplicate features with values of 0

# 5.2 Duplicate rows
dup_rows_main <- subset(secom,duplicated(secom))
dup_rows_main

# 6 Negative values -----------------------------------------------------------
neg_val <- secom %>%
  select(-c("label")) %>%
  keep(~any(.x<0 & !is.na(.x)))
tibble(neg_val)
# 36 features have negative values


# V Split data into training and test set -------------------------------------
# Make it reproducible
set.seed(42)

# Create an id
secom$id <- seq.int(nrow(secom))
secom
# 80:20 train to test split
train_set <- secom %>%
  group_by(label) %>%
  mutate(num_rows = n()) %>%
  sample_frac(size = 0.8, 
              replace = FALSE,
              weight = num_rows) %>%
  ungroup()
test_set <- anti_join(secom, train_set, by = "id")

# Remove unnecessary id column
secom <- secom[,!names(secom) %in% c("id")]
tibble(secom)

train_set <- train_set[,!names(train_set) %in% c("id","num_rows")]
tibble(train_set)

test_set <- test_set[,!names(test_set) %in% c("id","num_rows")]
tibble(test_set)
# Verify that distribution of target variable is the same in both sets
target_freq(secom,label)
# Data: 93.4 % pass cases and 6.64 fail cases
target_freq(train_set,label)
# Train set: 93.4% pass cases and 6.64% fail cases
target_freq(test_set, label)
# Test set: 93.3% pass cases and 6.69% fail cases

bar.target.freq.train <- 
  ggplot(target_freq(train_set, label), aes(x = label, y = frequency, fill = label))+
  geom_bar(stat = "identity", 
           color = "black",
           alpha = 0.7)+
  labs(title = "Frequency Distribution of Target Variable",
       x = "Label",
       y = "Frequency")+
  theme_bw() +
  scale_fill_manual(values = c("darkred", "darkgreen"))
print(bar.target.freq.train)

# VI Descriptive analysis of training data ------------------------------------
# 1 Identify duplicates
# 1.1 Duplicate rows 
dup_rows <- subset(train_set,duplicated(train_set))
# 0 duplicate rows

#1.2 Duplicate columns
dup_cols <- train_set[duplicated(as.list(train_set))]
# 104 duplicated columns
  
# 2 Identify missing values
# 2.1 Missing values in columns
miss_col_train <- missing_cols(train_set)

# Define threshold
# Check the variance and correlation of the 28 features with over 50% missing values
miss_50_list <- miss_col_train %>%
  filter(percent_missing >= 50) %>%
  select(column) %>%
  pull(column)

miss_50_cv <- feature_cv_all(train_set[miss_50_list])
miss_50_val <- miss_col_train %>%
  filter(percent_missing >= 50)
miss_50_var <- merge(miss_50_cv, miss_50_val, by = "feature")

miss_50_var %>%
  ggplot(aes(coef_var))+
  geom_histogram(color = "black",
                 fill = "navyblue",
                 alpha = 0.7,
                 binwidth = 30)+
  labs(title = "Histogram of CV of features with missing values",
       x = "Coefficient of variance",
       y = "Frequency")+
  theme_bw()

# Check the correlation of the 28 features
miss_50_cor <- train_set %>%
  select(all_of(c(miss_50_list,"label")))

corr_mat <- corrplot(cor(as.matrix(miss_50_cor), use ="pairwise.complete.obs"),
         tl.cex = 0.5,
         method = "circle",
         diag = F, 
         type = "lower")
print(corr_mat)

# 2.2 Missing values in rows
missing_rows(train_set)
