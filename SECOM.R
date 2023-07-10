# I Install/Load required packages --------------------------------------------
if (!require("pacman")) install.packages("pacman")
pacman::p_load(tidyverse, rlang, Hmisc, lubridate, corrplot, VIM, mice, Boruta, EFAtools, FactoMineR, psych, ROSE, smotefamily, randomForest, caret, ROCR)

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

saveRDS(secom, "secom.RDS")
secom <- readRDS("secom.RDS")

# IV Split data into training and test set -------------------------------------
# 1 Frequency distribution of target values -----------------------------------
# 1.1 Isolate the target variable 'label' to create a frequency table of target variable
target_freq <- function(x, col1) {
  col1 <- enquo(col1)
  target_freq_table <- x %>%
    dplyr::select(!!col1) %>%
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

#2 Split data into training and test set
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
saveRDS(train_set,"train_set.RDS")
train_set <- readRDS("train_set.RDS")

test_set <- test_set[,!names(test_set) %in% c("id","num_rows")]
tibble(test_set)
saveRDS(test_set, "test_set.RDS")
test_set <- readRDS("test_set.RDS")
# Verify that distribution of target variable is the same in both sets
target_freq(secom,label)
# Data: 93.4 % pass cases and 6.64 fail cases
target_freq(train_set,label)
# Train set: 93.4% pass cases and 6.64% fail cases
target_freq(test_set, label)
# Test set: 93.3% pass cases and 6.69% fail cases

# V Descriptive analysis ------------------------------------------------------ 

# 1 Calculate volatility of each feature in train set -------------------------
# 1.1 Calculate variance
# Function to calculate the variance of a single feature
feature_var <- function(x) {
  value.var <- var(x, na.rm = TRUE)
  return(value.var)
}
feature_var(train_set$feature10)

# Function to calculate the variance of all features
feature_var_all <- function(x) {
  column1 <- colnames(x)
  value_var_all <- data_frame(column1, apply(x, 2, var, na.rm = T)) %>%
    rename(feature = 1, variance = 2)
  hist.var <- value_var_all %>%
    ggplot(aes(variance))+
    geom_histogram(col = "black",
                   fill = "navyblue",
                   alpha = 0.7)+
    labs(title="Histogram of Feature Volatility",
         x = "Feature Variance",
         y = "Frequency")+
    theme_bw()
  print(hist.var)
  return(value_var_all)
}
# Select only the feature columns 
data_hist_var <- feature_var_all(train_set[,!names(train_set) %in% c("label","date")])

# Majority of features have relatively low volatility
# Check number of features with variance of zero
data_hist_var %>%
  filter(variance == 0) %>%
  count()
# 122 features with variance of 0

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
data_hist_cv <- feature_cv_all(train_set[,!names(train_set) %in% c("label","date")])

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
# Function for calculating values in columns
missing_cols <- function(x) {
  cols_missing <- data_frame(feature = colnames(x),
                             missing_values = colSums(is.na(x)),
                             percent_missing = round((missing_values / nrow(x))*100,digits = 4)) %>%
    filter(missing_values > 0) %>%
    arrange(desc(percent_missing))
  return(cols_missing)
}
miss_col <- missing_cols(train_set)
sum(miss_col$missing_values)
# 538 features with missing values and 33521 total missing values

miss_col %>%
  filter(percent_missing > 50) %>%
  nrow()
# 28 features with over 50% missing values

# Define threshold
# Check the variance and correlation of the 28 features with over 50% missing values
miss_50_list <- miss_col %>%
  filter(percent_missing >= 50) %>%
  dplyr::select(feature) %>%
  pull(feature)

miss_50_cv <- feature_cv_all(train_set[miss_50_list])
miss_50_val <- miss_col %>%
  filter(percent_missing >= 50)
miss_50_var <- merge(miss_50_cv, miss_50_val, by = "feature") %>%
  arrange(desc(coef_var))
# Set treshold to 65% missing values due to high variance 

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
  dplyr::select(all_of(c(miss_50_list,"label")))

corr_mat <- corrplot(cor(as.matrix(miss_50_cor), use ="pairwise.complete.obs"),
                     tl.cex = 0.5,
                     method = "circle",
                     diag = F, 
                     type = "lower")
print(corr_mat)


# 2.2 Check for missing values in each row
# Function for calculating missing values in rows
missing_rows <- function(x) {
  row_missing <- data_frame(row_id = seq.int(nrow(x)),
                            missing_values = rowSums(is.na(x)),
                            percent_missing = round((missing_values/length(x))*100,2)) %>%
    print()
}
miss_rows <- missing_rows(train_set)
# There are no complete rows

missing_rows(train_set) %>%
  filter(percent_missing <= 10) %>%
  nrow()
# 1230 rows have <= 10% missing values

# 2.3 Create a histogram of percentages of missing values in columns
hist.missing.cols <- 
  ggplot(missing_cols(train_set), aes(percent_missing)) +
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
  ggplot(missing_rows(train_set), aes(percent_missing)) +
  geom_histogram(binwidth = 1, 
                 color = "black", 
                 fill = "navyblue", 
                 alpha = 0.7) +
  labs(title = "Histogram of Percentages of Missing Values per Row", 
       x = "% Missing Values", 
       y = "Count") +
  theme_bw()
print(hist.missing.rows)


# 3 Correlation analysis ------------------------------------------------------
# Calculate correlation coefficients 
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
  
  # Select significant correlation coefficients
  p = 0.05
  corr_mat_sig <- subset(df_merge, p.val < p)
  corr_mat_sig_n <- corr_mat_sig %>%
    nrow()
  
  # Number of perfect correlations
  corr_mat_per_n <- corr_mat_sig %>%
    filter(corr == 1 | corr == -1) %>%
    nrow()
  
  # Filter for strong correlations
  corr_mat_str_n <- corr_mat_sig %>%
    dplyr::select(-p.val) %>%
    filter(corr >= 0.8 | corr <= -0.8) %>%
    nrow()
  
  # Calculate correlation coefficients
  corr_coef_pos <- corr_mat_sig %>%
    filter(corr >= 0) %>%
    arrange(corr)
  corr_coef_pos <- corr_coef_pos %>% 
    mutate(row_id = row_number(),
           percentage = (row_id/nrow(corr_coef_pos))*100)
  
  corr_coef_pos %>%
    filter(corr <= 0.81) %>%
    arrange(desc(corr)) %>%
    print(n = 1)
  corr_pos_graph <- ggplot(corr_coef_pos,aes(x = percentage, y = corr)) +
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
    arrange(desc(corr))
  corr_coef_neg <- corr_coef_neg %>%
    mutate(row_id = row_number(),
           percentage = (row_id/nrow(corr_coef_neg))*100)
  corr_neg_graph <- ggplot(corr_coef_neg,aes(x = percentage, y = corr)) +
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
# Results for all features
feature_corr_all(train_set[,!names(train_set) %in% c("label","date")])

# Run VI Dimensionality reduction first
# Results after removal of features with variance of 0 and missing values > 65%
feature_corr_all(train_red[,!names(train_red) %in% c("label","date")])

# 4 Duplicates ----------------------------------------------------------------
# 4.1 Duplicate columns
dup_cols_main <- train_set[duplicated(as.list(train_set))]
dup_cols_main
# 109 duplicate features with values of 0

# 4.2 Duplicate rows
dup_rows_main <- subset(train_set,duplicated(train_set))
dup_rows_main
# No duplicate rows 

# 5 Negative values -----------------------------------------------------------
neg_val <- train_set %>%
  dplyr::select(-c("label")) %>%
  keep(~any(.x<0 & !is.na(.x)))
tibble(neg_val)
# 36 features have negative values

# VI Rough dimension reduction
dim_reduce <- function(x) {
  feature_var_x <- feature_var_all(x)
  # Create character vector of features with a variance of 0
  # pull() extracts the filtered values into a character vector
  feature_var_0 <- feature_var_x %>%
    filter(variance == 0) %>%
    dplyr::select(feature) %>%
    pull()
  # Remove features with a variance of 0
  x_red_var <- x %>%
    dplyr::select(-all_of(feature_var_0))
  # Remove features with over 65% missing values
  # Create character vector of features to exclude
  feature_miss_x <- missing_cols(x) 
  feature_miss_65 <- feature_miss_x %>%
    filter(percent_missing >= 65) %>%
    dplyr::select(feature) %>%
    pull()
  # Remove features with over 65% missing values
  x_red <- x_red_var %>%
    dplyr::select(-all_of(feature_miss_65))
  return(x_red)
}
train_red <- dim_reduce(train_set)
# Overall dimensionality reduction of 131 features
# Save reduced train set as RDS file
saveRDS(train_red, "train_red.RDS")
train_red <- readRDS("train_red.RDS")


# VII Outliers identification and handling ------------------------------------
# 1 Outlier identification ----------------------------------------------------
# Function to identify outliers outside 4 standard deviation boundaries
identify_outliers <- function(data, handling) {
  # Initialize an empty data frame to store results
  outlier_counts <- data.frame(variable = character(),
                               outlier_count = numeric(),
                               stringsAsFactors = FALSE)
  
  # Loop through each variable in the data frame
  for (col in names(data)) {
    # Calculate mean and standard deviation of the variable
    var_mean <- mean(data[[col]], na.rm = TRUE)
    var_sd <- sd(data[[col]], na.rm = TRUE)
    
    # Calculate the upper and lower boundaries for outliers
    upper_bound <- var_mean + 4 * var_sd
    lower_bound <- var_mean - 4 * var_sd
    
    # Count the number of outliers in the variable
    num_outliers <- sum(data[[col]] > upper_bound | data[[col]] < lower_bound, na.rm = TRUE)
    
    # Add the variable and outlier count to the result data frame
    outlier_counts <- rbind(outlier_counts, data.frame(variable = col,
                                                       outlier_count = num_outliers,
                                                       stringsAsFactors = FALSE))
  }
  
  # Filter the data frame to include only variables with outliers
  outlier_counts <- subset(outlier_counts, outlier_count >= 1)

  # Return the data frame with outlier counts
  tibble(outlier_counts)
}

# Apply identify_outliers function to the training set
outlier_results <- identify_outliers(train_red[,!names(train_red) %in% c("label","date")])
sum(outlier_results$outlier_count)
#There are 368 features that have outliers outside the 4S-boundaries and a total of 2597 outlier values

# 2 Outlier handling ---------------------------------------------------------
# FUnction to either replace outliers with 4S boundary or with NAs
outlier_replace <- function(x, method = c("NA","SD")) {
  # Partial string matching of input for method
  method <- match.arg(method)
  # Calculate mean and standard deviation
  mu = mean(x, na.rm = TRUE)
  s = sd(x, na.rm = TRUE)
  # Calculate lower and upper 4s boundary
  upper = mu + 4*s
  lower = mu - 4*s
  is_upb <- x > upper 
  is_lwb <- x < lower
  # If-else statement for outlier handling, if no method is given it defaults to NA
  if (method == "NA") {
    x[is_upb | is_lwb] <- NA
  } else {
    x[is_upb] <- upper
    x[is_lwb] <- lower
  }
  x
}
# Apply function to all numeric columns
# 2.1 Set to NAs
outlier_na <- train_red %>%
  mutate(across(where(is.numeric),outlier_replace))
# Check results based on NA method (24051 original NAs in train_red + 2597 outlier NAs)
sum(is.na(outlier_na))
# 26648 NAs

# 2.2 Set to 4SD
# Lamda method
outlier_sd <- train_red %>%
  mutate(across(where(is.numeric), ~ outlier_replace(.x, method = "SD")))
# Specify function method
outlier_sd <- train_red %>%
  mutate(across(where(is.numeric), function(x) outlier_replace(x, method = "SD")))

# VIII Missing value imputation -----------------------------------------------
# 1 Missingness pattern -------------------------------------------------------
# Check for missing values in reduced training set
miss_red <- missing_cols(train_red)
# 408 features with missing variables

# Arrange features with missing values in ascending order and extract
# Assumption: features are sensors and sensor numbers are their number in the checking sequence
miss_list <- miss_red %>%
  filter(percent_missing > 0) %>%
  arrange(as.integer(substring(feature,8))) %>%
  dplyr::select(feature) %>%
  pull(feature)
as_tibble(miss_list)

# Check pattern
md.pattern(train_red[,names(train_red) %in% miss_list])
# There are some patterns in the missigness 
# Not MCAR > complete case analysis not possible without bias
# Single imputaton for MAR
# Multiple imputation methods for MNAR

# 2 Scaling -------------------------------------------------------------------
# Because some imputation methods are distance based
scaled <- function(x) {
  return((x-min(x, na.rm = T))/(max(x, na.rm = T)-min(x, na.rm = T)))
}
# 2.2.1 Scaling after outlier handling
scaled_na <- scaled(outlier_na[,!names(outlier_na) %in% c("label","date")])
scaled_sd <- scaled(outlier_sd[,!names(outlier_sd) %in% c("label","date")])

# 2.2.2 Scaling before outlier handling
scaled_train_red <- scaled(train_red[,!names(train_red) %in% c("label","date")])

# 3 Imputation ----------------------------------------------------------------
# 3.1 Hot deck method ---------------------------------------------------------
# Non-parametric, makes no assumptions about the data distribution
# Can therefore be used before AND after outlier handling
# 3.1.1 Imputation after outlier handling
# Computation time <30s 
hot_na <- hotdeck(outlier_na, impNA = T, imp_var = F)
# Check that imputation was successful
sum(is.na(hot_na))

hot_sd <- hotdeck(outlier_sd, impNA = T, imp_var = F)
sum(is.na(hot_sd))

# Check effects of imputation
# For NA set
hot_na_var <- feature_var_all(hot_na)
hot_na_corr <- feature_corr_all(hot_na[,!names(hot_na) %in% c("label","date")])
# For SD set
hot_sd_var <- feature_var_all(hot_sd[,!names(hot_sd) %in% c("label","date")])
hot_sd_corr <- feature_corr_all(hot_sd)

# 3.1.2 Imputation before outlier hanndling
hot_red <- hotdeck(train_red, impNA = T, imp_var = F)
sum(is.na(hot_red))

# Outlier handling
train_hot_outlier <- hot_red %>%
  mutate(across(where(is.numeric),~ outlier_replace(.x, method = "SD")))
as_tibble(train_hot_outlier)

# Check effects
train_hot_outlier_var <- feature_var_all(train_hot_outlier)
train_hot_outlier_corr <- feature_corr_all(train_hot_outlier[,!names(train_hot_na) %in% c("label","date")])

# 3.2 kNN method --------------------------------------------------------------
# Non-parametric, makes no assumptions about the data distribution
# Can therefore be used before AND after outlier handling

# 3.2.1 Imputation after outlier handling
# Computation time ~ 5-6 min
# Merge label and date back together with the rest of the data for the imputation
knn_na <- kNN(bind_cols(outlier_na[,names(outlier_na) %in% c("label","date")],scaled_na), 
              k = 5, impNA = T, imp_var = F)
as_tibble(knn_na)
sum(is.na(knn_na))
saveRDS(knn_na, "knn_na.RDS")
knn_na <- readRDS("knn_na.RDS")

# Merge label and date back together with the rest of the data for the imputation
knn_sd <- kNN(bind_cols(outlier_sd[,names(outlier_sd) %in% c("label","date")],scaled_sd), 
              k = 5, impNA = T, imp_var = F)
as_tibble(knn_sd)
sum(is.na(knn_sd))
saveRDS(knn_sd, "knn_sd.RDS")
knn_sd <- readRDS("knn_sd.RDS")

# Check effects of imputation
# Reverse scaling to check the effects of imputation 
reverse_scaling <- function(old_data, scaled_data) {
  min_o <- min(old_data, na.rm = T)
  max_o <- max(old_data, na.rm = T)
  reverse_scaled <- (scaled_data*(max_o-min_o))+min_o
  return(reverse_scaled)
}
# For NA set
train_knn_na <- reverse_scaling(outlier_na[,!names(outlier_na) %in% c("label","date")], knn_na[,!names(outlier_na) %in% c("label","date")])
train_knn_na <- as_tibble(bind_cols(knn_na[,names(knn_na) %in% c("label","date")],train_knn_na))
# Check effects
train_knn_na_var <- feature_var_all(train_knn_na)
train_knn_na_corr <- feature_corr_all(train_knn_na[,!names(train_knn_na) %in% c("label","date")])

# For SD set
train_knn_sd <- reverse_scaling(outlier_sd[,!names(outlier_sd) %in% c("label","date")], knn_sd[,!names(outlier_sd) %in% c("label","date")])
train_knn_sd <- as_tibble(bind_cols(knn_sd[,names(knn_sd) %in% c("label","date")],train_knn_sd))
# Check effects
train_knn_sd_var <- feature_var_all(train_knn_sd)
train_knn_sd_corr <- feature_corr_all(train_knn_sd[,!names(train_knn_sd) %in% c("label","date")])

# Save imputation files as RDS to reduce processing time
saveRDS(train_knn_na, "train_knn_na.RDS")
train_knn_na <- readRDS("train_knn_na.RDS")

saveRDS(train_knn_sd, "train_knn_sd.RDS")
train_knn_sd <- readRDS("train_knn_sd.RDS")

# 3.2.2 Imputation before outlier handling
# Merge label and date back together with the rest of the data for the imputation
knn_train_red <- kNN(bind_cols(train_red[,names(train_red) %in% c("label","date")],scaled_train_red), 
                     k = 5, impNA = T, imp_var = F)
as_tibble(knn_train_red)
sum(is.na(knn_train_red))

saveRDS(knn_train_red, "knn_train_red.RDS")
knn_train_red <- readRDS("knn_train_red.RDS")
# Reverse scaling
train_red_knn <- reverse_scaling(train_red[,!names(train_red) %in% c("label","date")], knn_train_red[,!names(knn_train_red) %in% c("label","date")])

# Outlier handling with 4S
train_knn_outlier <- train_red_knn %>%
  mutate(across(where(is.numeric),~ outlier_replace(.x, method = "SD")))
train_knn_outlier <- as_tibble(bind_cols(train_red[,names(train_red) %in% c("label","date")], train_red_knn))

# Check effects
train_knn_outlier_var <- feature_var_all(train_knn_outlier)
train_knn_outlier_corr <- feature_corr_all(train_knn_outlier[,!names(train_knn_outlier) %in% c("label","date")])

saveRDS(train_knn_outlier, "train_knn_outlier.RDS")
train_knn_outlier <- readRDS("train_knn_outlier.RDS")


# 3.3.2 Single imputation before outlier handling -----------------------------
# Use robust methods to suppress influence of outliers in the dataset
# Computation time > 2h so Will not be implemented

# 2.2.4 MICE ------------------------------------------------------------------
# Parametric, assumes mutlivariate normal distribution, can therefore only be used after outlier handling
# Computation time ~ 50 mins per iteration so will not be implemented


# IX Feature selection/reduction

# 1 Feature reduction with PCA ------------------------------------------------
# 1.1 Function to check if dataset is suitable for PCA
# 1.1.1 Check if suitable for PCA
# PCA_suitable <- function(x) {
#   KMO <- EFAtools::KMO(cor(as.matrix(x)))
#   bart <- EFAtools::BARTLETT(cor(as.matrix(x)), N = nrow(x))
#   return_list <- list("Kaiser-Meyer-Olkin (KMO) factor adequacy" = KMO$KMO,
#                       "Bartlett's test of sphericity" = bart)
#   return(return_list)
# }
# # 1.1 On hot deck imputed datasets
# PCA_suitable(hot_na[,!names(hot_na) %in% c("label","date")])
# # KMO: 0.679
# # Bartlett p < .001
# 
# PCA_suitable(hot_sd[,!names(hot_sd) %in% c("label","date")])
# # KMO: 0.690
# # Bartlett p < .001
# 
# PCA_suitable(train_hot_outlier[,!names(train_hot_outlier) %in% c("label","date")])
# # KMO: 0.689
# # Bartlett p < .001
# 
# # Z-transformation due to skewed data and correlation matrix as input
# hot_na_norm <- scale(hot_na[,!names(hot_na)%in%c("label","date")], scale = T, center = T)
# hot_sd_norm <- scaled(hot_sd[,!names(hot_sd)%in%c("label","date")], scale = T, center = T)
# hot_outlier_norm <- scaled(train_hot_outlier[,!names(train_hot_outlier)%in%c("label","date")], scale = T, center = T)
# 
# # Scree plots
# scree_na <- VSS.scree(hot_na_norm)
# scree_sd <- VSS.scree(hot_sd_norm)
# scree_outlier <- VSS.scree(hot_outlier_norm)
# 
# # Extraction
# PCA_extract <- function(x, method = c("Kaiser", "Variance")) {
#   pca <- PCA(x, graph = F)
#   eigen <- as.tibble(pca$eig)
#   # Set to 80 to avoid overfitting
#   if (method == "Variance") {
#     nf <- eigen %>%
#       rename(pov = 2, cpov = 3) %>%
#       filter(cpov <= 80) %>%
#       nrow()
#   } else {
#     nf <- eigen %>%
#       filter(eigenvalue >= 1) %>%
#       nrow()
#   }
#   pca_extract <- principal(x, nfactors = nf, covar = F, scores = T)
#   as.tibble(pca_extract$scores)
# }
# # NA_set
# # Kaiser criterion
# hot_na_k <- PCA_extract(hot_na_norm, "Kaiser")
# # Variance of at least 90%
# hot_na_var <- PCA_extract(hot_na_norm, "Variance")
# 
# # SD set
# # Kaiser criterion
# hot_sd_k <- PCA_extract(hot_sd_norm, "Kaiser")
# # Variance of at least 90%
# hot_sd_var <- PCA_extract(hot_sd_norm, "Variance")
# 
# # Reduced training set
# # Kaiser criterion
# hot_outlier_k <- PCA_extract(hot_outlier_norm, "Kaiser")
# # Variance of at least 90%
# hot_outlier_var <- PCA_extract(hot_outlier_norm, "Variance")
# 
# # 2.2 On kNN imputed datasets knn_na, knn_sd and knn_train_red 
# # 2.2.1 Kaiser-Meyer-Olkin (KMO) factor adequacy
# PCA_suitable(train_knn_na[,!names(train_knn_na) %in% c("label","date")])
# # KMO: 0.68 and p.valie < 0.01
# 
# PCA_suitable(train_knn_sd[,!names(train_knn_sd) %in% c("label","date")])
# # KMO: 0.695 and p.value < 0.01
# 
# PCA_suitable(train_knn_outlier[,!names(train_knn_outlier) %in% c("label","date")])
# KMO: 0.70 and p.value < 0.01

# Transformation and PCA to be done


# 2 Feature selection by BORUTA -----------------------------------------------

# use Boruta algorithm on the imputed dataset
set.seed(123456789)

# Build a function to perform BORUTA and obtain confirmed features------------

performBoruta <- function(data) {
  # Perform Boruta algorithm on scaled data
  boruta_result <- Boruta(
    label ~ .,
    data = data,
    pValue = 0.05,
    maxRuns = 300,
    doTrace = 2
  )
  
  # Take a call on tentative features
  boruta_tentative <- TentativeRoughFix(boruta_result)
  print(boruta_tentative)
  
  # Confirm the importance of the features
  selected_attributes <- getSelectedAttributes(boruta_tentative, withTentative = TRUE)
  boruta_stats <- attStats(boruta_tentative)
  plot(normHits ~ meanImp, col = boruta_stats$decision, data = boruta_stats)
  
  # Return the selected attributes
  print(selected_attributes)
}

# --------------------------------------------------------------------

# 9.1 Perform Boruta algorithm on scaled 'knn_na'
selected_knn_na_boruta <- performBoruta(knn_na)
# there are 16 important features, stable, unstable results
remaining_knn_na <- knn_na[,c("label",selected_knn_na_boruta)]
saveRDS(remaining_knn_na, "remaining_knn_na.RDS")
remaining_knn_na <- readRDS("remaining_knn_na.RDS")


# 9.2 Perform Boruta algorithm on scaled 'knn_sd'
selected_knn_sd_boruta <- performBoruta(knn_sd)
# there are 19 important features, unstable results (sometimes 27 and sometimes 18)
remaining_knn_sd <- knn_sd[,c("label", selected_knn_sd_boruta)]
saveRDS(remaining_knn_sd, "remaining_knn_sd.RDS")
remaining_knn_sd <- readRDS("remaining_knn_sd.RDS")

# 9.3 Perform Boruta algorithm on scaled "knn_train_red"
selected_knn_train_red_boruta <- performBoruta(knn_train_red)
# there are 20 important features, unstable results
remaining_knn_train <- knn_train_red[,c("label", selected_knn_train_red_boruta)]
saveRDS(remaining_knn_train, "remaining_knn_train.RDS")
remaining_knn_train <- readRDS("remaining_knn_train.RDS")

# 9.4 Perform Boruta algorithm on unscaled "train_knn_na"
selected_train_knn_na_boruta <- performBoruta(train_knn_na)
# there are 24 important features
remaining_train_knn_na <- train_knn_na[,c("label", selected_train_knn_na_boruta)]
saveRDS(remaining_train_knn_na, "remaining_train_knn_na.RDS")
remaining_train_knn_na <- readRDS("remaining_train_knn_na.RDS")

# 9.5 Perform Boruta algorithm on unscaled "train_knn_sd"
selected_train_knn_sd_boruta <- performBoruta(train_knn_sd)
# there are 19 important features
remaining_train_knn_sd <- train_knn_sd[,c("label", selected_train_knn_sd_boruta)]
saveRDS(remaining_train_knn_sd, "remaining_train_knn_sd.RDS")
remaining_train_knn_sd <- readRDS("remaining_train_knn_sd.RDS")

# 9.6 Perform Boruta algorithm on unscaled 'train_knn_outlier'
selected_knn_outlier_boruta <- performBoruta(train_knn_outlier)
# there are 18 important features
remaining_train_knn_outlier<- train_knn_outlier[,c("label", selected_knn_outlier_boruta)]
saveRDS(remaining_train_knn_outlier, "remaining_train_knn_outlier.RDS")
remaining_train_knn_outlier <- readRDS("remaining_train_knn_outlier.RDS")

# 9.7 Perform Boruta algorithm on scaled "hot_na"
scaled_hot_na_boruta <- scaled(hot_na[,!names(hot_na) %in% c("label","date")])
scaled_hot_na_boruta <- bind_cols(hot_na[,names(hot_na) %in% c("label","date")], scaled_hot_na_boruta)
selected_sc_hot_na_boruta <- performBoruta(scaled_hot_na_boruta)
# there are 20 important features
remaining_sc_hot_na <- hot_na[,c("label", selected_sc_hot_na_boruta)]
saveRDS(remaining_sc_hot_na, "remaining_sc_hot_na.RDS")
remaining_sc_hot_na <- readRDS("remaining_sc_hot_na.RDS")

# 9.8 Perform Boruta algorithm on scaled 'hot_sd'
scaled_hot_sd_boruta <- scaled(hot_sd[,!names(hot_sd) %in% c("label","date")])
scaled_hot_sd_boruta <- bind_cols(hot_sd[,names(hot_sd) %in% c("label","date")], scaled_hot_sd_boruta)
selected_sc_hot_sd_boruta <- performBoruta(scaled_hot_sd_boruta)
# there are 7 important features
remaining_sc_hot_sd <- hot_sd[,c("label", selected_sc_hot_sd_boruta)]
saveRDS(remaining_sc_hot_sd, "remaining_sc_hot_sd.RDS")
remaining_sc_hot_sd <- readRDS("remaining_sc_hot_sd.RDS")

# 9.9 Perform Boruta algorithm on scaled "hot_outlier"
scaled_hot_outlier_boruta <- scaled(train_hot_outlier[,!names(train_hot_outlier) %in% c("label","date")])
scaled_hot_outlier_boruta <- bind_cols(train_hot_outlier[,names(train_hot_outlier) %in% c("label","date")], scaled_hot_outlier_boruta)
selected_sc_hot_outlier_boruta <- performBoruta(scaled_hot_outlier_boruta)
# there are 12 important features
remaining_sc_hot_outlier <- train_hot_outlier[,c("label", selected_sc_hot_outlier_boruta)]
saveRDS(remaining_sc_hot_outlier, "remaining_sc_hot_outlier.RDS")
remaining_sc_hot_outlier <- readRDS("remaining_sc_hot_outlier.RDS")

# 9.10 Perform Boruta algorithm on unscaled "hot_na"
selected_hot_na_boruta <- performBoruta(hot_na)
# there are 15 important features
remaining_hot_na <- hot_na[,c("label", selected_hot_na_boruta)]
saveRDS(remaining_hot_na, "remaining_hot_na.RDS")
remaining_hot_na <- readRDS("remaining_hot_na.RDS")

# 9.11 Perform Boruta algorithm on unscaled 'hot_sd'
selected_hot_sd_boruta <- performBoruta(hot_sd)
# there are 19 important features
remaining_hot_sd <- hot_sd[,c("label", selected_hot_sd_boruta)]
saveRDS(remaining_hot_sd, "remaining_hot_sd.RDS")
remaining_hot_sd <- readRDS("remaining_hot_sd.RDS")

# 9.12 Perform Boruta algorithm of unscaled "train_hot_outlier"
selected_hot_outlier_boruta <- performBoruta(train_hot_outlier)
# there are 16 important features
remaining_hot_outlier <- train_hot_outlier[,c("label", selected_hot_outlier_boruta)]
saveRDS(remaining_hot_outlier, "remaining_hot_outlier.RDS")
remaining_hot_outlier <- readRDS("remaining_hot_outlier.RDS")


# Overview of remaining features using statistical methods after BORUTA---------

# select only features that got confirmed by boruta algorithm of the training set 
remaining_hot_na_boruta <- hot_na[, selected_hot_na_boruta]
print(remaining_hot_na_boruta)

# bind the label back with the dataset
remaining_hot_na_boruta <- bind_cols(select(hot_na, label = label), remaining_hot_na_boruta)

# Descriptive statistics - Variance and Coefficient of Variance
remaining_hot_na_boruta_var <- feature_var_all(remaining_hot_na_boruta[,!names(remaining_hot_na_boruta) %in% c("label")])
print(remaining_hot_na_boruta_var) 
remaining_hot_na_boruta_cv <- feature_cv_all(remaining_hot_na_boruta[,!names(remaining_hot_na_boruta) %in% c("label")])
print(remaining_hot_na_boruta_cv) 

# Histogram of the coefficients of variance
remaining_hot_na_boruta_hist.cv <- remaining_hot_na_boruta_cv %>%
  ggplot(aes(coef_var))+
  geom_histogram(col = "black",
                 fill = "navyblue",
                 alpha = 0.7)+
  labs(title="Histogram of Feature Volatility",
       x = "Coefficient of variance",
       y = "Frequency")+
  theme_bw()
print(remaining_hot_na_boruta_hist.cv)

# Correlation matrix
remaining_hot_na_boruta_cor <- cor(remaining_hot_na_boruta)
corrplot(remaining_hot_na_boruta_cor, method = 'square', type = 'upper')


# X Balancing

# For this round, we will start with dataset 'hot_pca_na_k' as example dataset 
# In order to try every single balancing method, and visualise the results from every method
# Then we can choose the some of the most reasonable methods to perform with other datasets.

# check frequency table of target variable
table(hot_na_boruta_bal$label)

# check classes distribution
prop.table(table(hot_na_boruta_bal$label))

# over sampling (majority*2 = 1170*2 = 2340)
data_balanced_over <- ovun.sample(label~., data = hot_na_boruta_bal, method = "over",N = 2340)$data
table(data_balanced_over$label)

# under sampling (minority*2 = 83*2 = 166)
data_balanced_under <- ovun.sample(label~., data = hot_na_boruta_bal, method = "under", N = 166, seed = 1)$data
table(data_balanced_under$label)

# both over and under sampling (total = 1170+83 = 1253)
data_balanced_both <- ovun.sample(label~., data = hot_na_boruta_bal, method = "both", p=0.5,N=1253, seed = 1)$data
table(data_balanced_both$label)

# ROSE
# all arguments of ROSE have been set to the default values
data_rose <- ROSE(label~., data = hot_na_boruta_bal, seed = 1)$data
table(data_rose$label)

#parameters have been shrunk, by setting hmult.majo = 0.25 and hmult.mino = 0.5.
data_rose_shrunk <- ROSE(label~., data = hot_na_boruta_bal, seed = 1,hmult.majo = 0.25 , hmult.mino = 0.5)$data
table(data_rose_shrunk$label)

#SMOTE
data_SMOTE <- SMOTE(X = hot_na_boruta_bal[,-1],hot_na_boruta_bal$label)$data
table(data_SMOTE$class)

#ADASYN
data_ADASYN<-ADAS(X=hot_na_boruta_bal[,-1], target=hot_na_boruta_bal$label)$data
table(data_ADASYN$class)

# -------- Visualising results after balancing --------------

par(mfrow=c(2,2))
#plot train data
plot(hot_na_boruta_bal$feature7,hot_na_boruta_bal$feature41,main="Training Data",
     xlim=c(70,140), ylim=c(50,120), col = adjustcolor(ifelse(hot_na_boruta_bal$label == -1, "blue", "red"), alpha.f = 0.5), pch=16,xlab="", ylab="")
legend("topleft", c("Majority class", "Minority class"), pch = 16, col = c("blue", "red"))

#plot oversampled data
plot(data_balanced_over$feature7,data_balanced_over$feature41,main="Oversampling",
     xlim=c(70,140), ylim=c(50,120), col = adjustcolor(ifelse(hot_na_boruta_bal$label == -1, "blue", "red"), alpha.f = 0.5), pch=16,xlab="", ylab="")
legend("topleft", c("Majority class", "Minority class"), pch = 16, col = c("blue", "red"))

#plot undersampled data
plot(data_balanced_under$feature7,data_balanced_under$feature41,main="Undersampling",
     xlim=c(70,140), ylim=c(50,120), col = adjustcolor(ifelse(hot_na_boruta_bal$label == -1, "blue", "red"), alpha.f = 0.5), pch=16,xlab="", ylab="")
legend("topleft", c("Majority class", "Minority class"), pch = 16, col = c("blue", "red"))

#plot over and undersampled data
plot(data_balanced_both$feature7,data_balanced_both$feature41,main="Over- and Undersampling",
     xlim=c(70,140), ylim=c(50,120), col = adjustcolor(ifelse(hot_na_boruta_bal$label == -1, "blue", "red"), alpha.f = 0.5), pch=16,xlab="", ylab="")
legend("topleft", c("Majority class", "Minority class"), pch = 16, col = c("blue", "red"))

par(mfrow=c(2,2))
#plot train data
plot(hot_na_boruta_bal$feature7,hot_na_boruta_bal$feature41,main="Training Data",
     xlim=c(70,140), ylim=c(50,120), col = adjustcolor(ifelse(hot_na_boruta_bal$label == -1, "blue", "red"), alpha.f = 0.5), pch=16,xlab="", ylab="")
legend("topleft", c("Majority class", "Minority class"), pch = 16, col = c("blue", "red"))

#plot ROSE resampled data
plot(data_rose$feature7,data_rose$feature41,main="ROSE resampling",
     xlim=c(70,140), ylim=c(50,120), col = adjustcolor(ifelse(hot_na_boruta_bal$label == -1, "blue", "red"), alpha.f = 0.5), pch=16,xlab="", ylab="")
legend("topleft", c("Majority class", "Minority class"), pch = 16, col = c("blue", "red"))

#plot ROSE shrunk resampled data
plot(data_rose_shrunk$feature7,data_rose_shrunk$feature41,main="ROSE shrunk resampling",
     xlim=c(70,140), ylim=c(50,120), col = adjustcolor(ifelse(hot_na_boruta_bal$label == -1, "blue", "red"), alpha.f = 0.5), pch=16,xlab="", ylab="")
legend("topleft", c("Majority class", "Minority class"), pch = 16, col = c("blue", "red"))

#plot SMOTE resampled data
plot(data_SMOTE$feature7,data_SMOTE$feature41,main="SMOTE resampling",
     xlim=c(70,140), ylim=c(50,120), col = adjustcolor(ifelse(hot_na_boruta_bal$label == -1, "blue", "red"), alpha.f = 0.5), pch=16,xlab="", ylab="")
legend("topleft", c("Majority class", "Minority class"), pch = 16, col = c("blue", "red"))

par(mfrow=c(2,2))
#plot train data
plot(hot_na_boruta_bal$feature7,hot_na_boruta_bal$feature41,main="Training Data",
     xlim=c(70,140), ylim=c(50,120), col = adjustcolor(ifelse(hot_na_boruta_bal$label == -1, "blue", "red"), alpha.f = 0.5), pch=16,xlab="", ylab="")
legend("topleft", c("Majority class", "Minority class"), pch = 16, col = c("blue", "red"))

#plot ROSE shrunk resampled data
plot(data_rose_shrunk$feature7,data_rose_shrunk$feature41,main="ROSE shrunk resampling",
     xlim=c(70,140), ylim=c(50,120), col = adjustcolor(ifelse(hot_na_boruta_bal$label == -1, "blue", "red"), alpha.f = 0.5), pch=16,xlab="", ylab="")
legend("topleft", c("Majority class", "Minority class"), pch = 16, col = c("blue", "red"))

#plot SMOTE resampled data
plot(data_SMOTE$feature7,data_SMOTE$feature41,main="SMOTE resampling",
     xlim=c(70,140), ylim=c(50,120), col = adjustcolor(ifelse(hot_na_boruta_bal$label == -1, "blue", "red"), alpha.f = 0.5), pch=16,xlab="", ylab="")
legend("topleft", c("Majority class", "Minority class"), pch = 16, col = c("blue", "red"))

#plot ADASYN resampled data
plot(data_ADASYN$feature7,data_ADASYN$feature41,main="ADASYN resampling",
     xlim=c(70,140), ylim=c(50,120), col = adjustcolor(ifelse(hot_na_boruta_bal$label == -1, "blue", "red"), alpha.f = 0.5), pch=16,xlab="", ylab="")
legend("topleft", c("Majority class", "Minority class"), pch = 16, col = c("blue", "red"))
                
# From visualising the results after balancing method in the plots above 
# It can seen that there are only 5 reasonable balancing methods that give us enough data to model
# The other methods not chosen give too little data to work with.
# which are Oversampling, Over and under Sampling, ROSE shrunk, SMOTE and ADASYN

# Below, the 5 chosen balancing methods will be applied to all datasets.

# # 10 Datasets from PCA (feature reduction) -------------------------------------
# 
# # 10.1 hot_na_k
# # Prepare for balancing by binding the target variable back with the data
# hot_pca_na_k_bal <- bind_cols(select(hot_na, label = label), hot_na_k)
# 
# # over sampling 
# hot_pca_na_k_bal_over <- ovun.sample(label~., data = hot_pca_na_k_bal, method = "over",N = 2340)$data
# table(hot_pca_na_k_bal_over$label)
# 
# # over and under sampling
# hot_pca_na_k_bal_both <- ovun.sample(label~., data = hot_pca_na_k_bal, method = "both", p=0.5,N=1253, seed = 1)$data
# table(hot_pca_na_k_bal_both$label)
# 
# #ROSE shrunk
# hot_pca_na_k_rose <- ROSE(label~., data = hot_pca_na_k_bal, seed = 1,hmult.majo = 0.25 , hmult.mino = 0.5)$data
# table(hot_pca_na_k_rose$label)
# 
# #SMOTE
# hot_pca_na_k_SMOTE <- SMOTE(X = hot_pca_na_k_bal[,-1], hot_pca_na_k_bal$label)$data
# table(hot_pca_na_k_SMOTE$class)
# 
# #ADASYN
# hot_pca_na_k_ADASYN<-ADAS(X=hot_pca_na_k_bal[,-1], target=hot_pca_na_k_bal$label)$data
# table(hot_pca_na_k_ADASYN$class)
# 
# #10.2 hot_sd_k
# # Prepare for balancing by binding the target variable back with the data
# hot_pca_sd_k_bal <- bind_cols(select(hot_sd, label = label), hot_sd_k)
# 
# # over sampling 
# hot_pca_sd_k_bal_over <- ovun.sample(label~., data = hot_pca_sd_k_bal, method = "over",N = 2340)$data
# table(hot_pca_sd_k_bal_over$label)
# 
# # over and under sampling
# hot_pca_sd_k_bal_both <- ovun.sample(label~., data = hot_pca_sd_k_bal, method = "both", p=0.5,N=1253, seed = 1)$data
# table(hot_pca_sd_k_bal_both$label)
# 
# #ROSE shrunk
# hot_pca_sd_k_rose <- ROSE(label~., data = hot_pca_sd_k_bal, seed = 1,hmult.majo = 0.25 , hmult.mino = 0.5)$data
# table(hot_pca_sd_k_rose$label)
# 
# #SMOTE
# hot_pca_sd_k_SMOTE <- SMOTE(X = hot_pca_sd_k_bal[,-1], hot_pca_sd_k_bal$label)$data
# table(hot_pca_sd_k_SMOTE$class)
# 
# #ADASYN
# hot_pca_sd_k_ADASYN<-ADAS(X=hot_pca_sd_k_bal[,-1], target=hot_pca_sd_k_bal$label)$data
# table(hot_pca_sd_k_ADASYN$class)
# 
# #10.3 hot_outlier_k
# # Prepare for balancing by binding the target variable back with the data
# hot_pca_outlier_k_bal <- bind_cols(select(train_hot_outlier, label = label), hot_outlier_k)
# 
# # over sampling 
# hot_pca_outlier_k_over <- ovun.sample(label~., data = hot_pca_outlier_k_bal, method = "over",N = 2340)$data
# table(hot_pca_outlier_k_over$label)
# 
# # over and under sampling
# hot_pca_outlier_k_both <- ovun.sample(label~., data = hot_pca_outlier_k_bal, method = "both", p=0.5,N=1253, seed = 1)$data
# table(hot_pca_outlier_k_both$label)
# 
# #ROSE shrunk
# hot_pca_outlier_k_rose <- ROSE(label~., data = hot_pca_outlier_k_bal, seed = 1,hmult.majo = 0.25 , hmult.mino = 0.5)$data
# table(hot_pca_outlier_k_rose$label)
# 
# #SMOTE
# hot_pca_outlier_k_SMOTE <- SMOTE(X = hot_pca_outlier_k_bal[,-1], hot_pca_outlier_k_bal$label)$data
# table(hot_pca_outlier_k_SMOTE$class)
# 
# #ADASYN
# hot_pca_outlier_k_ADASYN<-ADAS(X=hot_pca_outlier_k_bal[,-1], target=hot_pca_outlier_k_bal$label)$data
# table(hot_pca_outlier_k_ADASYN$class)
# 
# #10.4 hot_na_var
# # Prepare for balancing by binding the target variable back with the data
# hot_pca_na_var_bal <- bind_cols(select(hot_na, label = label), hot_na_var)
# 
# # over sampling 
# hot_pca_na_var_over <- ovun.sample(label~., data = hot_pca_na_var_bal, method = "over",N = 2340)$data
# table(hot_pca_na_var_over$label)
# 
# # over and under sampling
# hot_pca_na_var_both <- ovun.sample(label~., data = hot_pca_na_var_bal, method = "both", p=0.5,N=1253, seed = 1)$data
# table(hot_pca_na_var_both$label)
# 
# #ROSE shrunk
# hot_pca_na_var_rose <- ROSE(label~., data = hot_pca_na_var_bal, seed = 1,hmult.majo = 0.25 , hmult.mino = 0.5)$data
# table(hot_pca_na_var_rose$label)
# 
# #SMOTE
# hot_pca_na_var_SMOTE <- SMOTE(X = hot_pca_na_var_bal[,-1], hot_pca_na_var_bal$label)$data
# table(hot_pca_na_var_SMOTE$class)
# 
# #ADASYN
# hot_pca_na_var_ADASYN<-ADAS(X=hot_pca_na_var_bal[,-1], target=hot_pca_na_var_bal$label)$data
# table(hot_pca_na_var_ADASYN$class)
# 
# #10.5 hot_na_var
# # Prepare for balancing by binding the target variable back with the data
# hot_pca_sd_var_bal <- bind_cols(select(hot_sd, label = label), hot_sd_var)
# 
# # over sampling 
# hot_pca_sd_var_over <- ovun.sample(label~., data = hot_pca_sd_var_bal, method = "over",N = 2340)$data
# table(hot_pca_sd_var_over$label)
# 
# # over and under sampling
# hot_pca_sd_var_both <- ovun.sample(label~., data = hot_pca_sd_var_bal, method = "both", p=0.5,N=1253, seed = 1)$data
# table(hot_pca_sd_var_both$label)
# 
# #ROSE shrunk
# hot_pca_sd_var_rose <- ROSE(label~., data = hot_pca_sd_var_bal, seed = 1,hmult.majo = 0.25 , hmult.mino = 0.5)$data
# table(hot_pca_sd_var_rose$label)
# 
# #SMOTE
# hot_pca_sd_var_SMOTE <- SMOTE(X = hot_pca_sd_var_bal[,-1], hot_pca_sd_var_bal$label)$data
# table(hot_pca_sd_var_SMOTE$class)
# 
# #ADASYN
# hot_pca_sd_var_ADASYN<-ADAS(X=hot_pca_sd_var_bal[,-1], target=hot_pca_sd_var_bal$label)$data
# table(hot_pca_sd_var_ADASYN$class)
# 
# #10.6 hot_outlier_var
# # Prepare for balancing by binding the target variable back with the data
# hot_pca_outlier_var_bal <- bind_cols(select(train_hot_outlier, label = label), hot_outlier_var)
# 
# # over sampling 
# hot_pca_outlier_var_over <- ovun.sample(label~., data = hot_pca_outlier_var_bal, method = "over",N = 2340)$data
# table(hot_pca_outlier_var_over$label)
# 
# # over and under sampling
# hot_pca_outlier_var_both <- ovun.sample(label~., data = hot_pca_outlier_var_bal, method = "both", p=0.5,N=1253, seed = 1)$data
# table(hot_pca_outlier_var_both$label)
# 
# #ROSE shrunk
# hot_pca_outlier_var_rose <- ROSE(label~., data = hot_pca_outlier_var_bal, seed = 1,hmult.majo = 0.25 , hmult.mino = 0.5)$data
# table(hot_pca_outlier_var_rose$label)
# 
# #SMOTE
# hot_pca_outlier_var_SMOTE <- SMOTE(X = hot_pca_outlier_var_bal[,-1], hot_pca_outlier_var_bal$label)$data
# table(hot_pca_outlier_var_SMOTE$class)
# 
# #ADASYN
# hot_pca_outlier_var_ADASYN<-ADAS(X=hot_pca_outlier_var_bal[,-1], target=hot_pca_outlier_var_bal$label)$data
# table(hot_pca_outlier_var_ADASYN$class)

# 10 Datasets from BORUTA (feature selection) ----------------------------------

#10.7 remaining_knn_na

# over sampling 
knn_na_over <- ovun.sample(label~., data = remaining_knn_na, method = "over",N = 2340)$data
table(knn_na_over$label)

# over and under sampling
knn_na_both <- ovun.sample(label~., data = remaining_knn_na, method = "both", p=0.5,N=1253, seed = 1)$data
table(knn_na_both$label)

#ROSE shrunk
knn_na_rose <- ROSE(label~., data = remaining_knn_na, seed = 1,hmult.majo = 0.25 , hmult.mino = 0.5)$data
table(knn_na_rose$label)

#SMOTE
knn_na_smote <- SMOTE(X = remaining_knn_na[,-1], remaining_knn_na$label)$data
table(knn_na_smote$class)

#ADASYN
knn_na_adasyn<-ADAS(X=remaining_knn_na[,-1], target=remaining_knn_na$label)$data
table(knn_na_adasyn$class)

#10.8 remaining_knn_sd

# over sampling 
knn_sd_over <- ovun.sample(label~., data = remaining_knn_sd, method = "over",N = 2340)$data
table(knn_sd_over$label)

# over and under sampling
knn_sd_both <- ovun.sample(label~., data = remaining_knn_sd, method = "both", p=0.5,N=1253, seed = 1)$data
table(knn_sd_both$label)

#ROSE shrunk
knn_sd_rose <- ROSE(label~., data = remaining_knn_sd, seed = 1,hmult.majo = 0.25 , hmult.mino = 0.5)$data
table(knn_sd_rose$label)

#SMOTE
knn_sd_smote <- SMOTE(X = remaining_knn_sd[,-1], remaining_knn_sd$label)$data
table(knn_sd_smote$class)

#ADASYN
knn_sd_adasyn <-ADAS(X=remaining_knn_sd[,-1], target=remaining_knn_sd$label)$data
table(knn_sd_adasyn$class)

# 10.9 remaining_knn_train

# over sampling 
knn_train_over <- ovun.sample(label~., data = remaining_knn_train, method = "over",N = 2340)$data
table(knn_train_over$label)

# over and under sampling
knn_train_both <- ovun.sample(label~., data = remaining_knn_train, method = "both", p=0.5,N=1253, seed = 1)$data
table(knn_train_both$label)

#ROSE shrunk
knn_train_rose <- ROSE(label~., data = remaining_knn_train, seed = 1,hmult.majo = 0.25 , hmult.mino = 0.5)$data
table(knn_train_rose$label)

#SMOTE
knn_train_smote <- SMOTE(X = remaining_knn_train[,-1], remaining_knn_train$label)$data
table(knn_train_smote$class)

#ADASYN
knn_train_adasyn <-ADAS(X=remaining_knn_train[,-1], target=remaining_knn_train$label)$data
table(knn_train_adasyn$class)

# 10.10 remaining_train_knn_na

# over sampling 
train_knn_na_over <- ovun.sample(label~., data =  remaining_train_knn_na, method = "over",N = 2340)$data
table(train_knn_na_over$label)

# over and under sampling
train_knn_na_both <- ovun.sample(label~., data =  remaining_train_knn_na, method = "both", p=0.5,N=1253, seed = 1)$data
table(train_knn_na_both$label)

#ROSE shrunk
train_knn_na_rose <- ROSE(label~., data =  remaining_train_knn_na, seed = 1,hmult.majo = 0.25 , hmult.mino = 0.5)$data
table(train_knn_na_rose$label)

#SMOTE
train_knn_na_smote <- SMOTE(X =  remaining_train_knn_na[,-1],  remaining_train_knn_na$label)$data
table(train_knn_na_smote$class)

#ADASYN
train_knn_na_adasyn <-ADAS(X= remaining_train_knn_na[,-1], target= remaining_train_knn_na$label)$data
table(train_knn_na_adasyn$class)

# 10.11 remaining_train_knn_sd

# over sampling 
train_knn_sd_over <- ovun.sample(label~., data =  remaining_train_knn_sd, method = "over",N = 2340)$data
table(train_knn_sd_over$label)

# over and under sampling
train_knn_sd_both <- ovun.sample(label~., data =  remaining_train_knn_sd, method = "both", p=0.5,N=1253, seed = 1)$data
table(train_knn_sd_both$label)

#ROSE shrunk
train_knn_sd_rose <- ROSE(label~., data =  remaining_train_knn_sd, seed = 1,hmult.majo = 0.25 , hmult.mino = 0.5)$data
table(train_knn_sd_rose$label)

#SMOTE
train_knn_sd_smote <- SMOTE(X = remaining_train_knn_sd[,-1],  remaining_train_knn_sd$label)$data
table(train_knn_sd_smote$class)

#ADASYN
train_knn_sd_adasyn <-ADAS(X = remaining_train_knn_sd[,-1], target = remaining_train_knn_sd$label)$data
table(train_knn_sd_adasyn$class)

# 10.12 remaining_train_knn_outlier

# over sampling 
train_knn_out_over <- ovun.sample(label~., data =  remaining_train_knn_outlier, method = "over",N = 2340)$data
table(train_knn_out_over$label)

# over and under sampling
train_knn_out_both <- ovun.sample(label~., data =  remaining_train_knn_outlier, method = "both", p=0.5,N=1253, seed = 1)$data
table(train_knn_out_both$label)

#ROSE shrunk
train_knn_out_rose <- ROSE(label~., data =  remaining_train_knn_outlier, seed = 1,hmult.majo = 0.25 , hmult.mino = 0.5)$data
table(train_knn_out_rose$label)

#SMOTE
train_knn_out_smote <- SMOTE(X = remaining_train_knn_outlier[,-1],  remaining_train_knn_outlier$label)$data
table(train_knn_out_smote$class)

#ADASYN
train_knn_out_adasyn <-ADAS(X = remaining_train_knn_outlier[,-1], target = remaining_train_knn_outlier$label)$data
table(train_knn_out_adasyn$class)
   
# 10.13 remaining_sc_hot_na

# over sampling 
hot_na_over <- ovun.sample(label~., data =  remaining_sc_hot_na, method = "over",N = 2340)$data
table(hot_na_over$label)

# over and under sampling
hot_na_both <- ovun.sample(label~., data =  remaining_sc_hot_na, method = "both", p=0.5,N=1253, seed = 1)$data
table(hot_na_both$label)

#ROSE shrunk
hot_na_rose <- ROSE(label~., data =  remaining_sc_hot_na, seed = 1,hmult.majo = 0.25 , hmult.mino = 0.5)$data
table(hot_na_rose$label)

#SMOTE
hot_na_smote <- SMOTE(X = remaining_sc_hot_na[,-1],  remaining_sc_hot_na$label)$data
table(hot_na_smote$class)

#ADASYN
hot_na_adasyn <-ADAS(X = remaining_sc_hot_na[,-1], target = remaining_sc_hot_na$label)$data
table(hot_na_adasyn$class)
 
# 10.14 remaining_sc_hot_sd

# over sampling 
hot_sd_over <- ovun.sample(label~., data =  remaining_sc_hot_sd, method = "over",N = 2340)$data
table(hot_sd_over$label)

# over and under sampling
hot_sd_both <- ovun.sample(label~., data =  remaining_sc_hot_sd, method = "both", p=0.5,N=1253, seed = 1)$data
table(hot_sd_both$label)

#ROSE shrunk
hot_sd_rose <- ROSE(label~., data =  remaining_sc_hot_sd, seed = 1,hmult.majo = 0.25 , hmult.mino = 0.5)$data
table(hot_sd_rose$label)

#SMOTE
hot_sd_smote <- SMOTE(X = remaining_sc_hot_sd[,-1],  remaining_sc_hot_sd$label)$data
table(hot_sd_smote$class)

#ADASYN
hot_sd_adasyn <-ADAS(X = remaining_sc_hot_sd[,-1], target = remaining_sc_hot_sd$label)$data
table(hot_sd_adasyn$class)

# 10.15 remaining_sc_hot_outlier

# over sampling 
hot_train_over <- ovun.sample(label~., data =  remaining_sc_hot_outlier, method = "over",N = 2340)$data
table(hot_train_over$label)

# over and under sampling
hot_train_both <- ovun.sample(label~., data =  remaining_sc_hot_outlier, method = "both", p=0.5,N=1253, seed = 1)$data
table(hot_train_both$label)

#ROSE shrunk
hot_train_rose <- ROSE(label~., data =  remaining_sc_hot_outlier, seed = 1,hmult.majo = 0.25 , hmult.mino = 0.5)$data
table(hot_train_rose$label)

#SMOTE
hot_train_smote <- SMOTE(X = remaining_sc_hot_outlier[,-1],  remaining_sc_hot_outlier$label)$data
table(hot_train_smote$class)

#ADASYN
hot_train_adasyn <-ADAS(X = remaining_sc_hot_outlier[,-1], target = remaining_sc_hot_outlier$label)$data
table(hot_train_adasyn$class)

# 10.16 remaining_hot_na

# over sampling 
train_hot_na_over <- ovun.sample(label~., data =  remaining_hot_na, method = "over",N = 2340)$data
table(train_hot_na_over$label)

# over and under sampling
train_hot_na_both <- ovun.sample(label~., data =  remaining_hot_na, method = "both", p=0.5,N=1253, seed = 1)$data
table(train_hot_na_both$label)

#ROSE shrunk
train_hot_na_rose <- ROSE(label~., data =  remaining_hot_na, seed = 1,hmult.majo = 0.25 , hmult.mino = 0.5)$data
table(train_hot_na_rose$label)

#SMOTE
train_hot_na_smote <- SMOTE(X = remaining_hot_na[,-1],  remaining_hot_na$label)$data
table(train_hot_na_smote$class)

#ADASYN
train_hot_na_adasyn <-ADAS(X = remaining_hot_na[,-1], target = remaining_hot_na$label)$data
table(train_hot_na_adasyn$class)

# 10.17 remaining_hot_sd

# over sampling 
train_hot_sd_over <- ovun.sample(label~., data =  remaining_hot_sd, method = "over",N = 2340)$data
table(train_hot_sd_over$label)

# over and under sampling
train_hot_sd_both <- ovun.sample(label~., data =  remaining_hot_sd, method = "both", p=0.5,N=1253, seed = 1)$data
table(train_hot_sd_both$label)

#ROSE shrunk
train_hot_sd_rose <- ROSE(label~., data =  remaining_hot_sd, seed = 1,hmult.majo = 0.25 , hmult.mino = 0.5)$data
table(train_hot_sd_rose$label)

#SMOTE
train_hot_sd_smote <- SMOTE(X = remaining_hot_sd[,-1],  remaining_hot_sd$label)$data
table(train_hot_sd_smote$class)

#ADASYN
train_hot_sd_adasyn <-ADAS(X = remaining_hot_sd[,-1], target = remaining_hot_sd$label)$data
table(train_hot_sd_adasyn$class)

# 10.18 remaining_hot_outlier

# over sampling 
train_hot_out_over <- ovun.sample(label~., data =  remaining_hot_outlier, method = "over",N = 2340)$data
table(train_hot_out_over$label)

# over and under sampling
train_hot_out_both <- ovun.sample(label~., data =  remaining_hot_outlier, method = "both", p=0.5,N=1253, seed = 1)$data
table(train_hot_out_both$label)

#ROSE shrunk
train_hot_out_rose <- ROSE(label~., data =  remaining_hot_outlier, seed = 1,hmult.majo = 0.25 , hmult.mino = 0.5)$data
table(train_hot_out_rose$label)

#SMOTE
train_hot_out_smote <- SMOTE(X = remaining_hot_outlier[,-1],  remaining_hot_outlier$label)$data
table(train_hot_out_smote$class)

#ADASYN
train_hot_out_adasyn <-ADAS(X = remaining_hot_outlier[,-1], target = remaining_hot_outlier$label)$data
table(train_hot_out_adasyn$class)

# XI Modeling
# 1 Prepare the test set
# 1.1 Dimensionality reduction
test_red <- dim_reduce(test_set)
# Overall dimensionality reduction of 124 features
saveRDS(test_red, "test_red.RDS")
test_reduced <- readRDS("test_red.RDS")
test_red <- test_reduced %>%
  dplyr::select(-c("label","date"))

# 1.2 Outliers
test_outlier <- identify_outliers(test_red)
sum(test_outlier$outlier_count)
# 751 outliers in test set

# NA approach
test_outlier_na <- test_red %>%
  mutate(across(where(is.numeric), outlier_replace))
sum(is.na(test_outlier_na))
# 6726 missing values

# SD approach
test_outlier_sd <- test_red %>%
  mutate(across(where(is.numeric), ~ outlier_replace(., method = "SD")))

# 1.3 Missing value imputaton
# 1.3.1 Hot deck
# After outlier handling
test_hot_na <- hotdeck(test_outlier_na, impNA = T, imp_var = F)
sum(is.na(test_hot_na))

saveRDS(test_hot_na, "test_hot_na.RDS")
test_hot_na <- readRDS("test_hot_na.RDS")

test_hot_sd <- hotdeck(test_outlier_sd, impNA = T, imp_var = F)
sum(is.na(test_hot_sd))

saveRDS(test_hot_sd, "test_hot_sd.RDS")
test_hot_sd <- readRDS("test_hot_sd.RDS")

# Before outlier handling
test_hot_outlier <- hotdeck(test_red, impNA = T, imp_var = F)
sum(is.na(test_hot_outlier))

test_hot_outlier <- test_hot_outlier %>%
  mutate(across(where(is.numeric),~ outlier_replace(., method = "SD")))

saveRDS(test_hot_outlier, "test_hot_outlier.RDS")
test_hot_outlier <- readRDS("test_hot_outlier.RDS")

# 1.3.2 kNN 
# Scaling
scaled_test_na <- scaled(test_outlier_na)
scaled_test_sd <- scaled(test_outlier_sd)
scaled_test_red <- scaled(test_red)
# After outlier handling
test_knn_na <- kNN(scaled_test_na, k = 5, impNA = T, imp_var = F)
sum(is.na(test_knn_na))
saveRDS(test_knn_na, "test_knn_na.RDS")
test_knn_na <- readRDS("test_knn_na.RDS")

# Merge label and date back together with the rest of the data for the imputation
test_knn_sd <- kNN(scaled_test_sd, k = 5, impNA = T, imp_var = F)
sum(is.na(test_knn_sd))
saveRDS(test_knn_sd, "test_knn_sd.RDS")
test_knn_sd <- readRDS("test_knn_sd.RDS")

# Reverse scaling
# For NA set
test_knn_na_rev <- reverse_scaling(test_outlier_na, test_knn_na)

# For SD set
test_knn_sd_rev <- reverse_scaling(test_outlier_sd, test_knn_sd)

# Save imputation files as RDS to reduce processing time
saveRDS(test_knn_na_rev, "test_knn_na_rev.RDS")
test_knn_na_rev <- readRDS("test_knn_na_rev.RDS")

saveRDS(test_knn_sd_rev, "test_knn_sd_rev.RDS")
test_knn_sd_rev <- readRDS("test_knn_sd_rev.RDS")

# Before outlier handling
knn_test_red <- kNN(scaled_test_red, k = 5, impNA = T, imp_var = F)
sum(is.na(knn_test_red))

saveRDS(knn_test_red, "knn_test_red.RDS")
knn_test_red <- readRDS("knn_test_red.RDS")
# Reverse scaling
test_red_knn <- reverse_scaling(test_red, knn_test_red)

# Outlier handling with 4S
test_knn_outlier <- test_red_knn %>%
  mutate(across(where(is.numeric),~ outlier_replace(.x, method = "SD")))

saveRDS(test_knn_outlier, "test_knn_outlier.RDS")
test_knn_outlier <- readRDS("test_knn_outlier.RDS")

 # # 1.4 PCA
# PCA_extract_test <- function(x, nf) {
#   pca <- PCA(x, graph = F)
#   eigen <- as.tibble(pca$eig)
#   nf <- nf
#   pca_extract <- principal(x, nfactors = nf, covar = F, scores = T)
#   as.tibble(pca_extract$scores)
# }
# # NA set
# test_hot_na_norm <- scaled(test_hot_na[,!names(test_hot_na) %in% c("label","date")])
# sum(is.nan(as.matrix(test_hot_na_norm)))
# # 2198 NaN values: feature75, feature207, feature210, feature343, feature348, feature479 and feature522
# # Produced due to data split and less unique values in these features
# test_hot_na_norm <- test_hot_na_norm[, colSums(is.na(test_hot_na_norm)) == 0]
# 
# # Kaiser criterion
# test_hot_na_k <- PCA_extract_test(test_hot_na_norm, nf = length(hot_na_k))
# test_hot_pca_na_k <- bind_cols(select(test_hot_na, label = label), test_hot_na_k)

# 2 Random forest
set.seed(12345)
# Custom random forest model needed to use cross validation on parameters ntree and nodesize
customRF <- list(type = "Classification", library = "randomForest", loop = NULL)
customRF$parameters <- data.frame(parameter = c("mtry", "ntree", "nodesize"), class = rep("numeric", 3), label = c("mtry", "ntree", "nodesize"))
customRF$grid <- function(x, y, len = NULL, search = "grid") {}
customRF$fit <- function(x, y, wts, param, lev, last, weights, classProbs, ...) {
  randomForest(x, y, mtry = param$mtry, ntree=param$ntree, nodesize=param$nodesize,...)
}
customRF$predict <- function(modelFit, newdata, preProc = NULL, submodels = NULL)
  predict(modelFit, newdata)
customRF$prob <- function(modelFit, newdata, preProc = NULL, submodels = NULL)
  predict(modelFit, newdata, type = "prob")
customRF$sort <- function(x) x[order(x[,1]),]
customRF$levels <- function(x) x$classes
customRF

# Model result functions
model_result <- function(predictions, target) {
  conf_mat <- confusionMatrix(data = predictions, reference = target)
  target <- as.factor(target)
  F1 <- F_meas(predictions, target)
  precision <- precision(predictions,target)
  TPR <- sensitivity(predictions, target)
  FPR <- 1- specificity(predictions, target)
  return_list <- list("Confusion Matrix" = conf_mat, 
                      "F1 Score" = F1, 
                      "True Positive Rate (aka sensitivity/recall)" = TPR, 
                      "False Positive Rate" = FPR)
  print(return_list)
}

ROC <- function(model, test_data) {
  pred_probs <- as.numeric(predict(model, newdata = test_data, type = "response"))
  pred <- prediction(as.numeric(pred_probs), test_data[1])          
  perf <- performance(pred, measure = "tpr", x.measure = "fpr") 
  roc_curve <- plot(perf, main = "ROC Curve", lwd = 3, col = "darkred")
  return(roc_curve)
}

AUC <- function(model, test_data) {
  pred_probs <- as.numeric(predict(model, newdata = test_data, type = "response"))
  pred <- prediction(as.numeric(pred_probs), test_data[1])
  perf <- performance(pred, measure = "tpr", x.measure = "fpr")
  auc_score <- as.numeric(performance(pred, measure = "auc")@y.values)
  return(auc_score)
}
# Train custom random forest model
# Parameters for controlling the training with cross validation
train_control <- trainControl(method = "cv", number = 5, search = "grid", summaryFunction = twoClassSummary)

# Parameters of the random forest to be optimised
tune_grid <- expand.grid(mtry = c(1:10), ntree = seq(50,400,50), nodesize = c(1:10))

# 2.1.1 knn_na_rose
knn_na_rose_rf <- randomForest(as.factor(label)~., data = knn_na_rose,
                               ntree = 400, importance = T, localImp = T,
                               replace = T, nodesize = 5, proximity = F)
test_knn_na_rf <- scaled(test_knn_na[,colnames(remaining_knn_na[-1])])
knn_na_rose_rf_pred <- predict(knn_na_rose_rf, newdata = test_knn_na_rf, type = "class")
knn_na_rose_rf_res <- model_result(knn_na_rose_rf_pred,label)

knn_na_rose_rf2 <- train(as.factor(label)~., data = train_knn_na_rose, 
                               method = customRF, metric = "Sens",
                               trControl = train_control, tuneGrid = tune_grid)

 # 2.1.2 knn_na_smote
knn_na_smote <- knn_na_smote %>% rename(label = class)
knn_na_smote_rf <- randomForest(as.factor(label)~., data = knn_na_smote,
                                ntree = 400, importance = T, localImp = T,
                                replace = T, nodesize = 5, proximity = F)
knn_na_smote_rf_pred <- predict(knn_na_smote_rf, newdata = test_knn_na_rf, type = "class")
knn_na_smote_rf_res <- model_result(knn_na_smote_rf_pred,label)

# 2.1.3 knn_na_adasyn
knn_na_adasyn <- knn_na_adasyn %>% rename(label = class)
knn_na_adasyn_rf <- randomForest(as.factor(label)~., data = knn_na_adasyn,
                                 ntree = 400, importance = T, localImp = T,
                                 replace = T, nodesize = 5, proximity = F)
knn_na_adasyn_rf_pred <- predict(knn_na_adasyn_rf, newdata = test_knn_na_rf, type = "class")
knn_na_adasyn_rf_res <- model_result(knn_na_adasyn_rf_pred,label)

# 2.2.1 knn_sd_rose
knn_sd_rose_rf <- randomForest(as.factor(label)~., data = knn_sd_rose,
                               ntree = 400, importance = T, localImp = T,
                               replace = T, nodesize = 5, proximity = F)
test_knn_sd_rf <- scaled(test_knn_sd[,colnames(remaining_knn_sd[-1])])
knn_sd_rose_rf_pred <- predict(knn_sd_rose_rf, newdata = test_knn_sd_rf, type = "class")
knn_sd_rose_rf_res <- model_result(knn_sd_rose_rf_pred,label)

# 2.2.2 knn_sd_smote
knn_sd_smote <- knn_sd_smote %>% rename(label = class)
knn_sd_smote_rf <- randomForest(as.factor(label)~., data = knn_sd_smote,
                                ntree = 400, importance = T, localImp = T,
                                replace = T, nodesize = 5, proximity = F)
knn_sd_smote_rf_pred <- predict(knn_sd_smote_rf, newdata = test_knn_sd_rf, type = "class")
knn_sd_smote_rf_res <- model_result(knn_sd_smote_rf_pred,label)

# 2.2.3 knn_sd_adasyn
knn_sd_adasyn <- knn_sd_adasyn %>% rename(label = class)
knn_sd_adasyn_rf <- randomForest(as.factor(label)~., data = knn_sd_adasyn,
                                 ntree = 400, importance = T, localImp = T,
                                 replace = T, nodesize = 5, proximity = F)
knn_sd_adasyn_rf_pred <- predict(knn_sd_adasyn_rf, newdata = test_knn_sd_rf, type = "class")
knn_sd_adasyn_rf_res <- model_result(knn_sd_adasyn_rf_pred,label)

# 2.3.1 knn_train_rose
knn_train_rose_rf <- randomForest(as.factor(label)~., data = knn_train_rose,
                                  ntree = 400, importance = T, localImp = T,
                                  replace = T, nodesize = 5, proximity = F)
test_knn_train_rf <- scaled(test_knn_outlier[,colnames(remaining_knn_train[-1])])
knn_train_rose_rf_pred <- predict(knn_train_rose_rf, newdata = test_knn_train_rf, type = "class")
knn_train_rose_rf_res <- model_result(knn_train_rose_rf_pred,label)

# 2.3.2 knn_train_smote
knn_train_smote <- knn_train_smote %>% rename(label = class)
knn_train_smote_rf <- randomForest(as.factor(label)~., data = knn_train_smote,
                                   ntree = 400, importance = T, localImp = T,
                                   replace = T, nodesize = 5, proximity = F)
knn_train_smote_rf_pred <- predict(knn_train_smote_rf, newdata = test_knn_train_rf, type = "class")
knn_train_smote_rf_res <- model_result(knn_train_smote_rf_pred,label)

# 2.3.2 knn_train_adasyn
knn_train_adasyn <- knn_train_adasyn %>% rename(label = class)
knn_train_adasyn_rf <- randomForest(as.factor(label)~., data = knn_train_adasyn,
                                    ntree = 400, importance = T, localImp = T,
                                    replace = T, nodesize = 5, proximity = F)
knn_train_adasyn_rf_pred <- predict(knn_train_adasyn_rf, newdata = test_knn_train_rf, type = "class")
knn_train_adasyn_rf_res <- model_result(knn_train_adasyn_rf_pred,label)

# 2.4.1 train_knn_na_rose
train_knn_na_rose_rf <- randomForest(as.factor(label)~., data = train_knn_na_rose,
                                     ntree = 400, importance = T, localImp = T,
                                     replace = T, nodesize = 5, proximity = F)
test_knn_na_rev_rf <- test_knn_na[,colnames(remaining_train_knn_na[-1])]
train_knn_na_rose_rf_pred <- predict(train_knn_na_rose_rf, newdata = test_knn_na_rev_rf, type = "class")
train_knn_na_rose_rf_res <- model_result(train_knn_na_rose_rf_pred,label)

# Train model, computation time ~ 30 mins
train_knn_na_rose_rf2 <- train(as.factor(label)~., data = train_knn_na_rose, 
                               method = customRF, metric = "Spec",
                               trControl = train_control, tuneGrid = tune_grid)
# mtry = 2, ntree = 350, nodesize = 1
train_knn_na_rose_rf <- randomForest(as.factor(label)~., data = train_knn_na_rose,
                                     mtry = 2, ntree = 350, nodesize = 1,importance = T, 
                                     localImp = T, replace = T, proximity = F)
test_knn_na_rev_rf <- test_knn_na[,colnames(remaining_train_knn_na[-1])]
train_knn_na_rose_rf_pred <- predict(train_knn_na_rose_rf, newdata = test_knn_na_rev_rf, type = "class")
train_knn_na_rose_rf_res <- model_result(train_knn_na_rose_rf_pred,label)


# 2.4.2 train_knn_na_smote
train_knn_na_smote <- train_knn_na_smote %>% rename(label = class)
train_knn_na_smote_rf <- randomForest(as.factor(label)~., data = train_knn_na_smote,
                                      ntree = 400, importance = T, localImp = T,
                                      replace = T, nodesize = 5, proximity = F)
train_knn_na_smote_rf_pred <- predict(train_knn_na_smote_rf, newdata = test_knn_na_rev_rf, type = "class")
train_knn_na_smote_rf_res <- model_result(train_knn_na_smote_rf_pred,label)

# 2.4.2 train_knn_na_adasyn
train_knn_na_adasyn <- train_knn_na_adasyn %>% rename(label = class)
train_knn_na_adasyn_rf <- randomForest(as.factor(label)~.,data = train_knn_na_adasyn,
                                       ntree = 400, importance = T, localImp = T,
                                       replace = T, nodesize = 5, proximity = F)
train_knn_na_adasyn_rf_pred <- predict(train_knn_na_adasyn_rf, newdata = test_knn_na_rev_rf, type = "class")
train_knn_na_adasyn_rf_res <- model_result(train_knn_na_adasyn_rf_pred,label)

# 2.5.1 train_knn_sd_rose
train_knn_sd_rose_rf <- randomForest(as.factor(label)~., data = train_knn_sd_rose,
                                     ntree = 400, importance = T, localImp = T,
                                     replace = T, nodesize = 5, proximity = F)
test_knn_sd_rev_rf <- test_knn_sd[,colnames(remaining_train_knn_sd[-1])]
train_knn_sd_rose_rf_pred <- predict(train_knn_sd_rose_rf, newdata = test_knn_sd_rev_rf, type = "class")
train_knn_sd_rose_rf_res <- model_result(train_knn_sd_rose_rf_pred,label)

# Train model, computation time ~ 30 mins
train_knn_sd_rose_rf2 <- train(as.factor(label)~., data = train_knn_na_rose, 
                               method = customRF, metric = "Spec",
                               trControl = train_control, tuneGrid = tune_grid)
# mtry = 5, ntree = 300, nodesize = 10 
train_knn_sd_rose_rf <- randomForest(as.factor(label)~., data = train_knn_sd_rose,
                                     mtry = 5, ntree = 300, nodesize = 10,
                                     importance = T, localImp = T, replace = T, proximity = F)
train_knn_sd_rose_rf_pred <- predict(train_knn_sd_rose_rf, newdata = test_knn_sd_rev_rf, type = "class")
train_knn_sd_rose_rf_res <- model_result(train_knn_sd_rose_rf_pred,label)

# 2.5.2 train_knn_sd_smote
train_knn_sd_smote <- train_knn_sd_smote %>% rename(label = class)
train_knn_sd_smote_rf <- randomForest(as.factor(label)~., data = train_knn_sd_smote,
                                      ntree = 400, importance = T, localImp = T,
                                      replace = T, nodesize = 5, proximity = F)
train_knn_sd_smote_rf_pred <- predict(train_knn_sd_smote_rf, newdata = test_knn_sd_rev_rf, type = "class")
train_knn_sd_smote_rf_res <- model_result(train_knn_sd_smote_rf_pred,label)

# 2.5.3 train_knn_sd_adasyn
train_knn_sd_adasyn <- train_knn_sd_adasyn %>% rename(label = class)
train_knn_sd_adasyn_rf <- randomForest(as.factor(label)~., data = train_knn_sd_adasyn,
                                       ntree = 400, importance = T, localImp = T,
                                       replace = T, nodesize = 5, proximity = F)
train_knn_sd_adasyn_rf_pred <- predict(train_knn_sd_adasyn_rf, newdata = test_knn_sd_rev_rf, type = "class")
train_knn_sd_adasyn_rf_res <- model_result(train_knn_sd_adasyn_rf_pred,label)

# 2.6.1 train_knn_out_rose
train_knn_out_rose_rf <- randomForest(as.factor(label)~., data = train_knn_out_rose,
                                      ntree = 400, importance = T, localImp = T,
                                      replace = T, nodesize = 5, proximity = F)
test_knn_out_rf <- test_knn_outlier[,colnames(remaining_train_knn_outlier[-1])]
train_knn_out_rose_rf_pred <- predict(train_knn_out_rose_rf, newdata = test_knn_out_rf, type = "class")
train_knn_out_rose_rf_res <- model_result(train_knn_out_rose_rf_pred,label)

# 2.6.2 train_knn_out_smote
train_knn_out_smote <- train_knn_out_smote %>% rename(label = class)
train_knn_out_smote_rf <- randomForest(as.factor(label)~., data = train_knn_out_smote,
                                       ntree = 400, importance = T, localImp = T,
                                       replace = T, nodesize = 5, proximity = F)
train_knn_out_smote_rf_pred <- predict(train_knn_out_smote_rf, newdata = test_knn_out_rf, type = "class")
train_knn_out_smote_rf_res <- model_result(train_knn_out_smote_rf_pred,label)

# 2.6.3 train_knn_out_smote
train_knn_out_adasyn <- train_knn_out_adasyn %>% rename(label = class)
train_knn_out_adasyn_rf <- randomForest(as.factor(label)~., data = train_knn_out_adasyn,
                                        ntree = 400, importance = T, localImp = T,
                                        replace = T, nodesize = 5, proximity = F)
train_knn_out_adasyn_rf_pred <- predict(train_knn_out_adasyn_rf, newdata = test_knn_out_rf, type = "class")
train_knn_out_adasyn_rf_res <- model_result(train_knn_out_adasyn_rf_pred,label)

# 2.7.1 hot_na_rose
hot_na_rose_rf <- randomForest(as.factor(label)~., data = hot_na_rose,
                               ntree = 400, importance = T, localImp = T,
                               replace = T, nodesize = 5, proximity = F)
test_hot_na_rf <- scaled(test_hot_na[,colnames(remaining_sc_hot_na[-1])])
hot_na_rose_rf_pred <- predict(hot_na_rose_rf, newdata = test_hot_na_rf, type = "class")
hot_na_rose_rf_res <- model_result(hot_na_rose_rf_pred,label)

# 2.7.2 hot_na_smote
hot_na_smote <- hot_na_smote %>% rename(label = class)
hot_na_smote_rf <- randomForest(as.factor(label)~., data = hot_na_smote,
                                ntree = 400, importance = T, localImp = T,
                                replace = T, nodesize = 5, proximity = F)
hot_na_smote_rf_pred <- predict(hot_na_smote_rf, newdata = test_hot_na_rf, type = "class")
hot_na_smote_rf_res <- model_result(hot_na_smote_rf_pred,label)

# 2.7.3 hot_na_adasyn
hot_na_adasyn <- hot_na_adasyn %>% rename(label = class)
hot_na_adasyn_rf <- randomForest(as.factor(label)~., data = hot_na_adasyn,
                                 ntree = 400, importance = T, localImp = T,
                                 replace = T, nodesize = 5, proximity = F)
hot_na_adasyn_rf_pred <- predict(hot_na_adasyn_rf, newdata = test_hot_na_rf, type = "class")
hot_na_adasyn_rf_res <- model_result(hot_na_adasyn_rf_pred,label)

# 2.8.1 hot_sd_rose
hot_sd_rose_rf <- randomForest(as.factor(label)~., data = hot_sd_rose,
                               ntree = 400, importance = T, localImp = T,
                               replace = T, nodesize = 5, proximity = F)
test_hot_sd_rf <- scaled(test_hot_sd[,colnames(remaining_sc_hot_sd[-1])])
hot_sd_rose_rf_pred <- predict(hot_sd_rose_rf, newdata = test_hot_sd_rf, type = "class")
hot_sd_rose_rf_res <- model_result(hot_sd_rose_rf_pred,label)

# 2.8.2 hot_sd_smote
hot_sd_smote <- hot_sd_smote %>% rename(label = class)
hot_sd_smote_rf <- randomForest(as.factor(label)~., data = hot_sd_smote,
                                ntree = 400, importance = T, localImp = T,
                                replace = T, nodesize = 5, proximity = F)
hot_sd_smote_rf_pred <- predict(hot_sd_smote_rf, newdata = test_hot_sd_rf, type = "class")
hot_sd_smote_rf_res <- model_result(hot_sd_smote_rf_pred,label)

# 2.8.3 hot_sd_adasyn
hot_sd_adasyn <- hot_sd_adasyn %>% rename(label = class)
hot_sd_adasyn_rf <- randomForest(as.factor(label)~., data = hot_sd_adasyn,
                                 ntree = 400, importance = T, localImp = T,
                                 replace = T, nodesize = 5, proximity = F)
hot_sd_adasyn_rf_pred <- predict(hot_sd_adasyn_rf, newdata = test_hot_sd_rf, type = "class")
hot_sd_adasyn_rf_res <- model_result(hot_sd_adasyn_rf_pred,label)

# 2.9.1 hot_train_rose
hot_train_rose_rf <- randomForest(as.factor(label)~., data = hot_train_rose,
                                  ntree = 400, importance = T, localImp = T,
                                  replace = T, nodesize = 5, proximity = F)
test_train_hot_rf <- scaled(test_hot_outlier[,colnames(remaining_sc_hot_outlier[-1])])
hot_train_rose_rf_pred <- predict(hot_train_rose_rf, newdata = test_train_hot_rf, type = "class")
hot_train_rose_rf_res <- model_result(hot_train_rose_rf_pred,label)

# 2.9.2 hot_train_smote
hot_train_smote <- hot_train_smote %>% rename(label = class)
hot_train_smote_rf <- randomForest(as.factor(label)~., data = hot_train_smote,
                                   ntree = 400, importance = T, localImp = T,
                                   replace = T, nodesize = 5, proximity = F)
hot_train_smote_rf_pred <- predict(hot_train_smote_rf, newdata = test_train_hot_rf, type = "class")
hot_train_smote_rf_res <- model_result(hot_train_smote_rf_pred,label)

# 2.9.3 hot_train_adasyn
hot_train_adasyn <- hot_train_adasyn %>% rename(label = class)
hot_train_adasyn_rf <- randomForest(as.factor(label)~., data = hot_train_adasyn,
                                    ntree = 400, importance = T, localImp = T,
                                    replace = T, nodesize = 5, proximity = F)
hot_train_adasyn_rf_pred <- predict(hot_train_adasyn_rf, newdata = test_train_hot_rf, type = "class")
hot_train_adasyn_rf_res <- model_result(hot_train_adasyn_rf_pred,label)

# 2.10.1 train_hot_na_rose
train_hot_na_rose_rf <- randomForest(as.factor(label)~., data = train_hot_na_rose,
                                     ntree = 400, importance = T, localImp = T,
                                     replace = T, nodesize = 5, proximity = F)
test_hot_na_rev_rf <- test_hot_na[,colnames(remaining_hot_na[-1])]
train_hot_na_rose_rf_pred <- predict(train_hot_na_rose_rf, newdata = test_hot_na_rev_rf, type = "class")
train_hot_na_rose_rf_res <- model_result(train_hot_na_rose_rf_pred,label)

train_hot_na_rose_rf2 <- train(as.factor(label)~., data = train_hot_na_rose, 
                               method = customRF, metric = "Spec",
                               trControl = train_control, tuneGrid = tune_grid)
 # mtry = 3, ntree = 50, nodesize = 3
train_hot_na_rose_rf <- randomForest(as.factor(label)~., data = train_hot_na_rose,
                                     mtry = 3, ntree = 50, nodesize = 3, importance = T, 
                                     localImp = T,replace = T, proximity = F)
train_hot_na_rose_rf_pred <- predict(train_hot_na_rose_rf, newdata = test_hot_na_rev_rf, type = "class")
train_hot_na_rose_rf_res <- model_result(train_hot_na_rose_rf_pred,label)

 # 2.10.2 train_hot_na_smote
train_hot_na_smote <- train_hot_na_smote %>% rename(label = class)
train_hot_na_smote_rf <- randomForest(as.factor(label)~., data = train_hot_na_smote,
                                      ntree = 400, importance = T, localImp = T,
                                      replace = T, nodesize = 5, proximity = F)
train_hot_na_smote_rf_pred <- predict(train_hot_na_smote_rf, newdata = test_hot_na_rev_rf, type = "class")
train_hot_na_smote_rf_res <- model_result(train_hot_na_smote_rf_pred,label)

train_hot_na_smote_rf2 <- train(as.factor(label)~., data = train_hot_na_smote, 
                               method = customRF, metric = "Spec",
                               trControl = train_control, tuneGrid = tune_grid)
# mtry =5, ntree = 200, nodesize = 1
train_hot_na_smote <- train_hot_na_smote %>% rename(label = class)
train_hot_na_smote_rf <- randomForest(as.factor(label)~., data = train_hot_na_smote,
                                      mtry = 5, ntree = 200, nodesize = 1, 
                                      importance = T, localImp = T, replace = T, proximity = F)
train_hot_na_smote_rf_pred <- predict(train_hot_na_smote_rf, newdata = test_hot_na_rev_rf, type = "class")
train_hot_na_smote_rf_res <- model_result(train_hot_na_smote_rf_pred,label)

# 2.10.3 train_hot_na_adasyn
train_hot_na_adasyn <- train_hot_na_adasyn %>% rename(label = class)
train_hot_na_adasyn_rf <- randomForest(as.factor(label)~., data = train_hot_na_adasyn,
                                       ntree = 400, importance = T, localImp = T,
                                       replace = T, nodesize = 5, proximity = F)
train_hot_na_adasyn_rf_pred <- predict(train_hot_na_adasyn_rf, newdata = test_hot_na_rev_rf, type = "class")
train_hot_na_adasyn_rf_res <- model_result(train_hot_na_adasyn_rf_pred,label)

# 2.11.1 train_hot_sd_rose
train_hot_sd_rose_rf <- randomForest(as.factor(label)~., data = train_hot_sd_rose,
                                     ntree = 400, importance = T, localImp = T,
                                     replace = T, nodesize = 5, proximity = F)
test_hot_sd_rev_rf <- test_hot_sd[,colnames(remaining_hot_sd[-1])]
train_hot_sd_rose_rf_pred <- predict(train_hot_sd_rose_rf, newdata = test_hot_sd_rev_rf, type = "class")
train_hot_sd_rose_rf_res <- model_result(train_hot_sd_rose_rf_pred,label)

train_hot_sd_rose_rf2 <- train(as.factor(label)~., data = train_hot_sd_rose, 
                               method = customRF, metric = "Spec",
                               trControl = train_control, tuneGrid = tune_grid)
# mtry = 4, ntree = 400, nodesize = 9
train_hot_sd_rose_rf <- randomForest(as.factor(label)~., data = train_hot_sd_rose,
                                     mtry = 4, ntree = 400, nodesize = 9,
                                     importance = T, localImp = T, replace = T, proximity = F)
train_hot_sd_rose_rf_pred <- predict(train_hot_sd_rose_rf, newdata = test_hot_sd_rev_rf, type = "class")
train_hot_sd_rose_rf_res <- model_result(train_hot_sd_rose_rf_pred,label)

# 2.11.2 train_hot_sd_smote
train_hot_sd_smote <- train_hot_sd_smote %>% rename(label = class)
train_hot_sd_smote_rf <- randomForest(as.factor(label)~., data = train_hot_sd_smote,
                                      ntree = 400, importance = T, localImp = T,
                                      replace = T, nodesize = 5, proximity = F)
train_hot_sd_smote_rf_pred <- predict(train_hot_sd_smote_rf, newdata = test_hot_sd_rev_rf, type = "class")
train_hot_sd_smote_rf_res <- model_result(train_hot_sd_smote_rf_pred,label)

# 2.11.3 train_hot_sd_adasyn
train_hot_sd_adasyn <- train_hot_sd_adasyn %>% rename(label = class)
train_hot_sd_adasyn_rf <- randomForest(as.factor(label)~., data = train_hot_sd_adasyn,
                                       ntree = 400, importance = T, localImp = T,
                                       replace = T, nodesize = 5, proximity = F)
train_hot_sd_adasyn_rf_pred <- predict(train_hot_sd_adasyn_rf, newdata = test_hot_sd_rev_rf, type = "class")
train_hot_sd_adasyn_rf_res <- model_result(train_hot_sd_adasyn_rf_pred,label)

# 2.12.1 train_hot_out_rose
train_hot_out_rose_rf <- randomForest(as.factor(label)~., data = train_hot_out_rose,
                                      ntree = 400, importance = T, localImp = T,
                                      replace = T, nodesize = 5, proximity = F)
test_hot_out_rev_rf <- test_hot_outlier[,colnames(remaining_hot_outlier[-1])]
train_hot_out_rose_rf_pred <- predict(train_hot_out_rose_rf, newdata = test_hot_out_rev_rf, type = "class")
train_hot_out_rose_rf_res <- model_result(train_hot_out_rose_rf_pred,label)

train_hot_out_rose_rf2 <- train(as.factor(label)~., data = train_hot_out_rose, 
                               method = customRF, metric = "Spec",
                               trControl = train_control, tuneGrid = tune_grid)
# mtry = 2, ntree = 100, nodesize = 7
train_hot_out_rose_rf <- randomForest(as.factor(label)~., data = train_hot_out_rose,
                                      mtry = 2, ntree = 100, nodesize = 7,
                                      importance = T, localImp = T, replace = T, proximity = F)
train_hot_out_rose_rf_pred <- predict(train_hot_out_rose_rf, newdata = test_hot_out_rev_rf, type = "class")
train_hot_out_rose_rf_res <- model_result(train_hot_out_rose_rf_pred,label)

# 2.12.2 train_hot_out_smote
train_hot_out_smote <- train_hot_out_smote %>% rename(label = class)
train_hot_out_smote_rf <- randomForest(as.factor(label)~., data = train_hot_out_smote,
                                       ntree = 400, importance = T, localImp = T,
                                       replace = T, nodesize = 5, proximity = F)
test_hot_out_rev_rf <- test_hot_outlier[,colnames(remaining_hot_outlier[-1])]
train_hot_out_smote_rf_pred <- predict(train_hot_out_smote_rf, newdata = test_hot_out_rev_rf, type = "class")
train_hot_out_smote_rf_res <- model_result(train_hot_out_smote_rf_pred,label)

# 2.12.3 train_hot_out_adasyn
train_hot_out_adasyn <- train_hot_out_adasyn %>% rename(label = class)
train_hot_out_adasyn_rf <- randomForest(as.factor(label)~., data = train_hot_out_adasyn,
                                        ntree = 400, importance = T, localImp = T,
                                        replace = T, nodesize = 5, proximity = F)
train_hot_out_adasyn_rf_pred <- predict(train_hot_out_adasyn_rf, newdata = test_hot_out_rev_rf, type = "class")
train_hot_out_adasyn_rf_res <- model_result(train_hot_out_adasyn_rf_pred,label)

# 3 kNN
# Train custom random forest model
# Parameters for controlling the training with cross validation
train_control_knn <- trainControl(method = "repeatedcv", repeats = 5, search = "grid")

# Parameters of the random forest to be optimised
tune_grid_knn <- expand.grid(k = seq(1,50,by=1))

# 3.1.1 knn_na_rose
knn_na_rose_knn <- train(as.factor(label)~., data = knn_na_rose, method = "knn",
                         trControl = train_control_knn, tuneGrid = tune_grid_knn)
test_knn_na_knn <- scaled(test_knn_na[,colnames(remaining_knn_na[-1])])
knn_na_rose_knn_pred <- predict(knn_na_rose_knn, newdata = test_knn_na_knn, type = "raw")
knn_na_rose_knn_res <- model_result(knn_na_rose_knn_pred,label)

# 3.1.2 knn_na_smote
knn_na_smote <- knn_na_smote %>% rename(label = class)
knn_na_smote_knn <- train(as.factor(label)~., data = knn_na_smote, method = "knn",
                          trControl = train_control_knn, tuneGrid = tune_grid_knn)
knn_na_smote_knn_pred <- predict(knn_na_smote_knn, newdata = test_knn_na_knn, type = "raw")
knn_na_smote_knn_res <- model_result(knn_na_smote_knn_pred,label)

# 3.1.3 knn_na_adasyn
knn_na_adasyn <- knn_na_adasyn %>% rename(label = class)
knn_na_adasyn_knn <- train(as.factor(label)~., data = knn_na_adasyn, method = "knn",
                          trControl = train_control_knn, tuneGrid = tune_grid_knn)
knn_na_adasyn_knn_pred <- predict(knn_na_adasyn_knn, newdata = test_knn_na_knn, type = "raw")
knn_na_adasyn_knn_res <- model_result(knn_na_adasyn_knn_pred,label)

# 3.2.1 knn_sd_rose
knn_sd_rose_knn <- train(as.factor(label)~., data = knn_sd_rose, method = "knn",
                           trControl = train_control_knn, tuneGrid = tune_grid_knn)
test_knn_sd_knn <- scaled(test_knn_sd[,colnames(remaining_knn_sd[-1])])
knn_sd_rose_knn_pred <- predict(knn_sd_rose_knn, newdata = test_knn_sd_knn, type = "raw")
knn_sd_rose_knn_res <- model_result(knn_sd_rose_knn_pred,label)

# 3.2.2 knn_sd_smote
knn_sd_smote_knn <- train(as.factor(label)~., data = knn_sd_smote, method = "knn",
                         trControl = train_control_knn, tuneGrid = tune_grid_knn)
knn_sd_smote_knn_pred <- predict(knn_sd_smote_knn, newdata = test_knn_sd_knn, type = "raw")
knn_sd_smote_knn_res <- model_result(knn_sd_smote_knn_pred,label)

# 3.2.3 knn_sd_adasyn
knn_sd_adasyn_knn <- train(as.factor(label)~., data = knn_sd_adasyn, method = "knn",
                          trControl = train_control_knn, tuneGrid = tune_grid_knn)
knn_sd_adasyn_knn_pred <- predict(knn_sd_adasyn_knn, newdata = test_knn_sd_knn, type = "raw")
knn_sd_adasyn_knn_res <- model_result(knn_sd_adasyn_knn_pred,label)

# 3.3.1 knn_train_rose
knn_train_rose_knn <- train(as.factor(label)~., data = knn_train_rose, method = "knn",
                           trControl = train_control_knn, tuneGrid = tune_grid_knn)
test_knn_train_knn <- scaled(test_knn_outlier[,colnames(remaining_knn_train[-1])])
knn_train_rose_knn_pred <- predict(knn_train_rose_knn, newdata = test_knn_train_knn, type = "raw")
knn_train_rose_knn_res <- model_result(knn_train_rose_knn_pred,label)

# 3.3.2 knn_train_smote
knn_train_smote_knn <- train(as.factor(label)~., data = knn_train_smote, method = "knn",
                            trControl = train_control_knn, tuneGrid = tune_grid_knn)
knn_train_smote_knn_pred <- predict(knn_train_smote_knn, newdata = test_knn_train_knn, type = "raw")
knn_train_smote_knn_res <- model_result(knn_train_smote_knn_pred,label)

# 3.3.3 knn_train_adasyn
knn_train_adasyn_knn <- train(as.factor(label)~., data = knn_train_adasyn, method = "knn",
                             trControl = train_control_knn, tuneGrid = tune_grid_knn)
knn_train_adasyn_knn_pred <- predict(knn_train_adasyn_knn, newdata = test_knn_train_knn, type = "raw")
knn_train_adasyn_knn_res <- model_result(knn_train_adasyn_knn_pred,label)

# 3.4.1 train_knn_na_rose
train_knn_na_rose_sc <- scale(train_knn_na_rose[-1], scale = T, center = T)
train_knn_na_rose_sc <- bind_cols(label = train_knn_na_rose$label, train_knn_na_rose_sc)  
train_knn_na_rose_knn <- train(as.factor(label)~., data = train_knn_na_rose_sc, method = "knn",
                            trControl = train_control_knn, tuneGrid = tune_grid_knn)
test_knn_na_sc_knn <- scale(test_knn_na[,colnames(remaining_train_knn_na[-1])], scale = T, center = T)
train_knn_na_rose_knn_pred <- predict(train_knn_na_rose_knn, newdata = test_knn_na_sc_knn, type = "raw")
train_knn_na_rose_knn_res <- model_result(train_knn_na_rose_knn_pred,label)

# 3.4.2 train_knn_na_smote
train_knn_na_smote_sc <- scale(train_knn_na_smote[-length(train_knn_na_smote)], scale = T, center = T)
train_knn_na_smote_sc <- bind_cols(label = train_knn_na_smote$label, train_knn_na_smote_sc)  
train_knn_na_smote_knn <- train(as.factor(label)~., data = train_knn_na_smote_sc, method = "knn",
                               trControl = train_control_knn, tuneGrid = tune_grid_knn)
train_knn_na_smote_knn_pred <- predict(train_knn_na_smote_knn, newdata = test_knn_na_sc_knn, type = "raw")
train_knn_na_smote_knn_res <- model_result(train_knn_na_smote_knn_pred,label)

# 3.4.3 train_knn_na_dasyn
train_knn_na_adasyn_sc <- scale(train_knn_na_adasyn[-length(train_knn_na_adasyn)], scale = T, center = T)
train_knn_na_adasyn_sc <- bind_cols(label = train_knn_na_adasyn$label, train_knn_na_adasyn_sc)  
train_knn_na_adasyn_knn <- train(as.factor(label)~., data = train_knn_na_adasyn_sc, method = "knn",
                                trControl = train_control_knn, tuneGrid = tune_grid_knn)
train_knn_na_adasyn_knn_pred <- predict(train_knn_na_adasyn_knn, newdata = test_knn_na_sc_knn, type = "raw")
train_knn_na_adasyn_knn_res <- model_result(train_knn_na_adasyn_knn_pred,label)

# 3.5.1 train_knn_sd_rose
train_knn_sd_rose_sc <- scale(train_knn_sd_rose[-1], scale = T, center = T)
train_knn_sd_rose_sc <- bind_cols(label = train_knn_sd_rose$label, train_knn_sd_rose_sc)  
train_knn_sd_rose_knn <- train(as.factor(label)~., data = train_knn_sd_rose_sc, method = "knn",
                               trControl = train_control_knn, tuneGrid = tune_grid_knn)
test_knn_sd_sc_knn <- scale(test_knn_sd[,colnames(remaining_train_knn_sd[-1])], scale = T, center = T)
train_knn_sd_rose_knn_pred <- predict(train_knn_sd_rose_knn, newdata = test_knn_sd_sc_knn, type = "raw")
train_knn_sd_rose_knn_res <- model_result(train_knn_sd_rose_knn_pred,label)

# 3.5.2 train_knn_sd_smote
train_knn_sd_smote_sc <- scale(train_knn_sd_smote[-length(train_knn_sd_smote)], scale = T, center = T)
train_knn_sd_smote_sc <- bind_cols(label = train_knn_sd_smote$label, train_knn_sd_smote_sc)  
train_knn_sd_smote_knn <- train(as.factor(label)~., data = train_knn_sd_smote_sc, method = "knn",
                                trControl = train_control_knn, tuneGrid = tune_grid_knn)
train_knn_sd_smote_knn_pred <- predict(train_knn_sd_smote_knn, newdata = test_knn_sd_sc_knn, type = "raw")
train_knn_sd_smote_knn_res <- model_result(train_knn_sd_smote_knn_pred,label)

# 3.5.3 train_knn_sd_adasy
train_knn_sd_adasyn_sc <- scale(train_knn_sd_adasyn[-length(train_knn_sd_adasyn)], scale = T, center = T)
train_knn_sd_adasyn_sc <- bind_cols(label = train_knn_sd_adasyn$label, train_knn_sd_adasyn_sc)  
train_knn_sd_adasyn_knn <- train(as.factor(label)~., data = train_knn_sd_adasyn_sc, method = "knn",
                                 trControl = train_control_knn, tuneGrid = tune_grid_knn)
train_knn_sd_adasyn_knn_pred <- predict(train_knn_sd_adasyn_knn, newdata = test_knn_sd_sc_knn, type = "raw")
train_knn_sd_adasyn_knn_res <- model_result(train_knn_sd_adasyn_knn_pred,label)

# 3.6.1 train_knn_out_rose
train_knn_out_rose_sc <- scale(train_knn_out_rose[-1], scale = T, center = T)
train_knn_out_rose_sc <- bind_cols(label = train_knn_out_rose$label, train_knn_out_rose_sc)  
train_knn_out_rose_knn <- train(as.factor(label)~., data = train_knn_out_rose_sc, method = "knn",
                               trControl = train_control_knn, tuneGrid = tune_grid_knn)
test_knn_out_sc_knn <- scale(test_knn_outlier[,colnames(remaining_train_knn_outlier[-1])], scale = T, center = T)
train_knn_out_rose_knn_pred <- predict(train_knn_out_rose_knn, newdata = test_knn_out_sc_knn, type = "raw")
train_knn_out_rose_knn_res <- model_result(train_knn_out_rose_knn_pred,label)

# 3.6.1 train_knn_out_smote
train_knn_out_smote_sc <- scale(train_knn_out_smote[-length(train_knn_out_smote)], scale = T, center = T)
train_knn_out_smote_sc <- bind_cols(label = train_knn_out_smote$label, train_knn_out_smote_sc)  
train_knn_out_smote_knn <- train(as.factor(label)~., data = train_knn_out_smote_sc, method = "knn",
                                trControl = train_control_knn, tuneGrid = tune_grid_knn)
train_knn_out_smote_knn_pred <- predict(train_knn_out_smote_knn, newdata = test_knn_out_sc_knn, type = "raw")
train_knn_out_smote_knn_res <- model_result(train_knn_out_smote_knn_pred,label)

# 3.6.3 train_knn_out_adasyn
train_knn_out_adasyn_sc <- scale(train_knn_out_adasyn[-length(train_knn_out_adasyn)], scale = T, center = T)
train_knn_out_adasyn_sc <- bind_cols(label = train_knn_out_adasyn$label, train_knn_out_adasyn_sc)  
train_knn_out_adasyn_knn <- train(as.factor(label)~., data = train_knn_out_adasyn_sc, method = "knn",
                                 trControl = train_control_knn, tuneGrid = tune_grid_knn)
train_knn_out_adasyn_knn_pred <- predict(train_knn_out_adasyn_knn, newdata = test_knn_out_sc_knn, type = "raw")
train_knn_out_adasyn_knn_res <- model_result(train_knn_out_adasyn_knn_pred,label)

# 3.7.1 hot_na_rose
hot_na_rose_knn <- train(as.factor(label)~., data = hot_na_rose, method = "knn",
                         trControl = train_control_knn, tuneGrid = tune_grid_knn)
test_hot_na_knn <- scaled(test_hot_na[,colnames(remaining_sc_hot_na[-1])])
hot_na_rose_knn_pred <- predict(hot_na_rose_knn, newdata = test_hot_na_knn, type = "raw")
hot_na_rose_knn_res <- model_result(hot_na_rose_knn_pred,label)

# 3.7.2 hot_na_smote
hot_na_smote <- hot_na_smote %>% rename(label = class)
hot_na_smote_knn <- train(as.factor(label)~., data = hot_na_smote, method = "knn",
                          trControl = train_control_knn, tuneGrid = tune_grid_knn)
hot_na_smote_knn_pred <- predict(hot_na_smote_knn, newdata = test_hot_na_knn, type = "raw")
hot_na_smote_knn_res <- model_result(hot_na_smote_knn_pred,label)

# 3.7.3 knn_na_adasyn
hot_na_adasyn <- hot_na_adasyn %>% rename(label = class)
hot_na_adasyn_knn <- train(as.factor(label)~., data = hot_na_adasyn, method = "knn",
                           trControl = train_control_knn, tuneGrid = tune_grid_knn)
hot_na_adasyn_knn_pred <- predict(hot_na_adasyn_knn, newdata = test_hot_na_knn, type = "raw")
hot_na_adasyn_knn_res <- model_result(hot_na_adasyn_knn_pred,label)

# 3.8.1 hot_na_rose
hot_sd_rose_knn <- train(as.factor(label)~., data = hot_sd_rose, method = "knn",
                         trControl = train_control_knn, tuneGrid = tune_grid_knn)
test_hot_sd_knn <- scaled(test_hot_sd[,colnames(remaining_sc_hot_sd[-1])])
hot_sd_rose_knn_pred <- predict(hot_sd_rose_knn, newdata = test_hot_sd_knn, type = "raw")
hot_sd_rose_knn_res <- model_result(hot_sd_rose_knn_pred,label)

# 3.8.2 hot_na_smote
hot_sd_smote <- hot_sd_smote %>% rename(label = class)
hot_sd_smote_knn <- train(as.factor(label)~., data = hot_sd_smote, method = "knn",
                          trControl = train_control_knn, tuneGrid = tune_grid_knn)
hot_sd_smote_knn_pred <- predict(hot_sd_smote_knn, newdata = test_hot_sd_knn, type = "raw")
hot_sd_smote_knn_res <- model_result(hot_sd_smote_knn_pred,label)

# 3.8.3 knn_na_adasyn
hot_sd_adasyn <- hot_sd_adasyn %>% rename(label = class)
hot_sd_adasyn_knn <- train(as.factor(label)~., data = hot_sd_adasyn, method = "knn",
                           trControl = train_control_knn, tuneGrid = tune_grid_knn)
hot_sd_adasyn_knn_pred <- predict(hot_sd_adasyn_knn, newdata = test_hot_sd_knn, type = "raw")
hot_sd_adasyn_knn_res <- model_result(hot_sd_adasyn_knn_pred,label)

# 3.9.1 hot_train_rose
hot_train_rose_knn <- train(as.factor(label)~., data = hot_train_rose, method = "knn",
                            trControl = train_control_knn, tuneGrid = tune_grid_knn)
test_hot_train_knn <- scaled(test_hot_outlier[,colnames(remaining_sc_hot_outlier[-1])])
hot_train_rose_knn_pred <- predict(hot_train_rose_knn, newdata = test_hot_train_knn, type = "raw")
hot_train_rose_knn_res <- model_result(hot_train_rose_knn_pred,label)

# 3.9.2 hot_train_smote
hot_train_smote_knn <- train(as.factor(label)~., data = hot_train_smote, method = "knn",
                             trControl = train_control_knn, tuneGrid = tune_grid_knn)
hot_train_smote_knn_pred <- predict(hot_train_smote_knn, newdata = test_hot_train_knn, type = "raw")
hot_train_smote_knn_res <- model_result(hot_train_smote_knn_pred,label)

# 3.9.3 hot_train_adasyn
hot_train_adasyn_knn <- train(as.factor(label)~., data = hot_train_adasyn, method = "knn",
                              trControl = train_control_knn, tuneGrid = tune_grid_knn)
hot_train_adasyn_knn_pred <- predict(hot_train_adasyn_knn, newdata = test_hot_train_knn, type = "raw")
hot_train_adasyn_knn_res <- model_result(hot_train_adasyn_knn_pred,label)

# 3.10.1 train_hot_na_rose
train_hot_na_rose_sc <- scale(train_hot_na_rose[-1], scale = T, center = T)
train_hot_na_rose_sc <- bind_cols(label = train_hot_na_rose$label, train_hot_na_rose_sc)  
train_hot_na_rose_knn <- train(as.factor(label)~., data = train_hot_na_rose_sc, method = "knn",
                               trControl = train_control_knn, tuneGrid = tune_grid_knn)
test_hot_na_sc_knn <- scale(test_hot_na[,colnames(remaining_hot_na[-1])], scale = T, center = T)
train_hot_na_rose_knn_pred <- predict(train_hot_na_rose_knn, newdata = test_hot_na_sc_knn, type = "raw")
train_hot_na_rose_knn_res <- model_result(train_hot_na_rose_knn_pred,label)

# 3.10.2 train_hot_na_smote
train_hot_na_smote_sc <- scale(train_hot_na_smote[-length(train_hot_na_smote)], scale = T, center = T)
train_hot_na_smote_sc <- bind_cols(label = train_hot_na_smote$label, train_hot_na_smote_sc)  
train_hot_na_smote_knn <- train(as.factor(label)~., data = train_hot_na_smote_sc, method = "knn",
                                trControl = train_control_knn, tuneGrid = tune_grid_knn)
train_hot_na_smote_knn_pred <- predict(train_hot_na_smote_knn, newdata = test_hot_na_sc_knn, type = "raw")
train_hot_na_smote_knn_res <- model_result(train_hot_na_smote_knn_pred,label)

# 3.10.3 train_hot_na_dasyn
train_hot_na_adasyn_sc <- scale(train_hot_na_adasyn[-length(train_hot_na_adasyn)], scale = T, center = T)
train_hot_na_adasyn_sc <- bind_cols(label = train_hot_na_adasyn$label, train_hot_na_adasyn_sc)  
train_hot_na_adasyn_knn <- train(as.factor(label)~., data = train_hot_na_adasyn_sc, method = "knn",
                                 trControl = train_control_knn, tuneGrid = tune_grid_knn)
train_hot_na_adasyn_knn_pred <- predict(train_hot_na_adasyn_knn, newdata = test_hot_na_sc_knn, type = "raw")
train_hot_na_adasyn_knn_res <- model_result(train_hot_na_adasyn_knn_pred,label)

# 3.11.1 train_hot_na_rose
train_hot_sd_rose_sc <- scale(train_hot_sd_rose[-1], scale = T, center = T)
train_hot_sd_rose_sc <- bind_cols(label = train_hot_sd_rose$label, train_hot_sd_rose_sc)  
train_hot_sd_rose_knn <- train(as.factor(label)~., data = train_hot_sd_rose_sc, method = "knn",
                               trControl = train_control_knn, tuneGrid = tune_grid_knn)
test_hot_sd_sc_knn <- scale(test_hot_sd[,colnames(remaining_hot_sd[-1])], scale = T, center = T)
train_hot_sd_rose_knn_pred <- predict(train_hot_sd_rose_knn, newdata = test_hot_sd_sc_knn, type = "raw")
train_hot_sd_rose_knn_res <- model_result(train_hot_sd_rose_knn_pred,label)

# 3.11.2 train_hot_na_smote
train_hot_sd_smote_sc <- scale(train_hot_sd_smote[-length(train_hot_sd_smote)], scale = T, center = T)
train_hot_sd_smote_sc <- bind_cols(label = train_hot_sd_smote$label, train_hot_sd_smote_sc)  
train_hot_sd_smote_knn <- train(as.factor(label)~., data = train_hot_sd_smote_sc, method = "knn",
                                trControl = train_control_knn, tuneGrid = tune_grid_knn)
train_hot_sd_smote_knn_pred <- predict(train_hot_sd_smote_knn, newdata = test_hot_sd_sc_knn, type = "raw")
train_hot_sd_smote_knn_res <- model_result(train_hot_sd_smote_knn_pred,label)

# 3.11.3 train_hot_na_dasyn
train_hot_sd_adasyn_sc <- scale(train_hot_sd_adasyn[-length(train_hot_sd_adasyn)], scale = T, center = T)
train_hot_sd_adasyn_sc <- bind_cols(label = train_hot_sd_adasyn$label, train_hot_sd_adasyn_sc)  
train_hot_sd_adasyn_knn <- train(as.factor(label)~., data = train_hot_sd_adasyn_sc, method = "knn",
                                 trControl = train_control_knn, tuneGrid = tune_grid_knn)
train_hot_sd_adasyn_knn_pred <- predict(train_hot_sd_adasyn_knn, newdata = test_hot_sd_sc_knn, type = "raw")
train_hot_sd_adasyn_knn_res <- model_result(train_hot_sd_adasyn_knn_pred,label)

# 3.12.1 train_hot_out_rose
train_hot_out_rose_sc <- scale(train_hot_out_rose[-1], scale = T, center = T)
train_hot_out_rose_sc <- bind_cols(label = train_hot_out_rose$label, train_hot_out_rose_sc)  
train_hot_out_rose_knn <- train(as.factor(label)~., data = train_hot_out_rose_sc, method = "knn",
                               trControl = train_control_knn, tuneGrid = tune_grid_knn)
test_hot_out_sc_knn <- scale(test_hot_outlier[,colnames(remaining_hot_outlier[-1])], scale = T, center = T)
train_hot_out_rose_knn_pred <- predict(train_hot_out_rose_knn, newdata = test_hot_out_sc_knn, type = "raw")
train_hot_out_rose_knn_res <- model_result(train_hot_out_rose_knn_pred,label)

# 3.12.2 train_hot_out_smote
train_hot_out_smote_sc <- scale(train_hot_out_smote[-length(train_hot_out_smote)], scale = T, center = T)
train_hot_out_smote_sc <- bind_cols(label = train_hot_out_smote$label, train_hot_out_smote_sc)  
train_hot_out_smote_knn <- train(as.factor(label)~., data = train_hot_out_smote_sc, method = "knn",
                                trControl = train_control_knn, tuneGrid = tune_grid_knn)
train_hot_out_smote_knn_pred <- predict(train_hot_out_smote_knn, newdata = test_hot_out_sc_knn, type = "raw")
train_hot_out_smote_knn_res <- model_result(train_hot_out_smote_knn_pred,label)

# 3.12.3 train_hot_out_adasyn
train_hot_out_adasyn_sc <- scale(train_hot_out_adasyn[-length(train_hot_out_adasyn)], scale = T, center = T)
train_hot_out_adasyn_sc <- bind_cols(label = train_hot_out_adasyn$label, train_hot_out_adasyn_sc)  
train_hot_out_adasyn_knn <- train(as.factor(label)~., data = train_hot_out_adasyn_sc, method = "knn",
                                 trControl = train_control_knn, tuneGrid = tune_grid_knn)
train_hot_out_adasyn_knn_pred <- predict(train_hot_out_adasyn_knn, newdata = test_hot_out_sc_knn, type = "raw")
train_hot_out_adasyn_knn_res <- model_result(train_hot_out_adasyn_knn_pred,label)


# 4. SVM - Support Vector Machine 
# There are 2 Kernels that can be performed for SVM, i.e. linear kernel and RBF kernel
# Since SVM RBF is under performing SVM Linear kernel, the implementation will only be shown for linear kernel. 
# Define the control parameters
train_control_svm <- trainControl(method = "cv", number = 5, search = "grid")

# 4.1.1 - 4.1.3 knn_na
# Between the rose. smote, and adasyn training set; edit the data argument in train() function to respective balancing methods
knn_na_svm <- train(as.factor(label)~., data = knn_na_rose, method = "svmLinear", trControl = train_control_svm)
test_knn_na_sc <- scaled(test_knn_na[,colnames(remaining_knn_na[-1])])
knn_na_svm_pred <- predict(knn_na_svm, newdata = test_knn_na_sc, type = "raw")
test_knn_na_sc <- bind_cols(select(test_set, label = label), test_knn_na_sc)
knn_na_svm_res <- model_result(knn_na_svm_pred, test_knn_na_sc$label)

# 4.2.1 - 4.2.3 knn_sd
knn_sd_svm <- train(as.factor(label)~., data = knn_sd_rose, method = "svmLinear", trControl = train_control_svm)
test_knn_sd_sc <- scaled(test_knn_sd[,colnames(remaining_knn_sd[-1])])
knn_sd_svm_pred <- predict(knn_sd_svm, newdata = test_knn_sd_sc, type = "raw")
test_knn_sd_sc <- bind_cols(select(test_set, label = label), test_knn_sd_sc)
knn_sd_svm_res <- model_result(knn_sd_svm_pred, test_knn_sd_sc$label)

# 4,3.1 - 4,3.3 knn_train
knn_train_svm <- train(as.factor(label)~., data = knn_train_rose, method = "svmLinear", trControl = train_control_svm)
test_knn_train_sc <- scaled(test_knn_outlier[,colnames(remaining_knn_train[-1])])
knn_train_svm_pred <- predict(knn_train_svm, newdata = test_knn_train_sc, type = "raw")
test_knn_train_sc <- bind_cols(select(test_set, label = label), test_knn_train_sc)
knn_train_svm_res <- model_result(knn_train_svm_pred, test_knn_train_sc$label)

# 4.4.1 - 4.4.3 train_knn_na
train_knn_na_svm <- train(as.factor(label)~., data = train_knn_na_rose, method = "svmLinear", trControl = train_control_svm)
test_knn_na_data <- test_knn_na[,colnames(remaining_train_knn_na[-1])]
train_knn_na_svm_pred <- predict(train_knn_na_svm, newdata = test_knn_na_data, type = "raw")
test_train_knn_na <- bind_cols(select(test_set, label = label), test_knn_na_data)
train_knn_na_svm_res <- model_result(train_knn_na_svm_pred, test_train_knn_na$label)

# 4.5.1 - 4.5.3 train_knn_sd
train_knn_sd_svm <- train(as.factor(label)~., data = train_knn_sd_rose, method = "svmLinear", trControl = train_control_svm)
test_knn_sd_data <- test_knn_sd[,colnames(remaining_train_knn_sd[-1])]
train_knn_sd_svm_pred <- predict(train_knn_sd_svm, newdata = test_knn_sd_data, type = "raw")
test_train_knn_sd <- bind_cols(select(test_set, label = label), test_knn_sd_data)
train_knn_sd_svm_res <- model_result(train_knn_sd_svm_pred, test_train_knn_sd$label)

# 4.6.1 - 4.6.3 train_knn_out
train_knn_out_svm <- train(as.factor(label)~., data = train_knn_out_rose, method = "svmLinear", trControl = train_control_svm)
test_knn_outlier_data <- test_knn_outlier[,colnames(remaining_train_knn_outlier[-1])]
train_knn_out_svm_pred <- predict(train_knn_out_svm, newdata = test_knn_outlier_data, type = "raw")
test_train_knn_out <- bind_cols(select(test_set, label = label), test_knn_outlier_data)
train_knn_out_svm_res <- model_result(train_knn_out_svm_pred, test_train_knn_out$label)

# 4.7.1 - 4.7.3 hot_na
hot_na_svm <- train(as.factor(label)~., data = hot_na_rose, method = "svmLinear", trControl = train_control_svm)
test_hot_na_sc <- scaled(test_hot_na[,colnames(remaining_sc_hot_na[-1])])
hot_na_svm_pred <- predict(hot_na_svm, newdata = test_hot_na_sc, type = "raw")
test_hot_na_sc <- bind_cols(select(test_set, label = label), test_hot_na_sc)
hot_na_svm_res <- model_result(hot_na_svm_pred, test_hot_na_sc$label)

# 4.8.1 - 4.8.3 hot_sd
hot_sd_svm <- train(as.factor(label)~., data = hot_sd_rose, method = "svmLinear", trControl = train_control_svm)
test_hot_sd_sc <- scaled(test_hot_sd[,colnames(remaining_sc_hot_sd[-1])])
hot_sd_svm_pred <- predict(hot_sd_svm, newdata = test_hot_sd_sc, type = "raw")
test_hot_sd_sc <- bind_cols(select(test_set, label = label), test_hot_sd_sc)
hot_sd_svm_res <- model_result(hot_sd_svm_pred, test_hot_sd_sc$label)

# 4.9.1 - 4.9.3 hot_train
hot_train_svm <- train(as.factor(label)~., data = hot_train_rose, method = "svmLinear", trControl = train_control_svm)
test_hot_train_sc <- scaled(test_hot_outlier[,colnames(remaining_sc_hot_outlier[-1])])
hot_train_svm_pred <- predict(hot_train_svm, newdata = test_hot_train_sc, type = "raw")
test_hot_train_sc <- bind_cols(select(test_set, label = label), test_hot_train_sc)
hot_train_svm_res <- model_result(hot_train_svm_pred, test_hot_train_sc$label)

# 4.10.1 - 4.10.3 train_hot_na
train_hot_na_svm <- train(as.factor(label)~., data = train_hot_na_rose, method = "svmLinear", trControl = train_control_svm)
test_hot_na_data <- test_hot_na[,colnames(remaining_hot_na[-1])]
train_hot_na_svm_pred <- predict(train_hot_na_svm, newdata = test_hot_na_data, type = "raw")
test_train_hot_na <- bind_cols(select(test_set, label = label), test_hot_na_data)
train_hot_na_svm_res <- model_result(train_hot_na_svm_pred, test_train_hot_na$label)

# 4.11.1 - 4.11.3 train_hot_sd
train_hot_sd_svm <- train(as.factor(label)~., data = train_hot_sd_rose, method = "svmLinear", trControl = train_control_svm)
test_hot_sd_data <- test_hot_sd[,colnames(remaining_hot_sd[-1])]
train_hot_sd_svm_pred <- predict(train_hot_sd_svm, newdata = test_hot_sd_data, type = "raw")
test_train_hot_sd <- bind_cols(select(test_set, label = label), test_hot_sd_data)
train_hot_sd_svm_res <- model_result(train_hot_sd_svm_pred, test_train_hot_sd$label)

# 4.12.1 - 4.12.3 train_hot_out
train_hot_out_svm <- train(as.factor(label)~., data = train_hot_out_rose, method = "svmLinear", trControl = train_control_svm)
test_hot_out_data <- test_hot_outlier[,colnames(remaining_hot_outlier[-1])]
train_hot_out_svm_pred <- predict(train_hot_out_svm, newdata = test_hot_out_data, type = "raw")
test_train_hot_out <- bind_cols(select(test_set, label = label), test_hot_out_data)
train_hot_out_svm_res <- model_result(train_hot_out_svm_pred, test_train_hot_out$label)


# 5. ANN - Artificial Neural Network
# Use Model Averaged Neural Network and define tuning parameters to improve stability of the model
# Set random seed
set.seed(54321)

# Define the control parameters
train_control_nn <- trainControl(method = "cv", number = 5, search = "grid")

# Define tune grid of number of hidden units, weight decay, and bagging to improve stability
train_grid_nn <- expand.grid(size = c(5,10,20), 
                             decay = c(0.01,0.001,0.0001),
                             bag = 10)

# The scaled data sets from 5.1 to 5.3 will not be implemented with ANN, since the result is a random model that classifies all wafers either as good or faulty.

# 5.4.1 - 5.4.3 train_knn_na
train_knn_na_nn <- train(as.factor(label)~., data = train_knn_na_rose, method = "avNNet", trControl = train_control_nn, tuneGrid = train_grid_nn)
test_knn_na_data <- test_knn_na[,colnames(remaining_train_knn_na[-1])]
train_knn_na_nn_pred <- predict(train_knn_na_nn, newdata = test_knn_na_data, type = "raw")
test_train_knn_na <- bind_cols(select(test_set, label = label), test_knn_na_data)
train_knn_na_nn_res <- model_result(train_knn_na_nn_pred, test_train_knn_na$label)

# 5.5.1 - 5.5.3 train_knn_sd
train_knn_sd_nn <- train(as.factor(label)~., data = train_knn_sd_rose, method = "avNNet", trControl = train_control_nn, tuneGrid = train_grid_nn)
test_knn_sd_data <- test_knn_sd[,colnames(remaining_train_knn_sd[-1])]
train_knn_sd_nn_pred <- predict(train_knn_sd_nn, newdata = test_knn_sd_data, type = "raw")
test_train_knn_sd <- bind_cols(select(test_set, label = label), test_knn_sd_data)
train_knn_sd_nn_res <- model_result(train_knn_sd_nn_pred, test_train_knn_sd$label)

# 5.6.1 - 5.6.3 train_knn_out
train_knn_out_nn <- train(as.factor(label)~., data = train_knn_out_rose, method = "avNNet", trControl = train_control_nn, tuneGrid = train_grid_nn)
test_knn_outlier_data <- test_knn_outlier[,colnames(remaining_train_knn_outlier[-1])]
train_knn_out_nn_pred <- predict(train_knn_out_nn, newdata = test_knn_outlier_data, type = "raw")
test_train_knn_out <- bind_cols(select(test_set, label = label), test_knn_outlier_data)
train_knn_out_nn_res <- model_result(train_knn_out_nn_pred, test_train_knn_out$label)

# The scaled data sets from 5.7 to 5.9 will not be implemented with ANN, since the result is a random model that classifies all wafers either as good or faulty.

# 5.10.1 - 5.10.3 train_hot_na
train_hot_na_nn <- train(as.factor(label)~., data = train_hot_na_rose, method = "avNNet", trControl = train_control_nn, tuneGrid = train_grid_nn)
test_hot_na_data <- test_hot_na[,colnames(remaining_hot_na[-1])]
train_hot_na_nn_pred <- predict(train_hot_na_nn, newdata = test_hot_na_data, type = "raw")
print(train_hot_na_nn_pred)
test_train_hot_na <- bind_cols(select(test_set, label = label), test_hot_na_data)
train_hot_na_nn_res <- model_result(train_hot_na_nn_pred, test_train_hot_na$label)

# 5.11.1 - 5.11.3 train_hot_sd
train_hot_sd_nn <- train(as.factor(label)~., data = train_hot_sd_rose, method = "avNNet", trControl = train_control_nn, tuneGrid = train_grid_nn)
test_hot_sd_data <- test_hot_sd[,colnames(remaining_hot_sd[-1])]
train_hot_sd_nn_pred <- predict(train_hot_sd_svm, newdata = test_hot_sd_data, type = "raw")
test_train_hot_sd <- bind_cols(select(test_set, label = label), test_hot_sd_data)
train_hot_sd_nn_res <- model_result(train_hot_sd_nn_pred, test_train_hot_sd$label)

# 5.12.1 - 5.12.3 train_hot_out
train_hot_out_nn <- train(as.factor(label)~., data = train_hot_out_rose, method = "avNNet", trControl = train_control_nn, tuneGrid = train_grid_nn)
test_hot_out_data <- test_hot_outlier[,colnames(remaining_hot_outlier[-1])]
train_hot_out_nn_pred <- predict(train_hot_out_nn, newdata = test_hot_out_data, type = "raw")
test_train_hot_out <- bind_cols(select(test_set, label = label), test_hot_out_data)
train_hot_out_nn_res <- model_result(train_hot_out_nn_pred, test_train_hot_out$label)
