# I Install/Load required packages --------------------------------------------
if (!require("pacman")) install.packages("pacman")
pacman::p_load(tidyverse, rlang, Hmisc, lubridate, corrplot, VIM, mice, Boruta, EFAtools, FactoMineR, psych, ROSE, smotefamily, randomForest, caret)

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

# 4 Based on correlation
feature_corr_all(train_red[,!names(train_red) %in% c("label","date")])
# 8 perfect correlations
# 1190 strong correlations

dim_red_corr <- function(x) {
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
  corr_mat_sig <- subset(df_merge, p.val <= p) %>%
    filter(corr >= 0.8 | corr <= -0.8) %>%
    arrange(desc(corr))
  
  # Extract unique features with significant correlations
  corr_mat_sig_tar <- corr_mat_sig %>%
    dplyr::select(features1) %>%
    distinct() %>%
    pull(as.vector(features1))
  
  #Extract target variable
  target <- x[1]
  
  # Create dataset with only target variable and features with significant correlations
  x2 <- x %>%
    dplyr::select(all_of(corr_mat_sig_tar))
  x2 <- as_tibble(cbind(target,x2))
  
  # Create empty data frame for the loop
  corr_test_results <- data_frame(feature = character(),
                                  p.value = numeric(),
                                  corr.value = numeric())
  
  # Loop through the dataset and calculate the correlation of each feature with the target variable
  for (col in names(x)) {
    target <- as.vector(x$label)
    predictor <- sign(as.vector(x[[col]]))*log((abs(as.vector(x[[col]])+1)))
    corr_test <- cor.test(target,predictor, method = "pearson")
    p.val <- corr_test$p.value
    corr.val <- corr_test$estimate
    corr_test_results <- rbind(corr_test_results, data_frame(feature = col,
                                                             p.value = corr_test$p.value,
                                                             corr.value = corr_test$estimate))
  }
  corr_test_results
  
  # Number of features with a significant correlation to the target variable
  corr_test_results %>%
    filter(p.value < p) %>%
    nrow()
  # 55 out of 374 (14.7%)
  
  # Create empty vector for features which are going to be eliminated
  feature_red <- c()
  
  # Loop through each highly correlated pair and check if each feature in that pair has a 
  # significant correlation with the target variable and if so, pick the feature which is
  # more strongly correlated to the target variable
  for (row in 1:nrow(corr_mat_sig)) {
    # Extract first feature of correlation pair
    feature_1 <- corr_mat_sig %>%
      slice(row,) %>%
      pull(as.vector(features1))
    # Extract second feature of correlation pair
    feature_2 <- corr_mat_sig %>%
      slice(row,) %>%
      pull(as.vector(features2))
    # Extract results for first feature of correlation pair
    check_feature_1 <- corr_test_results %>%
      filter(feature == feature_1)
    # Extract results from second feature of correlation pair
    check_feature_2 <- corr_test_results %>%
      filter(feature == feature_2)
    
    # Conditions for choosing which feature to eliminate
    # If one or both features do not have a significant correlation with the target variable
    # If one feature has a stronger correlation to the target variable
    if ((check_feature_1$p.value < p & check_feature_2$p.value < p) & check_feature_1$corr.value > check_feature_2$corr.value) {
      feature_red <- c(feature_red, feature_2)
    } else if ((check_feature_1$p.value < p & check_feature_2$p.value < p) & check_feature_1$corr.value < check_feature_2$corr.value) {
      feature_red <- c(feature_red, feature_1)
    }
feature_red
  }
  feature_red
  x <- x %>%
    dplyr::select(-all_of(unique(feature_red)))
  x
}
train_red2 <- dim_red_corr(select_if(train_red, is.numeric))
# 37 features are removed

# Add date back to reduced dataset
train_red2 <- as_tibble(bind_cols(label = train_red2$label, date = train_red$date, train_red2[-1]))

saveRDS(train_red2, "train_red2.RDS")
train_red2 <- readRDS("train_red2.RDS")


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
  scaled <- scale(x, scale = T, center = T)
  scaled <- as.tibble(scaled) %>%
    mutate(across(everything(), as.numeric))
  return(scaled)
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
# For SD set
hot_sd_var <- feature_var_all(hot_sd)

# 3.1.2 Imputation before outlier hanndling
hot_red <- hotdeck(train_red, impNA = T, imp_var = F)
sum(is.na(hot_red))

# Check effects
train_red_hot_var <- feature_var_all(hot_red)

# Outlier handling
train_hot_outlier <- hot_red %>%
  mutate(across(where(is.numeric),~ outlier_replace(.x, method = "SD")))
as_tibble(train_hot_outlier)

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

# Merge label and date back together with the rest of the data for the imputation
knn_sd <- kNN(bind_cols(outlier_sd[,names(outlier_sd) %in% c("label","date")],scaled_sd), 
              k = 5, impNA = T, imp_var = F)
as_tibble(knn_sd)
sum(is.na(knn_sd))

# Check effects of imputation
# Reverse scaling to check the effects of imputation 
reverse_scaling <- function(old_data, scaled_data) {
  mu <- sapply(old_data, mean, na.rm = T)
  s <- sapply(old_data, sd, na.rm = T)
  reverse_scaled <- (scaled_data*s)+mu
  return(reverse_scaled)
}
# For NA set
train_knn_na <- reverse_scaling(outlier_na[,!names(outlier_na) %in% c("label","date")], knn_na[,!names(outlier_na) %in% c("label","date")])
train_knn_na <- as_tibble(bind_cols(knn_na[,names(knn_na) %in% c("label","date")],train_knn_na))
# Huge increase in variance by setting k = 5
train_knn_na_var <- feature_var_all(train_knn_na)

# For SD set
train_knn_sd <- reverse_scaling(outlier_sd[,!names(outlier_sd) %in% c("label","date")], knn_sd[,!names(outlier_sd) %in% c("label","date")])
train_knn_sd <- as_tibble(bind_cols(knn_sd[,names(knn_sd) %in% c("label","date")],train_knn_sd))
# Increase in variance not that significant for k = 5 in comparison to _na set
train_knn_sd_var <- feature_var_all(train_knn_sd)

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

# Reverse scaling
train_red_knn <- reverse_scaling(train_red[,!names(train_red) %in% c("label","date")], knn_train_red[,!names(knn_train_red) %in% c("label","date")])
# Increase in variance for k =5 in comparison to _na set
train_red_knn_var <- feature_var_all(train_red_knn)

# Outlier handling with 4S
train_knn_outlier <- train_red_knn %>%
  mutate(across(where(is.numeric),~ outlier_replace(.x, method = "SD")))
train_knn_outllier <- as_tibble(bind_cols(train_red[,names(train_red) %in% c("label","date")], train_red_knn))

saveRDS(train_knn_outlier, "train_knn_outlier.RDS")
train_knn_outlier <- readRDS("train_knn_outlier.RDS")

# 3.3 Iterative robust model-based imputation (irmi) --------------------------
# Option to be robust and handle outliers, can therefore be used before or after outlier handling
# 3.3.1 Single imputation after outlier handling ------------------------------
# Computation time ~ 20 mins
# irmi function can't process datetimes 
irmi_na <- irmi(bind_cols(outlier_na[,names(outlier_na) %in% c("label")],scaled_na),
                maxit = 5,
                robust = F,
                init.method = "kNN",
                imp_var = F)
as_tibble(irmi_na)
sum(is.na(irmi_na))

irmi_sd <- irmi(bind_cols(outlier_sd[,names(outlier_sd) %in% c("label")],scaled_sd),
                maxit = 5,
                robust = F,
                init.method = "kNN",
                imp_var = F)
as_tibble(irmi_na)
sum(is.na(irmi_na))

# Check effects of imputation
# Reverse scaling to check the effects of imputation 
# For NA set
train_irmi_na <- reverse_scaling(outlier_na[,names(outlier_na) %in% c("label","date")],irmi_na[,!names(irmi_na) %in% c("label")])
train_irmi_na <- as_tibble(bind_cols(outlier_na[,names(outlier_na) %in% c("label","date")],train_irmi_na))

saveRDS(train_irmi_na, "train_irmi_na.RDS")
train_irmi_na <- readRDS("train_irmi_na.RDS")

# Huge increase is volatility
train_irmi_na_var <- feature_var_all(train_irmi_na)

# For SA set
train_irmi_sd <- reverse_scaling(outlier_sd[,names(outlier_sd) %in% c("label","date")],irmi_sd[,!names(irmi_sd) %in% c("label")])
train_irmi_sd <- as_tibble(bind_cols(outlier_sd[,names(outlier_sd) %in% c("label","date")],train_irmi_sd))

saveRDS(train_irmi_sd, "train_irmi_sd.RDS")
train_irmi_sd <- readRDS("train_irmi_sd.RDS")

# Small increase in volatility
train_irmi_sd_var <- feature_var_all(train_irmi_sd)

# 3.3.2 Single imputation before outlier handling -----------------------------
# Use robust methods to suppress influence of outliers in the dataset
# Computation time > 2h so Will not be implemented

# 2.2.4 MICE ------------------------------------------------------------------
# Parametric, assumes mutlivariate normal distribution, can therefore only be used after outlier handling
# Computation time ~ 50 mins per iteration so will not be implemented


# IX Feature selection/reduction
# 1 Feature selection by BORUTA -----------------------------------------------

# use Boruta algorithm on the imputed dataset
set.seed(1234)

# 9.1 Perform Boruta algorithm on scaled 'knn_na'
knn_na_boruta <- Boruta(
  label~.,
  data = knn_na,
  pValue = 0.05,   # Set the p-value threshold to 0.05
  maxRuns = 200,   # Set the maximum number of runs to 200
  doTrace = 2      # Allows you to get a report of the progress of the process
)
print(knn_na_boruta)

#take a call on tentative features
knn_na_boruta_tent <- TentativeRoughFix(knn_na_boruta)
print(knn_na_boruta_tent)

# confirm the importance of the features
getSelectedAttributes(knn_na_boruta_tent, withTentative = T)
# there are 17 confirmed important features and the rest 444 features are rejected.
knn_na_boruta_stats <- attStats(knn_na_boruta_tent)
plot(normHits~meanImp,col=knn_na_boruta_stats$decision,data=knn_na_boruta_stats)
print(knn_na_boruta_stats)
                
# 9.2 Perform Boruta algorithm on scaled 'knn_sd'
knn_sd_boruta <- Boruta(
  label~.,
  data = knn_sd,
  pValue = 0.05,   # Set the p-value threshold to 0.05
  maxRuns = 200,   # Set the maximum number of runs to 200
  doTrace = 2      # Allows you to get a report of the progress of the process
)
print(knn_sd_boruta)
#take a call on tentative features
knn_sd_boruta_tent <- TentativeRoughFix(knn_sd_boruta)
print(knn_sd_boruta_tent)

# you can confirm the importance of the features by typing
getSelectedAttributes(knn_sd_boruta_tent, withTentative = T)
# there are  25 confirmed important features and the rest 436 features are rejected.
knn_sd_boruta_stats <- attStats(knn_sd_boruta_tent)
plot(normHits~meanImp,col=knn_sd_boruta_stats$decision,data=knn_sd_boruta_stats)
print(knn_sd_boruta_stats)

# 9.3 Perform Boruta algorithm on scaled 'irmi_na'
irmi_na_boruta <- Boruta(
  label~.,
  data = irmi_na,
  pValue = 0.05,   # Set the p-value threshold to 0.05
  maxRuns = 200,   # Set the maximum number of runs to 200
  doTrace = 2      # Allows you to get a report of the progress of the process
)
print(irmi_na_boruta)
#take a call on tentative features
irmi_na_boruta_tent <- TentativeRoughFix(irmi_na_boruta)
print(irmi_na_boruta_tent)

# you can confirm the importance of the features by typing
getSelectedAttributes(irmi_na_boruta_tent, withTentative = T)
# there are  22 confirmed important features and the rest 438 features are rejected.
irmi_na_boruta_stats <- attStats(irmi_na_boruta_tent)
plot(normHits~meanImp,col=irmi_na_boruta_stats$decision,data=irmi_na_boruta_stats)
print(irmi_na_boruta_stats)

# 9.4 Perform Boruta algorithm on scaled 'irmi_sd'
irmi_sd_boruta <- Boruta(
  label~.,
  data = irmi_sd,
  pValue = 0.05,   # Set the p-value threshold to 0.05
  maxRuns = 200,   # Set the maximum number of runs to 200
  doTrace = 2      # Allows you to get a report of the progress of the process
)
print(irmi_sd_boruta)
#take a call on tentative features
irmi_sd_boruta_tent <- TentativeRoughFix(irmi_sd_boruta)
print(irmi_sd_boruta_tent)

# you can confirm the importance of the features by typing
getSelectedAttributes(irmi_sd_boruta_tent, withTentative = T)
# there are  35 confirmed important features and the rest 425 features are rejected.
irmi_sd_boruta_stats <- attStats(irmi_sd_boruta_tent)
plot(normHits~meanImp,col=irmi_sd_boruta_stats$decision,data=irmi_sd_boruta_stats)
print(irmi_sd_boruta_stats)


# 9.5 Perform Boruta algorithm on unscaled 'train_knn_na'
train_knn_na_boruta <- bind_cols(select(knn_na, label = label), train_knn_na)

train_knn_na_boruta <- Boruta(
  label~.,
  data = train_knn_na_boruta,
  pValue = 0.05,   # Set the p-value threshold to 0.05
  maxRuns = 200,   # Set the maximum number of runs to 200
  doTrace = 2      # Allows you to get a report of the progress of the process
)
print(train_knn_na_boruta)

#take a call on tentative features
train_knn_na_boruta_tent <- TentativeRoughFix(train_knn_na_boruta)
print(train_knn_na_boruta_tent)

# confirm the importance of the features
getSelectedAttributes(train_knn_na_boruta_tent, withTentative = T)
# there are  7 confirmed important features and the rest 453 features are rejected.
train_knn_na_boruta_stats <- attStats(train_knn_na_boruta_tent)
plot(normHits~meanImp,col=train_knn_na_boruta_stats$decision,data=train_knn_na_boruta_stats)
print(train_knn_na_boruta_stats)

# 9.6 Perform Boruta algorithm on unscaled 'train_knn_sd'
train_knn_sd_boruta <- bind_cols(select(knn_sd, label = label), train_knn_sd)

train_knn_sd_boruta <- Boruta(
  label~.,
  data = train_knn_sd_boruta,
  pValue = 0.05,   # Set the p-value threshold to 0.05
  maxRuns = 200,   # Set the maximum number of runs to 200
  doTrace = 2      # Allows you to get a report of the progress of the process
)
print(train_knn_sd_boruta)

#take a call on tentative features
train_knn_sd_boruta_tent <- TentativeRoughFix(train_knn_sd_boruta)
print(train_knn_sd_boruta_tent)

# confirm the importance of the features
getSelectedAttributes(train_knn_sd_boruta_tent, withTentative = T)
# there is only 1 confirmed important features and the rest 453 features are rejected.
train_knn_sd_boruta_stats <- attStats(train_knn_sd_boruta_tent)
plot(normHits~meanImp,col=train_knn_sd_boruta_stats$decision,data=train_knn_sd_boruta_stats)
print(train_knn_sd_boruta_stats)

# 9.7 Perform Boruta algorithm on unscaled 'train_irmi_na'
train_irmi_na_boruta <- Boruta(
  label~.,
  data = train_irmi_na,
  pValue = 0.05,   # Set the p-value threshold to 0.05
  maxRuns = 200,   # Set the maximum number of runs to 200
  doTrace = 2      # Allows you to get a report of the progress of the process
)
print(train_irmi_na_boruta)
#take a call on tentative features
train_irmi_na_boruta_tent <- TentativeRoughFix(train_irmi_na_boruta)
print(train_irmi_na_boruta_tent)

# you can confirm the importance of the features by typing
getSelectedAttributes(train_irmi_na_boruta_tent, withTentative = T)
# there are  22 confirmed important features and the rest 438 features are rejected.
train_irmi_na_boruta_stats <- attStats(train_irmi_na_boruta_tent)
plot(normHits~meanImp,col=train_irmi_na_boruta_stats$decision,data=train_irmi_na_boruta_stats)
print(train_irmi_na_boruta_stats)

# 9.8 Perform Boruta algorithm on unscaled 'train_irmi_sd'
train_irmi_sd_boruta <- Boruta(
  label~.,
  data = train_irmi_sd,
  pValue = 0.05,   # Set the p-value threshold to 0.05
  maxRuns = 200,   # Set the maximum number of runs to 200
  doTrace = 2      # Allows you to get a report of the progress of the process
)
print(train_irmi_sd_boruta)
#take a call on tentative features
train_irmi_sd_boruta_tent <- TentativeRoughFix(train_irmi_sd_boruta)
print(train_irmi_sd_boruta_tent)

# you can confirm the importance of the features by typing
getSelectedAttributes(train_irmi_sd_boruta_tent, withTentative = T)
# there are  22 confirmed important features and the rest 438 features are rejected.
train_irmi_sd_boruta_stats <- attStats(train_irmi_sd_boruta_tent)
plot(normHits~meanImp,col=train_irmi_sd_boruta_stats$decision,data=train_irmi_sd_boruta_stats)
print(train_irmi_sd_boruta_stats)

# 9.9 Perform scaling and Boruta algorithm on unscaled 'hot_na'
hot_na_boruta <- bind_cols(select(outlier_na, label = label), hot_na_norm)
hot_na_boruta <- Boruta(
  label~.,
  data = hot_na_boruta,
  pValue = 0.05,   # Set the p-value threshold to 0.05
  maxRuns = 200,   # Set the maximum number of runs to 200
  doTrace = 2      # Allows you to get a report of the progress of the process
)
print(hot_na_boruta)
#take a call on tentative features
hot_na_boruta_tent <- TentativeRoughFix(hot_na_boruta)
print(hot_na_boruta_tent)

# you can confirm the importance of the features by typing
getSelectedAttributes(hot_na_boruta_tent, withTentative = T)
# there are  13 confirmed important features and the rest 438 features are rejected.
hot_na_boruta_stats <- attStats(hot_na_boruta_tent)
plot(normHits~meanImp,col=hot_na_boruta_stats$decision,data=hot_na_boruta_stats)
print(hot_na_boruta_stats)

# 9.10 Perform scaling and Boruta algorithm on unscaled 'hot_na'
hot_sd_boruta <- bind_cols(select(outlier_sd, label = label), hot_sd_norm)
hot_sd_boruta <- Boruta(
  label~.,
  data = hot_sd_boruta,
  pValue = 0.05,   # Set the p-value threshold to 0.05
  maxRuns = 200,   # Set the maximum number of runs to 200
  doTrace = 2      # Allows you to get a report of the progress of the process
)
print(hot_sd_boruta)
#take a call on tentative features
hot_sd_boruta_tent <- TentativeRoughFix(hot_sd_boruta)
print(hot_sd_boruta_tent)

# you can confirm the importance of the features by typing
getSelectedAttributes(hot_sd_boruta_tent, withTentative = T)
# there are  13 confirmed important features and the rest 438 features are rejected.
hot_sd_boruta_stats <- attStats(hot_sd_boruta_tent)
plot(normHits~meanImp,col=hot_sd_boruta_stats$decision,data=hot_sd_boruta_stats)
print(hot_sd_boruta_stats)

# 9.10 Perform scaling and Boruta algorithm on unscaled 'hot_na'
hot_sd_boruta <- bind_cols(select(outlier_sd, label = label), hot_sd_norm)
hot_sd_boruta <- Boruta(
  label~.,
  data = hot_sd_boruta,
  pValue = 0.05,   # Set the p-value threshold to 0.05
  maxRuns = 200,   # Set the maximum number of runs to 200
  doTrace = 2      # Allows you to get a report of the progress of the process
)
print(hot_sd_boruta)
#take a call on tentative features
hot_sd_boruta_tent <- TentativeRoughFix(hot_sd_boruta)
print(hot_sd_boruta_tent)

# you can confirm the importance of the features by typing
getSelectedAttributes(hot_sd_boruta_tent, withTentative = T)
# there are  13 confirmed important features and the rest 438 features are rejected.
hot_sd_boruta_stats <- attStats(hot_sd_boruta_tent)
plot(normHits~meanImp,col=hot_sd_boruta_stats$decision,data=hot_sd_boruta_stats)
print(hot_sd_boruta_stats)

# 9.11 Perform scaling and Boruta algorithm on unscaled 'train_knn_outlier'
train_knn_outlier_norm <- scale(train_knn_outlier, scale = T, center = T)
train_knn_outlier_boruta <- bind_cols(select(hot_red, label = label), train_knn_outlier_norm)
train_knn_outlier_boruta <- Boruta(
  label~.,
  data = train_knn_outlier_boruta ,
  pValue = 0.05,   # Set the p-value threshold to 0.05
  maxRuns = 200,   # Set the maximum number of runs to 200
  doTrace = 2      # Allows you to get a report of the progress of the process
)
print(train_knn_outlier_boruta)
#take a call on tentative features
train_knn_outlier_boruta_tent <- TentativeRoughFix(train_knn_outlier_boruta)
print(train_knn_outlier_boruta_tent)

# you can confirm the importance of the features by typing
getSelectedAttributes(train_knn_outlier_boruta_tent, withTentative = T)
# there are  4 confirmed important features and the rest 456 features are rejected.
train_knn_outlier_boruta_stats <- attStats(train_knn_outlier_boruta_tent)
plot(normHits~meanImp,col=train_knn_outlier_boruta_stats$decision,data=train_knn_outlier_boruta_stats)
print(train_knn_outlier_boruta_stats)

# 9.12 Perform scaling and Boruta algorithm on unscaled 'train_hot_outlier'
train_hot_outlier_norm <- scale(train_hot_outlier[,!names(train_hot_outlier)%in%c("label","date")], scale = T, center = T)
train_hot_outlier_boruta <- bind_cols(select(hot_red, label = label), train_knn_outlier_norm)
train_hot_outlier_boruta <- Boruta(
  label~.,
  data = train_hot_outlier_boruta ,
  pValue = 0.05,   # Set the p-value threshold to 0.05
  maxRuns = 200,   # Set the maximum number of runs to 200
  doTrace = 2      # Allows you to get a report of the progress of the process
)
print(train_hot_outlier_boruta)
#take a call on tentative features
train_hot_outlier_boruta_tent <- TentativeRoughFix(train_hot_outlier_boruta)
print(train_hot_outlier_boruta_tent)

# you can confirm the importance of the features by typing
getSelectedAttributes(train_hot_outlier_boruta_tent, withTentative = T)
# there are 6 confirmed important features and the rest 454 features are rejected.
train_hot_outlier_boruta_stats <- attStats(train_hot_outlier_boruta_tent)
plot(normHits~meanImp,col=train_hot_outlier_boruta_stats$decision,data=train_hot_outlier_boruta_stats)
print(train_hot_outlier_boruta_stats)

                
# 2 Feature reduction with PCA ------------------------------------------------
# 2.1 Function to check if dataset is suitable for PCA
# 2.1.1 Check if suitable for PCA
PCA_suitable <- function(x) {
  KMO <- EFAtools::KMO(cor(as.matrix(x)))
  bart <- EFAtools::BARTLETT(cor(as.matrix(x)), N = nrow(x))
  return_list <- list("Kaiser-Meyer-Olkin (KMO) factor adequacy" = KMO$KMO,
                      "Bartlett's test of sphericity" = bart)
  return(return_list)
}
# 2.1 On hot deck imputed datasets
PCA_suitable(hot_na[,!names(hot_na) %in% c("label","date")])
# KMO: 0.679
# Bartlett p < .001

PCA_suitable(hot_sd[,!names(hot_sd) %in% c("label","date")])
# KMO: 0.690
# Bartlett p < .001

PCA_suitable(train_hot_outlier[,!names(train_hot_outlier) %in% c("label","date")])
# KMO: 0.689
# Bartlett p < .001

# Normalize data
# Z-transformation due to skewed data and correlation matrix as input
hot_na_norm <- scaled(hot_na[,!names(hot_na)%in%c("label","date")])
hot_sd_norm <- scaled(hot_sd[,!names(hot_sd)%in%c("label","date")])
hot_outlier_norm <- scaled(train_hot_outlier[,!names(train_hot_outlier)%in%c("label","date")])

# Scree plots
scree_na <- VSS.scree(hot_na_norm)
scree_sd <- VSS.scree(hot_sd_norm)
scree_outlier <- VSS.scree(hot_outlier_norm)

# Extraction
PCA_extract <- function(x, method = c("Kaiser", "Variance")) {
  pca <- PCA(x, graph = F)
  eigen <- as.tibble(pca$eig)
  if (method == "Variance") {
    nf <- eigen %>%
      rename(pov = 2, cpov = 3) %>%
      filter(cpov <= 90) %>%
      nrow()
  } else {
    nf <- eigen %>%
      filter(eigenvalue >= 1) %>%
      nrow()
  }
  pca_extract <- principal(x, nfactors = nf, covar = F, scores = T)
  as.tibble(pca_extract$scores)
}
# NA_set
# Kaiser criterion
hot_na_k <- PCA_extract(hot_na_norm, "Kaiser")
# Variance of at least 90%
hot_na_var <- PCA_extract(hot_na_norm, "Variance")

# SD set
# Kaiser criterion
hot_sd_k <- PCA_extract(hot_sd_norm, "Kaiser")
# Variance of at least 90%
hot_sd_var <- PCA_extract(hot_sd_norm, "Variance")

# Reduced training set
# Kaiser criterion
hot_outlier_k <- PCA_extract(hot_outlier_norm, "Kaiser")
# Variance of at least 90%
hot_outlier_var <- PCA_extract(hot_outlier_norm, "Variance")

# 2.2 On kNN imputed datasets knn_na, knn_sd and knn_train_red 
# 2.2.1 Kaiser-Meyer-Olkin (KMO) factor adequacy
PCA_suitable(train_knn_na[,!names(train_knn_na) %in% c("label","date")])
# KMO: 0.42 > Not suitable for PCA

PCA_suitable(train_knn_sd[,!names(train_knn_sd) %in% c("label","date")])
# KMO: 0.415 > Not suitable for PCA

PCA_suitable(train_knn_outlier[,!names(train_knn_outlier) %in% c("label","date")])
# KMO: 0.406 > Not suitable for PCA

# 2.3 On irmi imputed datasets 
# 2.3.1 Kaiser-Meyer-Olkin (KMO) factor adequacy
PCA_suitable(train_irmi_na[,!names(train_irmi_na) %in% c("label","date")])
# KMO: 0.42 > Not suitable for PCA

PCA_suitable(train_irmi_sd[,!names(train_irmi_sd) %in% c("label","date")])
# KMO: 0.01 > Not suitable for PCA

# X Balancing

# For this round, we will start with dataset 'hot_pca_na_k' as example dataset 
# In order to try every single balancing method, and visualise the results from every method
# Then we can choose the some of the most reasonable methods to perform with other datasets.

# bind the target variable back with the data to prepare for balancing
hot_pca_na_k_bal <- bind_cols(select(hot_na, label = label), hot_na_k)
view(hot_pca_na_k_bal)

# check frequency table of target variable
table(hot_pca_na_k_bal$label)

# check classes distribution
prop.table(table(hot_pca_na_k_bal$label))

# over sampling (majority*2 = 1170*2 = 2340)
data_balanced_over <- ovun.sample(label~., data = hot_pca_na_k_bal, method = "over",N = 2340)$data
table(data_balanced_over$label)

# under sampling (minority*2 = 83*2 = 166)
data_balanced_under <- ovun.sample(label~., data = hot_pca_na_k_bal, method = "under", N = 166, seed = 1)$data
table(data_balanced_under$label)

# both over and under sampling (total = 1170+83 = 1253)
data_balanced_both <- ovun.sample(label~., data = hot_pca_na_k_bal, method = "both", p=0.5,N=1253, seed = 1)$data
table(data_balanced_both$label)

# ROSE
# all arguments of ROSE have been set to the default values
data_rose <- ROSE(label~., data = hot_pca_na_k_bal, seed = 1)$data
table(data_rose$label)

#parameters have been shrunk, by setting hmult.majo = 0.25 and hmult.mino = 0.5.
data_rose_shrunk <- ROSE(label~., data = hot_pca_na_k_bal, seed = 1,hmult.majo = 0.25 , hmult.mino = 0.5)$data
table(data_rose_shrunk$label)

#SMOTE
data_SMOTE <- SMOTE(X = hot_pca_na_k_bal[,-1], hot_pca_na_k_bal$label)$data
table(data_SMOTE$class)

#ADASYN
data_ADASYN<-ADAS(X=hot_pca_na_k_bal[,-1], target=hot_pca_na_k_bal$label)$data
table(data_ADASYN$class)


# -------- Visualising results after balancing --------------

par(mfrow=c(2,2))
#plot train data
plot(hot_pca_na_k_bal$RC1,hot_pca_na_k_bal$RC2,main="Training Data",
     xlim=c(-5,5), ylim=c(-5,5), col = adjustcolor(ifelse(hot_pca_na_k_bal$label == -1, "blue", "red"), alpha.f = 0.5), pch=16,xlab="", ylab="")
legend("topleft", c("Majority class", "Minority class"), pch = 16, col = c("blue", "red"))

#plot oversampled data
plot(data_balanced_over$RC1,data_balanced_over$RC2,main="Oversampling",
     xlim=c(-5,5), ylim=c(-5,5), col = adjustcolor(ifelse(hot_pca_na_k_bal$label == -1, "blue", "red"), alpha.f = 0.5), pch=16,xlab="", ylab="")
legend("topleft", c("Majority class", "Minority class"), pch = 16, col = c("blue", "red"))

#plot undersampled data
plot(data_balanced_under$RC1,data_balanced_under$RC2,main="Undersampling",
     xlim=c(-5,5), ylim=c(-5,5), col = adjustcolor(ifelse(hot_pca_na_k_bal$label == -1, "blue", "red"), alpha.f = 0.5), pch=16,xlab="", ylab="")
legend("topleft", c("Majority class", "Minority class"), pch = 16, col = c("blue", "red"))

#plot over and undersampled data
plot(data_balanced_both$RC1,data_balanced_both$RC2,main="Over- and Undersampling",
     xlim=c(-5,5), ylim=c(-5,5), col = adjustcolor(ifelse(hot_pca_na_k_bal$label == -1, "blue", "red"), alpha.f = 0.5), pch=16,xlab="", ylab="")
legend("topleft", c("Majority class", "Minority class"), pch = 16, col = c("blue", "red"))

par(mfrow=c(2,2))
#plot train data
plot(hot_pca_na_k_bal$RC1,hot_pca_na_k_bal$RC2,main="Training Data",
     xlim=c(-5,5), ylim=c(-5,5), col = adjustcolor(ifelse(hot_pca_na_k_bal$label == -1, "blue", "red"), alpha.f = 0.5), pch=16,xlab="", ylab="")
legend("topleft", c("Majority class", "Minority class"), pch = 16, col = c("blue", "red"))

#plot ROSE resampled data
plot(data_rose$RC1,data_rose$RC2,main="ROSE resampling",
     xlim=c(-5,5), ylim=c(-5,5), col = adjustcolor(ifelse(hot_pca_na_k_bal$label == -1, "blue", "red"), alpha.f = 0.5), pch=16,xlab="", ylab="")
legend("topleft", c("Majority class", "Minority class"), pch = 16, col = c("blue", "red"))

#plot ROSE shrunk resampled data
plot(data_rose_shrunk$RC1,data_rose_shrunk$RC2,main="ROSE shrunk resampling",
     xlim=c(-5,5), ylim=c(-5,5), col = adjustcolor(ifelse(hot_pca_na_k_bal$label == -1, "blue", "red"), alpha.f = 0.5), pch=16,xlab="", ylab="")
legend("topleft", c("Majority class", "Minority class"), pch = 16, col = c("blue", "red"))

#plot SMOTE resampled data
plot(data_SMOTE$RC1,data_SMOTE$RC2,main="SMOTE resampling",
     xlim=c(-5,5), ylim=c(-5,5), col = adjustcolor(ifelse(hot_pca_na_k_bal$label == -1, "blue", "red"), alpha.f = 0.5), pch=16,xlab="", ylab="")
legend("topleft", c("Majority class", "Minority class"), pch = 16, col = c("blue", "red"))

par(mfrow=c(2,2))
#plot train data
plot(hot_pca_na_k_bal$RC1,hot_pca_na_k_bal$RC2,main="Training Data",
     xlim=c(-5,5), ylim=c(-5,5),col = adjustcolor(ifelse(hot_pca_na_k_bal$label == -1, "blue", "red"), alpha.f = 0.5), pch=16,xlab="", ylab="")
legend("topleft", c("Majority class", "Minority class"), pch = 16, col = c("blue", "red"))

#plot ROSE shrunk resampled data
plot(data_rose_shrunk$RC1,data_rose_shrunk$RC2,main="ROSE shrunk resampling",
     xlim=c(-5,5), ylim=c(-5,5), col = adjustcolor(ifelse(hot_pca_na_k_bal$label == -1, "blue", "red"), alpha.f = 0.5), pch=16,xlab="", ylab="")
legend("topleft", c("Majority class", "Minority class"), pch = 16, col = c("blue", "red"))

#plot SMOTE resampled data
plot(data_SMOTE$RC1,data_SMOTE$RC2,main="SMOTE resampling",
     xlim=c(-5,5), ylim=c(-5,5), col = adjustcolor(ifelse(hot_pca_na_k_bal$label == -1, "blue", "red"), alpha.f = 0.5), pch=16,xlab="", ylab="")
legend("topleft", c("Majority class", "Minority class"), pch = 16, col = c("blue", "red"))

#plot ADASYN resampled data
plot(data_ADASYN$RC1,data_ADASYN$RC2,main="ADASYN resampling",
     xlim=c(-5,5), ylim=c(-5,5), col = adjustcolor(ifelse(hot_pca_na_k_bal$label == -1, "blue", "red"), alpha.f = 0.5), pch=16,xlab="", ylab="")
legend("topleft", c("Majority class", "Minority class"), pch = 16, col = c("blue", "red"))

# From visualising the results after balancing method in the plots above 
# It can seen that there are only 5 reasonable balancing methods that give us enough data to model
# The other methods not chosen give too little data to work with.
# which are Oversampling, Over and under Sampling, ROSE shrunk, SMOTE and ADASYN

# Below, the 5 chosen balancing methods will be applied to all datasets.

# 10 Datasets from PCA (feature reduction) -------------------------------------

# 10.1 hot_na_k
# Prepare for balancing by binding the target variable back with the data
hot_pca_na_k_bal <- bind_cols(select(hot_na, label = label), hot_na_k)

# over sampling 
hot_pca_na_k_bal_over <- ovun.sample(label~., data = hot_pca_na_k_bal, method = "over",N = 2340)$data
table(hot_pca_na_k_bal_over$label)

# over and under sampling
hot_pca_na_k_bal_both <- ovun.sample(label~., data = hot_pca_na_k_bal, method = "both", p=0.5,N=1253, seed = 1)$data
table(hot_pca_na_k_bal_both$label)

#ROSE shrunk
hot_pca_na_k_rose <- ROSE(label~., data = hot_pca_na_k_bal, seed = 1,hmult.majo = 0.25 , hmult.mino = 0.5)$data
table(hot_pca_na_k_rose$label)

#SMOTE
hot_pca_na_k_SMOTE <- SMOTE(X = hot_pca_na_k_bal[,-1], hot_pca_na_k_bal$label)$data
table(hot_pca_na_k_SMOTE$class)

#ADASYN
hot_pca_na_k_ADASYN<-ADAS(X=hot_pca_na_k_bal[,-1], target=hot_pca_na_k_bal$label)$data
table(hot_pca_na_k_ADASYN$class)

#10.2 hot_sd_k
# Prepare for balancing by binding the target variable back with the data
hot_pca_sd_k_bal <- bind_cols(select(hot_sd, label = label), hot_sd_k)

# over sampling 
hot_pca_sd_k_bal_over <- ovun.sample(label~., data = hot_pca_sd_k_bal, method = "over",N = 2340)$data
table(hot_pca_sd_k_bal_over$label)

# over and under sampling
hot_pca_sd_k_bal_both <- ovun.sample(label~., data = hot_pca_sd_k_bal, method = "both", p=0.5,N=1253, seed = 1)$data
table(hot_pca_sd_k_bal_both$label)

#ROSE shrunk
hot_pca_sd_k_rose <- ROSE(label~., data = hot_pca_sd_k_bal, seed = 1,hmult.majo = 0.25 , hmult.mino = 0.5)$data
table(hot_pca_sd_k_rose$label)

#SMOTE
hot_pca_sd_k_SMOTE <- SMOTE(X = hot_pca_sd_k_bal[,-1], hot_pca_sd_k_bal$label)$data
table(hot_pca_sd_k_SMOTE$class)

#ADASYN
hot_pca_sd_k_ADASYN<-ADAS(X=hot_pca_sd_k_bal[,-1], target=hot_pca_sd_k_bal$label)$data
table(hot_pca_sd_k_ADASYN$class)

#10.3 hot_outlier_k
# Prepare for balancing by binding the target variable back with the data
hot_pca_outlier_k_bal <- bind_cols(select(train_hot_outlier, label = label), hot_outlier_k)

# over sampling 
hot_pca_outlier_k_over <- ovun.sample(label~., data = hot_pca_outlier_k_bal, method = "over",N = 2340)$data
table(hot_pca_outlier_k_over$label)

# over and under sampling
hot_pca_outlier_k_both <- ovun.sample(label~., data = hot_pca_outlier_k_bal, method = "both", p=0.5,N=1253, seed = 1)$data
table(hot_pca_outlier_k_both$label)

#ROSE shrunk
hot_pca_outlier_k_rose <- ROSE(label~., data = hot_pca_outlier_k_bal, seed = 1,hmult.majo = 0.25 , hmult.mino = 0.5)$data
table(hot_pca_outlier_k_rose$label)

#SMOTE
hot_pca_outlier_k_SMOTE <- SMOTE(X = hot_pca_outlier_k_bal[,-1], hot_pca_outlier_k_bal$label)$data
table(hot_pca_outlier_k_SMOTE$class)

#ADASYN
hot_pca_outlier_k_ADASYN<-ADAS(X=hot_pca_outlier_k_bal[,-1], target=hot_pca_outlier_k_bal$label)$data
table(hot_pca_outlier_k_ADASYN$class)

#10.4 hot_na_var
# Prepare for balancing by binding the target variable back with the data
hot_pca_na_var_bal <- bind_cols(select(hot_na, label = label), hot_na_var)

# over sampling 
hot_pca_na_var_over <- ovun.sample(label~., data = hot_pca_na_var_bal, method = "over",N = 2340)$data
table(hot_pca_na_var_over$label)

# over and under sampling
hot_pca_na_var_both <- ovun.sample(label~., data = hot_pca_na_var_bal, method = "both", p=0.5,N=1253, seed = 1)$data
table(hot_pca_na_var_both$label)

#ROSE shrunk
hot_pca_na_var_rose <- ROSE(label~., data = hot_pca_na_var_bal, seed = 1,hmult.majo = 0.25 , hmult.mino = 0.5)$data
table(hot_pca_na_var_rose$label)

#SMOTE
hot_pca_na_var_SMOTE <- SMOTE(X = hot_pca_na_var_bal[,-1], hot_pca_na_var_bal$label)$data
table(hot_pca_na_var_SMOTE$class)

#ADASYN
hot_pca_na_var_ADASYN<-ADAS(X=hot_pca_na_var_bal[,-1], target=hot_pca_na_var_bal$label)$data
table(hot_pca_na_var_ADASYN$class)

#10.5 hot_na_var
# Prepare for balancing by binding the target variable back with the data
hot_pca_sd_var_bal <- bind_cols(select(hot_sd, label = label), hot_sd_var)

# over sampling 
hot_pca_sd_var_over <- ovun.sample(label~., data = hot_pca_sd_var_bal, method = "over",N = 2340)$data
table(hot_pca_sd_var_over$label)

# over and under sampling
hot_pca_sd_var_both <- ovun.sample(label~., data = hot_pca_sd_var_bal, method = "both", p=0.5,N=1253, seed = 1)$data
table(hot_pca_sd_var_both$label)

#ROSE shrunk
hot_pca_sd_var_rose <- ROSE(label~., data = hot_pca_sd_var_bal, seed = 1,hmult.majo = 0.25 , hmult.mino = 0.5)$data
table(hot_pca_sd_var_rose$label)

#SMOTE
hot_pca_sd_var_SMOTE <- SMOTE(X = hot_pca_sd_var_bal[,-1], hot_pca_sd_var_bal$label)$data
table(hot_pca_sd_var_SMOTE$class)

#ADASYN
hot_pca_sd_var_ADASYN<-ADAS(X=hot_pca_sd_var_bal[,-1], target=hot_pca_sd_var_bal$label)$data
table(hot_pca_sd_var_ADASYN$class)

#10.6 hot_outlier_var
# Prepare for balancing by binding the target variable back with the data
hot_pca_outlier_var_bal <- bind_cols(select(train_hot_outlier, label = label), hot_outlier_var)

# over sampling 
hot_pca_outlier_var_over <- ovun.sample(label~., data = hot_pca_outlier_var_bal, method = "over",N = 2340)$data
table(hot_pca_outlier_var_over$label)

# over and under sampling
hot_pca_outlier_var_both <- ovun.sample(label~., data = hot_pca_outlier_var_bal, method = "both", p=0.5,N=1253, seed = 1)$data
table(hot_pca_outlier_var_both$label)

#ROSE shrunk
hot_pca_outlier_var_rose <- ROSE(label~., data = hot_pca_outlier_var_bal, seed = 1,hmult.majo = 0.25 , hmult.mino = 0.5)$data
table(hot_pca_outlier_var_rose$label)

#SMOTE
hot_pca_outlier_var_SMOTE <- SMOTE(X = hot_pca_outlier_var_bal[,-1], hot_pca_outlier_var_bal$label)$data
table(hot_pca_outlier_var_SMOTE$class)

#ADASYN
hot_pca_outlier_var_ADASYN<-ADAS(X=hot_pca_outlier_var_bal[,-1], target=hot_pca_outlier_var_bal$label)$data
table(hot_pca_outlier_var_ADASYN$class)

# 10 Datasets from BORUTA (feature selection) ----------------------------------

#10.7 knn_na_boruta

# select only features that got confirmed by boruta from the training set
knn_na_boruta_data <- knn_na[, selected_knn_na_boruta]

# Prepare for balancing by binding the target variable back with the data
knn_na_boruta_bal <- bind_cols(select(knn_na, label = label), knn_na_boruta_data)

# over sampling 
knn_na_boruta_over <- ovun.sample(label~., data = knn_na_boruta_bal, method = "over",N = 2340)$data
table(knn_na_boruta_over$label)

# over and under sampling
knn_na_boruta_both <- ovun.sample(label~., data = knn_na_boruta_bal, method = "both", p=0.5,N=1253, seed = 1)$data
table(knn_na_boruta_both$label)

#ROSE shrunk
knn_na_boruta_rose <- ROSE(label~., data = knn_na_boruta_bal, seed = 1,hmult.majo = 0.25 , hmult.mino = 0.5)$data
table(knn_na_boruta_rose$label)

#SMOTE
knn_na_boruta_SMOTE <- SMOTE(X = knn_na_boruta_bal[,-1], knn_na_boruta_bal$label)$data
table(knn_na_boruta_SMOTE$class)

#ADASYN
knn_na_boruta_ADASYN<-ADAS(X=knn_na_boruta_bal[,-1], target=knn_na_boruta_bal$label)$data
table(knn_na_boruta_ADASYN$class)

#10.8 knn_sd_boruta

# select only features that got confirmed by boruta from the training set
knn_sd_boruta_data <- knn_sd[, selected_knn_sd_boruta]

# Prepare for balancing by binding the target variable back with the data
knn_sd_boruta_bal <- bind_cols(select(knn_sd, label = label), knn_sd_boruta_data)

# over sampling 
knn_sd_boruta_over <- ovun.sample(label~., data = knn_sd_boruta_bal, method = "over",N = 2340)$data
table(knn_sd_boruta_over$label)

# over and under sampling
knn_sd_boruta_both <- ovun.sample(label~., data = knn_sd_boruta_bal, method = "both", p=0.5,N=1253, seed = 1)$data
table(knn_sd_boruta_both$label)

#ROSE shrunk
knn_sd_boruta_rose <- ROSE(label~., data = knn_sd_boruta_bal, seed = 1,hmult.majo = 0.25 , hmult.mino = 0.5)$data
table(knn_sd_boruta_rose$label)

#SMOTE
knn_sd_boruta_SMOTE <- SMOTE(X = knn_sd_boruta_bal[,-1], knn_sd_boruta_bal$label)$data
table(knn_sd_boruta_SMOTE$class)

#ADASYN
knn_sd_boruta_ADASYN<-ADAS(X=knn_sd_boruta_bal[,-1], target=knn_sd_boruta_bal$label)$data
table(knn_sd_boruta_ADASYN$class)

#10.9 irmi_na_boruta

# select only features that got confirmed by boruta from the training set
irmi_na_boruta_data <- irmi_na[, selected_irmi_na_boruta]

# Prepare for balancing by binding the target variable back with the data
irmi_na_boruta_bal <- bind_cols(select(irmi_na, label = label), irmi_na_boruta_data)

# over sampling 
irmi_na_boruta_over <- ovun.sample(label~., data = irmi_na_boruta_bal, method = "over",N = 2340)$data
table(irmi_na_boruta_over$label)

# over and under sampling
irmi_na_boruta_both <- ovun.sample(label~., data = irmi_na_boruta_bal, method = "both", p=0.5,N=1253, seed = 1)$data
table(irmi_na_boruta_both$label)

#ROSE shrunk
irmi_na_boruta_rose <- ROSE(label~., data = irmi_na_boruta_bal, seed = 1,hmult.majo = 0.25 , hmult.mino = 0.5)$data
table(irmi_na_boruta_rose$label)

#SMOTE
irmi_na_boruta_SMOTE <- SMOTE(X = irmi_na_boruta_bal[,-1], irmi_na_boruta_bal$label)$data
table(irmi_na_boruta_SMOTE$class)

#ADASYN
irmi_na_boruta_ADASYN<-ADAS(X=irmi_na_boruta_bal[,-1], target=irmi_na_boruta_bal$label)$data
table(irmi_na_boruta_ADASYN$class)

#10.10 irmi_sd_boruta

# select only features that got confirmed by boruta from the training set
irmi_sd_boruta_data <- irmi_sd[, selected_irmi_sd_boruta]

# Prepare for balancing by binding the target variable back with the data
irmi_sd_boruta_bal <- bind_cols(select(irmi_sd, label = label), irmi_sd_boruta_data)

# over sampling 
irmi_sd_boruta_over <- ovun.sample(label~., data = irmi_sd_boruta_bal, method = "over",N = 2340)$data
table(irmi_sd_boruta_over$label)

# over and under sampling
irmi_sd_boruta_both <- ovun.sample(label~., data = irmi_sd_boruta_bal, method = "both", p=0.5,N=1253, seed = 1)$data
table(irmi_sd_boruta_both$label)

#ROSE shrunk
irmi_sd_boruta_rose <- ROSE(label~., data = irmi_sd_boruta_bal, seed = 1,hmult.majo = 0.25 , hmult.mino = 0.5)$data
table(irmi_sd_boruta_rose$label)

#SMOTE
irmi_sd_boruta_SMOTE <- SMOTE(X = irmi_sd_boruta_bal[,-1], irmi_sd_boruta_bal$label)$data
table(irmi_sd_boruta_SMOTE$class)

#ADASYN
irmi_sd_boruta_ADASYN<-ADAS(X=irmi_sd_boruta_bal[,-1], target=irmi_sd_boruta_bal$label)$data
table(irmi_sd_boruta_ADASYN$class)

#10.11 hot_na_boruta

# select only features that got confirmed by boruta from the training set
hot_na_boruta_data <- hot_na[, selected_hot_na_boruta]

# Prepare for balancing by binding the target variable back with the data
hot_na_boruta_bal <- bind_cols(select(hot_na, label = label), hot_na_boruta_data)

# over sampling 
hot_na_boruta_over <- ovun.sample(label~., data = hot_na_boruta_bal, method = "over",N = 2340)$data
table(hot_na_boruta_over$label)

# over and under sampling
hot_na_boruta_both <- ovun.sample(label~., data = hot_na_boruta_bal, method = "both", p=0.5,N=1253, seed = 1)$data
table(hot_na_boruta_both$label)

#ROSE shrunk
hot_na_boruta_rose <- ROSE(label~., data = hot_na_boruta_bal, seed = 1,hmult.majo = 0.25 , hmult.mino = 0.5)$data
table(hot_na_boruta_rose$label)

#SMOTE
hot_na_boruta_SMOTE <- SMOTE(X = hot_na_boruta_bal[,-1], hot_na_boruta_bal$label)$data
table(hot_na_boruta_SMOTE$class)

#ADASYN
hot_na_boruta_ADASYN<-ADAS(X=hot_na_boruta_bal[,-1], target=hot_na_boruta_bal$label)$data
table(hot_na_boruta_ADASYN$class)

#10.12 hot_sd_boruta

# select only features that got confirmed by boruta from the training set
hot_sd_boruta_data <- hot_sd[, selected_hot_sd_boruta]

# Prepare for balancing by binding the target variable back with the data
hot_sd_boruta_bal <- bind_cols(select(hot_sd, label = label), hot_sd_boruta_data)

# over sampling 
hot_sd_boruta_over <- ovun.sample(label~., data = hot_sd_boruta_bal, method = "over",N = 2340)$data
table(hot_sd_boruta_over$label)

# over and under sampling
hot_sd_boruta_both <- ovun.sample(label~., data = hot_sd_boruta_bal, method = "both", p=0.5,N=1253, seed = 1)$data
table(hot_sd_boruta_both$label)

#ROSE shrunk
hot_sd_boruta_rose <- ROSE(label~., data = hot_sd_boruta_bal, seed = 1,hmult.majo = 0.25 , hmult.mino = 0.5)$data
table(hot_sd_boruta_rose$label)

#SMOTE
hot_sd_boruta_SMOTE <- SMOTE(X = hot_sd_boruta_bal[,-1], hot_sd_boruta_bal$label)$data
table(hot_sd_boruta_SMOTE$class)

#ADASYN
hot_sd_boruta_ADASYN<-ADAS(X=hot_sd_boruta_bal[,-1], target=hot_sd_boruta_bal$label)$data
table(hot_sd_boruta_ADASYN$class)
                
                
# XI Modeling
# 1 Prepare the test set
# 1.1 Dimensionality reduction
test_red <- dim_reduce(test_set)
# Overall dimensionality reduction of 124 features
saveRDS(test_red, "test_red.RDS")
readRDS(test_red, "test_red.RDS")

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

test_hot_sd <- hotdeck(test_outlier_sd, impNA = T, imp_var = F)
sum(is.na(test_hot_sd))

# Before outlier handling
test_hot_outlier <- hotdeck(test_red, impNA = T, imp_var = F)
sum(is.na(test_hot_outlier))

test_hot_outlier <- test_hot_outlier %>%
  mutate(across(where(is.numeric),~ outlier_replace(., method = "SD")))
as.tibble(test_hot_outlier)

# 1.3.2 kNN

# 1.3.3 irmi

# 1.4 PCA
PCA_extract_test <- function(x, nf) {
  pca <- PCA(x, graph = F)
  eigen <- as.tibble(pca$eig)
  nf <- nf
  pca_extract <- principal(x, nfactors = nf, covar = F, scores = T)
  as.tibble(pca_extract$scores)
}
# NA set
test_hot_na_norm <- scaled(test_hot_na[,!names(test_hot_na) %in% c("label","date")])
sum(is.nan(as.matrix(test_hot_na_norm)))
# 2198 NaN values: feature75, feature207, feature210, feature343, feature348, feature479 and feature522
# Produced due to data split and less unique values in these features
test_hot_na_norm <- test_hot_na_norm[, colSums(is.na(test_hot_na_norm)) == 0]

# Kaiser criterion
test_hot_na_k <- PCA_extract_test(test_hot_na_norm, nf = length(hot_na_k))
test_hot_pca_na_k <- bind_cols(select(test_hot_na, label = label), test_hot_na_k)

# 2 Random forest
# 2.1 On PCA datasets
# 2.1.1 hot_na_k sets = hot deck imputed datasets with PCA by Kaiser criterion and balancing
# ROSE shrunk balancing
# Train model on hot_pca_n_k_rose_rf set
set.seed(12345)
hot_pca_na_k_rose_rf <- randomForest(data = hot_pca_na_k_rose,
                                     x = hot_pca_na_k_rose[,!names(hot_pca_na_k_rose) %in% ("label")],
                                     y = as.factor(hot_pca_na_k_rose$label),
                                     ntree = 400,
                                     importance = T,
                                     localImp = T,
                                     replace = T,
                                     nodesize = 5,
                                     proximity = F)
# The higher the number of trees, the lower the error rates
# Nodesize refers to size of the trees, the larger the number the smaller the trees
# and the faster the computation time

# Function to extract results
pred_rf1 <- predict(hot_pca_na_k_rose_rf, newdata = test_hot_pca_na_k, type = "class")

model_result <- function(predictions, target) {
  conf_mat <- confusionMatrix(table(predictions, target))
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
result <- model_result(pred_rf1, test_hot_pca_na_k$label)

ROC <- function(model, test_data) {
  pred_probs <- as.numeric(predict(model, newdata = test_data, type = "response"))
  pred <- prediction(as.numeric(pred_test1), test_hot_pca_na_k$label)          
  perf <- performance(pred, measure = "tpr", x.measure = "fpr") 
  roc_curve <- plot(perf, main = "ROC Curve", lwd = 3, col = "darkred")
  return(roc_curve)
}
roc <- ROC(hot_pca_na_k_rose_rf,test_hot_pca_na_k)

