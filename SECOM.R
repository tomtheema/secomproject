# I Install/Load required packages
if (!require("pacman")) install.packages("pacman")
pacman::p_load(tidyverse, rlang)

# II Check/Set working directory
getwd()

# Set working directory to your desktop
# setwd("C:/Users/You/Desktop")
setwd("C:/Users/SimonaPaskaleva/Desktop")
# Make sure to download the data files to your desktop

# III Load data into R
# 1 SECOM dataset
# Separator/delimitor is a space, decimal separator is a point
# Missing values are displayed as NaN
secom.data <- read.table("secom.data", 
                    header = FALSE, 
                    sep = " ", 
                    dec = ".", 
                    na.strings = "NaN")
# Check that data is imported correctly
tibble(secom.data)

# 2 Labels
# Separator/delimitor is a space, no decimal separator
secom.labels <- read.table("secom_labels.data", 
                           header = FALSE, 
                           sep = " ", 
                           quote ="'\"")
# Check that data is imported correctly
tibble(secom.labels)

# Rename column names of secom.labels dataset in order to merge the two datasets
secom.labels.renamed <- secom.labels %>%
  rename(label = 1, date = 2)

# Rename column names of secom.data dataset to display feature
secom.data.renamed <- secom.data %>% 
  set_names(~ str_replace_all(
  colnames(secom.data), "V", "feature")) 

# 3 Merge secom dataset with the labels data set
df1 <- data.frame(secom.data.renamed)
df2 <- data.frame(secom.labels.renamed)
secom <- bind_cols(df2, df1)
tibble(secom)

# IV Descriptive analysis
# 1 Calculate variance of each feature