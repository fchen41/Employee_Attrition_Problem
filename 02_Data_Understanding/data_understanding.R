# Data Understantding ----
library(tidyquant)
library(tidyverse)
library(readxl)
library(skimr)
library(GGally)

# Load data
path_train <- "./00_Data/telco_train.xlsx"
path_data_definition <- "./00_Data/telco_data_definitions.xlsx"

train_raw_tbl <- read_excel(path_train, sheet = 1)
definitions_raw_tbl <- read_excel(path_data_definition, sheet = 1, col_names = FALSE)

# Exploratory Data Analysis (EDA) ----

# Step 1: Data Summarization ----
skim(train_raw_tbl)

# Character Data Type
train_raw_tbl %>% 
  select_if(is.character) %>% 
  glimpse()

train_raw_tbl %>% 
  select_if(is.character) %>% 
  map(unique)

train_raw_tbl %>% 
  select_if(is.character) %>% 
  map(~ table(.) %>% prop.table())

# Numeric Data
train_raw_tbl %>% 
  select_if(is.numeric) %>% 
  map(~unique(.) %>% length())

train_raw_tbl %>% 
  select_if(is.numeric) %>% 
  map_df(~unique(.) %>% length()) %>% 
  gather() %>% 
  arrange(value) %>% #lower values are more likely the discrete numbers
  filter(value <= 10)
