# Bank of England Test Answers 
# Created on 13/09/2021 
# Created by: Abbas Fazil 

# 1. Create Data ---------------------------

# Load Packages -----------------------------------------------------------

# load all required libraries 

library(readxl) # for reading in excel files
library(dplyr) # for data cleaning 
library(tidyr) # for tidying data into cleaner format
library(purrr)


# Load Data ---------------------------------------------------------------

# load both data files into R - only load the sheets that we need. 

# Natwest Data 
# sheet 1.1 Incmoe Statement 
# list out names of sheets first: 
readxl::excel_sheets(path = "Data/NatWest Group 2020 EOY .xlsx")
readxl::excel_sheets(path = "Data/Lloyds Banking Group 2020 EOY.xlsx")

# load in the natwest data sheet
natwest_df <- readxl::read_xlsx(path = "Data/NatWest Group 2020 EOY .xlsx", sheet = "1.1 - Income Statement" )

# load in the lloyds banking group data sheet: 

lbg_df <- readxl::read_xlsx(path = "Data/Lloyds Banking Group 2020 EOY.xlsx", sheet = "35")


# Extract Rows into one dataframe -----------------------------------------

# we need to extract out the following rows: 

# Net interest income (this is the amount of income the firm receives from interest payments)
# Total non - interest income / other income (this is income from other sources e.g. commission)
# Total income
# Operating expenses (this is the cost / expense of running the business)
# Impairments (this is the amount of money the firm has lost due to loan defaults)

# Extract Rows for Natwest dataframe first ---------------

# Begin by:
# filtering down to the rows we need
# then subsetting the columns that are corresponding to 2020 data only

natwest_df <- natwest_df %>%
  dplyr::filter(
    Highlights %in% c(
      "Net interest income",
      "Total non-interest income",
      "Total income",
      "Operating expenses",
      "Impairment losses"
    )
  ) %>% select("Highlights", "...14", "...15", "...16", "...17", "...18") %>%
  mutate_at(c("...14",
              "...15",
              "...16",
              "...17"), as.numeric) %>% 
rename("full_year_2020" = "...18") %>% 
  mutate("full_year_2020" = rowSums(across(where(is.numeric))), 
         bank_name = "Natwest") %>% 
  select(Highlights, full_year_2020, bank_name) %>% 
  
  # then caluclate the row column sums to make full_year_2020

  # spread the data into a different format with Highlight as column names
  tidyr::pivot_wider(names_from = Highlights, values_from = full_year_2020)


# Extract Rows for LLyods banking group next ------------------------------

lbg_df <- lbg_df %>%
  dplyr::filter(
    `CONDENSED CONSOLIDATED FINANCIAL STATEMENTS` %in% c(
      "Net interest income",
      "Other income",
      "Total income",
      "Total operating expenses",
      "Impairment"
    )
  ) %>%
  rename("full_year_2020" = "...5",
         "Highlights" = `CONDENSED CONSOLIDATED FINANCIAL STATEMENTS`) %>%
  mutate(full_year_2020 = as.numeric(full_year_2020)) %>%
  select(Highlights, full_year_2020) %>%
  mutate(bank_name = "Llyods Banking Group") %>%
  tidyr::pivot_wider(names_from = Highlights, values_from = full_year_2020) %>%
  rename(
    "Total non-interest income" = "Other income",
    "Operating expenses" = "Total operating expenses",
    "Impairment losses" = "Impairment"
  )


# Combine both dataframes into one dataframe ------------------------------

combined_df <- bind_rows(natwest_df, lbg_df)



# Create new dataframe contained all instances of the word income  --------
#    Create a new data frame containing all instances of the word ‘income’ and their respective values for 2020

new_df <- combined_df %>%  select(-`Operating expenses`, -`Impairment losses`)


# 2. Calculations  -----------------------------------------------------------------

# This is the second section of the test. 
# covers: Calculations (we want to check the data and create a new metric)


# Create Calculations ---------------------------------------------

# create a data validation rule = net interest income + non-interest income = total income 
# and create a flag/exception if rule fails - I have chosen to use 1 as TRUE and 0 as FALSE flag
# so if it equals total income then it is 1, if it is not then it is 0 

# Calculate a new metric, profit before tax = total income - expenses - impairments

# Modelling assumptions (We want to apply modelling assumptions to the data)
# 1.       For Natwest, adjust the net interest income line up by 50%
# 2.       For LBG, adjust operating expenses line up by 75%


# Recalculate profit before tax for both firms


# we will be using the combined_df dataframe for this section to create one final dataframe with all calculation metrics: 

final_df <- combined_df %>%
  mutate(
    data_validation_rule = `Net interest income` + `Total non-interest income`,
    data_validation_rule_flag = ifelse(data_validation_rule == `Total income`, 1, 0),
    profit_before_tax = `Total income` + `Operating expenses` + `Impairment losses`,
    `Net interest income` = ifelse(
      bank_name == "Natwest",
      `Net interest income` * 1.50,
      `Net interest income`
    ),
    `Operating expenses` = ifelse(
      bank_name == "Llyods Banking Group",
      `Operating expenses` * 1.75,
      `Operating expenses`
    ),
    recalculated_profit_before_tax = `Total income` + `Operating expenses` + `Impairment losses`
  ) 



# 3. Conclusion -----------------------------------------------------------

# Determine which firm is the most profit before and after the adjustments and explain why using the data extracted for this test

# Answer: 
# The firm with the most profit before and after adjustments is Llyods Banking Group, when calculating profit before tax without any modelling adjustments we have £(m) 15,267 as compared to Natwest who have a profit of negative -£(m) 351, when recalculating for profit before tax with the modelling assumptions multiplier of 1.75 applied to the Operating Expenses we are still at a larger profit with Llyods banking group, since the modelling multiplier of 1.50 is applied for the Net interest income for Natwest and this is not used in the calculation for profit before tax in the recalculated column. 
# 
# So Llyods Banking Group is the company most in profit both before and after adjustments. 

# End of Script. 



