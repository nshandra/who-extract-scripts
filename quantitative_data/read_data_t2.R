library(readxl)
library(tidyverse)
library(stringr)
library(dplyr)
library(purrr)

# =============================================================================================
# **************************************** Table 1 from T2 ************************************
# =============================================================================================

extract_table_one_t2 <- function(excel_file_path, save_csv_path){
  # Mean annual per capita OOP by income quintile
  t2_n <- readxl::read_excel(excel_file_path, sheet = "T2", skip = 2, n_max = 7)
  
  # Title years
  years_extracted <- readxl::read_excel(excel_file_path, sheet = "T2", skip = 0, n_max = 1) %>% 
    gather() %>% 
    select(value) %>% 
    drop_na() %>% 
    pull() %>% 
    as.character()
  
  if(length(t2_n) == 1){
    t2_n[years_extracted] <- NA
  }
  
  # Fix column names
  names(t2_n) <- c('quintile', years_extracted)
  # Transform from wide to long
  # t2a %>%
  #   tidyr::gather(year, value, `2005`:`2018`)
  # Add the indicator
  table_one_title <- str_replace_all('Mean annual per capita OOP by income quintile', pattern = ' ', replacement = '_')
  # write.csv(t2_n, file = str_glue('{save_csv_path}T2_ONE_{table_one_title}.csv'))
  # message("Table one from t2 extracted")
  table_one_title <- as.character(str_glue('T2_ONE_{table_one_title}.csv'))
  
  for(j in 2:ncol(t2_n)){
    t2_n[,j] <- format(round(as.numeric(round(unlist(t2_n[,j]), digits = 3)), digits = 3), nsmall = 3)
  }
  
  reslt <- list(
    df = t2_n,
    title = table_one_title
  )
  
  write.csv(x = reslt$df, file = save_csv_path)
  
  return(reslt)
}


# ====================================================================================
# USAGE: extract_table_one_t2() function example
# ====================================================================================

 # one <- extract_table_one_t2(excel_file_path = "../data-raw/LVA_Appendix_tables_Aug_2020.xlsx", save_csv_path = "../test.csv")
 # as.data.frame(one$df) %>% class()
# one

# ====================================================================================




# =============================================================================================
# **************************************** Table 2 from T2 ************************************
# =============================================================================================

extract_table_two_t2 <- function(excel_file_path, save_csv_path){
  # Mean annual per capita OOP by income quintile
  # excel_file_path <- '../../../../../../Desktop/appendix tables/SWE_Appendix_tables_May 2017_clean 02 Jun 2021.xlsx'
  t2_n <- readxl::read_excel(excel_file_path, sheet = "T2", skip = 10, n_max = 7)
  
  
  # Title years
  years_extracted <- readxl::read_excel(excel_file_path, sheet = "T2", skip = 0, n_max = 1) %>% 
    gather() %>% 
    select(value) %>% 
    drop_na() %>% 
    pull() %>% 
    as.character()
  
  if(length(t2_n) == 1){
    
    t2_n[years_extracted] <- NA
    
  }
  
  # Fix column names
  names(t2_n) <- c('service', years_extracted)
  t2_n <- t2_n[!is.na(names(t2_n))]
  # Transform from wide to long
  # t2a %>%
  #   tidyr::gather(year, value, `2005`:`2018`)
  # Add the indicator
  table_one_title <- str_replace_all('Mean annual per capita OOP by structure', pattern = ' ', replacement = '_')
  # write.csv(t2_n, file = str_glue('{save_csv_path}T2_TWO_{table_one_title}.csv'))
  # message("Table two from t2 extracted")
  
  table_one_title <- as.character(str_glue('T2_TWO_{table_one_title}.csv'))
  
  for(j in 2:ncol(t2_n)){
    t2_n[,j] <- format(round(as.numeric(round(unlist(t2_n[,j]), digits = 3)), digits = 3),nsmall = 3)
  }
  
  reslt <- list(
    df = t2_n,
    title = table_one_title
  )
  
  write.csv(t2_n, file = str_glue('{save_csv_path}T2_TWO_{table_one_title}.csv'))
  message("Table two from t2 extracted")
  
  return(reslt)
}


# ====================================================================================
# USAGE: extract_table_two_t2() function example
# ====================================================================================

# extract_table_two_t2(excel_file_path = "../data-raw/BUL_Appendix_tables.xlsx",
#                      save_csv_path = "../data-raw/extracted_csvs/T2/")

# read_csv("../data-raw/extracted_csvs/T2/T2_TWO_Mean_annual_per_capita_OOP_by_structure.csv")

# ====================================================================================




# =============================================================================================
# **************************************** Table 3 from T2 ************************************
# =============================================================================================

extract_table_three_t2 <- function(excel_file_path, save_csv_path){
  # Mean annual per capita OOP by income quintile
  t2_n <- readxl::read_excel(excel_file_path, sheet = "T2", skip = 18, n_max = 7)
  
  # Title years
  years_extracted <- readxl::read_excel(excel_file_path, sheet = "T2", skip = 0, n_max = 1) %>% 
    gather() %>% 
    select(value) %>% 
    drop_na() %>% 
    pull() %>% 
    as.character()
  
  if(length(t2_n) == 1){
    t2_n[years_extracted] <- NA
  }
  
  # Fix column names
  names(t2_n) <- c('service', years_extracted)
  # Transform from wide to long
  # t2a %>%
  #   tidyr::gather(year, value, `2005`:`2018`)
  # Add the indicator
  table_one_title <- str_replace_all('Share of total OOP spending by structure (total population)', pattern = ' ', replacement = '_')
  # write.csv(t2_n, file = str_glue('{save_csv_path}T2_THREE_{table_one_title}.csv'))
  # message("Table three from t2 extracted")
  
  table_one_title <- as.character(str_glue('T2_THREE_{table_one_title}.csv'))
  
  for(j in 2:ncol(t2_n)){
    t2_n[,j] <- format(round(as.numeric(round(unlist(t2_n[,j]), digits = 3)), digits = 3),nsmall = 3)
  }
  
  reslt <- list(
    df = t2_n,
    title = table_one_title
  )
  
  write.csv(t2_n, file = str_glue('{save_csv_path}T2_THREE_{table_one_title}.csv'))
  message("Table three from t2 extracted")
  
  return(reslt)
  
}


# ====================================================================================
# USAGE: extract_table_three_t2() function example
# ====================================================================================

# extract_table_three_t2(excel_file_path = "../data-raw/BUL_Appendix_tables.xlsx",
#                      save_csv_path = "../data-raw/extracted_csvs/T2/")

# read_csv("../data-raw/extracted_csvs/T2/T2_THREE_Share_of_total_OOP_spending_by_structure_(total_population).csv")

# ====================================================================================



# =============================================================================================
# **************************************** Table 4 from T2 ************************************
# =============================================================================================

extract_table_four_t2 <- function(excel_file_path, save_csv_path){
  # Mean annual per capita OOP by income quintile
  t2_n <- readxl::read_excel(excel_file_path, sheet = "T2", skip = 26, n_max = 7)
  
  # Title years
  years_extracted <- readxl::read_excel(excel_file_path, sheet = "T2", skip = 0, n_max = 1) %>% 
    gather() %>% 
    select(value) %>% 
    drop_na() %>% 
    pull() %>% 
    as.character()
  
  if(length(t2_n) == 1){
    t2_n[years_extracted] <- NA
  }
  
  # Fix column names
  names(t2_n) <- c('service', years_extracted)
  # Transform from wide to long
  # t2a %>%
  #   tidyr::gather(year, value, `2005`:`2018`)
  # Add the indicator
  table_title <- str_replace_all('Share of OOP spending by structure (1st quintile poorest)', pattern = ' ', replacement = '_')
  # write.csv(t2_n, file = str_glue('{save_csv_path}T2_FOUR_{table_title}.csv'))
  # message("Table four from t2 extracted")
  
  table_title <- as.character(str_glue('T2_FOUR_{table_title}.csv'))
  
  for(j in 2:ncol(t2_n)){
    t2_n[,j] <- format(round(as.numeric(round(unlist(t2_n[,j]), digits = 3)), digits = 3), nsmall = 3)
  }
  
  reslt <- list(
    df = t2_n,
    title = table_title
  )
  
  write.csv(t2_n, file = str_glue('{save_csv_path}T2_FOUR_{table_title}.csv'))
  message("Table four from t2 extracted")
  
  return(reslt)
  
}


# ====================================================================================
# USAGE: extract_table_four_t2() function example
# ====================================================================================

# extract_table_four_t2(excel_file_path = "../data-raw/BUL_Appendix_tables.xlsx",
#                        save_csv_path = "../data-raw/extracted_csvs/T2/")

# read_csv("../data-raw/extracted_csvs/T2/T2_FOUR_Share_of_OOP_spending_by_structure_(1st_quintile_poorest).csv")

# ====================================================================================



# =============================================================================================
# **************************************** Table 5 from T2 ************************************
# =============================================================================================

extract_table_five_t2 <- function(excel_file_path, save_csv_path){
  # Mean annual per capita OOP by income quintile
  t2_n <- readxl::read_excel(excel_file_path, sheet = "T2", skip = 34, n_max = 7)
  
  # Title years
  years_extracted <- readxl::read_excel(excel_file_path, sheet = "T2", skip = 0, n_max = 1) %>% 
    gather() %>% 
    select(value) %>% 
    drop_na() %>% 
    pull() %>% 
    as.character()
  
  if(length(t2_n) == 1){
    t2_n[years_extracted] <- NA
  }
  
  # Fix column names
  names(t2_n) <- c('service', years_extracted)
  # Transform from wide to long
  # t2a %>%
  #   tidyr::gather(year, value, `2005`:`2018`)
  # Add the indicator
  table_title <- str_replace_all('Share of OOP spending by structure (2nd quintile)', pattern = ' ', replacement = '_')
  # write.csv(t2_n, file = str_glue('{save_csv_path}T2_FIVE_{table_title}.csv'))
  # message("Table five from t2 extracted")
  
  table_title <- as.character(str_glue('T2_FIVE_{table_title}.csv'))
  
  for(j in 2:ncol(t2_n)){
    t2_n[,j] <- format(round(as.numeric(round(unlist(t2_n[,j]), digits = 3)), digits = 3),nsmall = 3)
  }
  
  reslt <- list(
    df = t2_n,
    title = table_title
  )
  
  write.csv(t2_n, file = str_glue('{save_csv_path}T2_FIVE_{table_title}.csv'))
  message("Table five from t2 extracted")
  
  return(reslt)
  
}


# ====================================================================================
# USAGE: extract_table_five_t2() function example
# ====================================================================================

# extract_table_five_t2(excel_file_path = "../data-raw/BUL_Appendix_tables.xlsx",
#                       save_csv_path = "../data-raw/extracted_csvs/T2/")

# read_csv("../data-raw/extracted_csvs/T2/T2_FIVE_Share_of_OOP_spending_by_structure_(2nd_quintile).csv")

# ====================================================================================





# =============================================================================================
# **************************************** Table 6 from T2 ************************************
# =============================================================================================

extract_table_six_t2 <- function(excel_file_path, save_csv_path){
  # Mean annual per capita OOP by income quintile
  t2_n <- readxl::read_excel(excel_file_path, sheet = "T2", skip = 42, n_max = 7)
  
  # Title years
  years_extracted <- readxl::read_excel(excel_file_path, sheet = "T2", skip = 0, n_max = 1) %>% 
    gather() %>% 
    select(value) %>% 
    drop_na() %>% 
    pull() %>% 
    as.character()
  
  if(length(t2_n) == 1){
    t2_n[years_extracted] <- NA
  }
  
  # Fix column names
  names(t2_n) <- c('service', years_extracted)
  # Transform from wide to long
  # t2a %>%
  #   tidyr::gather(year, value, `2005`:`2018`)
  # Add the indicator
  table_title <- str_replace_all('Share of OOP spending by structure (3rd quintile)', pattern = ' ', replacement = '_')
  # write.csv(t2_n, file = str_glue('{save_csv_path}T2_SIX_{table_title}.csv'))
  # message("Table six from t2 extracted")
  
  table_title <- as.character(str_glue('T2_SIX_{table_title}.csv'))
  
  for(j in 2:ncol(t2_n)){
    t2_n[,j] <- format(round(as.numeric(round(unlist(t2_n[,j]), digits = 3)), digits = 3),nsmall = 3)
  }
  
  reslt <- list(
    df = t2_n,
    title = table_title
  )
  
  write.csv(t2_n, file = str_glue('{save_csv_path}T2_SIX_{table_title}.csv'))
  message("Table six from t2 extracted")
  
  return(reslt)
  
}


# ====================================================================================
# USAGE: extract_table_six_t2() function example
# ====================================================================================
# 
# extract_table_six_t2(excel_file_path = "../data-raw/BUL_Appendix_tables.xlsx",
#                       save_csv_path = "../data-raw/extracted_csvs/T2/")

# read_csv("../data-raw/extracted_csvs/T2/T2_SIX_Share_of_OOP_spending_by_structure_(3rd_quintile).csv")

# ====================================================================================




# =============================================================================================
# **************************************** Table 7 from T2 ************************************
# =============================================================================================

extract_table_seven_t2 <- function(excel_file_path, save_csv_path){
  # Mean annual per capita OOP by income quintile
  t2_n <- readxl::read_excel(excel_file_path, sheet = "T2", skip = 50, n_max = 7)
  
  # Title years
  years_extracted <- readxl::read_excel(excel_file_path, sheet = "T2", skip = 0, n_max = 1) %>% 
    gather() %>% 
    select(value) %>% 
    drop_na() %>% 
    pull() %>% 
    as.character()
  
  if(length(t2_n) == 1){
    t2_n[years_extracted] <- NA
  }
  
  # Fix column names
  names(t2_n) <- c('service', years_extracted)
  # Transform from wide to long
  # t2a %>%
  #   tidyr::gather(year, value, `2005`:`2018`)
  # Add the indicator
  table_title <- str_replace_all('Share of OOP spending by structure (4th quintile)', pattern = ' ', replacement = '_')
  # write.csv(t2_n, file = str_glue('{save_csv_path}T2_SEVEN_{table_title}.csv'))
  # message("Table seven from t2 extracted")
  table_title <- as.character(str_glue('T2_SEVEN_{table_title}.csv'))
  
  for(j in 2:ncol(t2_n)){
    t2_n[,j] <- format(round(as.numeric(round(unlist(t2_n[,j]), digits = 3)), digits = 3),nsmall = 3)
  }
  
  reslt <- list(
    df = t2_n,
    title = table_title
  )
  
  write.csv(t2_n, file = str_glue('{save_csv_path}T2_SEVEN_{table_title}.csv'))
  message("Table seven from t2 extracted")
  
  return(reslt)
}


# ====================================================================================
# USAGE: extract_table_seven_t2() function example
# ====================================================================================

# extract_table_seven_t2(excel_file_path = "../data-raw/BUL_Appendix_tables.xlsx",
#                      save_csv_path = "../data-raw/extracted_csvs/T2/")

# read_csv("../data-raw/extracted_csvs/T2/T2_SEVEN_Share_of_OOP_spending_by_structure_(3rd_quintile).csv")

# ====================================================================================





# =============================================================================================
# **************************************** Table 8 from T2 ************************************
# =============================================================================================

extract_table_eight_t2 <- function(excel_file_path, save_csv_path){
  # Mean annual per capita OOP by income quintile
  t2_n <- readxl::read_excel(excel_file_path, sheet = "T2", skip = 58, n_max = 7)
  
  # Title years
  years_extracted <- readxl::read_excel(excel_file_path, sheet = "T2", skip = 0, n_max = 1) %>% 
    gather() %>% 
    select(value) %>% 
    drop_na() %>% 
    pull() %>% 
    as.character()
  
  if(length(t2_n) == 1){
    t2_n[years_extracted] <- NA
  }
  
  # Fix column names
  names(t2_n) <- c('indicator', years_extracted)
  # Transform from wide to long
  # t2a %>%
  #   tidyr::gather(year, value, `2005`:`2018`)
  # Add the indicator
  table_title <- str_replace_all('Share of OOP spending by structure (5th quintile richest)', pattern = ' ', replacement = '_')
  # write.csv(t2_n, file = str_glue('{save_csv_path}T2_EIGHT_{table_title}.csv'))
  # message("Table eight from t2 extracted")
  
  table_title <- as.character(str_glue('T2_EIGHT_{table_title}.csv'))
  
  for(j in 2:ncol(t2_n)){
    t2_n[,j] <- format(round(as.numeric(round(unlist(t2_n[,j]), digits = 3)), digits = 3),nsmall = 3)
  }
  
  
  t2_n <- t2_n %>%
    rename(
      `service` = `indicator`
    )
  
  reslt <- list(
    df = t2_n,
    title = table_title
  )
  
  write.csv(t2_n, file = str_glue('{save_csv_path}T2_EIGHT_{table_title}.csv'))
  message("Table eight from t2 extracted")
  
  return(reslt)
  
}


# ====================================================================================
# USAGE: extract_table_eight_t2() function example
# ====================================================================================

# extract_table_eight_t2(excel_file_path = "../data-raw/BUL_Appendix_tables.xlsx",
#                        save_csv_path = "../data-raw/extracted_csvs/T2/")

# read_csv("../data-raw/extracted_csvs/T2/T2_EIGHT_Share_of_OOP_spending_by_structure_(5th_quintile_richest).csv")

# ====================================================================================




# =============================================================================================
# **************************************** All tables from T2 ************************************
# =============================================================================================

extract_all_t2 <- function(excel_file_path, save_csv_path){
  
  extract_table_one_t2(excel_file_path = excel_file_path, save_csv_path = save_csv_path)
  extract_table_two_t2(excel_file_path = excel_file_path, save_csv_path = save_csv_path)
  extract_table_three_t2(excel_file_path = excel_file_path, save_csv_path = save_csv_path)
  extract_table_four_t2(excel_file_path = excel_file_path, save_csv_path = save_csv_path)
  extract_table_five_t2(excel_file_path = excel_file_path, save_csv_path = save_csv_path)
  extract_table_six_t2(excel_file_path = excel_file_path, save_csv_path = save_csv_path)
  extract_table_seven_t2(excel_file_path = excel_file_path, save_csv_path = save_csv_path)
  extract_table_eight_t2(excel_file_path = excel_file_path, save_csv_path = save_csv_path)
  
}


# ====================================================================================
# USAGE: extract_all_t2() function example
# ====================================================================================

### NOTES ###
# just changes `excel_file_path` where .xls is located, 
# also you can change `save_csv_path`- directory where to save extracted .csv (it is optional)
# Then just run the R script


# extract_all_t2(excel_file_path = "../data-raw/BUL_Appendix_tables.xlsx",
#                        save_csv_path = "../data-raw/extracted_csvs/T2/")

# read_csv("../data-raw/extracted_csvs/T2/T2_EIGHT_Share_of_OOP_spending_by_structure_(5th_quintile_richest).csv")

# ====================================================================================

