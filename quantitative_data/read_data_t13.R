library(readxl)
library(tidyverse)
library(stringr)
library(dplyr)
library(purrr)


# =============================================================================================
# **************************************** Table from T13 **************************************
# =============================================================================================



extract_t13 <- function(excel_file_path){ 
  
  # excel_file_path <- '../../../Downloads/GEO_Appendix_tables_Dec 2016_clean 03 Jun 2021.xlsx'
  # excel_file_path <- '../../../Downloads/Background material DB 10 Feb 2022/Bulgaria/BUL_Appendix_tables.xlsx'
  
  sheet_num_extract <- excel_sheets(excel_file_path) %>% str_trim() %>% str_which(pattern = "\\bT13\\b")
  
  table1 <- readxl::read_excel(excel_file_path, sheet = sheet_num_extract, skip = 3) %>% 
    select(1, 19:20)
  
  
  years <- readxl::read_excel(excel_file_path, sheet = sheet_num_extract, skip = 3) %>% 
    select(1, 19:20) %>%
    select(Year) %>% drop_na() %>% pull() 
  
  # Fill in missing values
  table1 <- table1 %>%
    mutate(Year = rep(years, nrow(table1)/length(years)) %>% sort())
  # Multiply to 100 to convert to percent
  table1[,2] <- table1[,2] * 100
  table1[,3] <- table1[,3] * 100
  
  titles <-  readxl::read_excel(excel_file_path, sheet = sheet_num_extract, skip = 1, n_max = 1) %>% 
    gather() %>% select(value) %>% slice((nrow(.)-1):nrow(.)) %>% pull()
  
  names(table1) <- c(names(table1)[1], str_glue('{titles} - {names(table1)[2:3]}')) 
  
  
  for(j in 2:ncol(table1)){
    table1[,j] <- as.numeric(unlist(table1[,j]))
    # table1[,j] <- format(round(as.numeric(unlist(table1[,j])), digits = 3),nsmall = 3)
  }
  
  table1 <- table1 %>%
    mutate(Year = as.integer(Year))
  
  # Tidy up names
  
  return(table1)
  
  # write.csv(table1, file = save_csv_path)
  # 
  # message("Table extracted from T0")
}


# ====================================================================================
# USAGE: extract_t13() function example
# ====================================================================================

### NOTES FOR KATE ###
# just changes `excel_file_path` where .xls is located, 
# also you can change `save_csv_path`- directory where to save extracted .csv (it is optional)
# Then just run the R script


# extract_t13(excel_file_path = "../data-raw/LVA_Appendix_tables_Aug_2020.xlsx",
#            save_csv_path = "../data-raw/extracted_csvs/T13/T13_Figure_22_and_24_Final.csv")


# ====================================================================================

# read_csv('../data-raw/extracted_csvs/T10/T10_Figure_15_Final.csv')
