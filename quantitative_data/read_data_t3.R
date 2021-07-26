library(readxl)
library(tidyverse)
library(stringr)
library(dplyr)
library(purrr)


# =============================================================================================
# **************************************** Table from T3** ************************************
# =============================================================================================



extract_t3 <- function(excel_file_path, save_csv_path){ 
  
  table1 <- readxl::read_excel(excel_file_path, sheet = "T3")
  
  # Title years
  years_extracted <- readxl::read_excel(excel_file_path, sheet = "T3", skip = 0, n_max = 1) %>% 
    gather() %>% 
    select(value) %>% 
    drop_na() %>% 
    pull() %>% 
    as.character()
  
  colnames(table1) <- c('indicator', years_extracted)
  
  for(j in 2:ncol(table1)){
    table1[,j] <- format(round(as.numeric(round(unlist(table1[,j]), digits = 2)), digits = 2),nsmall = 2)
  }
  
  table1 <- table1 %>%
    slice(52) %>%
    pivot_longer(cols = 2:ncol(table1), names_to = "Year", values_to = "value", names_repair = "unique") %>%
    pivot_wider(names_from = "indicator", values_from = "value")
  
  write.csv(table1, file = save_csv_path)

  message("Table extracted from T3")
  
  return(table1)
  
}


# ====================================================================================
# USAGE: extract_t3() function example
# ====================================================================================

### NOTES ###
# just changes `excel_file_path` where .xls is located, 
# also you can change `save_csv_path`- directory where to save extracted .csv (it is optional)
# Then just run the R script


# extract_t3(excel_file_path = "../data-raw/BUL_Appendix_tables.xlsx",
#            save_csv_path = "../data-raw/extracted_csvs/T3/T3_Figure_3_Final.csv")

# read_csv('../data-raw/extracted_csvs/T3/T3_Figure_3_Final.csv')

# ====================================================================================


extract_t3_fig13 <- function(excel_file_path, save_csv_path) {
  table <- readxl::read_excel(excel_file_path, sheet = "T3")
  
  # Title years
  years_extracted <- readxl::read_excel(excel_file_path, sheet = "T3", skip = 0, n_max = 1) %>% 
    gather() %>% 
    select(value) %>% 
    drop_na() %>% 
    pull() %>% 
    as.character()
  
  colnames(table) <- c('indicator', years_extracted)
  
  for(j in 2:ncol(table)){
    table[,j] <- as.numeric(round(unlist(table[,j]), digits = 2))
  }
  table[47, ]$indicator <- str_glue('{table[44, ]$indicator}- {table[47, ]$indicator}') 
  
  table <- table %>%
    slice(c(42:43, 47)) %>%
    pivot_longer(cols = 2:ncol(table), names_to = "Year", values_to = "value", names_repair = "unique") %>%
    pivot_wider(names_from = "indicator", values_from = "value")
  
  write.csv(table, file = save_csv_path)
  
  message("Table extracted from T3 (fig13)")  
  
  return(table)
  
}