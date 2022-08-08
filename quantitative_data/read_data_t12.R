library(readxl)
library(tidyverse)
library(stringr)
library(dplyr)
library(purrr)


# =============================================================================================
# **************************************** Table from T12 **************************************
# =============================================================================================

# excel_file_path = '../../../Downloads/CRO_Appendix_tables_July 2017_clean 03 Jun 2021 (1).xlsx'

extract_t12 <- function(excel_file_path) { 
  
  sheet_num_extract <- excel_sheets(excel_file_path) %>% str_trim() %>% str_which(pattern = "\\bT12\\b")
  
  table1 <- readxl::read_excel(excel_file_path, sheet = sheet_num_extract, skip = 2) %>% 
    select(1, 2:3)
  
  
  years <- readxl::read_excel(excel_file_path, sheet = sheet_num_extract, skip = 2) %>% 
    select(1, 2:3) %>%
    select(Year) %>% drop_na() %>% pull() 
  
  # Fill in missing values
  table1 <- table1 %>%
    mutate(Year = rep(years, nrow(table1)/length(years)) %>% sort())
  
  names(table1)[2:3] <- c(
                          'Out-of-pocket payments for health care as a share of household consumption (total)',
                          'Out-of-pocket payments for health care as a share of household consumption (by quintile)'
                          ) 
  
  
  for(j in 2:ncol(table1)){
    table1[,j] <- as.numeric(unlist(table1[,j]))
    # table1[,j] <- format(round(as.numeric(unlist(table1[,j])), digits = 3),nsmall = 3)
  }
  
  table1 <- table1 %>%
    mutate(Year = as.integer(Year)) %>%
    mutate(
      `Out-of-pocket payments for health care as a share of household consumption (total)` = `Out-of-pocket payments for health care as a share of household consumption (total)` * 100,
      `Out-of-pocket payments for health care as a share of household consumption (by quintile)` = `Out-of-pocket payments for health care as a share of household consumption (by quintile)` * 100     
    )
  
  # Tidy up names
  
  return(table1)
  
  # write.csv(table1, file = save_csv_path)
  # 
  # message("Table extracted from T0")
}