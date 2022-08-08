library(readxl)
library(tidyverse)
library(stringr)
library(dplyr)
library(purrr)


# =============================================================================================
# **************************************** Table from T9 **************************************
# =============================================================================================



extract_t9 <- function(excel_file_path, save_csv_path){ 
  
  # excel_file_path <- '../WHO_extract_excel/appendix tables/changed/ALB_Appendix_tables_Feb 2017_clean 31 May 2021.xlsx'
  sheet_num_extract <- excel_sheets(excel_file_path) %>% str_trim() %>% str_which(pattern = "\\bT9\\b")
  table1 <- readxl::read_excel(excel_file_path, sheet = sheet_num_extract)
  
  # Title years
  years_extracted <- readxl::read_excel(excel_file_path, sheet = sheet_num_extract, skip = 0, n_max = 1) %>% 
    gather() %>% 
    select(value) %>% 
    drop_na() %>% 
    pull() %>% 
    as.character()
  
  table1 <- table1[colSums(!is.na(table1)) > 0] 
  
  colnames(table1) <- c('table_label', 'quintile', years_extracted)
  
  for(j in 3:ncol(table1)){
    table1[,j] <- as.numeric(unlist(table1[,j]))
     # table1[,j] <- format(round(as.numeric(unlist(table1[,j])), digits = 2),nsmall = 2)
  }
  
  # Tidy up names
  s_part <- rep(c('Medicines', 'Medical products', 'Outpatient care', 'Dental', 'Diagnostic tests', 'Inpatient care'), length(years_extracted))
  y_part <- rep(years_extracted, each = 6) 
  
  new_names <- paste0(s_part, '_', y_part)
  
  names(table1) <- c('table_label', 'quintile', new_names)
  
  table1 <- table1 %>% slice(97:101)
  
  
  # Transform from wide to long
  table1 <- table1 %>%
    tidyr::gather(year, value, names(table1)[3]:names(table1)[ncol(table1)])
  # Split the "year" column into its respective parts
  table1 <- table1 %>%
    mutate(service = unlist(lapply(strsplit(year, split = '_'), function(x){x[1]}))) %>%
    mutate(year = unlist(lapply(strsplit(year, split = '_'), function(x){x[2]}))) %>%
    select(year, quintile, service, value, table_label) %>%
    mutate(table_label = (table1 %>% select(table_label) %>% drop_na() %>% distinct() %>% pull())) %>%
    pivot_wider(names_from = 'service', values_from = 'value')
  
  
  return(table1)
  
  # write.csv(table1, file = save_csv_path)
  # 
  # message("Table extracted from T8")  
}


# ====================================================================================
# USAGE: extract_t8() function example
# ====================================================================================
