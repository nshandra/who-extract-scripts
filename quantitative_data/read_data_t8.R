library(readxl)
library(tidyverse)
library(stringr)
library(dplyr)
library(purrr)


# =============================================================================================
# **************************************** Table from T8 **************************************
# =============================================================================================



extract_t8 <- function(excel_file_path, save_csv_path){ 
  
  table1 <- readxl::read_excel(excel_file_path, sheet = "T8")
  
  # Title years
  years_extracted <- readxl::read_excel(excel_file_path, sheet = "T8", skip = 0, n_max = 1) %>% 
    gather() %>% 
    select(value) %>% 
    drop_na() %>% 
    pull() %>% 
    as.character()
  
  colnames(table1) <- c('indicator', years_extracted)
  
  for(j in 2:ncol(table1)){
    table1[,j] <- format(round(as.numeric(unlist(table1[,j])), digits = 2),nsmall = 2)
  }
  
  # Tidy up names
  q_part <- rep(c('Medicines', 'Medical products', 'Outpatient care', 'Dental', 'Diagnostic tests', 'Inpatient care'), 6)
  y_part <- rep(years_extracted, each = 6) 
  
  new_names <- paste0(q_part, '_', y_part)
  
  names(table1)[2:ncol(table1)] <- new_names
  
  table1 <- table1 %>% slice(25) 
  # Transform from wide to long
  table1 <- table1 %>%
    tidyr::gather(year, value, names(table1)[2]:names(table1)[ncol(table1)])
  # Split the "year" column into its respective parts
  table1 <- table1 %>%
    mutate(quintile = unlist(lapply(strsplit(year, split = '_'), function(x){x[1]}))) %>%
    mutate(year = unlist(lapply(strsplit(year, split = '_'), function(x){x[2]}))) %>%
    select(year, quintile, value, indicator) %>%
    rename(
      Service = quintile,
      Year = year
    ) %>% pivot_wider(names_from = 'indicator', values_from = 'value')
  
  write.csv(table1, file = save_csv_path)
  
  message("Table extracted from T8")

  return(table1)
    
}


# ====================================================================================
# USAGE: extract_t8() function example
# ====================================================================================

### NOTES FOR KATE ###
# just changes `excel_file_path` where .xls is located, 
# also you can change `save_csv_path`- directory where to save extracted .csv (it is optional)
# Then just run the R script


# extract_t8(excel_file_path = "../data-raw/BUL_Appendix_tables.xlsx",
#            save_csv_path = "../data-raw/extracted_csvs/T8/T8_Figure_20_Final.csv")


# ====================================================================================

# read_csv('../data-raw/extracted_csvs/T8/T8_Figure_20_Final.csv')
