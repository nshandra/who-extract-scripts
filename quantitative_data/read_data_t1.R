library(readxl)
library(tidyverse)
library(stringr)
library(dplyr)
library(purrr)


# =============================================================================================
# **************************************** Table from T1** ************************************
# =============================================================================================



extract_t1 <- function(excel_file_path, save_csv_path){ 
  
  table1 <- readxl::read_excel(excel_file_path, sheet = "T1")
    
    # Title years
  years_extracted <- readxl::read_excel(excel_file_path, sheet = "T1", skip = 0, n_max = 1) %>% 
    gather() %>% 
    select(value) %>% 
    drop_na() %>% 
    pull() %>% 
    as.character()
  
  colnames(table1) <- c('indicator', years_extracted)
  
  for(j in 2:ncol(table1)){
    table1[,j] <- format(round(as.numeric(round(unlist(table1[,j]), digits = 3)), digits = 3), nsmall = 3)
  }
  
  table1 <- table1 %>%
    slice(46:48) %>%
    pivot_longer(cols = 2:ncol(table1), names_to = "Year", values_to = "value", names_repair = "unique") %>%
    pivot_wider(names_from = "indicator", values_from = "value")
  
  write.csv(table1, file = save_csv_path)
  # 
  message("Table extracted from T1")  
  
  return(table1)
}


# ====================================================================================
# USAGE: extract_t1() function example
# ====================================================================================

### NOTES FOR KATE ###
# just changes `excel_file_path` where .xls is located, 
# also you can change `save_csv_path`- directory where to save extracted .csv (it is optional)
# Then just run the R script

# extract_t1(excel_file_path = "data-raw/BUL_Appendix_tables.xlsx",
#                      save_csv_path = "../fig_test.csv")


# ====================================================================================

