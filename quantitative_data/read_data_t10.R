library(readxl)
library(tidyverse)
library(stringr)
library(dplyr)
library(purrr)


# =============================================================================================
# **************************************** Table from T10 **************************************
# =============================================================================================



extract_t10 <- function(excel_file_path, save_csv_path){ 
  
  table1 <- readxl::read_excel(excel_file_path, sheet = "T10", skip = 2) %>%
    select(1, 10:13)
  
  names(table1) <-  c(names(table1)[1], substr(names(table1)[2:5],1,nchar(names(table1)[2:5])-4)) %>% str_remove(pattern = '[.]') 
  
  # Title 
  header_title <- readxl::read_excel(excel_file_path, sheet = "T10", skip = 0, n_max = 1) %>% 
    gather() %>% 
    select(value) %>% 
    drop_na() %>%
    slice(4) %>%
    pull() %>% 
    as.character()
  
  header_title <- str_replace_all(str_to_title(header_title), ' ', replacement = '_') 
  
  for(j in 2:ncol(table1)){
    table1[,j] <- format(round(as.numeric(unlist(table1[,j])), 3), nsmall = 3)
  }
  
  table1[,1] <- as.integer(unlist(table1[,1]))
  
  # Tidy up names
  
  write.csv(table1, file = save_csv_path)

  message("Table extracted from T0")
  
  return(table1)
  
}


# ====================================================================================
# USAGE: extract_t10() function example
# ====================================================================================

### NOTES ###
# just changes `excel_file_path` where .xls is located, 
# also you can change `save_csv_path`- directory where to save extracted .csv (it is optional)
# Then just run the R script


# extract_t10(excel_file_path = "../data-raw/LVA_Appendix_tables_Aug_2020.xlsx",
#            save_csv_path = "../data-raw/extracted_csvs/T10/T10_Figure_15_Final.csv")


# ====================================================================================

# read_csv('../data-raw/extracted_csvs/T10/T10_Figure_15_Final.csv')
