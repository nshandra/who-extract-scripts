library(readxl)
library(tidyverse)
library(stringr)
library(dplyr)


# =============================================================================================
# **************************** Table from Population Entitlement ******************************
# =============================================================================================


extract_pe <- function(excel_file_path, save_csv_path){ 
  
  sheet_num_extract <- excel_sheets(excel_file_path) %>% str_trim() %>% str_which(pattern = "\\bpe\\b")
  
  table <- readxl::read_excel(path = excel_file_path, sheet = sheet_num_extract, skip = 4)
  
  # names(table)[ncol(table)] <- "Notes"
  
  table$Residence <-  table$Residence %>% str_remove_all(pattern = '\r|\n')
  table$Citizenship <-  table$Citizenship %>% str_remove_all(pattern = '\r|\n')
  table$Notes <-  table$Notes %>% str_remove_all(pattern = '\r|\n')
  
  write.csv(x = table, file = save_csv_path)
  
  return(table)
  
}


# ====================================================================================
# USAGE: extract_pe() function example
# ====================================================================================

# extract_pe('../../../WHO_extract_excel/data_qualitative/Ireland Template.xlsx', save_csv_path = '../save_path.csv')
