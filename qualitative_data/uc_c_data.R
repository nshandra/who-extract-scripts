library(readxl)
library(tidyverse)
library(stringr)
library(dplyr)

# =============================================================================================
# ***************************** Table from User Charges Changes *******************************
# =============================================================================================


extract_uc_c <- function(excel_file_path, save_csv_path){ 
  
  table <- readxl::read_excel(path = excel_file_path, sheet = "uc_c", skip = 2)
  table$Year <- as.integer(table$Year)
  
  write.csv(x = table, file = save_csv_path)
  
  return(table)
  
}


# ====================================================================================
# USAGE: extract_uc_c() function example
# ====================================================================================

# extract_uc_c('../../../WHO_extract_excel/data_qualitative/Ireland Template.xlsx', save_csv_path = '../save_path.csv')
