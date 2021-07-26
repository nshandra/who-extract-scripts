library(readxl)
library(tidyverse)
library(stringr)
library(dplyr)


# =============================================================================================
# *********************** Table from Population Entitlement Changes ***************************
# =============================================================================================


extract_pe_c <- function(excel_file_path, save_csv_path){ 
  
  table <- readxl::read_excel(path = excel_file_path, sheet = "pe_c", skip = 2)
  table$Year <- as.integer(table$Year)
  
  write.csv(x = table, file = save_csv_path)
  
  return(table)
  
}


# ====================================================================================
# USAGE: extract_pc_c() function example
# ====================================================================================

# extract_pc_c('../../../WHO_extract_excel/data_qualitative/Poland Template.xlsx', save_csv_path = '../save_path.csv')