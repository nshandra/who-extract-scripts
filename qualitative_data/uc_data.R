library(readxl)
library(tidyverse)
library(stringr)
library(dplyr)


# =============================================================================================
# ********************************** Table from User Charges **********************************
# =============================================================================================


extract_uc <- function(excel_file_path, save_csv_path){ 
  
  table <- readxl::read_excel(path = excel_file_path, sheet = "uc", skip = 2)
  names(table)[1:2] <- c('Service', 'Service Category')
  # if(ncol(table) == 42) {
  #   names(table)[(ncol(table)-2):ncol(table)] <- c('VHI covers user charges for publicly financed health services - NO', 
  #                                                  'Updated until Year', 
  #                                                  'NOTES')
  #   table$`Updated until Year` <- as.integer(table$`Updated until Year`)
  # } else {
  #   names(table)[(ncol(table)-1):ncol(table)] <- c('VHI covers user charges for publicly financed health services - NO', 
  #                                                  'Updated until Year')
  #   table <- table %>%
  #     mutate(NOTES = NA)
  #   
  # }
  
  table$`Updated until Year` <- as.integer(table$`Updated until Year`)
  
  write.csv(x = table, file = save_csv_path)
  
  return(table)
  
}


# ====================================================================================
# USAGE: extract_uc() function example
# ====================================================================================

# extract_uc('../../../WHO_extract_excel/data_qualitative/Poland Template.xlsx', save_csv_path = '../save_path.csv')
