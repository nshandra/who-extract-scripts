library(readxl)
library(tidyverse)
library(stringr)
library(dplyr)
library(purrr)

# =============================================================================================
# *************************************** Figure 2 from T0 ************************************
# =============================================================================================
extract_t0_fig2 <- function(excel_file_path, save_csv_path) {
  
  fig2 <- readxl::read_excel(excel_file_path, sheet = 2, skip = 12, n_max = 7)
  
  years_extracted <- readxl::read_excel(excel_file_path, sheet = 2, skip = 10, n_max = 7) %>% 
    slice(1) %>% 
    gather() %>%
    select(value) %>% drop_na() %>% pull() %>% as.numeric()
  
  # Change name of first column
  names(fig2)[1] <- 'grp'
  # Transform from character to numeric
  for(j in 2:ncol(fig2)){
    fig2[,j] <- format(round(as.numeric(unlist(fig2[,j])) ,digits = 4),nsmall = 4)
  }
  # Tidy up names
  q_part <- rep(c('Poorest', '2nd', '3rd', '4th', 'Richest', 'Total'), 6)
  y_part <- rep(years_extracted, each = 6)
  new_names <- paste0(q_part, '_', y_part)
  names(fig2)[2:ncol(fig2)] <- new_names
  # Transform from wide to long
  fig2 <- fig2 %>%
    tidyr::gather(year, value, names(fig2)[2]:names(fig2)[ncol(fig2)])
  # Split the "year" column into its respective parts
  fig2 <- fig2 %>%
    mutate(quintile = unlist(lapply(strsplit(year, split = '_'), function(x){x[1]}))) %>%
    mutate(year = unlist(lapply(strsplit(year, split = '_'), function(x){x[2]})))
  # Transpose table
  fig2 <- fig2 %>%
    pivot_wider(names_from = "grp", values_from = "value") %>%
    rename(
      Year = year,
      `Income Quintile` = quintile
    ) 
  
  fig2 %>%
    write.csv(x = ., file = save_csv_path)
  
  message("Fig2 extracted from T0")
  
  return(fig2)
  
}

# ====================================================================================
# USAGE: extract_t0_fig2() function example
# ====================================================================================

# extract_t0_fig2(excel_file_path = "../data-raw/BUL_Appendix_tables.xlsx",
#                 save_csv_path = "../data-raw/extracted_csvs/T0/Figure_6_Final.csv")


# ====================================================================================


# ====================================================================================
# ****************************** Figure 3 from T0 ************************************
# ====================================================================================

extract_t0_fig3 <- function(excel_file_path, save_csv_path) {

  fig3 <- readxl::read_excel(excel_file_path, sheet = 2, skip = 21, n_max = 7)
  
  # Modify first name
  names(fig3)[1] <- 'indicator'
  # Transform from character to numeric
  for(j in 2:ncol(fig3)){
    fig3[,j] <- format(round(as.numeric(unlist(fig3[,j])), digits = 4),nsmall = 4)
  }
  # Transform from wide to long
  fig3 <- fig3 %>%
    pivot_longer(cols = names(fig3)[2]:names(fig3)[ncol(fig3)], names_to = "Year") 
  
  fig3 %>%
    write.csv(x = ., file = save_csv_path)
  
  message("Fig3 extracted from T0")
  
  return(fig3)
  
  
}

# ====================================================================================
# USAGE: extract_t0_fig3() function example
# ====================================================================================

# extract_t0_fig3(excel_file_path = "../data-raw/BUL_Appendix_tables.xlsx",
#                 save_csv_path = "../data-raw/extracted_csvs/T0/Figure_13_14_Final.csv")

# ====================================================================================



# ====================================================================================
# ****************************** Figure 5 from T0 ************************************
# ====================================================================================


extract_t0_fig5 <- function(excel_file_path, save_csv_path) {
  
  fig5 <- readxl::read_excel(excel_file_path, sheet = 2, skip = 57, n_max = 7)
  # Extract years
  years_extracted <- readxl::read_excel(excel_file_path, sheet = 2, skip = 55, n_max = 7) %>% 
    slice(1) %>% 
    gather() %>%
    select(value) %>% drop_na() %>% pull() %>% as.numeric()
  
  # Change name of first column
  names(fig5)[1] <- 'grp'
  # Transform from character to numeric
  for(j in 2:ncol(fig5)){
    fig5[,j] <- format(round(as.numeric(unlist(fig5[,j])), digits = 4), nsmall = 4)
  }
  
  
  # Tidy up names
  q_part <- rep(c('Poorest', '2nd', '3rd', '4th', 'Richest', 'Total'), 6)
  y_part <- rep(years_extracted, each = 6) 
  
  new_names <- paste0(q_part, '_', y_part)
  
  names(fig5)[2:ncol(fig5)] <- new_names
  # Transform from wide to long
  fig5 <- fig5 %>%
    tidyr::gather(year, value, names(fig5)[2]:names(fig5)[ncol(fig5)])
  # Split the "year" column into its respective parts
  fig5 <- fig5 %>%
    mutate(quintile = unlist(lapply(strsplit(year, split = '_'), function(x){x[1]}))) %>%
    mutate(year = unlist(lapply(strsplit(year, split = '_'), function(x){x[2]})))
  
  fig5 <- fig5 %>%
    rename(
      Indicator = grp,
      Year = year,
      `Income Quintile` = quintile
    )
  
  fig5%>%
    write.csv(x = ., file = save_csv_path)


  message("Fig5 extracted from T0")
  
  return(fig5)
  
}

# ====================================================================================
# USAGE: extract_t0_fig5() function example
# ====================================================================================

# extract_t0_fig5(excel_file_path = "../data-raw/BUL_Appendix_tables.xlsx",
#                 save_csv_path = "../data-raw/extracted_csvs/T0/Figure_21_Final.csv")

# ====================================================================================

# ====================================================================================
# ************** Extract fig2, fig3, fig5 from T0 with one fucntion ******************
# ====================================================================================


general_extraction_t0 <- function(excel_file_path, fig2_save_path, fig3_save_path, fig5_save_path){
  extract_t0_fig2(excel_file_path = excel_file_path, save_csv_path = fig2_save_path)
  extract_t0_fig3(excel_file_path = excel_file_path, save_csv_path = fig3_save_path)
  extract_t0_fig5(excel_file_path = excel_file_path, save_csv_path = fig5_save_path)
}

# ====================================================================================
# USAGE: general_extraction_t0() function example
# ====================================================================================

# general_extraction_t0(excel_file_path = "../data-raw/BUL_Appendix_tables.xlsx",
#                       fig2_save_path = "../data-raw/extracted_csvs/T0/Figure_6_Final.csv",
#                       fig3_save_path = "../data-raw/extracted_csvs/T0/Figure_13_14_Final.csv",
#                       fig5_save_path = "../data-raw/extracted_csvs/T0/Figure_21_Final.csv"
#                       )


# ====================================================================================

#### NOTES ####

# -1) Load required libraries 1 - 5 strings, and declare fucntions (run the pieces of code with functions body)
# -2) If you would like to extract only fig.3 for example just run the `extract_t0_fig3()`
#     function. It has two arguments `excel_file_path` and `save_csv_path`. In the `excel_file_path`
#     you should specify the path to the excel file, in the `save_csv_path` you should specify path where to save
#     to save created csv file. 
# -3) If you would like to extract all csv files in one step just call the function `general_extraction_t0()` it 
#     has four arguments `excel_file_path`, `fig2_save_path`, `fig3_save_path`, `fig5_save_path`


