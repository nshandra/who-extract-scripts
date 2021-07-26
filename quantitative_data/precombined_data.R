library(tidyverse)
library(dplyr)
library(readxl)

source(file = 'qt_data_scripts/read_data_t1.R')
source(file = 'qt_data_scripts/read_data_t2.R')
source(file = 'qt_data_scripts/read_data_t3.R')
source(file = 'qt_data_scripts/read_data_t4.R')
source(file = 'qt_data_scripts/read_data_t5.R')
source(file = 'qt_data_scripts/read_data_t6.R')
source(file = 'qt_data_scripts/read_data_t8.R')
source(file = 'qt_data_scripts/read_data_t9.R')
source(file = 'qt_data_scripts/read_data_t10.R')
source(file = 'qt_data_scripts/read_data_t13.R')

# T1
# extract_t1('../../../WHO_extract_excel/OneDrive_1_07.06.2021/Quantitative files/appendix tables/LVA_Appendix_tables_Aug 2020.xlsx')

extract_t1_precombined <- function(extracted_t1_sheet) {
  # extracted_t1_sheet <- extract_t1('../../../../Downloads/LVA_Appendix_tables_Aug 2020_clean 03 Jun 2021.xlsx')
  
  
  extracted_t1_sheet <- extracted_t1_sheet %>% pivot_longer(cols = 2:4, names_to = "table_label", values_to = "values") %>%
    mutate(values = round(as.numeric(values), 3)) %>% 
    mutate(table = "T1") %>%
    mutate(figure_code = "F26")
  
  return(extracted_t1_sheet)
}

# USAGE:
# extract_t1_precombined(extract_t1('../../../WHO_extract_excel/OneDrive_1_07.06.2021/Quantitative files/appendix tables/LVA_Appendix_tables_Aug 2020.xlsx'))


# T2
# TODO extract_t2_table8_precombined_N merge into one function: UPDATE: DONE!

extract_t2_table_n_precombined <- function(extract_t2_table_sheet, label_title, figure_code_name, table_label_val, quantile_val = NULL) {
  # extract_t2_table_sheet <- extract_table_four_t2(excel_file_path = extract_t2_table_sheet)
  extract_t2_table <- extract_t2_table_sheet$df
  
  extract_t2_table <- extract_t2_table %>% pivot_longer(cols = 2:length(extract_t2_table), names_to = "Year", values_to = "values") %>%
    mutate(values = round(as.numeric(values), 3)) %>% 
    mutate(table = label_title) %>%
    mutate(figure_code = figure_code_name) %>%
    mutate(table_label = table_label_val)
  
  if(!is.null(quantile_val)){
    extract_t2_table <- extract_t2_table %>%
      mutate(quintile = quantile_val)
  }
  
  
  return(extract_t2_table)
}

# extract_t2_table_n_precombined(extract_t2_table_sheet = '../../../../Downloads/LVA_Appendix_tables_Aug 2020_clean 03 Jun 2021.xlsx',
#                                label_title = 'T2 Table 4',
#                                figure_code_name = 'F8', 
#                                table_label_val = 'Share of OOP by structure (Poorest quintile)'
#                                )

# USAGE:
# extract_t2_table_n_precombined(
#   extract_table_eight_t2('../../../WHO_extract_excel/OneDrive_1_07.06.2021/Quantitative files/appendix tables/LVA_Appendix_tables_Aug 2020.xlsx'),
#   "T2 Table 8"
# )



# T3 fig.3
# extract_t3('../../../WHO_extract_excel/OneDrive_1_07.06.2021/Quantitative files/appendix tables/LVA_Appendix_tables_Aug 2020.xlsx')

extract_t3_precombined <- function(extracted_t3_sheet) {
  # extracted_t3_sheet <- extract_t3('../../../../Downloads/LVA_Appendix_tables_Aug 2020_clean 03 Jun 2021.xlsx')
  
  extracted_t3_sheet <- extracted_t3_sheet %>% pivot_longer(cols = 2, names_to = "table_label", values_to = "values") %>%
    mutate(values = round(as.numeric(values), 3)) %>% 
    mutate(table = "T3") %>%
    mutate(figure_code = "F3")
  
  return(extracted_t3_sheet)
}

# T3 fig.13
# extract_t3('../../../WHO_extract_excel/OneDrive_1_07.06.2021/Quantitative files/appendix tables/LVA_Appendix_tables_Aug 2020.xlsx')

extract_t3_fig13_precombined <- function(extracted_t3_sheet) {
  # extracted_t3_sheet <- extract_t3_fig13('../../../../Downloads/LVA_Appendix_tables_Aug 2020_clean 03 Jun 2021.xlsx')
  
  extracted_t3_sheet <- extracted_t3_sheet %>% pivot_longer(cols = 2:4, names_to = "table_label", values_to = "values") %>%
    mutate(values = round(as.numeric(values), 3)) %>% 
    mutate(table = "T3") %>%
    mutate(figure_code = "F13")
  
  return(extracted_t3_sheet)
}

# extract_t3_precombined(extract_t3('../../../WHO_extract_excel/OneDrive_1_07.06.2021/Quantitative files/appendix tables/LVA_Appendix_tables_Aug 2020.xlsx'))

# T4
extract_t4_precombined <- function(extracted_t4_sheet) {
  # extracted_t4_sheet <- extract_t4('../../../../Downloads/LVA_Appendix_tables_Aug 2020_clean 03 Jun 2021.xlsx')
  
  extracted_t4_sheet <- extracted_t4_sheet %>% pivot_longer(cols = 2, names_to = "table_label", values_to = "values") %>%
    mutate(values = round(as.numeric(values), 3)) %>% 
    mutate(table = "T4") %>%
    mutate(figure_code = "F14")
  
  return(extracted_t4_sheet)
}


# T5
# extract_t5('../../../WHO_extract_excel/OneDrive_1_07.06.2021/Quantitative files/appendix tables/LVA_Appendix_tables_Aug 2020.xlsx')

extract_t5_precombined <- function(extracted_t5_sheet) {
  # extracted_t5_sheet <- extract_t5('../../../../Downloads/LVA_Appendix_tables_Aug 2020_clean 03 Jun 2021.xlsx')
  
  extracted_t5_sheet <- extracted_t5_sheet %>% pivot_longer(cols = 3, names_to = "table_label", values_to = "values") %>%
    mutate(values = round(as.numeric(values), 3)) %>% 
    mutate(table = "T5") %>%
    mutate(figure_code = "F4")
  
  return(extracted_t5_sheet)
}

# extract_t5_precombined(
#   extract_t5('../../../WHO_extract_excel/OneDrive_1_07.06.2021/Quantitative files/appendix tables/LVA_Appendix_tables_Aug 2020.xlsx')
# )


# T6
# extract_t6('../../../WHO_extract_excel/OneDrive_1_07.06.2021/Quantitative files/appendix tables/LVA_Appendix_tables_Aug 2020.xlsx') 

extract_t6_precombined <- function(extracted_t6_sheet) {
  # extracted_t6_sheet <- extract_t6('../../../../Downloads/LVA_Appendix_tables_Aug 2020_clean 03 Jun 2021.xlsx')
  
  extracted_t6_sheet <- extracted_t6_sheet %>% pivot_longer(cols = 3, names_to = "table_label", values_to = "values") %>%
    mutate(values = round(as.numeric(values), 3)) %>% 
    mutate(table = "T6") %>%
    mutate(figure_code = "F16")
  
  return(extracted_t6_sheet)
}

# extract_t6_precombined(
#   extract_t6('../../../WHO_extract_excel/OneDrive_1_07.06.2021/Quantitative files/appendix tables/LVA_Appendix_tables_Aug 2020.xlsx')
# )

#T8
# extract_t8('../../../WHO_extract_excel/OneDrive_1_07.06.2021/Quantitative files/appendix tables/LVA_Appendix_tables_Aug 2020.xlsx') 

extract_t8_precombined <- function(extracted_t8_sheet) {
  # extracted_t8_sheet <- extract_t8('../../../../Downloads/LVA_Appendix_tables_Aug 2020_clean 03 Jun 2021.xlsx')
  
  extracted_t8_sheet <- extracted_t8_sheet %>% pivot_longer(cols = 3, names_to = "table_label", values_to = "values") %>%
    mutate(values = round(as.numeric(values), 3)) %>% 
    mutate(table = "T8") %>%
    mutate(figure_code = "F20")
  
  return(extracted_t8_sheet)
}

extract_t9_precombined <- function(extracted_t9_sheet) {
  # extracted_t9_sheet <- extract_t9('../../../../Downloads/LVA_Appendix_tables_Aug 2020_clean 03 Jun 2021.xlsx')
  
  extracted_t9_sheet <- extracted_t9_sheet %>% pivot_longer(cols = 4:9, names_to = "service", values_to = "values") %>%
    mutate(values = round(as.numeric(values), 3)) %>% 
    mutate(table = "T9") %>%
    mutate(figure_code = "F21")
  
  return(extracted_t9_sheet)
}

# extract_t8_precombined(
#   extract_t8('../../../WHO_extract_excel/OneDrive_1_07.06.2021/Quantitative files/appendix tables/LVA_Appendix_tables_Aug 2020.xlsx')
# )

# T10 
# extract_t10('../../../WHO_extract_excel/OneDrive_1_07.06.2021/Quantitative files/appendix tables/LVA_Appendix_tables_Aug 2020.xlsx') 

extract_t10_precombined <- function(extracted_t10_sheet) {
  # extracted_t10_sheet <- extract_t10('../../../../Downloads/LVA_Appendix_tables_Aug 2020_clean 03 Jun 2021.xlsx')
  
  extracted_t10_sheet <- extracted_t10_sheet %>% pivot_longer(cols = 2:5, names_to = "table_label", values_to = "values") %>%
    mutate(values = round(as.numeric(values), 3)) %>% 
    mutate(table = "T10") %>%
    mutate(figure_code = "F15")
  
  return(extracted_t10_sheet)
}

# extract_t10_precombined(
#   extract_t10('../../../WHO_extract_excel/OneDrive_1_07.06.2021/Quantitative files/appendix tables/LVA_Appendix_tables_Aug 2020.xlsx')
# )


# T13
# extract_t13('../../../WHO_extract_excel/OneDrive_1_07.06.2021/Quantitative files/appendix tables/LVA_Appendix_tables_Aug 2020.xlsx') 

extract_t13_precombined <- function(extracted_t13_sheet) {
  # extracted_t13_sheet <- extract_t13('../../../../Downloads/LVA_Appendix_tables_Aug 2020_clean 03 Jun 2021.xlsx')
  
  q_rep <- rep(c('Poorest', '2nd', '3rd', '4th', 'Richest'), length(unique(extracted_t13_sheet$Year)))
  
  extracted_t13_sheet <- extracted_t13_sheet %>%
    select(-3) %>% 
    mutate(quintile = q_rep) %>%
    mutate(figure_code = "F22") %>%
    mutate(table = "T13") %>%
    full_join(y = (
      extracted_t13_sheet %>%
        select(-2) %>% 
        mutate(figure_code = "F24") %>%
        mutate(table = "T13")
    ), by = c("Year" = "Year", "figure_code" = "figure_code", "table" = "table"))
  
  extracted_t13_sheet <- extracted_t13_sheet %>% pivot_longer(cols = c(2, 6), names_to = "table_label", values_to = "values") %>%
    mutate(values = round(as.numeric(values), 3)) %>% 
    drop_na(values) 
  
  return(extracted_t13_sheet)
}

# extract_t13_precombined(
#   extract_t13('../../../WHO_extract_excel/OneDrive_1_07.06.2021/Quantitative files/appendix tables/LVA_Appendix_tables_Aug 2020.xlsx')
# )
