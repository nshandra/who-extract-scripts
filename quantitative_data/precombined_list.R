library(tidyverse)
library(dplyr)
library(readxl)

source("qt_data_scripts/precombined_data.R")

# xls_path <- '../../../../Downloads/LVA_Appendix_tables_Aug 2020_clean 03 Jun 2021.xlsx'

precombined_figures_full_list_func <- function(xls_path){
  precombined_figures <- list(
    extract_t1_precombined_val = extract_t1_precombined(
                                    extract_t1(xls_path, save_csv_path = NULL)
                                  ),
    extract_t2_table_1_precombined_val = extract_t2_table_n_precombined(
                                            extract_t2_table_sheet = extract_table_one_t2(xls_path, save_csv_path = NULL),
                                            label_title = "T2 Table 1",
                                            figure_code_name = "F5",
                                            table_label_val = "Mean annual per capita OOP by quintile"
                                          ),
    extract_t2_table_2_precombined_val = extract_t2_table_n_precombined(
                                            extract_t2_table_sheet = extract_table_two_t2(xls_path, save_csv_path = NULL),
                                            label_title = "T2 Table 2",
                                            figure_code_name = "F9",
                                            table_label_val = "Mean annual per capita OOP by structure"
                                          ),
    extract_t2_table_3_precombined_val = extract_t2_table_n_precombined(
                                            extract_t2_table_sheet = extract_table_three_t2(xls_path, save_csv_path = NULL),
                                            label_title = "T2 Table 3",
                                            figure_code_name = "F7",
                                            table_label_val = "Share of total OOP by structure (total population)"
                                          ), 
    extract_t2_table_4_precombined_val = extract_t2_table_n_precombined(
                                            extract_t2_table_sheet = extract_table_four_t2(xls_path, save_csv_path = NULL),
                                            label_title = "T2 Table 4",
                                            figure_code_name = "F8",
                                            table_label_val = "Share of OOP by structure (Poorest quintile)",
                                            quantile_val = "Poorest"
                                          ),
    extract_t2_table_5_precombined_val = extract_t2_table_n_precombined(
                                            extract_t2_table_sheet = extract_table_five_t2(xls_path, save_csv_path = NULL),
                                            label_title = "T2 Table 5",
                                            figure_code_name = "F8",
                                            table_label_val = "Share of OOP by structure (2nd quintile)",
                                            quantile_val = "2nd"
                                          ),
    extract_t2_table_6_precombined_val = extract_t2_table_n_precombined(
                                            extract_table_six_t2(xls_path, save_csv_path = NULL),
                                            "T2 Table 6",
                                            figure_code_name = "F8",
                                            table_label_val = "Share of OOP spending by structure (3rd quintile)",
                                            quantile_val = "3rd"
                                          ),
    extract_t2_table_7_precombined_val = extract_t2_table_n_precombined(
                                            extract_table_seven_t2(xls_path, save_csv_path = NULL),
                                            "T2 Table 7",
                                            figure_code_name = "F8",
                                            table_label_val = "Share of OOP by structure (4th quintile)",
                                            quantile_val = "4th"
                                          ),
    extract_t2_table_8_precombined_val = extract_t2_table_n_precombined(
                                            extract_table_eight_t2(xls_path, save_csv_path = NULL),
                                            "T2 Table 8",
                                            figure_code_name = "F8",
                                            table_label_val = "Share of OOP by structure (Richest quintile)",
                                            quantile_val = "Richest"
                                          ),
    extract_t3_precombined_val = extract_t3_precombined(
                                    extract_t3(xls_path, save_csv_path = NULL)
                                  ),
    extract_t3_fig13_precombined_val = extract_t3_fig13_precombined(
                                        extract_t3_fig13(xls_path, save_csv_path = NULL)
    ),
    extract_t4_precombined_val = extract_t4_precombined(
                                    extract_t4(xls_path, save_csv_path = NULL)
    ),
    extract_t5_precombined_val = extract_t5_precombined(
                                    extract_t5(xls_path, save_csv_path = NULL)
                                  ),
    extract_t6_precombined_val = extract_t6_precombined(
                                    extract_t6(xls_path, save_csv_path = NULL)
                                  ),
    extract_t8_precombined_val = extract_t8_precombined(
                                    extract_t8(xls_path, save_csv_path = NULL)
                                  ),
    extract_t9_precombined_val = extract_t9_precombined(
                                     extract_t9(xls_path, save_csv_path = NULL)
    ),
    extract_t10_precombined_val = extract_t10_precombined(
                                      extract_t10(xls_path, save_csv_path = NULL)
                                    ),
    extract_t13_precombined_val = extract_t13_precombined(
                                      extract_t13(xls_path, save_csv_path = NULL)
                                    )
  
  )
  
  return(precombined_figures)
  
}


precombined_table2_list_func <- function(xls_path){
  precombined_tables_t2 <- list(
    extract_t2_table_1_precombined_val = extract_t2_table_n_precombined(
      extract_t2_table_sheet = extract_table_one_t2(xls_path, save_csv_path = NULL),
      label_title = "T2 Table 1",
      figure_code_name = "F5",
      table_label_val = "Mean annual per capita OOP by quintile"
    ),
    extract_t2_table_4_precombined_val = extract_t2_table_n_precombined(
      extract_t2_table_sheet = extract_table_four_t2(xls_path, save_csv_path = NULL),
      label_title = "T2 Table 4",
      figure_code_name = "F8",
      table_label_val = "Share of OOP by structure (Poorest quintile)",
      quantile_val = "Poorest"
    ),
    extract_t2_table_5_precombined_val = extract_t2_table_n_precombined(
      extract_t2_table_sheet = extract_table_five_t2(xls_path, save_csv_path = NULL),
      label_title = "T2 Table 5",
      figure_code_name = "F8",
      table_label_val = "Share of OOP by structure (2nd quintile)",
      quantile_val = "2nd"
    ),
    extract_t2_table_6_precombined_val = extract_t2_table_n_precombined(
      extract_table_six_t2(xls_path, save_csv_path = NULL),
      "T2 Table 6",
      figure_code_name = "F8",
      table_label_val = "Share of OOP spending by structure (3rd quintile)",
      quantile_val = "3rd"
    ),
    extract_t2_table_7_precombined_val = extract_t2_table_n_precombined(
      extract_table_seven_t2(xls_path, save_csv_path = NULL),
      "T2 Table 7",
      figure_code_name = "F8",
      table_label_val = "Share of OOP by structure (4th quintile)",
      quantile_val = "4th"
    ),
    extract_t2_table_8_precombined_val = extract_t2_table_n_precombined(
      extract_table_eight_t2(xls_path, save_csv_path = NULL),
      "T2 Table 8",
      figure_code_name = "F8",
      table_label_val = "Share of OOP by structure (Richest quintile)",
      quantile_val = "Richest"
    )
  )
  
  return(precombined_tables_t2)
}


figs10n_func <- function(xls_path) {
# '../../../WHO_extract_excel/appendix tables/POL_Appendix_tables_Oct 2016_clean 02 Jun 2021.xlsx'

t2_aplph <- precombined_table2_list_func(xls_path)

  figs_alpabet_comb <- t2_aplph$extract_t2_table_4_precombined_val %>%
                          full_join(t2_aplph$extract_t2_table_5_precombined_val) %>%
                          full_join(t2_aplph$extract_t2_table_6_precombined_val) %>%
                          full_join(t2_aplph$extract_t2_table_7_precombined_val) %>%
                          full_join(t2_aplph$extract_t2_table_8_precombined_val)
  
  t2t1_const <- t2_aplph$extract_t2_table_1_precombined_val %>%
    select(1:3) %>%
    rename(
      `constant` = values
    )
  
  figs10n <- figs_alpabet_comb %>%
    select(service, Year, values, quintile) %>%
    left_join(t2t1_const, by = c('quintile' = 'quintile', 'Year' = 'Year')) %>%
      mutate(mult_val = values * constant) %>%
      mutate(figure_code = case_when(
        service == 'Medicines' ~ 'F10a',
        service == 'Inpatient care' ~ 'F10b',
        service == 'Dental' ~ 'F10c',
        service == 'Outpatient care' ~ 'F10d',
        service == 'Diagnostic tests' ~ 'F10e',
        service == 'Medical products' ~ 'F10f',
        TRUE ~ 'NA'
      )) %>% 
    select(-c(values, constant)) %>%
    mutate(table = 'T2') %>%
    rename(
      values = mult_val
    )
  
  return(figs10n)

}
