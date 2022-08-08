library(gsheet)
library(furrr)
library(dplyr)
library(tidyverse)

readRDS('data/indicators_list.rds')
source('qt_data_scripts/currency_converter.R')
source('qt_data_scripts/precombined_list.R')

# xls_path <- "../../../Downloads/Background material DB 10 Feb 2022/Bulgaria/BUL_Appendix_tables.xlsx"

combine_data_by_country <- function(xls_path, file_name) {
  
precombined_figures_full_list <- precombined_figures_full_list_func(xls_path = xls_path)

indicators_tbl   <- gsheet::gsheet2tbl('https://docs.google.com/spreadsheets/d/1SeQpfenUnnqvES9EZO7jN2lA8NT_ZEQ1gY_FM1kl3kM/edit#gid=920678271')
dictionary       <- gsheet::gsheet2tbl('https://docs.google.com/spreadsheets/d/1SeQpfenUnnqvES9EZO7jN2lA8NT_ZEQ1gY_FM1kl3kM/edit#gid=50834238')
country_currency <- gsheet::gsheet2tbl('https://docs.google.com/spreadsheets/d/1SeQpfenUnnqvES9EZO7jN2lA8NT_ZEQ1gY_FM1kl3kM/edit#gid=2123433253')
value_type       <- gsheet::gsheet2tbl('https://docs.google.com/spreadsheets/d/1SeQpfenUnnqvES9EZO7jN2lA8NT_ZEQ1gY_FM1kl3kM/edit#gid=1738577030')

arranged_cleaned_figures_list <- list()

# extract_t1_precombined_val; F26
arranged_cleaned_figures_list$figure_26 <- precombined_figures_full_list$extract_t1_precombined_val %>%
  left_join(dictionary, by = c('figure_code' = 'figure_code', 'table_label' = 'table_label_extracted')) %>%
  select(-table_label) %>%
  rename(table_label = table_label_cleaned) %>%
  left_join(indicators_tbl, c('table_label' = 'table_label', 'figure_code' = 'figure_code')) %>%
  setNames(names(.) %>% str_to_lower())
  
# extract_t2_table_1_precombined_val; F5; By quintile and By total
arranged_cleaned_figures_list$figure_5 <- precombined_figures_full_list$extract_t2_table_1_precombined_val %>% 
  select(-table_label) %>%
  setNames(names(.) %>% str_to_lower()) %>%
  # left_join(indicators_tbl) %>% View() # demonstrate the issue; initially the value don't splited by service
  left_join(indicators_tbl %>% select(-service), by = c('figure_code' = 'figure_code', 'quintile' = 'quintile')) %>% 
  distinct() %>%
  mutate(service = NA)


# extract_t2_table_2_precombined_val; F9; treat as total - quantile column absent from dataset
arranged_cleaned_figures_list$figure_9 <- precombined_figures_full_list$extract_t2_table_2_precombined_val %>% 
  select(-table_label) %>%
  mutate(quintile = 'Total') %>%
  mutate(service = ifelse(service == 'Dental', 'Dental care', service)) %>% # create a function to check word difference
  left_join(indicators_tbl, 
            by = c('figure_code' = 'figure_code', 'quintile' = 'quintile', 'service' = 'service')
            ) %>%
  setNames(names(.) %>% str_to_lower())



# extract_t2_table_3_precombined_val; F7; treat as total - quantile column absent from dataset
arranged_cleaned_figures_list$figure_7 <- precombined_figures_full_list$extract_t2_table_3_precombined_val %>%
  select(-table_label) %>%
  mutate(quintile = 'Total') %>% # add quintile
  mutate(service = ifelse(service == 'Dental', 'Dental care', service)) %>% # create a function to check word difference
  left_join(indicators_tbl, 
            by = c('figure_code' = 'figure_code', 'quintile' = 'quintile', 'service' = 'service')
  ) %>%
  setNames(names(.) %>% str_to_lower())


# extract_t2_table_4_precombined_val; F8; 
arranged_cleaned_figures_list$figure_8_quintile <- precombined_figures_full_list$extract_t2_table_4_precombined_val %>% 
  select(-table_label) %>%
  # mutate(quintile = 'Total') %>% # add quintile
  mutate(service = ifelse(service == 'Dental', 'Dental care', service)) %>% # create a function to check word difference
  left_join(indicators_tbl, 
            by = c('figure_code' = 'figure_code', 'quintile' = 'quintile', 'service' = 'service')
  ) %>% 
union(
# extract_t2_table_5_precombined_val; F8; 
precombined_figures_full_list$extract_t2_table_5_precombined_val %>% 
  select(-table_label) %>%
  # mutate(quintile = 'Total') %>% # add quintile
  mutate(service = ifelse(service == 'Dental', 'Dental care', service)) %>% # create a function to check word difference
  left_join(indicators_tbl, 
            by = c('figure_code' = 'figure_code', 'quintile' = 'quintile', 'service' = 'service')
  )
) %>%
union(
# extract_t2_table_6_precombined_val; F8; 
precombined_figures_full_list$extract_t2_table_6_precombined_val %>% 
  select(-table_label) %>%
  # mutate(quintile = 'Total') %>% # add quintile
  mutate(service = ifelse(service == 'Dental', 'Dental care', service)) %>% # create a function to check word difference
  left_join(indicators_tbl, 
            by = c('figure_code' = 'figure_code', 'quintile' = 'quintile', 'service' = 'service')
  )
) %>%
union(
# extract_t2_table_7_precombined_val; F8; 
precombined_figures_full_list$extract_t2_table_7_precombined_val %>% 
  select(-table_label) %>%
  # mutate(quintile = 'Total') %>% # add quintile
  mutate(service = ifelse(service == 'Dental', 'Dental care', service)) %>% # create a function to check word difference
  left_join(indicators_tbl, 
            by = c('figure_code' = 'figure_code', 'quintile' = 'quintile', 'service' = 'service')
  )
) %>%
union(
# extract_t2_table_8_precombined_val; F8; 
precombined_figures_full_list$extract_t2_table_8_precombined_val %>% 
  select(-table_label) %>%
  # mutate(quintile = 'Total') %>% # add quintile
  mutate(service = ifelse(service == 'Dental', 'Dental care', service)) %>% # create a function to check word difference
  left_join(indicators_tbl, 
            by = c('figure_code' = 'figure_code', 'quintile' = 'quintile', 'service' = 'service')
  ) 
) %>%
  setNames(names(.) %>% str_to_lower())



# extract_t3_precombined_val; F3; F3_new  
arranged_cleaned_figures_list$figure_3_3_new <- precombined_figures_full_list$extract_t3_precombined_val %>%
  left_join(dictionary, by = c('figure_code' = 'figure_code', 'table_label' = 'table_label_extracted')) %>%
  select(-table_label) %>%
  rename(table_label = table_label_cleaned) %>%
  left_join(indicators_tbl, c('table_label' = 'table_label', 'figure_code' = 'figure_code')) %>%
  setNames(names(.) %>% str_to_lower())
  # MISSED!

# extract_t3_fig13_precombined_val; F13
arranged_cleaned_figures_list$figure_13 <- precombined_figures_full_list$extract_t3_fig13_precombined_val %>%
  left_join(dictionary, by = c('figure_code' = 'figure_code', 'table_label' = 'table_label_extracted')) %>%
  select(-table_label) %>%
  rename(table_label = table_label_cleaned) %>%
  left_join(indicators_tbl, c('table_label' = 'table_label', 'figure_code' = 'figure_code')) %>%
  setNames(names(.) %>% str_to_lower()) %>%
  # atrisk_impov_all indicator id multiply by 100 to convert into percent
  mutate(values = ifelse(measure_code_id == 'atrisk_impov_all', values*100, values))


# extract_t4_precombined_val; F14; Treated as total; Without services
arranged_cleaned_figures_list$figure_14 <- precombined_figures_full_list$extract_t4_precombined_val %>%
  left_join(dictionary, by = c('figure_code' = 'figure_code', 'table_label' = 'table_label_extracted')) %>%
  select(-table_label) %>%
  setNames(names(.) %>% str_to_lower()) %>%
  rename(table_label = table_label_cleaned) %>%
  mutate(quintile = 'Total') %>% # add quintile
  left_join(indicators_tbl %>% select(-service), c('table_label' = 'applicable_figure', 'figure_code' = 'figure_code', 'quintile' = 'quintile')) %>%
  rename(
    applicable_figure = table_label,
    table_label = table_label.y
  ) %>%
  distinct() %>%
  mutate(service = NA)


# extract_t5_precombined_val; F4; F4_new
arranged_cleaned_figures_list$figure_4_4new <- precombined_figures_full_list$extract_t5_precombined_val %>%
  left_join(dictionary, by = c('figure_code' = 'figure_code', 'table_label' = 'table_label_extracted')) %>%
    select(-table_label) %>%
    rename(table_label = table_label_cleaned) %>%
    left_join(indicators_tbl, c('table_label' = 'table_label', 'figure_code' = 'figure_code', 'Income Quintile' = 'quintile')) %>%
    rename(
      quintile = `Income Quintile`
    ) %>%
    distinct() %>%
    setNames(names(.) %>% str_to_lower())

# extract_t6_precombined_val; F16; Missed service
arranged_cleaned_figures_list$figure_16_quintile <- precombined_figures_full_list$extract_t6_precombined_val %>% 
  rename(
    quintile = `Income Quintile`
  ) %>%
  setNames(names(.) %>% str_to_lower()) %>%
  left_join(dictionary, by = c('figure_code' = 'figure_code', 'table_label' = 'table_label_extracted')) %>%
  select(-table_label) %>%
  rename(applicable_figure = table_label_cleaned) %>%
  left_join(indicators_tbl %>% select(-service), c('applicable_figure' = 'applicable_figure', 'figure_code' = 'figure_code', 'quintile' = 'quintile')) %>% 
  distinct() %>%
  mutate(service = NA) %>%
  mutate(values = values * 0.2)


# extract_t8_precombined_val; F20; Treated as total
arranged_cleaned_figures_list$figure_20 <- precombined_figures_full_list$extract_t8_precombined_val %>%
  left_join(dictionary, by = c('figure_code' = 'figure_code', 'table_label' = 'table_label_extracted')) %>%
  select(-table_label) %>%
  rename(applicable_figure = table_label_cleaned) %>%
  setNames(str_to_lower(names(.))) %>%
  mutate(quintile = 'Total') %>%
  mutate(service = ifelse(service == 'Dental', 'Dental care', service)) %>% # create a function to check word difference
  left_join(indicators_tbl, c('applicable_figure' = 'applicable_figure', 'figure_code' = 'figure_code', 'service' = 'service', 'quintile' = 'quintile')) %>% 
  distinct() 

  
# extract_t9_precombined_val; F21; by quintile
arranged_cleaned_figures_list$figure_21_quintile <- precombined_figures_full_list$extract_t9_precombined_val  %>% 
  mutate(service = ifelse(service == 'Dental', 'Dental care', service)) %>%
  left_join(dictionary, by = c('figure_code' = 'figure_code', 'table_label' = 'table_label_extracted')) %>%
  select(-table_label) %>%
  rename(applicable_figure = table_label_cleaned) %>% 
  left_join(indicators_tbl, c('applicable_figure' = 'applicable_figure', 'figure_code' = 'figure_code', 'service' = 'service', 'quintile' = 'quintile')) 

  

# extract_t10_precombined_val; F15; Treated as total; 
arranged_cleaned_figures_list$figure_15 <- precombined_figures_full_list$extract_t10_precombined_val %>%
  mutate(quintile = 'Total') %>%
  setNames(names(.) %>% str_to_lower()) %>% 
  left_join(dictionary, by = c('figure_code' = 'figure_code', 'table_label' = 'table_label_extracted')) %>%
  select(-table_label) %>%
  rename(table_label = table_label_cleaned) %>%
  left_join(indicators_tbl, c('table_label' = 'table_label', 'figure_code' = 'figure_code', 'quintile' = 'quintile')) %>%
  mutate(year = as.character(year)) %>%
  mutate(values = 100 * values)


# extract_t12_precombined_val; F6; 
arranged_cleaned_figures_list$figure_6 <- precombined_figures_full_list$extract_t12_precombined_val %>% 
  left_join(indicators_tbl, c('table_label' = 'table_label', 'figure_code' = 'figure_code', 'quintile' = 'quintile')) %>%
  setNames(names(.) %>% str_to_lower()) %>%
  mutate(year = as.character(year))


# extract_t13_precombined_val; F22/F24; 
arranged_cleaned_figures_list$figure_22_24 <- precombined_figures_full_list$extract_t13_precombined_val %>% 
  mutate(quintile = replace_na(quintile, 'Total')) %>%
  left_join(dictionary, by = c('figure_code' = 'figure_code', 'table_label' = 'table_label_extracted')) %>%
  select(-table_label) %>%
  rename(table_label = table_label_cleaned) %>% 
  left_join(indicators_tbl, c('table_label' = 'table_label', 'figure_code' = 'figure_code', 'quintile' = 'quintile')) %>%
  setNames(names(.) %>% str_to_lower()) %>%
  mutate(year = as.character(year))


# F10abcdef - by quintiles
arranged_cleaned_figures_list$figure_10abcdef_quintile <- figs10n_func(xls_path = xls_path) %>%
  mutate(service = ifelse(service == 'Dental', 'Dental care', service)) %>%
  setNames(names(.) %>% str_to_lower()) %>%
  left_join(indicators_tbl, c('figure_code' = 'figure_code', 'quintile' = 'quintile', 'service' = 'service')) 

  

### Joint table
joint_table <- arranged_cleaned_figures_list[[1]]

final <- furrr::future_map(2:length(names(arranged_cleaned_figures_list)), function(x){
  joint_table <<- joint_table %>% union_all(arranged_cleaned_figures_list[[x]])
}) 

# final[[15]] %>% write_csv('../../../Desktop/figures/merged_extracted_new_figures.csv')

currency_name <- country_currency[country_currency$country_code == substr(file_name, start = 1, stop = 3), ]$currency

merged_data <- final[[15]] %>%
  mutate(country_code = substr(file_name, start = 1, stop = 3)) %>%
  select(country_code, everything()) %>%
  mutate(year = as.character(as.numeric(year, digits = 0))) %>%
  rowwise() %>%
  mutate(
    real_value = ifelse(
      figure_code %in% c('F5', 'F9', 'F10a', 'F10b', 'F10c', 'F10d', 'F10e', 'F10f', 'F26'),
      currency_converter_script(values, country_code, year),
      NA
    ),
    conversion_year = ifelse(
      figure_code %in% c('F5', 'F9', 'F10a', 'F10b', 'F10c', 'F10d', 'F10e', 'F10f', 'F26'),
      '2020',
      NA
    ),
    currency = ifelse(
      figure_code %in% c('F5', 'F9', 'F10a', 'F10b', 'F10c', 'F10d', 'F10e', 'F10f', 'F26'),
      currency_name,
      NA
    ),
    real_value = ifelse(
      measure_code_id == 'poverty_line',
      NA,
      real_value
    )
  ) %>% 
  left_join(
    value_type, by = c('measure_code_id' = 'indicator_id') 
  ) %>%
  select(
    measure_code_id,
    table_label,
    country_code,
    year,
    quintile,
    service,
    values,
    real_value,
    currency,
    conversion_year,
    category,
    type,
    table,
    figure_code
  ) %>%
  rename(
    indicator_id = measure_code_id,
    country = country_code,
    value = values,
    indicator_name = table_label,
    table_id = table,
    figure_id = figure_code,
    value_type = type
  ) 


return(merged_data)

}

