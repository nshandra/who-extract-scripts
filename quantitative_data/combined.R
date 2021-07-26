library(tidyverse)
library(dplyr)

source('qt_data_scripts/precombined_list.R')


combine_data_by_country <- function(xls_path, file_name) {
  
# precombined_figures_full_list <- precombined_figures_full_list_func('../../../../../../Downloads/LVA_Appendix_tables_Aug 2020_clean 03 Jun 2021.xlsx')
precombined_figures_full_list <- precombined_figures_full_list_func(xls_path = xls_path)
indicators_df <- readRDS(file = 'data/indicators_list.rds')

# Initialization 
t2 <- precombined_figures_full_list$extract_t2_table_1_precombined_val %>%
        full_join(precombined_figures_full_list$extract_t2_table_2_precombined_val, by = c('Year' = 'Year', 'values' = 'values', 'table' = 'table', 'table_label' = 'table_label', 'figure_code' = 'figure_code')) %>% 
        full_join(precombined_figures_full_list$extract_t2_table_3_precombined_val, by = c('Year' = 'Year', 'values' = 'values', 'table' = 'table', 'table_label' = 'table_label', 'figure_code' = 'figure_code', 'service' = 'service')) %>% 
        full_join(precombined_figures_full_list$extract_t2_table_4_precombined_val, by = c('Year' = 'Year', 'values' = 'values', 'table' = 'table', 'table_label' = 'table_label', 'figure_code' = 'figure_code', 'service' = 'service', 'quintile' = 'quintile')) %>% 
        full_join(precombined_figures_full_list$extract_t2_table_5_precombined_val, by = c('Year' = 'Year', 'values' = 'values', 'table' = 'table', 'table_label' = 'table_label', 'figure_code' = 'figure_code', 'service' = 'service', 'quintile' = 'quintile')) %>% 
        full_join(precombined_figures_full_list$extract_t2_table_6_precombined_val, by = c('Year' = 'Year', 'values' = 'values', 'table' = 'table', 'table_label' = 'table_label', 'figure_code' = 'figure_code', 'service' = 'service', 'quintile' = 'quintile')) %>% 
        full_join(precombined_figures_full_list$extract_t2_table_7_precombined_val, by = c('Year' = 'Year', 'values' = 'values', 'table' = 'table', 'table_label' = 'table_label', 'figure_code' = 'figure_code', 'service' = 'service', 'quintile' = 'quintile')) %>% 
        full_join(precombined_figures_full_list$extract_t2_table_8_precombined_val, by = c('Year' = 'Year', 'values' = 'values', 'table' = 'table', 'table_label' = 'table_label', 'figure_code' = 'figure_code', 'service' = 'service', 'quintile' = 'quintile'))
t13 <- precombined_figures_full_list$extract_t13_precombined_val %>%
  drop_na(values) %>%
  mutate(Year = as.character(Year))

t3 <- precombined_figures_full_list$extract_t3_precombined_val %>%
  full_join(precombined_figures_full_list$extract_t3_fig13_precombined_val, by = c('Year' = 'Year', 'values' = 'values', 'table' = 'table', 'table_label' = 'table_label', 'figure_code' = 'figure_code'))

t10 <- precombined_figures_full_list$extract_t10_precombined_val %>% mutate(Year = as.character(Year))
figs10abcdef <- figs10n_func(xls_path = xls_path)


precombined_figures_full_list$extract_t4_precombined_val
precombined_figures_full_list$extract_t9_precombined_val

# Create a merged data frame
merged_data <- precombined_figures_full_list$extract_t1_precombined_val %>%
    full_join(t2, by = c('Year' = 'Year', 
                         'values' = 'values', 
                         'table' = 'table', 
                         'figure_code' = 'figure_code', 
                         'table_label' = 'table_label')) %>%
    full_join(t3, by = c('Year' = 'Year', 
                         'values' = 'values', 
                         'table' = 'table', 
                         'table_label' = 'table_label',
                         'figure_code' = 'figure_code')) %>% 
    full_join(precombined_figures_full_list$extract_t4_precombined_val, by = c('Year' = 'Year', 
                                                                               'values' = 'values', 
                                                                               'table' = 'table', 
                                                                               'table_label' = 'table_label', 
                                                                               'figure_code' = 'figure_code')) %>%
    full_join(precombined_figures_full_list$extract_t5_precombined_val, by = c('Year' = 'Year', 
                                                                               'values' = 'values', 
                                                                               'table' = 'table', 
                                                                               'table_label' = 'table_label', 
                                                                               'quintile' = 'Income Quintile',
                                                                               'figure_code' = 'figure_code')) %>%
    full_join(precombined_figures_full_list$extract_t6_precombined_val, by = c('Year' = 'Year', 
                                                                               'values' = 'values', 
                                                                               'table' = 'table', 
                                                                               'table_label' = 'table_label', 
                                                                               'quintile' = 'Income Quintile',
                                                                               'figure_code' = 'figure_code')) %>%
    full_join(precombined_figures_full_list$extract_t8_precombined_val, by = c('Year' = 'Year', 
                                                                               'values' = 'values', 
                                                                               'table' = 'table', 
                                                                               'table_label' = 'table_label', 
                                                                               'service' = 'Service',
                                                                               'figure_code' = 'figure_code')) %>%
    full_join(precombined_figures_full_list$extract_t9_precombined_val, by = c('Year' = 'year', 
                                                                               'values' = 'values', 
                                                                               'table' = 'table', 
                                                                               'table_label' = 'table_label', 
                                                                               'service' = 'service',
                                                                               'figure_code' = 'figure_code',
                                                                               'quintile' = 'quintile')) %>%
    full_join(t10, by = c('Year' = 'Year', 
                          'values' = 'values', 
                          'table' = 'table', 
                          'table_label' = 'table_label',
                          'figure_code' = 'figure_code')) %>%
    full_join(t13, by = c('Year' = 'Year', 
                          'values' = 'values', 
                          'table' = 'table', 
                          'table_label' = 'table_label',
                          'quintile' = 'quintile',
                          'figure_code' = 'figure_code')) %>%
    select(Year, quintile, table_label, service, values, table, figure_code) %>%
    mutate(country = substr(file_name, start = 1, stop = 3)) %>%
    mutate(real_value = NA) %>%
    mutate(currency = NA) %>%
    mutate(conv_year = NA) %>%
    full_join(figs10abcdef, by = c('Year' = 'Year', 'quintile' = 'quintile', 'service' = 'service', 'values' = 'values', 'figure_code' = 'figure_code', 'table' = 'table')) %>%
    left_join(indicators_df, by = c('figure_code' = 'figure_code')) %>%
    select(country, Year, quintile, indicator, table_label, service, values, table, figure_code, real_value, currency, conv_year) %>%
    mutate(values = round(values, 2)) 

return(merged_data)

}
