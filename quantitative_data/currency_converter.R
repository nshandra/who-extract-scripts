### Currency converter
data_for_calculation_currency_tbl <- gsheet::gsheet2tbl('https://docs.google.com/spreadsheets/d/1SeQpfenUnnqvES9EZO7jN2lA8NT_ZEQ1gY_FM1kl3kM/edit#gid=56805701')

data_for_calculation_currency_tbl <- data_for_calculation_currency_tbl %>% 
  mutate(
    across(.cols = everything(), as.character)
  ) %>%
  pivot_longer(!c(country_name, code), names_to = 'year') %>% 
  mutate(value = as.numeric(str_replace(value, pattern = ',', '\\.'), digits = 7))


currency_converter <- function(amount, country, selected_year) {
  
  coefficient <- data_for_calculation_currency_tbl %>%
    filter(country_name == country) %>%
    filter(year %in% selected_year) %>% 
    pull(value)
  
  adjusting_for_inflation <- round(amount / coefficient, digits = 2)
  
  return(adjusting_for_inflation)
  
}

currency_converter_script <- function(amount, country_code, selected_year) {
  
  coefficient <- data_for_calculation_currency_tbl %>%
    filter(code == country_code) %>%
    filter(year %in% selected_year) %>%
    pull(value)
  
  adjusting_for_inflation <- round((amount / coefficient), digits = 2)
  
  return(adjusting_for_inflation)
  
}