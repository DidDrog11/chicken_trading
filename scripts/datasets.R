library('janitor')
library("here")

source(here('scripts', 'geographic_regions.R'))


production_heads_wide <- Production_meat_labelled %>%
  pivot_wider(id_cols = year, values_from = heads, names_from = c(country)) %>%
  janitor::clean_names()
  
write_csv(production_heads_wide, 'cleaned_data/production_heads_wide.csv')

production_tonnes_wide <- Production_tonnes_labelled %>%
  pivot_wider(id_cols = year, values_from = tonnes, names_from = c(country)) %>%
  janitor::clean_names()

write_csv(production_tonnes_wide, 'cleaned_data/production_tonnes_wide.csv')

production_heads_long <- Production_meat_labelled %>%
  janitor::clean_names() %>%
  mutate_all(.funs = tolower) %>%
  mutate(country = str_replace_all(country, " ", "_")) %>%
  mutate(country = str_replace_all(country, "[^_[:^punct:]]", "")) %>%
  mutate(country = recode(country, 'côte_divoire' = 'cote_divoire')) %>%
  mutate(region_name = str_replace_all(region_name, " ", "_")) %>%
  mutate(sub_region_name = str_replace_all(sub_region_name, " ", "_"))

write_csv(production_heads_long, 'cleaned_data/production_heads_long.csv')


production_tonnes_long <- Production_tonnes_labelled %>%
  janitor::clean_names() %>%
  mutate_all(.funs = tolower) %>%
  mutate(country = str_replace_all(country, " ", "_")) %>%
  mutate(country = str_replace_all(country, "[^_[:^punct:]]", "")) %>%
  mutate(country = recode(country, 'côte_divoire' = 'cote_divoire')) %>%
  mutate(region_name = str_replace_all(region_name, " ", "_")) %>%
  mutate(sub_region_name = str_replace_all(sub_region_name, " ", "_"))

write_csv(production_tonnes_long, 'cleaned_data/production_tonnes_long.csv')
