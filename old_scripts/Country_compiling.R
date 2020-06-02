# Country compile ---------------------------------------------------------
source(here('scripts', 'data_cleaning_functions.R'))
source(here('Plot production.R'))
source(here('scripts', 'data_cleaning_v2.R'))


  # Meat production for all countries that do not have missing data ---------
a_meat <- Chicken_production_meat %>%
  select('Area', 'Year', 'Value') %>%
  rename('Country' = Area, '1000 Heads' = Value) %>%
  pivot_wider(id_cols = Year, names_from = Country, values_from = '1000 Heads') %>%
  select_if(~ !any(is.na(.))) %>%
  select(-Israel, -'Saint Pierre and Miquelon')


  # Meat production for all countries that had missing data -----------------
b_meat <- bind_rows(Final_former_czechoslovakia_meat, Final_former_Ethiopia_meat, Final_former_SM_meat, (filter(Final_former_YSFR_meat, Country != "Serbia and Montenegro")),
               Final_former_IsraelPalestine_meat, Final_former_Sudan_meat, Final_micronesia_meat, Final_former_USSR_meat, Final_former_BeneLux_meat) %>%
  pivot_wider(id_cols = Year, names_from = Country, values_from = '1000 Heads')


# Combining the original and cleaned datasets -----------------------------
Final_meat_wide <- left_join(a_meat, b_meat, by = 'Year')
Final_meat_long <- pivot_longer(Final_meat_wide, cols = 2:209) %>%
  rename('year' = Year, 'country' = name, 'heads' = value)

  # Tonnage production for all countries that do not have missing data ---------
a_tonnes <- Chicken_production_tonnes %>%
  select('Area', 'Year', 'Value') %>%
  rename('Country' = Area, 'Tonnes' = Value) %>%
  pivot_wider(id_cols = Year, names_from = Country, values_from = 'Tonnes') %>%
  select_if(~ !any(is.na(.))) %>%
  select(-Israel)


  # Meat production for all countries that had missing data -----------------
b_tonnes <- bind_rows(Final_former_czechoslovakia_tonnes, Final_former_Ethiopia_tonnes, Final_former_SM_tonnes, (filter(Final_former_YSFR_tonnes, Country != "Serbia and Montenegro")),
               Final_former_IsraelPalestine_tonnes, Final_former_Sudan_tonnes, Final_micronesia_tonnes, Final_former_USSR_tonnes, Final_former_BeneLux_tonnes) %>%
  pivot_wider(id_cols = Year, names_from = Country, values_from = 'Tonnes')

# Combining the original and cleaned datasets -----------------------------
Final_tonnes_wide <- left_join(a_tonnes, b_tonnes, by = 'Year')
Final_tonnes_long <- pivot_longer(Final_tonnes_wide, cols = 2:209) %>%
  rename('year' = Year, 'country' = name, 'tonnes' = value)

