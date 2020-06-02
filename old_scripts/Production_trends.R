source('Plot production.R')
source('Data_cleaning_functions.R')
source('Data_cleaning_v2.R')
source('Country_compiling.R')

# Graphs to explore production trends -------------------------------------
year <- 2018
Production_meat_2018 <- Final_meat_wide %>%
  filter(., Year == year) %>%
  pivot_longer(., cols = 2:209) %>%
  rename('Country' = 2, '1000 Heads' = 3)

Production_meat <- Final_meat_wide %>%
  pivot_longer(., cols = 2:209) %>%
  rename('Country' = 2, '1000 Heads' = 3)

Production_tonnes_2018 <- Final_tonnes_wide %>%
  filter(., Year == year) %>%
  pivot_longer(., cols = 2:209) %>%
  rename('Country' = 2, 'Tonnes' = 3)

Production_tonnes <- Final_tonnes_wide %>%
  pivot_longer(., cols = 2:209) %>%
  rename('Country' = 2, 'Tonnes' = 3)

  # Top 10 producers --------------------------------------------------------
Top_10_meat <- Production_meat_2018 %>%
  arrange(desc(`1000 Heads`)) %>%
  filter(Country != 'China, mainland') %>%
  top_n(10)

Top_10_meat <- Production_meat %>%
  filter(Country %in% Top_10_production$Country) %>%
  group_by(Country, Year)

Top_10_tonnes <- Production_tonnes_2018 %>%
  arrange(desc(`Tonnes`)) %>%
  filter(Country != 'China, mainland') %>%
  top_n(10)

Top_10_tonnes <- Production_tonnes %>%
  filter(Country %in% Top_10_production$Country) %>%
  group_by(Country, Year)


Plot_meat(Top_10_meat, 'Annual production (1000 heads) over time for the top 10 (2018) global chicken producers')
Plot_tonnes(Top_10_tonnes, 'Annual production (tonnes) over time for the top 10 (2018) global chicken producers')


  # Next 10 producers -------------------------------------------------------
Next_11_20_meat <- Production_meat_2018 %>%
  arrange(desc(`1000 Heads`)) %>%
  slice(11:20)

Next_11_20_meat <- Production_meat %>%
  filter(Country %in% Next_11_20_meat$Country) %>%
  group_by(Country, Year)

Next_11_20_tonnes <- Production_tonnes_2018 %>%
  arrange(desc(Tonnes)) %>%
  slice(11:20)

Next_11_20_tonnes <- Production_tonnes %>%
  filter(Country %in% Next_11_20_tonnes$Country) %>%
  group_by(Country, Year)


Plot_meat(Next_11_20_meat, 'Annual production (1000 heads) over time for the 11-20th ranked (2018) global chicken producers')
Plot_tonnes(Next_11_20_tonnes, 'Annual production (tonnes) over time for the 11-20th ranked (2018) global chicken producers')

  # Mid producers -----------------------------------------------------------
Mid_producers_meat <-  Production_meat_2018 %>%
  arrange(desc(`1000 Heads`)) %>%
  slice(99:108)

Mid_producers_meat <- Production_meat %>%
  filter(Country %in% Mid_producers_meat$Country) %>%
  group_by(Country, Year)

Mid_producers_tonnes <-  Production_tonnes_2018 %>%
  arrange(desc(Tonnes)) %>%
  slice(99:108)

Mid_producers_tonnes <- Production_tonnes %>%
  filter(Country %in% Mid_producers_tonnes$Country) %>%
  group_by(Country, Year)

Plot_meat(Mid_producers_meat, 'Annual production (1000 heads) over time for the mid ranked (2018) global chicken producers')
Plot_tonnes(Mid_producers_tonnes, 'Annual production (tonnes) over time for the mid ranked (2018) global chicken producers')


  # Bottom 20-10 producers --------------------------------------------------
Low_20_11_meat <-  Production_meat_2018 %>%
  arrange(desc(`1000 Heads`)) %>%
  slice(190:199)

Low_20_11_meat <- Production_meat %>%
  filter(Country %in% Low_20_11_meat$Country) %>%
  group_by(Country, Year)

Low_20_11_tonnes <-  Production_tonnes_2018 %>%
  arrange(desc(Tonnes)) %>%
  slice(190:199)

Low_20_11_tonnes <- Production_tonnes %>%
  filter(Country %in% Low_20_11_tonnes$Country) %>%
  group_by(Country, Year)

Plot_meat(Low_20_11_meat, 'Annual production (1000 heads) over time for the 190th-199th ranked (2018) global chicken producers')
Plot_tonnes(Low_20_11_tonnes, 'Annual production (tonnes) over time for the 190th-199th ranked (2018) global chicken producers')

  # Bottom 10 producers --------------------------------------------------
Low_10_meat <-  Production_meat_2018 %>%
  arrange(desc(`1000 Heads`)) %>%
  slice(200:209)

Low_10_meat <- Production_meat %>%
  filter(Country %in% Low_10_meat$Country) %>%
  group_by(Country, Year)

Low_10_tonnes <-  Production_tonnes_2018 %>%
  arrange(desc(Tonnes)) %>%
  slice(200:209)

Low_10_tonnes <- Production_tonnes %>%
  filter(Country %in% Low_10_tonnes$Country) %>%
  group_by(Country, Year)

Plot_meat(Low_10_meat, 'Annual production (1000 heads) over time for the lowest 10 ranked (2018) global chicken producers')
Plot_tonnes(Low_10_tonnes, 'Annual production (tonnes) over time for the lowest 10 ranked (2018) global chicken producers')