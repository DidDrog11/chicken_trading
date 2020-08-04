# By geographic region ----------------------------------------------------
#source(here('Plot production.R'))
source(here('scripts', 'data_cleaning_functions.R'))
source(here('scripts', 'data_cleaning_v2.R'))
#source(here('Country_compiling.R'))
library('maptools')

data("wrld_simpl")
column_names <- c('fips', 'iso2', 'iso3', 'un', 'country', 'area', 'pop2005', 'region', 'subregion', 'longitude', 'latitude')
world_data <- wrld_simpl@data
colnames(world_data) <- c(column_names)
world_data <- world_data %>%
  select('country','area', 'region', 'subregion', 'longitude', 'latitude') %>%
  ## Matching country names between the maptools data and chicken data
  mutate(country = recode(country, 'Bolivia' = 'Bolivia (Plurinational State of)',
                          'Cape Verde' = 'Cabo Verde',
                          'Hong Kong' = 'China, Hong Kong SAR',
                          'Macau' = 'China, Macao SAR',
                          'Taiwan' = 'China, Taiwan Province of',
                          "Cote d'Ivoire" = "Côte d'Ivoire",
                          "Korea, Democratic People's Republic of" = "Democratic People's Republic of Korea",
                          'Swaziland' = 'Eswatini',
                          'Libyan Arab Jamahiriya' = 'Libya',
                          'Burma' = 'Myanmar',
                          'Netherlands Antilles' = 'Netherlands Antilles (former)',
                          'Korea, Republic of' = 'Republic of Korea',
                          'Reunion' = 'Réunion',
                          'United States' = 'United States of America',
                          'Venezuela' = 'Venezuela (Bolivarian Republic of)',
                          'Czech Republic' = 'Czechia',
                          'The former Yugoslav Republic of Macedonia' = 'North Macedonia',
                          'Micronesia, Federated States of' = 'Micronesia (Federated States of)',
                          'Russia' = 'Russian Federation')) %>%
  ## Creating data for south sudan and sudan, from wikipedia/google
  filter(country != 'Sudan') %>%
  add_row(., 'country' = 'Sudan', 'area' = 188607, 'region' = 2, 'subregion' = 15, 'longitude' = 30.21, 'latitude' = 12.86) %>%
  add_row(., 'country' = 'South Sudan', 'area' = 61975, 'region' = 2, 'subregion' = 15, 'longitude' = 31.31, 'latitude' = 12.86)


Production_meat_labelled <- Final_meat_long %>%
  left_join(., world_data, by = 'country') %>%
  group_by(region, subregion) %>%
  filter(country != 'China, mainland')

Production_tonnes_labelled <- Final_tonnes_long %>%
  left_join(., world_data, by = 'country') %>%
  group_by(region, subregion) %>%
  filter(country != 'China, mainland')

##All countries now have an assigned region, subregion and coordinates to 2 d.p for plotting of the network.

# Graphs by subregion -----------------------------------------------------

region_names <- data.frame(region_name = c('Africa', 'Americas', 'Asia', 'Europe', 'Oceania'),
                           region = as.numeric(c(2,19,142,150,9)))
sub_region_names <- data.frame(sub_region_name = c('South America', 'Western Africa', 'Central America', 'Eastern Africa', 'Northern Africa', 'Middle Africa', 'Southern Africa',
                                                   'Northern America', 'Caribbean', 'Eastern Asia', 'Southern Asia', 'South-Eastern Asia', 'Southern Europe',
                                                   'Australia and New Zealand', 'Melanesia', 'Micronesia', 'Polynesia', 'Central Asia', 'Western Asia',
                                                   'Eastern Europe', 'Northern Europe', 'Western Europe'),
                               subregion = as.numeric(c(5, 11, 13, 14, 15, 17, 18, 21,
                                                        29, 30, 34, 35, 39, 53, 54, 57,
                                                        61, 143, 145, 151, 154, 155)))
##Fixing Taiwan
Production_meat_labelled$region[Production_meat_labelled$region == 0] <-  142
Production_meat_labelled$subregion[Production_meat_labelled$subregion == 0] <- 30
Production_tonnes_labelled$region[Production_tonnes_labelled$region == 0] <-  142
Production_tonnes_labelled$subregion[Production_tonnes_labelled$subregion == 0] <- 30

Production_meat_labelled <- full_join(Production_meat_labelled, region_names, by = 'region')
Production_meat_labelled <- full_join(Production_meat_labelled, sub_region_names, by = 'subregion')
Production_tonnes_labelled <- full_join(Production_tonnes_labelled, region_names, by = 'region')
Production_tonnes_labelled <- full_join(Production_tonnes_labelled, sub_region_names, by = 'subregion')

# Regional production levels ----------------------------------------------
Regional_meat(Production_meat_labelled, 'Africa', 'Chicken production (heads) on the African continent', regional_plot = filter(Production_meat_labelled, region_name == 'Africa'))
Regional_meat(Production_meat_labelled, 'Americas', 'Chicken production (heads) on the American continent', regional_plot = filter(Production_meat_labelled, region_name == 'Americas'))
Regional_meat(Production_meat_labelled, 'Asia', 'Chicken production (heads) on the American continent', regional_plot = filter(Production_meat_labelled, region_name == 'Asia'))
Regional_meat(Production_meat_labelled, 'Europe', 'Chicken production (heads) on the European continent', regional_plot = filter(Production_meat_labelled, region_name == 'Europe'))
Regional_meat(Production_meat_labelled, 'Oceania', 'Chicken production (heads) within Oceania', regional_plot = filter(Production_meat_labelled, region_name == 'Oceania'))

Regional_tonnes(Production_tonnes_labelled, 'Africa', 'Chicken production (tonnes) on the African continent', regional_plot = filter(Production_tonnes_labelled, region_name == 'Africa'))
Regional_tonnes(Production_tonnes_labelled, 'Americas', 'Chicken production (tonnes) on the American continent', regional_plot = filter(Production_tonnes_labelled, region_name == 'Americas'))
Regional_tonnes(Production_tonnes_labelled, 'Asia', 'Chicken production (tonnes) on the American continent', regional_plot = filter(Production_tonnes_labelled, region_name == 'Asia'))
Regional_tonnes(Production_tonnes_labelled, 'Europe', 'Chicken production (tonnes) on the European continent', regional_plot = filter(Production_tonnes_labelled, region_name == 'Europe'))
Regional_tonnes(Production_tonnes_labelled, 'Oceania', 'Chicken production (tonnes) within Oceania', regional_plot = filter(Production_tonnes_labelled, region_name == 'Oceania'))

# Sub-regional production levels ------------------------------------------
Sub_regional_meat(Production_meat_labelled, 'South America', 'Chicken production (heads) within the South American region', regional_plot = filter(Production_meat_labelled, sub_region_name == 'South America'))
Sub_regional_meat(Production_meat_labelled, 'Western Africa', 'Chicken production (heads) within the Western Africa region', regional_plot = filter(Production_meat_labelled, sub_region_name == 'Western Africa'))
Sub_regional_meat(Production_meat_labelled, 'South-Eastern Asia', 'Chicken production (heads) within the South-Eastern Asia region', regional_plot = filter(Production_meat_labelled, sub_region_name == 'South-Eastern Asia'))
Sub_regional_meat(Production_meat_labelled, 'Southern Asia', 'Chicken production (heads) within the Southern Asia region', regional_plot = filter(Production_meat_labelled, sub_region_name == 'Southern Asia'))

Sub_regional_tonnes(Production_tonnes_labelled, 'South America', 'Chicken production (tonnes) within the South American region', regional_plot = filter(Production_tonnes_labelled, sub_region_name == 'South America'))
Sub_regional_tonnes(Production_tonnes_labelled, 'Western Africa', 'Chicken production (tonnes) within the Western Africa region', regional_plot = filter(Production_tonnes_labelled, sub_region_name == 'Western Africa'))
Sub_regional_tonnes(Production_tonnes_labelled, 'South-Eastern Asia', 'Chicken production (tonnes) within the South-Eastern Asia region', regional_plot = filter(Production_tonnes_labelled, sub_region_name == 'South-Eastern Asia'))
Sub_regional_tonnes(Production_tonnes_labelled, 'Southern Asia', 'Chicken production (tonnes) within the Southern Asia region', regional_plot = filter(Production_tonnes_labelled, sub_region_name == 'Southern Asia'))