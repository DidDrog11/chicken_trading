library('maptools')
library('linelist')

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
  add_row(., 'country' = 'South Sudan', 'area' = 61975, 'region' = 2, 'subregion' = 15, 'longitude' = 31.31, 'latitude' = 12.86) %>%
  add_row(., 'country' = 'china_mainland', 'region' = 142, 'subregion' = 30) %>%
  add_row(., 'country' = 'china_taiwan_province_of', 'region' = 142, 'subregion' = 30) %>%
  add_row(., 'country' = 'cote_divoire', 'region' = 2, 'subregion' = 11) %>%
  clean_data()

region_names <- data.frame(region_name = c('Africa', 'Americas', 'Asia', 'Europe', 'Oceania'),
                           region = as.numeric(c(2,19,142,150,9))) %>%
  clean_data()

sub_region_names <- data.frame(sub_region_name = c('South America', 'Western Africa', 'Central America', 'Eastern Africa', 'Northern Africa', 'Middle Africa', 'Southern Africa',
                                                   'Northern America', 'Caribbean', 'Eastern Asia', 'Southern Asia', 'South-Eastern Asia', 'Southern Europe',
                                                   'Australia and New Zealand', 'Melanesia', 'Micronesia', 'Polynesia', 'Central Asia', 'Western Asia',
                                                   'Eastern Europe', 'Northern Europe', 'Western Europe'),
                               subregion = as.numeric(c(5, 11, 13, 14, 15, 17, 18, 21,
                                                        29, 30, 34, 35, 39, 53, 54, 57,
                                                        61, 143, 145, 151, 154, 155))) %>%
  clean_data()

world_data <- left_join(world_data, region_names, by = 'region')
world_data <- left_join(world_data, sub_region_names, by = 'subregion')

write_rds(world_data, here('cleaned_data', 'world_data.RDS'))
