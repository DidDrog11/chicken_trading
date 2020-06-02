#Data Cleaning Functions
#The same function can be used for the old country and the collection of the new countries

Region_meat <- function(Region_name) { # Region_name is the area of production to be extracted from the chicken production dataset
  Chicken_production_meat %>%
    filter(Area %in% Region_name) %>% # This can allow the single country or subsequent group of countries to be extracted
    select('Area', 'Year', 'Value') %>%
    rename('1000 Heads' = 'Value', 'Country' = 'Area')
}

S_M_meat <- function(Region_name) { # Region_name is the area of production to be extracted from the chicken production dataset
  Final_former_YSFR_meat %>%
    filter(Country %in% Region_name) %>% # This can allow the single country or subsequent group of countries to be extracted
    select('Country', 'Year', '1000 Heads') %>%
    ungroup()
}

Region_tonnes <- function(Region_name) { # Region_name is the area of production to be extracted from the chicken production dataset
  Chicken_production_tonnes %>%
  select('Area', 'Year', 'Value') %>%
  filter(Area %in% Region_name) %>% # This can allow the single country or subsequent group of countries to be extracted
  rename('Tonnes' = 'Value', 'Country' = 'Area')
}

S_M_tonnes <- function(Region_name) { # Region_name is the area of production to be extracted from the chicken production dataset
  Final_former_YSFR_tonnes %>%
    filter(Country %in% Region_name) %>% # This can allow the single country or subsequent group of countries to be extracted
    select('Country', 'Year', 'Tonnes') %>%
    ungroup()
}

Combined_meat <- function(Current_countries_meat, Region_name, Old_country_meat) { # Current_countries is the modern group of countries whose production will be aggregated
  aggregate(Current_countries_meat$`1000 Heads`, by = list(Current_countries_meat$Year), FUN = sum) %>%
  cbind(Region_name) %>% # Region_name is the name for the current collection of countries in quotation marks, e.g. 'Former USSR'
  rename('Year' = 1, '1000 Heads' = 2, 'Country' = 3) %>%
  rbind(Old_country_meat) %>% # This is to bind the data to the old countries name prior to division, e.g. 'USSR'
  arrange(Year)
}

Combined_tonnes <- function(Current_countries_tonnes, Region_name, Old_country_tonnes) { # Current_countries is the modern group of countries whose production will be aggregated
  aggregate(Current_countries_tonnes$Tonnes, by = list(Current_countries_tonnes$Year), FUN = sum) %>%
    cbind(Region_name) %>% # Region_name is the name for the current collection of countries in quotation marks, e.g. 'Former USSR'
    rename('Year' = 1, 'Tonnes' = 2, 'Country' = 3) %>%
    rbind(Old_country_tonnes) %>% # This is to bind the data to the old countries name prior to division, e.g. 'USSR'
    arrange(Year)
}

#Calculate the proportional production of each country
Production_partitioned_countries_meat <- function(Start_year, Partition_year, Partitioned_countries, Aggregated_production, Post_partition_countries){
  year <- as.numeric(Partition_year)
  year_start <- as.numeric(Start_year)
  year_diff <- (year - year_start)
  Country_list <- Post_partition_countries
  a <- filter(Partitioned_countries, Year == year)
  b <- Aggregated_production
  a <- a %>%
    cbind(a$`1000 Heads`/b$`1000 Heads`[b$Year == year]) %>%
    rename('Proportional production' = 4)
  c <- sort(rep(Country_list, length(year_start:(year-1))))
  d <- rep(year_start:(year-1), length(unique(Country_list)))
  e <- filter(b, Year < year)
  f <- tibble(d, c) %>%
    rename('Country' = 2, 'Year' = 1) %>%
    mutate('Proportional production' = rep(a$`Proportional production`, each = year_diff)) %>%
    mutate('Production' = `Proportional production`*e$`1000 Heads`) %>%
    select('Year', 'Country', 'Production') %>%
    rename('1000 Heads' = 'Production' )
  bind_rows(f, Partitioned_countries) %>%
    group_by(Country) %>%
    arrange(Year, .by_group = T)
}

Production_partitioned_countries_tonnes <- function(Start_year, Partition_year, Partitioned_countries, Aggregated_production, Post_partition_countries){
  year <- as.numeric(Partition_year)
  year_start <- as.numeric(Start_year)
  year_diff <- (year - year_start)
  Country_list <- Post_partition_countries
  a <- filter(Partitioned_countries, Year == year)
  b <- Aggregated_production
  a <- a %>%
    cbind(a$Tonnes/b$Tonnes[b$Year == year]) %>%
    rename('Proportional production' = 4)
  c <- sort(rep(Country_list, length(year_start:(year-1))))
  d <- rep(year_start:(year-1), length(unique(Country_list)))
  e <- filter(b, Year < year)
  f <- tibble(d, c) %>%
    rename('Country' = 2, 'Year' = 1) %>%
    mutate('Proportional production' = rep(a$`Proportional production`, each = year_diff)) %>%
    mutate('Production' = `Proportional production`*e$Tonnes) %>%
    select('Year', 'Country', 'Production') %>%
    rename('Tonnes' = 'Production' )
  bind_rows(f, Partitioned_countries) %>%
    group_by(Country) %>%
    arrange(Year, .by_group = T)
}

#Detailed trading matrix cleaning
clean_trade <- function(detailed_trading) {
  selected_columns <- c(3:6, 8, 12:14)
  selected_columns <- as.character(colnames(detailed_trading[selected_columns]))
  detailed_trading %>%
    select(selected_columns) %>%
    janitor::clean_names() %>%
    mutate_all(.funs = tolower) %>%
    mutate(reporter_countries = str_replace_all(reporter_countries, " ", "_")) %>%
    mutate(partner_countries = str_replace_all(partner_countries, " ", "_")) %>%
    mutate(element = str_replace_all(element, " ", "_")) %>%
    mutate(unit = str_replace_all(unit, " ", "_")) %>%
    mutate(reporter_countries = str_replace_all(reporter_countries, "[^_[:^punct:]]", "")) %>%
    mutate(partner_countries = str_replace_all(partner_countries, "[^_[:^punct:]]", "")) %>%
    mutate(unit = str_replace_all(unit, "[^_[:^punct:]]", ""))
} 

heads <- function(detailed_trading) {
  detailed_trading %>%
    filter(unit %in% '1000_head')
}

tonnes <- function(detailed_trading) {
  detailed_trading %>%
    filter(unit %in% 'tonnes')
}

dollars <- function(detailed_trading) {
  detailed_trading %>%
    filter(unit %in% '1000_us$')
}



