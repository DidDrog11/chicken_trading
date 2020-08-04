library("tidyverse")
library("data.table")
library("ggplot2")

chicken_production <- read_csv('FAOStat_Data/Chicken_meat_production.csv')

# Regions with changed names ----------------------------------------------
former_ussr <- c('Armenia', 'Azerbaijan', 'Belarus', 'Estonia', 'Georgia', 'Kazakhstan', 'Kyrgyzstan',
                 'Latvia', 'Lithuania', 'Republic of Moldova', 'Russian Federation', 'Tajikistan',
                 'Turkmenistan', 'Ukraine', 'Uzbekistan')
former_benelux <- c('Belgium', 'Luxembourg')
former_ysfr <- c('Bosnia and Herzegovina', 'Croatia', 'Serbia and Montenegro', 'North Macedonia', 'Slovenia')
former_sm <- c('Montenegro', 'Serbia')
former_israel <- c('Israel', 'Palestine')
former_czechoslovakia <- c('Czechia', 'Slovakia')
former_ethiopia <- c('Eritrea', 'Ethiopia')
former_sudan <- c('Sudan', 'South Sudan')
former_pacific <- c('Micronesia (Federated States of)')

#Data Cleaning Functions
#The same function can be used for the old country and the collection of the new countries

region_meat <- function(region_name) { # Region_name is the area of production to be extracted from the chicken production dataset
  chicken_production %>%
    filter(Area %in% region_name) %>% # This can allow the single country or subsequent group of countries to be extracted
    select('Area', 'Year', 'Value') %>%
    rename('1000 Heads' = 'Value', 'Country' = 'Area')
}

combined_meat <- function(current_countries_meat, region_name, old_country_meat) { # Current_countries is the modern group of countries whose production will be aggregated
  aggregate(current_countries_meat$`1000 Heads`, by = list(current_countries_meat$Year), FUN = sum) %>%
    cbind(region_name) %>% # Region_name is the name for the current collection of countries in quotation marks, e.g. 'Former USSR'
    rename('Year' = 1, '1000 Heads' = 2, 'Country' = 3) %>%
    rbind(old_country_meat) %>% # This is to bind the data to the old countries name prior to division, e.g. 'USSR'
    arrange(Year)
}

#Calculate the proportional production of each country
production_partitioned_countries <- function(start_year, partition_year, partitioned_countries, aggregated_production, post_partition_countries){
  year <- as.numeric(partition_year)
  year_start <- as.numeric(start_year)
  year_diff <- (year - year_start)
  country_list <- post_partition_countries
  a <- filter(partitioned_countries, Year == year)
  b <- aggregated_production
  a <- a %>%
    cbind(a$`1000 Heads`/b$`1000 Heads`[b$Year == year]) %>%
    rename('Proportional production' = 4)
  c <- sort(rep(country_list, length(year_start:(year-1))))
  d <- rep(year_start:(year-1), length(unique(country_list)))
  e <- filter(b, Year < year)
  f <- tibble(d, c) %>%
    rename('Country' = 2, 'Year' = 1) %>%
    mutate('Proportional production' = rep(a$`Proportional production`, each = year_diff)) %>%
    mutate('Production' = `Proportional production`*e$`1000 Heads`) %>%
    select('Year', 'Country', 'Production') %>%
    rename('1000 Heads' = 'Production' )
  bind_rows(f, partitioned_countries) %>%
    group_by(Country) %>%
    arrange(Year, .by_group = T)
}

plot_meat <- function(x, title, partition_year) {
  ggplot(data = x, aes(x = Year, y = log(`1000 Heads`), colour = Country))+
    geom_line(size = 0.8)+
    geom_vline(aes(xintercept = partition_year))+
    annotate(x = partition_year,y = max(log(x$`1000 Heads`))+0.7,label="Year of partition",vjust=2,geom="label")+
    theme_minimal()+
    labs(title = title,
         colour = 'Country')+
    scale_x_continuous(name="Year", breaks = seq(1960,2020, by = 10))+
    scale_y_continuous(name = 'log(Number of chickens produced)', breaks = seq(round(min(log(x$`1000 Heads`)), digits = 0),
                                                                               round(max(log(x$`1000 Heads`)), digits = 0)))+
    theme(axis.text.x= element_text(angle = 45))
}

# Imputing data for previously combined countries ------------------------
#   For the USSR ------------------------------------------------------------
ussr_meat <- region_meat('USSR')
former_ussr_meat <- region_meat(former_ussr)
# Production levels in the countries that made up the USSR for individual chickens

# To work out the proportion of production each subsequent country contributed the following function can be used to combine their production to a single column
post_ussr_meat <- combined_meat(former_ussr_meat, 'former ussr', ussr_meat)

#Production levels in the countries after partition
final_former_ussr <- production_partitioned_countries('1961','1992', former_ussr_meat, post_ussr_meat, former_ussr)

plot_meat(final_former_ussr, "Chicken production in the countries of the USSR", 1992)



