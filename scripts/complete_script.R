library("tidyverse")
library("data.table")
library("ggplot2")

chicken_production <- read_csv("FAOStat_Data/Chicken_meat_production.csv")

# Regions with changed names ----------------------------------------------
former_ussr <- c("Armenia", "Azerbaijan", "Belarus", "Estonia", "Georgia", "Kazakhstan", "Kyrgyzstan",
                 "Latvia", "Lithuania", "Republic of Moldova", "Russian Federation", "Tajikistan",
                 "Turkmenistan", "Ukraine", "Uzbekistan")
former_benelux <- c("Belgium", "Luxembourg")
former_ysfr <- c("Bosnia and Herzegovina", "Croatia", "Serbia and Montenegro", "North Macedonia", "Slovenia")
former_sm <- c("Montenegro", "Serbia")
former_israel <- c("Israel", "Palestine")
former_czechoslovakia <- c("Czechia", "Slovakia")
former_ethiopia <- c("Eritrea", "Ethiopia")
former_sudan <- c("Sudan", "South Sudan")
former_pacific <- c("Micronesia (Federated States of)")

remove_states <- c("USSR", "Belgium-Luxembourg", "Yugoslav SFR", "Czechoslovakia", "Ethiopia PDR", "Pacific Islands Trust Territory", "Israel", "Sudan (former)")

#Data Cleaning Functions
#The same function can be used for the old country and the collection of the new countries

region_meat <- function(region_name) { # Region_name is the area of production to be extracted from the chicken production dataset
  chicken_production %>%
    filter(Area %in% region_name) %>% # This can allow the single country or subsequent group of countries to be extracted
    select("Area", "Year", "Value") %>%
    rename("1000 Heads" = "Value", "Country" = "Area")
}

combined_meat <- function(current_countries_meat, region_name, old_country_meat) { # Current_countries is the modern group of countries whose production will be aggregated
  aggregate(current_countries_meat$`1000 Heads`, by = list(current_countries_meat$Year), FUN = sum) %>%
    cbind(region_name) %>% # Region_name is the name for the current collection of countries in quotation marks, e.g. "Former USSR"
    rename("Year" = 1, "1000 Heads" = 2, "Country" = 3) %>%
    rbind(old_country_meat) %>% # This is to bind the data to the old countries name prior to division, e.g. "USSR"
    arrange(Year)
}

s_m_meat <- function(region_name) { # Region_name is the area of production to be extracted from the chicken production dataset
  final_former_ysfr_meat %>%
    filter(Country %in% region_name) %>% # This can allow the single country or subsequent group of countries to be extracted
    select("Country", "Year", "1000 Heads") %>%
    ungroup()
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
    rename("Proportional production" = 4)
  c <- sort(rep(country_list, length(year_start:(year-1))))
  d <- rep(year_start:(year-1), length(unique(country_list)))
  e <- filter(b, Year < year)
  f <- tibble(d, c) %>%
    rename("Country" = 2, "Year" = 1) %>%
    mutate("Proportional production" = rep(a$`Proportional production`, each = year_diff)) %>%
    mutate("Production" = `Proportional production`*e$`1000 Heads`) %>%
    select("Year", "Country", "Production") %>%
    rename("1000 Heads" = "Production" )
  bind_rows(f, partitioned_countries) %>%
    group_by(Country) %>%
    arrange(Year, .by_group = T)
}

plot_meat <- function(x, title, partition_year) {
  ggplot(data = x, aes(x = Year, y = log(`1000 Heads`), colour = Country))+
    geom_line(size = 0.8)+
    geom_vline(xintercept = partition_year)+
    annotation_logticks()+
    annotate(x = min(partition_year),y = max(log(x$`1000 Heads`))+0.7,label="Year of partition",geom="label")+
    theme_minimal()+
    labs(title = title,
         colour = "Country")+
    scale_x_continuous(name="Year", breaks = seq(1960,2020, by = 10))+
    scale_y_continuous(name = "log(Number of chickens produced)", 
                       breaks = seq(round(min(log(x$`1000 Heads`)), digits = 0),
                                    round(max(log(x$`1000 Heads`)), digits = 0)),
                       limits = c(NA, round(max(log(x$`1000 Heads`)+1), digits = 0)))+
    theme(axis.text.x= element_text(angle = 45))
}

# Imputing data for previously combined countries ------------------------
#   For the USSR ------------------------------------------------------------
ussr_meat <- region_meat("USSR")
former_ussr_meat <- region_meat(former_ussr)
# Production levels in the countries that made up the USSR for individual chickens

# To work out the proportion of production each subsequent country contributed the following function can be used to combine their production to a single column
post_ussr_meat <- combined_meat(former_ussr_meat, "Former USSR", ussr_meat)

# Production levels in the countries after partition
final_former_ussr <- production_partitioned_countries("1961","1992", former_ussr_meat, post_ussr_meat, former_ussr) %>%
  bind_rows(post_ussr_meat %>%
              mutate(Country = "USSR"))

plot_meat(final_former_ussr, "Chicken production in the countries of the USSR", 1992)
# This graph shows chicken production in the USSR and countries that comprised it. Production fell prior to partition and took several decades for it to return to prepartition levels.

# For the FPRY and Serbia and Montenegro ------------------------------------------------------------
ysfr_meat <- region_meat("Yugoslav SFR")
# Production levels in the former YSFR for individuals chickens
former_ysfr_meat <- region_meat(former_ysfr)
# Production levels in the countries that made up the YSFR for individual chickens

# To work out the proportion of production each subsequent country contributed the following function can be used to combine their production to a single column
post_ysfr_meat <- combined_meat(former_ysfr_meat, "Former YSFR", ysfr_meat)

# Production levels in the countries after partition
final_former_ysfr <- production_partitioned_countries("1961", "1992", former_ysfr_meat, post_ysfr_meat, former_ysfr)

plot_meat(final_former_ysfr, "Chicken production of the YSFR", 1992)
# Serbia and Montenegro is a bit different as it also divided in 2005

sm_meat <- s_m_meat("Serbia and Montenegro")
# Production levels in the former SM for individual chickens

former_sm_meat <- region_meat(former_sm)
# Production levels in the countries that made up SM

# To work out the proportion of production each subsequent country contributed the following function can be used to combine their production to a single column
post_sm_meat <- combined_meat(former_sm_meat, "Former SM", sm_meat)

#Production levels in the countries after partition
final_former_sm <- production_partitioned_countries("1961", "2006", former_sm_meat, post_sm_meat, former_sm)

plot_meat(final_former_sm, "Chicken production Serbia and Montenegro", 2006)

# Combine the data for both SM and FPRY
complete_ysfr_meat <- final_former_ysfr_meat %>%
  bind_rows(final_former_sm) %>%
  bind_rows(post_ysfr_meat %>%
              mutate(Country = "FPRY")) %>%
  filter(Country != "Serbia and Montenegro")

plot_meat(complete_ysfr_meat, "Chicken production Former Yugoslavian countries", partition_year = c(1992, 2004))

# Benelux -----------------------------------------------------------------
bel_lux_meat <- region_meat("Belgium-Luxembourg")
# Production levels in Belgium-Luxembourg for individual chickens

former_bel_lux_meat <- region_meat(former_benelux)
# Production levels in the countries that made up Belgium-Luxembourg for individual chickens

# To work out the proportion of production each subsequent country contributed the following function can be used to combine their production to a single column
post_bel_lux_meat <- combined_meat(former_bel_lux_meat, "Former Belgium-Luxembourg", bel_lux_meat)

#Production levels in the countries after partition
final_former_benelux <- production_partitioned_countries("1961","2000", former_bel_lux_meat, post_bel_lux_meat, former_benelux)

plot_meat(final_former_benelux, "Chicken production in Belgium and Luxembourg", 2000)

# Czechia -----------------------------------------------------------------
czech_meat <- region_meat("Czechoslovakia")

czechoslovakia_meat <- region_meat(former_czechoslovakia)

post_czechoslovakia_meat <- combined_meat(czechoslovakia_meat, "Former Czechoslovakia", czech_meat)

final_former_czechoslovakia <- production_partitioned_countries("1961","1993", czechoslovakia_meat, post_czechoslovakia_meat, former_czechoslovakia)

plot_meat(final_former_czechoslovakia, "Chicken production in Czechia and Slovakia", 1993)

# Ethiopia ----------------------------------------------------------------
ethiopia_meat <- region_meat("Ethiopia PDR")

ethiopia_meat_production <- region_meat(former_ethiopia)

post_ethiopia_pdr_meat <- combined_meat(ethiopia_meat_production, "Former Ethiopia PDR", ethiopia_meat)

final_former_ethiopia <- production_partitioned_countries("1961","1993", ethiopia_meat_production, post_ethiopia_pdr_meat, former_ethiopia)

plot_meat(final_former_ethiopia, "Chicken production in Ethiopia and Eritrea", 1993)

# Micronesia --------------------------------------------------------------
pacific_meat <- region_meat("Pacific Islands Trust Territory")

micronesia_meat <- region_meat(former_pacific)

final_micronesia <- rbind(pacific_meat, micronesia_meat) %>%
  mutate(Country = "Micronesia (Federated States of)")

# Israel ------------------------------------------------------------------
##Problem as is called Israel both before and after creation of Palestine
producing_country_meat <- data.frame(chicken_production$Area, chicken_production$Year,
                                     chicken_production$Value)
year <- c(1961:2018)
post_division <- c("Israel", "Palestine")
# Meat --------------------------------------------------------------------
israel <- producing_country_meat %>%
  filter(chicken_production$Area == "Israel")

palestine <- producing_country_meat %>%
  filter(chicken_production$Area == "Palestine")

israel_palestine <- producing_country_meat %>%
  filter(chicken_production$Area %in% post_division)

israel <- aggregate(israel$chicken_production.Value, by = list(israel$chicken_production.Year), FUN = sum)

a <- as_tibble(israel_palestine) %>%
  pivot_wider(id_cols = chicken_production.Year,
              names_from = chicken_production.Area,
              values_from = chicken_production.Value) %>%
  rename(Year = chicken_production.Year)

proportion_production <- c(sum(a[31,2]/(a[31,2]+a[31,3])), sum(a[31,3]/(a[31,2]+a[31,3])))

b <- tibble(.rows = 58)
for (country in 1:2){
  b[,country] <- a$Israel*proportion_production[country]
}
b <- b[1:30,]
a <- a[31:58,]

b <- b %>%
  add_column(year[1:30]) %>%
  rename("Israel" = 1, "Palestine" = 2, "Year" = "year[1:30]")


final_israelpalestine <- bind_rows(b, a) %>%
  pivot_longer(., cols = 1:2) %>%
  rename("Country" = 2, "1000 Heads" = 3)

plot_meat(final_israelpalestine, "Chicken production in Israel and Palestine", 1991)

# Sudan -------------------------------------------------------------------
##Problem as is called Sudan both before and after creation of South Sudan
post_division <- c("Sudan", "South Sudan")

sudan_former <- producing_country_meat %>%
  filter(chicken_production$Area == "Sudan (former)") %>%
  as_tibble()

sudan <- producing_country_meat %>%
  filter(chicken_production.Area %in% post_division) %>%
  as_tibble()

a <- sudan %>%
  pivot_wider(id_cols = chicken_production.Year,
              names_from = chicken_production.Area,
              values_from = chicken_production.Value) %>%
  rename(Year = chicken_production.Year)

proportion_production <- c(sum(a[1,2]/(a[1,2]+a[1,3])), sum(a[1,3]/(a[1,2]+a[1,3])))
b <- tibble(.rows = 51)
for (country in 1:2){
  b[,country] <- sudan_former$chicken_production.Value*proportion_production[country]
}

b <- b %>%
  add_column(year[1:51]) %>%
  rename("Sudan" = 2, "South Sudan" = 1, "Year" = "year[1:51]")

final_sudan <- bind_rows(b, a) %>%
  pivot_longer(., cols = 1:2) %>%
  rename("Country" = 2, "1000 Heads" = 3)

plot_meat(final_sudan, "Chicken production in Sudan and South Sudan", 2011)

# Combining these data to produce a complete production dataset
production_long <- chicken_production %>%
  select(Area, Year, Value) %>%
  rename("Country" = Area,
         "1000 Heads" = Value) %>%
  filter(!Country %in% remove_states) %>%
  bind_rows(final_former_ussr,
            final_former_ysfr,
            final_former_benelux,
            final_former_czechoslovakia,
            final_former_ethiopia,
            final_micronesia,
            final_israelpalestine,
            final_sudan)

# Mapping -----------------------------------------------------------------
## Mapping
#Country names differ between datasets either due to spelling, use of special characters or naming conventions. I use and maintain the following dictionary to ease the changing names.
country_dictionary <- c("Bolivia" = "Bolivia (Plurinational State of)",
                        "Cape Verde" = "Cabo Verde",
                        "Hong Kong" = "China, Hong Kong SAR",
                        "Macau" = "China, Macao SAR",
                        "Taiwan" = "China, Taiwan Province of",
                        "Cote d'Ivoire" = "Côte d'Ivoire",
                        "Korea, Democratic People's Republic of" = "Democratic People's Republic of Korea",
                        "Swaziland" = "Eswatini",
                        "Libyan Arab Jamahiriya" = "Libya",
                        "Burma" = "Myanmar",
                        "Netherlands Antilles" = "Netherlands Antilles (former)",
                        "Korea, Republic of" = "Republic of Korea",
                        "Reunion" = "Réunion",
                        "United States" = "United States of America",
                        "Venezuela" = "Venezuela (Bolivarian Republic of)",
                        "Czech Republic" = "Czechia",
                        "The former Yugoslav Republic of Macedonia" = "North Macedonia",
                        "Micronesia, Federated States of" = "Micronesia (Federated States of)",
                        "Russia" = "Russian Federation")
#This is a named character vector

data("wrld_simpl")
column_names <- c("iso3", "un", "country", "area", "pop2005", "region", "subregion", "longitude", "latitude")
world_data <- wrld_simpl@data
colnames(world_data) <- c(column_names)
world_data <- world_data %>%
  mutate(country = plyr::mapvalues(country, from = names(country_dictionary), to = country_dictionary))

urladdress <- "https://raw.githubusercontent.com/datasets/population/master/data/population.csv"
global_historical_pop <- read_csv(url(urladdress))
