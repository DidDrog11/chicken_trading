library('tidyverse')
library('data.table')
library('igraph')
library('ggplot2')
source(here('scripts', 'data_cleaning_functions.R'))
source(here('Plot production.R'))

Chicken_production_meat <- read_csv('FAOStat_Data/Chicken_meat_production.csv') #Producing animals/Slaughtered element measured in 1000 head of animals
Chicken_production_tonnes <- read_csv('FAOStat_Data/Chicken_production_tonnes.csv') #Production element measured in tonnes

# Regions with changed names ----------------------------------------------
Former_USSR <- c('Armenia', 'Azerbaijan', 'Belarus', 'Estonia', 'Georgia', 'Kazakhstan', 'Kyrgyzstan',
                 'Latvia', 'Lithuania', 'Republic of Moldova', 'Russian Federation', 'Tajikistan',
                 'Turkmenistan', 'Ukraine', 'Uzbekistan')
Former_benelux <- c('Belgium', 'Luxembourg')
Former_YSFR <- c('Bosnia and Herzegovina', 'Croatia', 'Serbia and Montenegro', 'North Macedonia', 'Slovenia')
Former_SM <- c('Montenegro', 'Serbia')
Former_Israel <- c('Israel', 'Palestine')
Former_Czechoslovakia <- c('Czechia', 'Slovakia')
Former_Ethiopia <- c('Eritrea', 'Ethiopia')
Former_Sudan <- c('Sudan', 'South Sudan')
Former_Pacific <- c('Micronesia (Federated States of)')

# Imputing data for previously combined countries ------------------------
#   For the USSR ------------------------------------------------------------
USSR_meat <- Region_meat('USSR')

USSR_tonnes <- Region_tonnes('USSR')
# Production levels in the USSR for both individual chickens and tonnage

Former_USSR_meat_production <- Region_meat(Former_USSR)

Former_USSR_tonnes_production <- Region_tonnes(Former_USSR)
# Production levels in the countries that made up the USSR for both individual chickens and tonnage

# To work out the proportion of production each subsequent country contributed the following function can be used to combine their production to a single column
Post_USSR_meat <- Combined_meat(Former_USSR_meat_production, 'Former USSR', USSR_meat)


Post_USSR_tonnes <- Combined_tonnes(Former_USSR_tonnes_production, 'Former USSR', USSR_tonnes)

#Production levels in the countries after partition
Final_former_USSR_meat <- Production_partitioned_countries_meat('1961','1992', Former_USSR_meat_production, Post_USSR_meat, Former_USSR)

Final_former_USSR_tonnes <- Production_partitioned_countries_tonnes('1961','1992', Former_USSR_tonnes_production, Post_USSR_tonnes, Former_USSR)

Compare_meat_tonnes(Final_former_USSR_meat, Final_former_USSR_tonnes, 'Countries of the USSR production, dotted = Tonnes')


  # For the FPRY and Serbia and Montenegro ------------------------------------------------------------
YSFR_meat <- Region_meat('Yugoslav SFR')

YSFR_tonnes <- Region_tonnes('Yugoslav SFR')
# Production levels in the former YSFR for both individuals chickens and tonnage

Former_YSFR_meat_production <- Region_meat(Former_YSFR)

Former_YSFR_tonnes_production <- Region_tonnes(Former_YSFR)
# Production levels in the countries that made up the YSFR for both individual chickens and tonnage

# To work out the proportion of production each subsequent country contributed the following function can be used to combine their production to a single column
Post_YSFR_meat <- Combined_meat(Former_YSFR_meat_production, 'Former YSFR', YSFR_meat)


Post_YSFR_tonnes <- Combined_tonnes(Former_YSFR_tonnes_production, 'Former YSFR', YSFR_tonnes)

#Production levels in the countries after partition
Final_former_YSFR_meat <- Production_partitioned_countries_meat('1961', '1992', Former_YSFR_meat_production, Post_YSFR_meat, Former_YSFR)

Final_former_YSFR_tonnes <- Production_partitioned_countries_tonnes('1961','1992', Former_YSFR_tonnes_production, Post_YSFR_tonnes, Former_YSFR)

Compare_meat_tonnes(Final_former_YSFR_meat, Final_former_YSFR_tonnes, 'Countries of the YSFR production, dotted = Tonnes')

#Serbia and montenegro is a bit different as it also divided in 2005
SM_meat <- S_M_meat('Serbia and Montenegro')

SM_tonnes <- S_M_tonnes('Serbia and Montenegro')
# Production levels in the former SM for both individuals chickens and tonnage

Former_SM_meat_production <- Region_meat(Former_SM)

Former_SM_tonnes_production <- Region_tonnes(Former_SM)
# Production levels in the countries that made up the SM for both individual chickens and tonnage

# To work out the proportion of production each subsequent country contributed the following function can be used to combine their production to a single column
Post_SM_meat <- Combined_meat(Former_SM_meat_production, 'Former SM', SM_meat)

Post_SM_tonnes <- Combined_tonnes(Former_SM_tonnes_production, 'Former SM', SM_tonnes)

#Production levels in the countries after partition
Final_former_SM_meat <- Production_partitioned_countries_meat('1961', '2006', Former_SM_meat_production, Post_SM_meat, Former_SM)

Final_former_SM_tonnes <- Production_partitioned_countries_tonnes('1961', '2006', Former_SM_tonnes_production, Post_SM_tonnes, Former_SM)
##For Serbia and Montenegro I need the YSFR data to feed in at the beginning not the chicken production


  # Benelux -----------------------------------------------------------------
Bel_Lux_meat <- Region_meat('Belgium-Luxembourg')

Bel_Lux_tonnes <- Region_tonnes('Belgium-Luxembourg')
# Production levels in Belgium-Luxembourg for both individual chickens and tonnage

Former_Bel_Lux_meat_production <- Region_meat(Former_benelux)

Former_Bel_Lux_tonnes_production <- Region_tonnes(Former_benelux)
# Production levels in the countries that made up Belgium-Luxembourg for both individual chickens and tonnage

# To work out the proportion of production each subsequent country contributed the following function can be used to combine their production to a single column
Post_Bel_Lux_meat <- Combined_meat(Former_Bel_Lux_meat_production, 'Former Belgium-Luxembourg', Bel_Lux_meat)


Post_Bel_Lux_tonnes <- Combined_tonnes(Former_Bel_Lux_tonnes_production, 'Former Belgium-Luxembourg', Bel_Lux_tonnes)

#Production levels in the countries after partition
Final_former_BeneLux_meat <- Production_partitioned_countries_meat('1961','2000', Former_Bel_Lux_meat_production, Post_Bel_Lux_meat, Former_benelux)

Final_former_BeneLux_tonnes <- Production_partitioned_countries_tonnes('1961','2000', Former_Bel_Lux_tonnes_production, Post_Bel_Lux_tonnes, Former_benelux)

Compare_meat_tonnes(Final_former_BeneLux_meat, Final_former_BeneLux_tonnes, 'Countries of Benelux production, dotted = Tonnes')

  # Czechia -----------------------------------------------------------------
Czech_meat <- Region_meat('Czechoslovakia')

Czech_tonnes <- Region_tonnes('Czechoslovakia')
# Production levels in Czechoslovakia for both individual chickens and tonnage

Czechoslovakia_meat_production <- Region_meat(Former_Czechoslovakia)

Czechoslovakia_tonnes_production <- Region_tonnes(Former_Czechoslovakia)
# Production levels in the countries that made up Czechoslovakia for both individual chickens and tonnage

# To work out the proportion of production each subsequent country contributed the following function can be used to combine their production to a single column
Post_Czechoslovakia_meat <- Combined_meat(Czechoslovakia_meat_production, 'Former Czechoslovakia', Czech_meat)


Post_Czechoslovakia_tonnes <- Combined_tonnes(Czechoslovakia_tonnes_production, 'Former Czechoslovakia', Czech_tonnes)

#Production levels in the countries after partition
Final_former_czechoslovakia_meat <- Production_partitioned_countries_meat('1961','1993', Czechoslovakia_meat_production, Post_Czechoslovakia_meat, Former_Czechoslovakia)

Final_former_czechoslovakia_tonnes <- Production_partitioned_countries_tonnes('1961','1993', Czechoslovakia_tonnes_production, Post_Czechoslovakia_tonnes, Former_Czechoslovakia)

Compare_meat_tonnes(Final_former_czechoslovakia_meat, Final_former_czechoslovakia_tonnes, 'Countries of Czechoslovakia production, dotted = Tonnes')

# Ethiopia ----------------------------------------------------------------
Ethiopia_meat <- Region_meat('Ethiopia PDR')

Ethiopia_tonnes <- Region_tonnes('Ethiopia PDR')
# Production levels in Czechoslovakia for both individual chickens and tonnage

Ethiopia_meat_production <- Region_meat(Former_Ethiopia)

Ethiopia_tonnes_production <- Region_tonnes(Former_Ethiopia)
# Production levels in the countries that made up Czechoslovakia for both individual chickens and tonnage

# To work out the proportion of production each subsequent country contributed the following function can be used to combine their production to a single column
Post_Ethiopia_PDR_meat <- Combined_meat(Ethiopia_meat_production, 'Former Ethiopia PDR', Ethiopia_meat)


Post_Ethiopia_PDR_tonnes <- Combined_tonnes(Ethiopia_tonnes_production, 'Former Ethiopia PDR', Ethiopia_tonnes)

#Production levels in the countries after partition
Final_former_Ethiopia_meat <- Production_partitioned_countries_meat('1961','1993', Ethiopia_meat_production, Post_Ethiopia_PDR_meat, Former_Ethiopia)

Final_former_Ethiopia_tonnes <- Production_partitioned_countries_tonnes('1961','1993', Ethiopia_tonnes_production, Post_Ethiopia_PDR_tonnes, Former_Ethiopia)

Compare_meat_tonnes(Final_former_Ethiopia_meat, Final_former_Ethiopia_tonnes, 'Countries of Ethiopia PDR production, dotted = Tonnes')


  # Micronesia --------------------------------------------------------------
Pacific_meat <- Region_meat('Pacific Islands Trust Territory')

Pacific_tonnes <- Region_tonnes('Pacific Islands Trust Territory')

Micronesia_meat <- Region_meat(Former_Pacific)

Micronesia_tonnes <- Region_tonnes(Former_Pacific)

Final_micronesia_meat <- rbind(Pacific_meat, Micronesia_meat)
Final_micronesia_meat$Country[Final_micronesia_meat$Country == 'Pacific Islands Trust Territory'] <- 'Micronesia (Federated States of)'

Final_micronesia_tonnes <- rbind(Pacific_tonnes, Micronesia_tonnes)
Final_micronesia_tonnes$Country[Final_micronesia_tonnes$Country == 'Pacific Islands Trust Territory'] <- 'Micronesia (Federated States of)'


  # Israel ------------------------------------------------------------------
##Problem as is called Israel both before and after creation of Palestine
##This is using the previous clumsy code and needs to be duplicated for meat and tonnage
Producing_country_meat <- data.frame(Chicken_production_meat$Area, Chicken_production_meat$Year, Chicken_production_meat$Value)
Producing_country_tonnes <- data.frame(Chicken_production_tonnes$Area, Chicken_production_tonnes$Year, Chicken_production_tonnes$Value)
Year <- c(1961:2018)
Post_division <- c('Israel', 'Palestine')
    # Meat --------------------------------------------------------------------
Israel <- Producing_country_meat %>%
  filter(Chicken_production_meat$Area == 'Israel')

Palestine <- Producing_country_meat %>%
  filter(Chicken_production_meat$Area == 'Palestine')

Israel_palestine <- Producing_country_meat %>%
  filter(Chicken_production_meat$Area %in% Post_division)

Israel <- aggregate(Israel$Chicken_production_meat.Value, by = list(Israel$Chicken_production_meat.Year), FUN = sum)

a <- as_tibble(Israel_palestine) %>%
  pivot_wider(id_cols = Chicken_production_meat.Year, names_from = Chicken_production_meat.Area, values_from = Chicken_production_meat.Value) %>%
  rename(Year = Chicken_production_meat.Year)

proportion_production <- c(sum(a[31,2]/(a[31,2]+a[31,3])), sum(a[31,3]/(a[31,2]+a[31,3])))
b <- tibble(.rows = 58)
for (country in 1:2){
  b[,country] <- a$Israel*proportion_production[country]
}
b <- b[1:30,]
a <- a[31:58,]

b <- b %>%
  add_column(Year[1:30]) %>%
  rename('Israel' = 'V1', 'Palestine' = 'V2', 'Year' = 'Year[1:30]')


Final_former_IsraelPalestine_meat <- bind_rows(b, a) %>%
  pivot_longer(., cols = 1:2) %>%
  rename('Country' = 2, '1000 Heads' = 3)

    # Tonnage -----------------------------------------------------------------
Israel <- Producing_country_tonnes %>%
  filter(Chicken_production_tonnes$Area == 'Israel')

Palestine <- Producing_country_tonnes %>%
  filter(Chicken_production_tonnes$Area == 'Palestine')

Israel_palestine <- Producing_country_tonnes %>%
  filter(Chicken_production_tonnes$Area %in% Post_division)

Israel <- aggregate(Israel$Chicken_production_tonnes.Value, by = list(Israel$Chicken_production_tonnes.Year), FUN = sum)

a <- as_tibble(Israel_palestine) %>%
  pivot_wider(id_cols = Chicken_production_tonnes.Year, names_from = Chicken_production_tonnes.Area, values_from = Chicken_production_tonnes.Value) %>%
  rename(Year = Chicken_production_tonnes.Year)

proportion_production <- c(sum(a[31,2]/(a[31,2]+a[31,3])), sum(a[31,3]/(a[31,2]+a[31,3])))
b <- tibble(.rows = 58)
for (country in 1:2){
  b[,country] <- a$Israel*proportion_production[country]
}
b <- b[1:30,]
a <- a[31:58,]

b <- b %>%
  add_column(Year[1:30]) %>%
  rename('Israel' = 'V1', 'Palestine' = 'V2', 'Year' = 'Year[1:30]')


Final_former_IsraelPalestine_tonnes <- bind_rows(b, a) %>%
  pivot_longer(., cols = 1:2) %>%
  rename('Country' = 2, 'Tonnes' = 3)

  # Sudan -------------------------------------------------------------------
##Problem as is called Sudan both before and after creation of South Sudan
Post_division <- c('Sudan', 'South Sudan')


    # Meat --------------------------------------------------------------------
Sudan_former <- Producing_country_meat %>%
  filter(Chicken_production_meat$Area == 'Sudan (former)') %>%
  as_tibble()

Sudan <- Producing_country_meat %>%
  filter(Chicken_production_meat.Area %in% Post_division) %>%
  as_tibble()

a <- Sudan %>%
  pivot_wider(id_cols = Chicken_production_meat.Year, names_from = Chicken_production_meat.Area, values_from = Chicken_production_meat.Value) %>%
  rename(Year = Chicken_production_meat.Year)

proportion_production <- c(sum(a[1,2]/(a[1,2]+a[1,3])), sum(a[1,3]/(a[1,2]+a[1,3])))
b <- tibble(.rows = 51)
for (country in 1:2){
  b[,country] <- Sudan_former$Chicken_production_meat.Value*proportion_production[country]
}

b <- b %>%
  add_column(Year[1:51]) %>%
  rename('Sudan' = 'V1', 'South Sudan' = 'V2', 'Year' = 'Year[1:51]')

Final_former_Sudan_meat <- bind_rows(b, a) %>%
  pivot_longer(., cols = 1:2) %>%
  rename('Country' = 2, '1000 Heads' = 3)


    # Tonnage -----------------------------------------------------------------
Sudan_former <- Producing_country_tonnes %>%
  filter(Chicken_production_tonnes$Area == 'Sudan (former)') %>%
  as_tibble()

Sudan <- Producing_country_tonnes %>%
  filter(Chicken_production_tonnes.Area %in% Post_division) %>%
  as_tibble()

a <- Sudan %>%
  pivot_wider(id_cols = Chicken_production_tonnes.Year, names_from = Chicken_production_tonnes.Area, values_from = Chicken_production_tonnes.Value) %>%
  rename(Year = Chicken_production_tonnes.Year)

proportion_production <- c(sum(a[1,2]/(a[1,2]+a[1,3])), sum(a[1,3]/(a[1,2]+a[1,3])))
b <- tibble(.rows = 51)
for (country in 1:2){
  b[,country] <- Sudan_former$Chicken_production_tonnes.Value*proportion_production[country]
}

b <- b %>%
  add_column(Year[1:51]) %>%
  rename('Sudan' = 'V1', 'South Sudan' = 'V2', 'Year' = 'Year[1:51]')

Final_former_Sudan_tonnes <- bind_rows(b, a) %>%
  pivot_longer(., cols = 1:2) %>%
  rename('Country' = 2, 'Tonnes' = 3)
