library('tidyverse')
library('data.table')
library('igraph')
library("ggplot2")
source('Plot production.R')

Chicken_production <- read_csv('FAOStat_Data/Chicken_meat_production.csv')

Producer_countries <- unique(Chicken_production$Area) #Identifying the countries that produce chickens
length(Producer_countries) #How many countries is this?
Producer_country_code <- unique(Chicken_production$`Area Code`) #Are there the same number of codes as countries
Producers <- data.frame(Producer_countries, Producer_country_code) #Collating the country code and name

Years <- unique(Chicken_production$Year) #There is data for 1961-2018 inclusive

Weight <- Chicken_production$Value * 1000 #To convert the number of chickens traded into absolute amounts

#Would be helpful to have each year as a seperate dataset within a larger list
Chicken_production_year <- split.data.frame(Chicken_production, Chicken_production$Year)
#The names of the producing countries for each year of data are stored in the producing countries list
Production <- (Chicken_production$Value)*1000
Producing_country <- data.frame(Chicken_production$Area, Chicken_production$Year, Production)

#This produces a wide dataset for the producing countries 
Producing_country_year <- pivot_wider(Producing_country, names_from = Chicken_production.Year, values_from = Production, values_fill = list(Production = 0))

#Calculating the mean production over the whole timeperiod for countries to crudely group them to allow easier visualisation of trends
decile <- aggregate(Producing_country$Production, by = list(Producing_country$Chicken_production.Area), FUN = mean)
#Allocating the deciles to these means
decile <- decile %>% mutate(decile = ntile(x, 10))
colnames(decile)[1] <-  'Chicken_production.Area'
#Merging the mean with the production values
Producing_country <- merge(Producing_country, decile, by = 'Chicken_production.Area')
Producing_country <- select(Producing_country, c(1:3,5))
#Dividing the dataframes by their decile
Producing_country_deciles <- split.data.frame(Producing_country, Producing_country$decile)

Number_producers <- c()
for (year in 1:length(Years)){
  Number_producers[year] <- length(unique(Chicken_production_year[[year]][["Area"]]))
}
#The number of producers of chickens doesn't remain constant in the dataset. This will need to be accounted for

#Looking at the most recent year, what countries are the highest producers?
Production_2018 <- as.data.frame(Chicken_production_year['2018'])
Production_2018_max <- Production_2018 %>% top_n(20, X2018.Value)

ggplot(data = Production_2018_max, aes(x = X2018.Area, y = X2018.Value))+
  geom_col()+
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
#China and it's subcategory mainland china are effectively duplicated. Perhaps dropping mainland China or if trade between the mainland and Taiwan is important
#To split their values

Chicken_trade <- read_csv('FAOStat_Data/Detailed_Trading_Matrix.csv')

Trading_countries <- unique(c((Chicken_trade$`Reporter Countries`), (Chicken_trade$`Partner Countries`)))
length(Trading_countries) #240 countries or regions are involved in the trade of chickens, this data can be reduced as for example there are 4 entries for china

Trading_years <- unique(Chicken_trade$Year) #There is trade data for the years 1986-2017 inclusive, perhaps the analysis can be held to this time
Trading_units <- unique(Chicken_trade$Unit) #There are three different measures for the amount of trade, tonnes, 1000 head and 1000 US$
table(Chicken_trade$Unit, Chicken_trade$Year) #Tonnes are only reported in the last 4 years and it appears there is a duplication of entries for both 1000 head and dollar amounts
Chicken_trade <- Chicken_trade %>% filter(Unit == '1000 Head') #This will remove the duplicate entries for the different trading volumes

Chicken_trade_year <- split.data.frame(Chicken_trade, Chicken_trade$Year)
Number_traders_reporter <- c()
Number_traders_partner <- c()
for (year in 1:length(Trading_years)){
  Number_traders_reporter[year] <- length(unique(Chicken_trade_year[[year]][["Reporter Countries"]]))
  Number_traders_partner[year] <- length(unique(Chicken_trade_year[[year]][["Partner Countries"]]))
}
Number_traders_reporter #There are increasing numbers of reporter or importing countries each year
Number_traders_partner #While the number of exporting countries increases it isn't by as much

# Managing country changes -----------------------------------------------
  # Former USSR -------------------------------------------------------------
Former_USSR <- c('Armenia', 'Azerbaijan', 'Belarus', 'Estonia', 'Georgia', 'Kazakhstan', 'Kyrgyzstan',
                 'Latvia', 'Lithuania', 'Republic of Moldova', 'Russian Federation', 'Tajikistan',
                 'Turkmenistan', 'Ukraine', 'Uzbekistan')
USSR_Production <- Producing_country %>%
  filter(Chicken_production.Area == 'USSR')

Former_USSR_Production <- Producing_country %>%
  filter(Chicken_production.Area %in% Former_USSR)


Post_USSR_Production <- aggregate(Former_USSR_Production$Production, by = list(Former_USSR_Production$Chicken_production.Year), FUN = sum)
Post_USSR_Production$Country <- c('Former USSR')

Production_in_USSR_Year <- c(USSR_Production$Chicken_production.Year, Post_USSR_Production$Group.1)
Production_in_USSR_Production <- c(USSR_Production$Production, Post_USSR_Production$x)
Production_in_USSR <- data.frame(Production_in_USSR_Year, Production_in_USSR_Production)

ggplot(data = Production_in_USSR, aes(x = Production_in_USSR_Year, y = Production_in_USSR_Production))+
  geom_line(size = 0.8)+
  theme_minimal()+
  labs(title = 'Production of chickens in USSR and post-USSR countries',
       y = 'Number of chickens produced')+
  scale_x_continuous(name="Year",breaks = seq(1960,2020, by = 10))+
  geom_vline(xintercept = 1991, col = 'red')+
  theme(axis.text.x= element_text(angle = 45))
#Huge drop in production post collapse of USSR ?reasonable

USSR_1991_production <- USSR_Production[31,3]
Former_USSR_1992_production <- Post_USSR_Production[1,2]

Former_USSR_1992 <- Former_USSR_Production %>%
  filter(Chicken_production.Year == '1992')

proportion_production <- c(1:15)
for (country in 1:length(Former_USSR_1992$Chicken_production.Area)){
  proportion_production[country] <- Former_USSR_1992$Production[country]/Former_USSR_1992_production
}
#What is the proportion of the USSR's production that can be attributed to each country and what would those values look like projected backwards
USSR_Production_Countries <- matrix(nrow = 31, ncol = length(Former_USSR))
colnames(USSR_Production_Countries) <- Former_USSR
rownames(USSR_Production_Countries) <- USSR_Production$Chicken_production.Year
for (country in 1:15){
  USSR_Production_Countries[,country] <- USSR_Production$Production*proportion_production[country]
}
rowSums(USSR_Production_Countries)
USSR_Production$Production
#The sum of the countries productions is equal to the USSR productions

#To create a data table with all these countries and their production for each year
USSR_Production_Countries <- as_tibble(USSR_Production_Countries, rownames = NA)

Former_USSR_Production <- pivot_wider(Former_USSR_Production, id_cols = Chicken_production.Year, names_from = Chicken_production.Area, values_from = Production)
Former_USSR_Production <- Former_USSR_Production[,2:16]

Former_USSR_Production <- bind_rows(USSR_Production_Countries, Former_USSR_Production)
Years <- c(1961:2018)
Production_data_USSR <- Former_USSR_Production %>% add_column(Year = Years, .before = 'Armenia')


  # Belgium-Luxembourg ------------------------------------------------------
Post_division <- c('Belgium', 'Luxembourg')

Belgium_Luxembourg <- Producing_country %>%
  filter(Chicken_production.Area == 'Belgium-Luxembourg')

Post_benelux <- Producing_country %>%
  filter(Chicken_production.Area %in% Post_division)

Belgium <- Producing_country %>%
  filter(Chicken_production.Area == 'Belgium')

Luxembourg <- Producing_country %>%
  filter(Chicken_production.Area == 'Luxembourg')

Belgium_Luxembourg <- aggregate(Belgium_Luxembourg$Production, by = list(Belgium_Luxembourg$Chicken_production.Year), FUN = sum)
Post_ben_lux <- aggregate(Post_benelux$Production, by = list(Post_benelux$Chicken_production.Year), FUN = sum)

proportion_production <- c(1:2)
proportion_production <- c((Post_benelux[1,3]/Post_ben_lux[1,2]),(Post_benelux[20,3]/Post_ben_lux[1,2]))
Benelux_Production_Countries <- matrix(nrow = 39, ncol = 2)
colnames(Benelux_Production_Countries) <- Post_division
rownames(Benelux_Production_Countries) <- Belgium_Luxembourg$Group.1
Benelux_Production_Countries[,1] <- Belgium_Luxembourg$x*proportion_production[1]
Benelux_Production_Countries[,2] <- Belgium_Luxembourg$x*proportion_production[2]
rowSums(Benelux_Production_Countries)
Belgium_Luxembourg$x
##The production levels are the same

Benelux_years <- c(Belgium_Luxembourg$Group.1, Post_ben_lux$Group.1)
Benelux_production <- c(Belgium_Luxembourg$x, Post_ben_lux$x)
Production_in_benelux <- data.frame(Benelux_years, Benelux_production)

ggplot(data = Production_in_benelux, aes(x = Benelux_years, y = Benelux_production))+
  geom_line(size = 0.8)+
  theme_minimal()+
  labs(title = 'Production of chickens in BeneLux and post-BeneLux countries',
       y = 'Number of chickens produced')+
  scale_x_continuous(name="Year",breaks = seq(1960,2020, by = 10))+
  geom_vline(xintercept = 2000, col = 'red')+
  theme(axis.text.x= element_text(angle = 45))

a <- tibble(Post_ben_lux$Group.1, Belgium$Production, Luxembourg$Production)
colnames(a) <- c('Year', 'Belgium', 'Luxembourg')
b <- tibble(Belgium_Luxembourg$Group.1, Benelux_Production_Countries[,1], Benelux_Production_Countries[,2])
colnames(b) <- c('Year', 'Belgium', 'Luxembourg')
benelux_graph <- rbind(a,b) %>%
  arrange(Year)

ggplot(data = benelux_graph, aes(x = Year))+
  geom_line(aes(y = Belgium), size = 0.8, col = 'red')+
  geom_line(aes(y = Luxembourg), size = 0.8, col = 'black')+
  theme_minimal()+
  labs(title = 'Production of chickens in Benelux and post-Benelux countries',
       y = 'Number of chickens produced')+
  scale_x_continuous(name="Year",breaks = seq(1960,2020, by = 10))+
  geom_vline(xintercept = 1999, col = 'red')+
  theme(axis.text.x= element_text(angle = 45))

Production_data_benelux <- benelux_graph

  # Former Yugoslavia -------------------------------------------------------
Former_FPRY <- c('Bosnia and Herzegovina', 'Croatia', 'Serbia and Montenegro', 'North Macedonia', 'Slovenia')
FPRY_Production <- Producing_country %>%
  filter(Chicken_production.Area == 'Yugoslav SFR')

Former_FPRY_Production <- Producing_country %>%
  filter(Chicken_production.Area %in% Former_FPRY)

Post_FPRY_Production <- aggregate(Former_FPRY_Production$Production, by = list(Former_FPRY_Production$Chicken_production.Year), FUN = sum)
Post_FPRY_Production$Country <- c('Former FPRY')

Production_in_FPRY_Year <- c(FPRY_Production$Chicken_production.Year, Post_FPRY_Production$Group.1)
Production_in_FPRY_Production <- c(FPRY_Production$Production, Post_FPRY_Production$x)
Production_in_FPRY <- data.frame(Production_in_FPRY_Year, Production_in_FPRY_Production)

ggplot(data = Production_in_FPRY, aes(x = Production_in_FPRY_Year, y = Production_in_FPRY_Production))+
  geom_line(size = 0.8)+
  theme_minimal()+
  labs(title = 'Production of chickens in FPRY and post-FPRY countries',
       y = 'Number of chickens produced')+
  scale_x_continuous(name="Year",breaks = seq(1960,2020, by = 10))+
  geom_vline(xintercept = 1991, col = 'red')+
  theme(axis.text.x= element_text(angle = 45))

FPRY_1991_production <- FPRY_Production[31,3]
Former_FPRY_1992 <- Former_FPRY_Production %>%
  filter(Chicken_production.Year == '1992')
Former_FPRY_1992_production <- sum(Former_FPRY_1992$Production)

proportion_production <- c(1:5)
for (country in 1:length(Former_FPRY_1992$Chicken_production.Area)){
  proportion_production[country] <- Former_FPRY_1992$Production[country]/Former_FPRY_1992_production
}

#What is the proportion of the FPRY's production that can be attributed to each country and what would those values look like projected backwards
FPRY_Production_Countries <- matrix(nrow = 31, ncol = length(Former_FPRY))
colnames(FPRY_Production_Countries) <- Former_FPRY
rownames(FPRY_Production_Countries) <- FPRY_Production$Chicken_production.Year
for (country in 1:5){
  FPRY_Production_Countries[,country] <- FPRY_Production$Production*proportion_production[country]
}
b <- as_tibble(FPRY_Production_Countries)
Years <- c(1961:1991)
b <- b %>% add_column(Year = Years)

a <- pivot_wider(Former_FPRY_Production, id_cols = Chicken_production.Year, names_from = Chicken_production.Area, values_from = Production)
a <- a %>% rename(Year = Chicken_production.Year)

Production_data_FDPR <- bind_rows(b, a)
    
    # Serbia and Montenegro ---------------------------------------------------
Post_division <- c('Montenegro', 'Serbia')

Serbia_and_Montenegro <- Producing_country %>%
  filter(Chicken_production.Area == 'Serbia and Montenegro')

Post_serbia_montenegro <- Producing_country %>%
  filter(Chicken_production.Area %in% Post_division)

Serbia_Montenegro <- aggregate(Serbia_and_Montenegro$Production, by = list(Serbia_and_Montenegro$Chicken_production.Year), FUN = sum)
Post_S_M <- aggregate(Post_serbia_montenegro$Production, by = list(Post_serbia_montenegro$Chicken_production.Year), FUN = sum)

Serbia_montenegro_years <- c(Serbia_Montenegro$Group.1, Post_S_M$Group.1)
Serbia_montengro_production <- c(Serbia_Montenegro$x, Post_S_M$x)
Production_in_serbia_montenegro <- data.frame(Serbia_montenegro_years, Serbia_montengro_production)

ggplot(data = Production_in_serbia_montenegro, aes(x = Serbia_montenegro_years, y = Serbia_montengro_production))+
  geom_line(size = 0.8)+
  theme_minimal()+
  labs(title = 'Production of chickens in Serbia and Montenegro and post-partition countries',
       y = 'Number of chickens produced')+
  scale_x_continuous(name="Year",breaks = seq(1960,2020, by = 10))+
  geom_vline(xintercept = 2005, col = 'red')+
  theme(axis.text.x= element_text(angle = 45))

a <- as_tibble(Post_serbia_montenegro) %>%
  pivot_wider(id_cols = Chicken_production.Year, names_from = Chicken_production.Area, values_from = Production) %>%
  rename(Year = Chicken_production.Year)

proportion_production <- c(sum(a[1,2]/(a[1,2]+a[1,3])), sum(a[1,3]/(a[1,2]+a[1,3])))
b <- tibble(.rows = 58)
for (country in 1:2){
  b[,country] <- Production_data_FDPR$`Serbia and Montenegro`*proportion_production[country]
}

Year <- Production_data_FDPR$Year

b <- b %>%
  add_column(Year) %>%
  rename('Montenegro' = 'V1', 'Serbia' = 'V2') %>%
  drop_na()

Production_data_former_SM <- bind_rows(b, a)

Production_data_FDPR <- select(Production_data_FDPR, -c(`Serbia and Montenegro`))
Production_data_FDPR <- bind_cols(Production_data_FDPR, Production_data_former_SM) %>%
  select(-c('Year1'))

  # Israel and Palestine ----------------------------------------------------
Post_division <- c('Israel', 'Palestine')
Israel <- Producing_country %>%
  filter(Chicken_production.Area == 'Israel')

Palestine <- Producing_country %>%
  filter(Chicken_production.Area == 'Palestine')

Israel_palestine <- Producing_country %>%
  filter(Chicken_production.Area %in% Post_division)

Israel <- aggregate(Israel$Production, by = list(Israel$Chicken_production.Year), FUN = sum)

ggplot(data = Israel_palestine, aes(x = Chicken_production.Year, y = Production, col = Chicken_production.Area))+
  geom_line(size = 0.8)+
  theme_minimal()+
  labs(title = 'Production of chickens in Israel and Palestine',
       y = 'Number of chickens produced')+
  scale_x_continuous(name="Year",breaks = seq(1960,2020, by = 10))+
  geom_vline(xintercept = 1990, col = 'red')+
  theme(axis.text.x= element_text(angle = 45))

a <- as_tibble(Israel_palestine) %>%
  pivot_wider(id_cols = Chicken_production.Year, names_from = Chicken_production.Area, values_from = Production) %>%
  rename(Year = Chicken_production.Year)

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
  

Production_data_Israel_Palestine <- bind_rows(b, a)

  # Czechoslovakia ----------------------------------------------------------
Post_division <- c('Czechia', 'Slovakia')

Czechoslovakia <- Producing_country %>%
  filter(Chicken_production.Area == 'Czechoslovakia') %>%
  as_tibble()

Czechia_Slovakia <- Producing_country %>%
  filter(Chicken_production.Area %in% Post_division) %>%
  as_tibble()

Slovakia <- Producing_country %>%
  filter(Chicken_production.Area == 'Slovakia') %>%
  as_tibble()

a <- Czechia_Slovakia %>%
  pivot_wider(id_cols = Chicken_production.Year, names_from = Chicken_production.Area, values_from = Production) %>%
  rename(Year = Chicken_production.Year)

proportion_production <- c(sum(a[1,2]/(a[1,2]+a[1,3])), sum(a[1,3]/(a[1,2]+a[1,3])))
b <- tibble(.rows = 32)
for (country in 1:2){
  b[,country] <- Czechoslovakia$Production*proportion_production[country]
}

b <- b %>%
  add_column(Year[1:32]) %>%
  rename('Czechia' = 'V1', 'Slovakia' = 'V2', 'Year' = 'Year[1:32]')


Production_data_Czechia_Slovakia <- bind_rows(b, a)


  # Ethiopia ----------------------------------------------------------------
Post_division <- c('Eritrea', 'Ethiopia')

Ethiopia_PDR <- Producing_country %>%
  filter(Chicken_production.Area == 'Ethiopia PDR') %>%
  as_tibble()

Eritrea_Ethiopia <- Producing_country %>%
  filter(Chicken_production.Area %in% Post_division) %>%
  as_tibble()

a <- Eritrea_Ethiopia %>%
  pivot_wider(id_cols = Chicken_production.Year, names_from = Chicken_production.Area, values_from = Production) %>%
  rename(Year = Chicken_production.Year)

proportion_production <- c(sum(a[1,2]/(a[1,2]+a[1,3])), sum(a[1,3]/(a[1,2]+a[1,3])))
b <- tibble(.rows = 32)
for (country in 1:2){
  b[,country] <- Ethiopia_PDR$Production*proportion_production[country]
}

b <- b %>%
  add_column(Year[1:32]) %>%
  rename('Eritrea' = 'V1', 'Ethiopia' = 'V2', 'Year' = 'Year[1:32]')

Production_data_Eritrea_Ethiopia <- bind_rows(b, a)

ggplot(data = Production_data_Eritrea_Ethiopia, aes(x = Year))+
  geom_line(aes(y = Eritrea),size = 0.8, col = 'red')+
  geom_line(aes(y = Ethiopia), size = 0.8, col = 'black')+
  theme_minimal()+
  labs(title = 'Annual production of chickens in Eritrea and Ethiopia',
       y = 'Number of chickens produced')+
  scale_x_continuous(name="Year",breaks = seq(1960,2020, by = 10))+
  geom_vline(xintercept = 1992, col = 'blue')+
  theme(axis.text.x= element_text(angle = 45))


  # Micronesia --------------------------------------------------------------

Pacific_islands <- Producing_country %>%
  filter(Chicken_production.Area == 'Pacific Islands Trust Territory') %>%
  pivot_wider(id_cols = Chicken_production.Year, names_from = Chicken_production.Area, values_from = Production) %>%
  rename('Micronesia (Federated States of)' = 'Pacific Islands Trust Territory')

Micronesia <- Producing_country %>%
  filter(Chicken_production.Area == 'Micronesia (Federated States of)') %>%
  pivot_wider(id_cols = Chicken_production.Year, names_from = Chicken_production.Area, values_from = Production)

a <- bind_rows(Pacific_islands, Micronesia)

Production_data_Micronesia <- a %>%
  rename(Year = Chicken_production.Year)

ggplot(data = Production_data_Micronesia, aes(x = Year))+
  geom_line(aes(y = `Micronesia (Federated States of)`),size = 0.8, col = 'red')+
  theme_minimal()+
  labs(title = 'Annual production of chickens in Micronesia',
       y = 'Number of chickens produced')+
  scale_x_continuous(name="Year",breaks = seq(1960,2020, by = 10))+
  theme(axis.text.x= element_text(angle = 45))


  # Sudan -------------------------------------------------------------------
Post_division <- c('Sudan', 'South Sudan')

Sudan_former <- Producing_country %>%
  filter(Chicken_production.Area == 'Sudan (former)') %>%
  as_tibble()

Sudan <- Producing_country %>%
  filter(Chicken_production.Area %in% Post_division) %>%
  as_tibble()

a <- Sudan %>%
  pivot_wider(id_cols = Chicken_production.Year, names_from = Chicken_production.Area, values_from = Production) %>%
  rename(Year = Chicken_production.Year)

proportion_production <- c(sum(a[1,2]/(a[1,2]+a[1,3])), sum(a[1,3]/(a[1,2]+a[1,3])))
b <- tibble(.rows = 51)
for (country in 1:2){
  b[,country] <- Sudan_former$Production*proportion_production[country]
}

b <- b %>%
  add_column(Year[1:51]) %>%
  rename('Sudan' = 'V1', 'South Sudan' = 'V2', 'Year' = 'Year[1:51]')

Production_data_Sudan_SouthSudan <- bind_rows(b, a)

ggplot(data = Production_data_Sudan_SouthSudan, aes(x = Year))+
  geom_line(aes(y = Sudan),size = 0.8, col = 'red')+
  geom_line(aes(y = `South Sudan`), size = 0.8, col = 'black')+
  theme_minimal()+
  labs(title = 'Annual production of chickens in Sudan and South Sudan',
       y = 'Number of chickens produced')+
  scale_x_continuous(name="Year",breaks = seq(1960,2020, by = 10))+
  geom_vline(xintercept = 2001, col = 'blue')+
  theme(axis.text.x= element_text(angle = 45))
         
         
         
         
         
         
         