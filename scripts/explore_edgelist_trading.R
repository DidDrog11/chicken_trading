library('tidyverse')
library('here')
library('linelist')
source(here('scripts', 'extract_function_edgelist.R'))

world_data <- read_rds(here('cleaned_data', 'world_data.RDS'))
region_names <- world_data %>%
  select(country, region_name, sub_region_name)
sender_regions <- region_names %>%
  rename('sender' = country,
         'sender_sub_region_name' = sub_region_name,
         'sender_region_name' = region_name) %>%
  select(sender, sender_region_name, sender_sub_region_name)
receiver_regions <- region_names %>%
  rename('receiver' = country,
         'receiver_sub_region_name' = sub_region_name,
         'receiver_region_name' = region_name) %>%
  select(receiver, receiver_region_name, receiver_sub_region_name)
         

all_edge_lists <- read_rds('cleaned_data/all_edge_lists.rds')

## For a given item, you can then compare values reported in Import and Export columns

edgelist_head <- Extract_EdgeList(all_edge_lists,"1000Head") %>%
  clean_data() %>%
  mutate(difference = ifelse(import > export, import-export, -(export-import))) %>%
  left_join(., sender_regions, by = 'sender') %>%
  left_join(., receiver_regions, by = 'receiver') %>%
  group_by(sender)

summary(edgelist_head$difference)

# We could perform the analysis on the greatest recorded between the countries
edgelist_greatest <- edgelist_head %>%
  mutate(greatest = ifelse(import > export, import, export))

write_rds(edgelist_greatest, here('cleaned_data', '1000_head_edgelist.rds'))


country_trade <- edgelist_greatest %>%
  add_tally() %>%
  group_by(sender, n) %>%
  summarise_at(c('greatest'), list(min, max, mean, median)) %>%
  rename('country' = 1, 'tally' = 2, 'min' = 3, 'max' = 4, 'mean' = 5, 'median' = 6)



a <- country_trade %>%
  ungroup() %>%
  mutate(country = factor(country, levels = country[order(.$tally)])) %>%
  mutate(quartile = ntile(tally, 4)) %>%
  arrange(-tally) %>%
  left_join(., region_names, by = 'country')


density_plot <- ggplot(edgelist_greatest, aes(x = greatest))+
  geom_density()

# Most trades are for small amounts with some very large trades


a <- edgelist %>%
  ungroup() %>%
  group_by(Sender, Receiver) %>%
  summarise(mean(Import), mean(Export))

sum( edgelist$Import==0 & edgelist$Export>0  )
sum( edgelist$Import>0  & edgelist$Export==0 )
sum( edgelist$Import>0  & edgelist$Export>0 )
nS <- edgelist$Import>0  & edgelist$Export>0
summary( (edgelist$Import[nS]-edgelist$Export[nS])/apply(edgelist[nS,c("Import","Export")],1,min) * 100 , digits = 1)
