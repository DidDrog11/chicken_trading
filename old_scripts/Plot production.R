#automated plot for meat production
Plot_meat <- function(x, title) {
  ggplot(data = x, aes(x = Year, y = log(`1000 Heads`), colour = Country))+
    geom_smooth()+
    geom_line(size = 0.8)+
    theme_minimal()+
    labs(title = title,
         y = 'log(Number of chickens produced)',
         colour = 'Country')+
    scale_x_continuous(name="Year",breaks = seq(1960,2020, by = 10))+
    theme(axis.text.x= element_text(angle = 45))
}

#automated plot for tonnage production
Plot_tonnes <- function(x, title) {
  ggplot(data = x, aes(x = Year, y = log(Tonnes), colour = Country))+
    geom_smooth()+
    geom_line(size = 0.8)+
    theme_minimal()+
    labs(title = title,
         y = 'log(Tonnes of chickens produced)',
         colour = 'Country')+
    scale_x_continuous(name="Year",breaks = seq(1960,2020, by = 10))+
    theme(axis.text.x= element_text(angle = 45))
}

Regional_meat <- function(x, region, title, regional_plot = filter(x, region_name == region)) {
  ggplot(regional_plot, aes(x = year, y = log(heads), colour = country))+
    geom_smooth()+
    theme_minimal()+
    labs(title = title,
         y = 'log(Number of chickens produced)',
         colour = 'Country')+
    scale_x_continuous(name="Year",breaks = seq(1960,2020, by = 10))+
    theme(axis.text.x= element_text(angle = 45))
}  

Regional_tonnes <- function(x, region, title, regional_plot = filter(x, region_name == region)) {
  ggplot(regional_plot, aes(x = year, y = log(tonnes), colour = country))+
    geom_smooth()+
    theme_minimal()+
    labs(title = title,
         y = 'log(Tonnes of chickens produced)',
         colour = 'Country')+
    scale_x_continuous(name="Year",breaks = seq(1960,2020, by = 10))+
    theme(axis.text.x= element_text(angle = 45))
}  


Sub_regional_meat <- function(x, sub_region_name, title, regional_plot = filter(x, sub_region_name == sub_region_name)) {
  ggplot(regional_plot, aes(x = year, y = log(heads), colour = country))+
    geom_smooth()+
    theme_minimal()+
    labs(title = title,
         y = 'log(Number of chickens produced)',
         colour = 'Country')+
    scale_x_continuous(name="Year",breaks = seq(1960,2020, by = 10))+
    theme(axis.text.x= element_text(angle = 45))
}  

Sub_regional_tonnes <- function(x, sub_region_name, title, regional_plot = filter(x, sub_region_name == sub_region_name)) {
  ggplot(regional_plot, aes(x = year, y = log(tonnes), colour = country))+
    geom_smooth()+
    theme_minimal()+
    labs(title = title,
         y = 'log(Tonnes of chickens produced)',
         colour = 'Country')+
    scale_x_continuous(name="Year",breaks = seq(1960,2020, by = 10))+
    theme(axis.text.x= element_text(angle = 45))
} 

Compare_meat_tonnes <- function(Head_data, Tonnes_data, title) {
  ggplot()+
    geom_smooth(data = Head_data, aes(x = Year, y = log(`1000 Heads`), colour = Country), linetype = 'longdash')+
    geom_smooth(data = Tonnes_data, aes(x = Year, y = log(Tonnes), colour = Country), linetype = 'dotted')+
    theme_minimal()+
    labs(title = title,
         y = 'log(Production)',
         colour = 'Country')+
    scale_x_continuous(name="Year",breaks = seq(1960,2020, by = 10))+
    theme(axis.text.x= element_text(angle = 45))
}
