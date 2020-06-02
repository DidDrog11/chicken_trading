plot(density(Producing_country$Production), col = 'red')
plot(ecdf(Producing_country$Production), col = 'red')

Producing_country$Production[Producing_country$Production == 0] <- 1
Producing_country$Transformed_production <- log(Producing_country$Production)

# Histogram with density plot
ggplot(Producing_country, aes(x = Transformed_production)) + 
  geom_histogram(aes(y=..density..), colour="black", fill="white")+
  geom_density(alpha=.2, fill="#FF6666")+
  geom_vline(aes(xintercept = mean(Transformed_production)),col = 'red')
# Approximates normal after log transformation

#ECDF plot
ggplot(Producing_country, aes(Transformed_production)) + stat_ecdf(geom = "step")+
  labs(title="Empirical Cumulative \n Density Function",
       y = "F(production)", x="log Production(chickens)")+
  theme_classic()

#QQ plot
qplot(sample = Transformed_production, data = Producing_country)+
  labs(title="Production of chickens \n log transformed",
       y = "Chicken production")+
  stat_qq_line()+
  theme_classic()
# Data is under-dispersed

cor(Producing_country$Transformed_production, Producing_country$Chicken_production.Year)
# There is a correlation between increasing chicken production and time

Linear_model <- lm(Transformed_production ~ Chicken_production.Year + Chicken_production.Area, data = Producing_country)
modelSummary <- summary(Linear_model)  # capture model summary as an object
modelCoeffs <- modelSummary$coefficients  # model coefficients