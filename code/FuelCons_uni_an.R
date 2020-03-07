#Analyzing Combined EmissionFuel Consumption

library(data.table)

sample_CO2 <- read.csv("./data/Sample_CO2Emission.csv")
typeof(sample_CO2$Fuel.Consuption..Comb..L.100.km.)


#Statistical Analysis of Combined EmissionFuel Consumption

mean(sample_CO2$Fuel.Consuption..Comb..L.100.km.)
median(sample_CO2$Fuel.Consuption..Comb..L.100.km.)

var(sample_CO2$Fuel.Consuption..Comb..L.100.km.)
sd(sample_CO2$Fuel.Consuption..Comb..L.100.km.)

stat = summary(sample_CO2$Fuel.Consuption..Comb..L.100.km.)
stat

iqr <-  stat[5] - stat[2] 
iqr

lower_lim <- stat[2] - 1.5*iqr
upper_lim <- stat[5] + 1.5*iqr
lower_lim
upper_lim

outlier_lower <- d[which(d < lower.lim)]
outlier_upper <- d[which(d > upper.lim)]




#Histogram of Combined EmissionFuel Consumption

ggplot(data = sample_CO2) + 
  geom_histogram(mapping=aes(x = Fuel.Consuption..Comb..L.100.km., fill="red"),  binwidth=1, origin=0) +
  theme_update() +
  scale_y_continuous(name = "Number Of Cars") +
  #  scale_x_continuous(name = "CO2 emissions (g/km)" , breaks = seq(min(sample_CO2$CO2.Emissions..g.km.), max(sample_CO2$CO2.Emissions..g.km.),50)) +
  scale_fill_manual(name = "Color", values = "LIGHTBLUE", labels(NULL)) +
  xlab("Combined Fuel Consumption (L/100km)") +
  ggtitle("Analyzing Combined Fuel Consumption")


#Box Plot of Combined EmissionFuel Consumption

ggplot(data = sample_CO2) + 
  scale_y_continuous(name = "Combined Fuel Consumption (L/100km)") +
  geom_boxplot(mapping=aes(y = CO2.Emissions..g.km.)) +
  scale_x_continuous(name = NULL,) +
  theme_update() + 
  ggtitle("CO2 emission")

boxplot(sample_CO2$Fuel.Consuption..Comb..L.100.km.,
        main = "Analyzing Combined Fuel Consumption")

