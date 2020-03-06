#Importing libraries and checking/renaming column names

library(data.table)
library(ggplot2)
library(splitstackshape)


#Import data as a data.table 
getwd()
CO2_emission <- fread("./data/MY2020 Fuel Consumption Ratings.csv")

summary(CO2_emission)
colnames(CO2_emission)
CO2_emission[,"Fuel Consuption: Comb (mpg)"]


#Stratified sampling based on car make
#Take a 25% sample from all department groups in dataset
CO2_emi_sample<- stratified(CO2_emission, "Make", .25)

summary(CO2_emi_sample)
colnames(CO2_emi_sample)

#Export the sampled data as CSV
fwrite(CO2_emi_sample, "./data/Sample_CO2Emission.csv")

#create dataframe
df<- data.frame(CO2_emi_sample)

#rename columns to be used
library(dplyr)
df<- df %>% rename(fuel.consumption.hwy = Fuel.Consumption..Hwy..L.100.km., 
             co2.emissions = CO2.Emissions..g.km., engine.size = Engine.Size..L., 
             fuel.consumption.comb = Fuel.Consuption..Comb..L.100.km., 
             fuel.consumption.city = Fuel.Consumption..City..L.100.km.,
             fuel.consumption.comb.mpg = Fuel.Consuption..Comb..mpg.)

library(olsrr)
#fitting models with all variables gives multicolinearity
#returns co2 rating, fuel con. comb, fuel type, engine size, make

#fitting model only for fuel consum(comb, hwy, city), cylinders, engine size, 
#fuel type, vehicle class, transmission
fit <- lm(co2.emissions~.-Model.Year -Make -Model.Name -fuel.consumption.comb.mpg -CO2.Rating -Smog.Rating, data = df)
summary(fit)
ols_step_both_p(fit) # default: pent = 0.1; prem = 0.3 
# returns fuel con. comb, fuel type, engine size, transmmissions

for (i in colnames(df)){
  if (i != "co2.emissions"){
    plot(df$i,df$co2.emissions,  ylab = "co2.emissions", xlab = names(df)[1])
  }
}

