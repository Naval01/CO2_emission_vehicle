#Importing libraries and checking/renaming column names
#install.packages("psych")
library(data.table)
library(ggplot2)
library(splitstackshape)
library(psych)

#Import data as a data.table 
getwd()
#CO2_emission <- fread("./data/MY2020 Fuel Consumption Ratings.csv")

#summary(CO2_emission)
#colnames(CO2_emission)
#CO2_emission[,"Fuel Consuption: Comb (mpg)"]


#Stratified sampling based on car make
#Take a 25% sample from all department groups in dataset
#CO2_emi_sample<- stratified(CO2_emission, "Make", .25)

#summary(CO2_emi_sample)
#colnames(CO2_emi_sample)

#Export the sampled data as CSV
#fwrite(CO2_emi_sample, "./data/Sample_CO2Emission.csv")

CO2_emission_sample <- fread("./data/Sample_CO2Emission.csv")
#summary(CO2_emission_sample)
attach(CO2_emission_sample)

summary(`CO2 Emissions (g/km)`)
describe(`CO2 Emissions (g/km)`)
hist(`CO2 Emissions (g/km)`)

barplot(table(Make) ,las=2)
barplot(table(Transmission),las=2)
barplot(table(`Fuel Type`))

describe(`Engine Size (L)`)
hist(`Engine Size (L)`)
boxplot(`Engine Size (L)`)

describe(`Cylinders`)
hist(`Cylinders`)
boxplot(`Cylinders`)

describe(`Fuel Consuption: Comb (L/100 km)`)
hist(`Fuel Consuption: Comb (L/100 km)`)
boxplot(`Fuel Consuption: Comb (L/100 km)`)
