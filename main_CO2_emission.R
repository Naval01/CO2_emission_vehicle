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

