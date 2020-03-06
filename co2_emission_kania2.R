co2_emi_sample <- read.csv("C:/Users/galz_/Desktop/DANA 4801/project/CO2_emission_vehicle/data/Sample_CO2Emission.csv")
attach(co2_emi_sample)

#print all output
options(print.max=10000000000)

#create data frame
df<- data.frame(co2_emi_sample)

#rename columns to be used
library(dplyr)
df<- df %>% rename(fuel.consumption.hwy = Fuel.Consumption..Hwy..L.100.km., 
                   co2.emissions = CO2.Emissions..g.km., engine.size = Engine.Size..L., 
                   fuel.consumption.comb = Fuel.Consuption..Comb..L.100.km., 
                   fuel.consumption.city = Fuel.Consumption..City..L.100.km.,
                   fuel.consumption.comb.mpg = Fuel.Consuption..Comb..mpg.)

#plot each variables with co2 emissions (y)
for (i in 1:length(colnames(df))){
  if (colnames(df)[i] != "co2.emissions"){
    plot(df[,i],df$co2.emissions,  ylab = "co2.emissions", xlab = colnames(df)[i])
  }
}


#fuel type, 4 levels; 3 dummy variable
#diesel,D, as baseline
ethanol <- rep(0,length(df$Fuel.Type)) #E
ethanol[df$Fuel.Type == "E"]<-1

reg.gas<- rep(0,length(df$Fuel.Type)) #X
reg.gas[df$Fuel.Type == "X"]<-1

prem.gas<- rep(0,length(df$Fuel.Type)) #Z
prem.gas[df$Fuel.Type == "Z"]<-1

##TO EDIT CODE: character matching instead of listing values
#vehicle class categorized into cars and trucks, 2 levels, 1 dummy variable
#cars: two-seater, minicompact, subcompact, compact, mid-size, full-size, station wagon (small & mid-size)
#trucks: pick-up truck(small & standard), SUV (small& standard), minivan, passenger van, special purpose vehicle 
car.vehicle <- rep(0, length(df$Vehicle.Class))
for (i in 1:length(df$Vehicle.Class))
  {
  if (df$Vehicle.Class[i] == "Two-seater" | df$Vehicle.Class[i] =="Minicompact" | df$Vehicle.Class[i] =="Subcompact" | 
      df$Vehicle.Class[i] =="Compact" | df$Vehicle.Class[i] =="Mid-size" | df$Vehicle.Class[i] =="Full-size" | 
      df$Vehicle.Class[i] =="Station wagon: Small" | df$Vehicle.Class[i] =="Station wagon: Mid-size")
    {
    car.vehicle[i] <-1
    }
}

#original level: 21; AS10, AS9, A8, AM7, AS8, AM8, M6, A9, AS6, A6, AV, M5, A10, A5, AV7, AM6, AS7, AV8, AV10, AV6, AS5 
#tranmission categorized into AS, AV, AM, A, M- 5 levels; 4 dummy variables
#manual as base level
###loop for A-automatic
autom<- rep(0, length(df$Transmission))
for (i in 1:length(df$Transmission))
  {
  if (df$Transmission[i] == "A8" | df$Transmission[i] =="A9" | df$Transmission[i] =="A6" | 
      df$Transmission[i] =="A10" | df$Transmission[i] =="A5")
    {
    autom[i] <-1
    }
}

###loop for AM-automated manual
auto.man<- rep(0, length(df$Transmission))
for (i in 1:length(df$Transmission)){
  if (df$Transmission[i] == "AM7" | df$Transmission[i] =="AM8" | df$Transmission[i] =="AM6")
  {
    auto.man[i] <-1
  }
}

###loop for AS-automatic w/ select shift
auto.shift<- rep(0, length(df$Transmission))
for (i in 1:length(df$Transmission)){
  if (df$Transmission[i] == "AS10" | df$Transmission[i] =="AS9" | df$Transmission[i] =="AS8" | 
      df$Transmission[i] =="AS6" | df$Transmission[i] =="AS5" | df$Transmission[i] =="AS7")
  {
    auto.shift[i] <-1
  }
}

###loop for AV-continuously variable
av<- rep(0, length(df$Transmission))
for (i in 1:length(df$Transmission)){
  if (df$Transmission[i] == "AV" | df$Transmission[i] =="AV7" | df$Transmission[i] =="AV8" | 
      df$Transmission[i] =="AV10" | df$Transmission[i] =="AV6")
  {
    av[i] <-1
  }
}


#get all possible regression model by all-possible-regression selection 
library(olsrr)
fit.var.sel <- lm(co2.emissions~ Vehicle.Class+Transmission+engine.size+Fuel.Type+fuel.consumption.comb+Cylinders, data = df)
summary(fit.var.sel)
ols_step_all_possible(fit.var.sel)
#lowest Cp-  Vehicle.Class, engine.size, Fuel.Type, fuel.consumption.comb


#initial fitting of model only for fuel consumption comb, cylinders, engine size, fuel type, vehicle class, transmission (linear 1st order r/s)
fit.first <- lm(co2.emissions~ fuel.consumption.comb+engine.size+Cylinders+ethanol+prem.gas+reg.gas+car.vehicle+autom+auto.man+auto.shift+av, data = df)
summary(fit.first)



#residual of fit.interest
res = residuals(fit.interest)
#plot each residuals
#check normality
qqnorm(res, ylab="Residuals", xlab="Normal Scores")





