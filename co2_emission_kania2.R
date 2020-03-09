co2_emi_sample <- read.csv("C:/Users/galz_/Desktop/DANA 4801/project/CO2_emission_vehicle/data/Sample_CO2Emission.csv")
attach(co2_emi_sample)

library(corpcor)
cor2pcor(cov(X))

#create data frame
df<- data.frame(co2_emi_sample)

#rename columns to be used
library(dplyr)
df<- df %>% rename(fuel.consumption.hwy = Fuel.Consumption..Hwy..L.100.km., 
                   co2.emissions = CO2.Emissions..g.km., engine.size = Engine.Size..L., 
                   fuel.consumption.comb = Fuel.Consuption..Comb..L.100.km., 
                   fuel.consumption.city = Fuel.Consumption..City..L.100.km.,
                   fuel.consumption.comb.mpg = Fuel.Consuption..Comb..mpg.)

#get all possible regression model by all-possible-regression selection 
library(olsrr)
fit.var.sel <- lm(co2.emissions~ Vehicle.Class+Transmission+engine.size+Fuel.Type+fuel.consumption.comb+Cylinders, data = df)
summary(fit.var.sel)
reg.combi<- ols_step_all_possible(fit.var.sel)

#plot re.combi results: R, R-squared, Cp, AIC, SBC, SBIC
#from graphs (baased on Cp, AIC,, SBC, SBIC): 3 variables- Vehicle.Class Fuel.Type fuel.consumption.comb
plot(reg.combi) 

#plot each variables with co2 emissions (y)
for (i in 1:length(colnames(df))){
  if (colnames(df)[i] != "co2.emissions"){
    plot(df[,i],df$co2.emissions,  ylab = "co2.emissions", xlab = colnames(df)[i])
  }
}

#fuel type, 4 levels; 3 dummy variable
#diesel,D, as baseline
df$ethanol <- rep(0,length(df$Fuel.Type)) #E
df$ethanol[df$Fuel.Type == "E"]<-1

df$reg.gas<- rep(0,length(df$Fuel.Type)) #X
df$reg.gas[df$Fuel.Type == "X"]<-1

df$prem.gas<- rep(0,length(df$Fuel.Type)) #Z
df$prem.gas[df$Fuel.Type == "Z"]<-1

##TO EDIT CODE: character matching instead of listing values
#vehicle class categorized into cars and trucks, 2 levels, 1 dummy variable
#cars: two-seater, minicompact, subcompact, compact, mid-size, full-size, station wagon (small & mid-size)
#trucks: pick-up truck(small & standard), SUV (small& standard), minivan, passenger van, special purpose vehicle 
df$car.vehicle <- rep(0, length(df$Vehicle.Class))
for (i in 1:length(df$Vehicle.Class))
  {
  if (df$Vehicle.Class[i] == "Two-seater" | df$Vehicle.Class[i] =="Minicompact" | df$Vehicle.Class[i] =="Subcompact" | 
      df$Vehicle.Class[i] =="Compact" | df$Vehicle.Class[i] =="Mid-size" | df$Vehicle.Class[i] =="Full-size" | 
      df$Vehicle.Class[i] =="Station wagon: Small" | df$Vehicle.Class[i] =="Station wagon: Mid-size")
    {
    df$car.vehicle[i] <-1
    }
}


#initial fitting of model only for fuel consumption comb, fuel type, vehicle class (linear 1st order r/s, no interaction)
fit.first <- lm(co2.emissions~ fuel.consumption.comb+ethanol+prem.gas+reg.gas+car.vehicle, data = df)
summary(fit.first)


#fit model with interaction terms: fuel consumption comb, fuel type and vehicle class
fit.inter <- lm(co2.emissions~ fuel.consumption.comb+ethanol+prem.gas+reg.gas+car.vehicle+
                  fuel.consumption.comb*ethanol+fuel.consumption.comb*prem.gas+fuel.consumption.comb*reg.gas+fuel.consumption.comb*car.vehicle+
                  car.vehicle*ethanol+car.vehicle*prem.gas+car.vehicle*reg.gas+
                  fuel.consumption.comb*ethanol*car.vehicle+fuel.consumption.comb*prem.gas*car.vehicle+fuel.consumption.comb*reg.gas*car.vehicle, data = df)
summary(fit.inter)
#colinearity present, to check car.vehicle and fuel type

#use stepwise regression to decide which corelated independent variable(s) to drop
#drop car. vehicle
ols_step_both_p(fit.first)


#fit model w/ interaction terms: fuel consumption and fuel type, fuel consumption and vehicle class; no interaction b/wn fuel type and vehicle class
#removing colinearity
fit.inter.reduced <- lm(co2.emissions~ fuel.consumption.comb+ethanol+prem.gas+reg.gas+car.vehicle+
                          fuel.consumption.comb*ethanol+fuel.consumption.comb*prem.gas+fuel.consumption.comb*reg.gas+fuel.consumption.comb*car.vehicle, data = df)
summary(fit.inter.reduced)


#fit model w/ interaction terms: fuel consumption and fuel type
fit.inter.reduced2 <- lm(co2.emissions~ fuel.consumption.comb+ethanol+prem.gas+reg.gas+
                           fuel.consumption.comb*ethanol+fuel.consumption.comb*prem.gas+fuel.consumption.comb*reg.gas, data = df)
summary(fit.inter.reduced2)

#partial F-test w/ fit.inter.reduced gives p-value>0.05. Fail to reject Ho 
#car.vehicle not significant; 
#final model: fit.inter.reduced2
anova(fit.inter.reduced, fit.inter.reduced2)


##detecting lack of fit
#residual plot: residuals vs fuel consumption 
#no trend in plot, ~95% residuals within 2s of 0
#sd=1.442
res <- residuals(fit.inter.reduced2)
plot(df$fuel.consumption.comb, res, ylab="Residuals", xlab="Fuel Consumption Comb", main="Residuals VS Fuel Consumption Comb")
stan.resid[(stan.resid >2.884) | (stan.resid < -2.884)]

##detecting lack of fit and unequal variances
#residual plot: residuals vs predicted y
#no trend in plot, ~95% residuals within 2s of 0, no transformation needed
pred <- predict(fit.inter.reduced2)
plot(pred, res, ylab="Residuals", xlab="Predicted CO2 Emission", main="Residuals VS Predicted CO2 Emission")

#double-checked order of fuel consumption and goodnes of fit
#straight line; statistically good fit
termplot(fit.inter.reduced2, partial.resid = TRUE, ylab="Partial Residuals", xlab="Fuel Consumption Comb", main="Partial Residual Plot of Fuel Consumption Comb")

#check normality
qqnorm(res, ylab="Residuals", xlab="Normal Scores")
#some outliers observed

#identify outliers
stan.resid <- rstandard(fit.inter.reduced2)
plot(df$fuel.consumption.comb, stan.resid, ylab="Standardized Residuals", xlab="Fuel Consumption Comb", main="Standardized Residual Plot of Fuel Consumption Comb")
abline(0,0)
abline(3,0) #show regions w/ outliers
abline(-3,0) #show regions w/ outliers
stan.resid[(stan.resid >3) | (stan.resid < -3)]

#create above graph highlighting outliers
#decide what to do with outliers
#figure indices of outliers in fuel consumption comb
#change base level too avoid categorical multicolinearity


