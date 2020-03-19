#print all output
options(print.max=10000000)

#create data frame
df<- data.frame(co2_emi_sample)

#rename columns to be used
library(dplyr)
df<- df %>% rename(fuel.consumption.hwy = Fuel.Consumption..Hwy..L.100.km., 
                   co2.emissions = CO2.Emissions..g.km., engine.size = Engine.Size..L., 
                   fuel.consumption.comb = Fuel.Consuption..Comb..L.100.km., 
                   fuel.consumption.city = Fuel.Consumption..City..L.100.km.,
                   fuel.consumption.comb.mpg = Fuel.Consuption..Comb..mpg.)

library(olsrr)
#fitting models with all variables gives multicolinearity- "69 not returned due to singularities"
#anova using fit.all as complete model does not produce p-values
#returns co2 rating, fuel con. comb, fuel type, engine size, make
fit.all <- lm(co2.emissions~., data = df)
summary(fit.all)
#returns co2 rating, fuel con comb, fuel type, engine size, make
ols_step_both_p(fit.all)

#fitting model only for fuel consum(comb, hwy, city), cylinders, engine size, 
#fuel type, vehicle class, transmission
fit.interest <- lm(co2.emissions~.-Model.Year -Make -Model.Name -fuel.consumption.comb.mpg -CO2.Rating -Smog.Rating, data = df)
summary(fit.interest)
#returns fuel con(comb, hwy, city), fuel type, some vehicle class
ols_step_both_p(fit.interest) # default: pent = 0.1; prem = 0.3 
# returns fuel con. comb, fuel type, engine size, transmmissions

#mutilinear regression for the 4 variables from selection variables; fit.interest
fit.sel<-lm(co2.emissions~ fuel.consumption.comb+Fuel.Type+engine.size+Transmission, data = df)
summary(fit.sel)
# returns fuel con. comb, fuel type, engine size
ols_step_both_p(fit.sel)
# returns fuel con. comb, fuel type, engine size, transmmissions
#partialF-test between fit.interest and fit.sel
anova(fit.interest, fit.sel) #p-value<0.05

fit.global<- lm(co2.emissions~ fuel.consumption.comb+fuel.consumption.city+fuel.consumption.hwy+Fuel.Type+Vehicle.Class, data = df)
summary(fit.global)
#returns fuel con.(comb,hwy,city), fuel type, some vehicle class
ols_step_both_p(fit.global)
#returns fuel.con.comb, fuel type
#partialF-test between fit.interest and fit.global
anova(fit.interest, fit.global) #p-value>0.05

#since fit.all is invalid, fit.fromall is also invalid
fit.fromall <- lm(co2.emissions~ CO2.Rating+fuel.consumption.comb+Fuel.Type+engine.size+Make, data = df)
summary(fit.fromall)
#returns co2 rating, fuel con. com., fuel type, engine size
ols_step_both_p(fit.fromall)
#returns co2 rating, fuel con comb, fuel type, engine size, make

#from all stepwise regression results- co2 rating, fuel con comb, fuel type, engine size, make, transmission
#since fit.all is invalid, cannot use co2.rating and make- results equivalent to fit.sel

#plot each variables with co2 emissions
for (i in 1:length(colnames(df))){
  if (colnames(df)[i] != "co2.emissions" & typeof(df[,i]) != "character"){
    plot(df[,i],df$co2.emissions,  ylab = "co2.emissions", xlab = colnames(df)[i])
  }
}

# partial F-test between all and all-1 models; none gives significant results, p>0.05
fit.all <- lm(co2.emissions~., data = df)
summary(fit.all)
#p-value=0.01183, Ra2=0.9998, standard error = 0.8955

fit.nomodel <- lm(co2.emissions~. -Model.Year, data = df)
summary(fit.nomodel)
anova(fit.all,fit.nomodel)
#p-value=0.01183, Ra2=0.9998, standard error = 0.8955, partial F p-value = 

fit.nocylinder <- lm(co2.emissions~. -Cylinders, data = df)
summary(fit.nocylinder)
anova(fit.all,fit.nocylinder)
#p-value=0.0001724, Ra2=0.9998, standard error = 0.794, partial F p-value = 0.5877

fit.nocomb<-lm(co2.emissions~. -fuel.consumption.comb, data = df)
summary(fit.nocomb)
anova(fit.all,fit.nocomb)
#p-value=0.001957, Ra2=0.998, standard error = 2.76, partial F p-value =0.1521

fit.nomake <- lm(co2.emissions~. -Make, data = df)
summary(fit.nomake)
anova(fit.all,fit.nomake)
#p-value=0.01183, Ra2=0.9998, standard error = 0.895, partial F p-value =

fit.notrans<-lm(co2.emissions~. -Transmission, data = df)
summary(fit.notrans)
anova(fit.all,fit.notrans)
#p-value=1.269e-08, Ra2=0.9986, standard error = 2.285, partial F p-value =0.2682

fit.nocomb.mpg<-lm(co2.emissions~. -fuel.consumption.comb.mpg, data = df)
summary(fit.nocomb.mpg)
anova(fit.all,fit.nocomb.mpg)
#p-value=0.001992, Ra2=0.998, standard error = 2.7, partial F p-value =0.1507

fit.nomodelname<-lm(co2.emissions~. -Model.Name, data = df)
summary(fit.nomodelname)
anova(fit.all,fit.nomodelname)
#p-value=2.2e-16, Ra2=0.9989, standard error = 2.028, partial F p-value =0.3394

fit.nofuel<-lm(co2.emissions~. -Fuel.Type, data = df)
summary(fit.nofuel)
anova(fit.all,fit.nofuel)
#p-value=0.002769, Ra2=0.9838, standard error = 7.667, partial F p-value =0.06743

fit.noclass<-lm(co2.emissions~. -Vehicle.Class, data = df)
summary(fit.noclass)
anova(fit.all,fit.noclass)
#p-value=0.01183, Ra2=0.9998, standard error = 0.8955, partial F p-value =

fit.nocity<-lm(co2.emissions~. -fuel.consumption.city, data = df)
summary(fit.nocity)
anova(fit.all,fit.nocity)
#p-value=0.003376, Ra2=0.9966, standard error = 3.516, partial F p-value = 0.1153

fit.noco2rating<-lm(co2.emissions~. -CO2.Rating, data = df)
summary(fit.noco2rating)
anova(fit.all,fit.noco2rating)
#p-value=0.001822, Ra2=0.9982, standard error = 2.582, partial F p-value =0.1577

fit.nosize<-lm(co2.emissions~. -engine.size, data = df)
summary(fit.nosize)
anova(fit.all,fit.nosize)
#p-value=0.002917, Ra2=0.9971, standard error = 3.268, partial F p-value =0.1241

fit.nohwy<-lm(co2.emissions~. -fuel.consumption.hwy, data = df)
summary(fit.nohwy)
anova(fit.all,fit.nohwy)
#p-value=0.002763, Ra2=0.9972, standard error = 3.18, partial F p-value =0.1276

fit.nosmog<-lm(co2.emissions~. -Smog.Rating, data = df)
summary(fit.nosmog)
anova(fit.all,fit.nosmog)
#p-value=0.002077, Ra2=0.9979, standard error = 2.757, partial F p-value =0.1475




# partial F-test between k(8) and k-1 models; 
fit.interest <- lm(co2.emissions~ Vehicle.Class+Transmission+fuel.consumption.hwy+engine.size+Fuel.Type+fuel.consumption.comb+fuel.consumption.city+Cylinders, data = df)
summary(fit.interest)
#p-value=2.2e-16, Ra2=0.9988, standard error = 2.119

fit.nocylinder2 <- lm(co2.emissions~ Vehicle.Class+Transmission+fuel.consumption.hwy+engine.size+Fuel.Type+fuel.consumption.comb+fuel.consumption.city, data = df)
summary(fit.nocylinder2)
anova(fit.interest,fit.nocylinder2)
#p-value=2.2e-16, Ra2=0.9988, standard error = 2.113, partial F p-value = 0.9141

fit.nocomb2<-lm(co2.emissions~ Vehicle.Class+Transmission+fuel.consumption.hwy+engine.size+Fuel.Type+fuel.consumption.city+Cylinders, data = df)
summary(fit.nocomb2)
anova(fit.interest,fit.nocomb2)
#p-value< 2.2e-16, Ra2=0.9987, standard error = 2.18, partial F p-value =0.001102

fit.notrans2<-lm(co2.emissions~ Vehicle.Class+fuel.consumption.hwy+engine.size+Fuel.Type+fuel.consumption.comb+fuel.consumption.city+Cylinders, data = df)
summary(fit.notrans2)
anova(fit.interest,fit.notrans2)
#p-value< 2.2e-16, Ra2=0.9987, standard error = 2.144, partial F p-value =0.236

fit.nofuel2<-lm(co2.emissions~ Vehicle.Class+Transmission+fuel.consumption.hwy+engine.size+fuel.consumption.comb+fuel.consumption.city+Cylinders, data = df)
summary(fit.nofuel2)
anova(fit.interest,fit.nofuel2)
#p-value=2.2e-16, Ra2=0.9387, standard error = 14.91, partial F p-value < 2.2e-16

fit.noclass2<-lm(co2.emissions~ Transmission+fuel.consumption.hwy+engine.size+Fuel.Type+fuel.consumption.comb+fuel.consumption.city+Cylinders, data = df)
summary(fit.noclass2)
anova(fit.interest,fit.noclass2)
#p-value< 2.2e-16, Ra2=0.9987, standard error = 2.136, partial F p-value =0.2763

fit.nocity2<-lm(co2.emissions~ Vehicle.Class+Transmission+fuel.consumption.hwy+engine.size+Fuel.Type+fuel.consumption.comb+Cylinders, data = df)
summary(fit.nocity2)
anova(fit.interest,fit.nocity2)
#p-value< 2.2e-16, Ra2=0.9987, standard error = 2.175, partial F p-value = 0.001631

fit.nosize2<-lm(co2.emissions~ Vehicle.Class+Transmission+fuel.consumption.hwy+Fuel.Type+fuel.consumption.comb+fuel.consumption.city+Cylinders, data = df)
summary(fit.nosize2)
anova(fit.interest,fit.nosize2)
#p-value< 2.2e-16, Ra2=0.9988, standard error = 2.118, partial F p-value =0.3896

fit.nohwy2<-lm(co2.emissions~ Vehicle.Class+Transmission+engine.size+Fuel.Type+fuel.consumption.comb+fuel.consumption.city+Cylinders, data = df)
summary(fit.nohwy2)
anova(fit.interest,fit.nohwy2)
#p-value< 2.2e-16, Ra2=0.9987, standard error = 2.183, partial F p-value =0.0008058

#from partial test: hwy, city, comb, fuel type
#additional: engine size, transmission
