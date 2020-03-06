#basic initial setup

    library(data.table)
    library(ggplot2)
    require(broom)
    options(scipen = 999)
    options(scipen=0, digits=7)
    
    sample_CO2 <- fread("./data/Sample_CO2Emission.csv")
    
    summary(sample_CO2)
    colnames(sample_CO2)
    
    
    #find all the col_names of old data
    old_col_names <- colnames(sample_CO2)
    old_col_names
    
    #Set all the new col_names
    new_col_names <- c(
      "model_year",
      "make",
      "model_name",
      "vehicle_class",
      "engine_size",
      "no_of_cylinders",
      "transmission_type",
      "fuel_type",
      "city_FC",
      "hwy_FC",
      "comb_FC",
      "comb_FC(mpg)",
      "CO2_em",
      "CO2_rating",
      "smog_rating"
    )
    
    
    setnames(sample_CO2, old = old_col_names, new = new_col_names)
    
    
    View(sample_CO2)
    sample_CO2[,"fule_type"]

    
#-----------------------------------------------------------------------------------------

#CO2 Emission vs Combined Fuel Consumption

  #Data for lr testc
    colnames(sample_CO2)
    temp_Co2_vs_CombFC <- sample_CO2[,c("CO2_em" , "comb_FC")]
    View(temp_Co2_vs_CombFC)

  #Linear Model
    mod_Co2Em_vs_CombFC <- lm(temp_Co2_vs_CombFC$CO2_em ~ temp_Co2_vs_CombFC$comb_FC)
    summary(mod_Co2Em_vs_CombFC)

    cor(sample_CO2$CO2_em, sample_CO2$comb_FC)
    plot(mod_Co2Em_vs_CombFC)

    summary(mod_Co2Em_vs_CombFC$residuals)
    mod_Co2Em_vs_CombFC$residuals
    
    names(summary(mod_Co2Em_vs_CombFC))
    summary(mod_Co2Em_vs_CombFC)$r.squared
  
    glance(mod_Co2Em_vs_CombFC)
    summary(mod_Co2Em_vs_CombFC)$coefficients[,4][2]
    
    format(summary(mod_Co2Em_vs_CombFC)$coefficients[,4][2], scientific = TRUE)
    format(summary(mod_Co2Em_vs_CombFC)$coefficients[,4][2], scientific = FALSE)
    
    anova(mod_Co2Em_vs_CombFC)
    
    #Using Model for analysis
    temp_Co2_vs_CombFC
    temp_Co2_vs_CombFC$pred_CO2emission <- predict(mod_Co2Em_vs_CombFC)
    temp_Co2_vs_CombFC$residuals <- residuals(mod_Co2Em_vs_CombFC)
    temp_Co2_vs_CombFC$res_squ <- (mod_Co2Em_vs_CombFC$residuals)^2 
    
    View(temp_Co2_vs_CombFC)
    
    mean(temp_Co2_vs_CombFC$comb_FC)
    mean(temp_Co2_vs_CombFC$CO2_em)
    
    max(temp_Co2_vs_CombFC$comb_FC)
    max(temp_Co2_vs_CombFC$CO2_em)    
    
    #graph
    #Scatter Plot CO2 Emission vs Combined Fuel Consumtion
    ggplot(data = temp_Co2_vs_CombFC) +
      geom_point(mapping = aes(x=temp_Co2_vs_CombFC$comb_FC, y=temp_Co2_vs_CombFC$CO2_em, color="blue" , alpha = 1/2),show.legend = FALSE) +
      geom_hline(yintercept = mean(temp_Co2_vs_CombFC$CO2_em)) +
      geom_vline(xintercept = mean(temp_Co2_vs_CombFC$comb_FC)) +
      geom_line(color='red',data = temp_Co2_vs_CombFC, aes(x=comb_FC, y=pred_CO2emission)) +
      annotate("text", label ="Mean CO2 emission", y=mean(temp_Co2_vs_CombFC$CO2_em)+10, x=mean(temp_Co2_vs_CombFC$comb_FC)-6) +
      annotate("text", label ="Mean Combined Fuel Consumption", y=mean(temp_Co2_vs_CombFC$CO2_em)+180, x=mean(temp_Co2_vs_CombFC$comb_FC)-2.5) +
      annotate("text", label ="Linear model", y=max(temp_Co2_vs_CombFC$CO2_em)+20, x=max(temp_Co2_vs_CombFC$comb_FC-1)) +
      xlab("Combined Fuel Consuption in (L/100 km)" ) +
      ylab("CO2 Emiision in grams per kilometer") +
      ggtitle("CO2 Emission vs Combined Fuel Consuption")
    

  #2nd order Model
    temp_Co2_vs_CombFC$sq_comb_FC <-     (temp_Co2_vs_CombFC$comb_FC)^2
    temp_Co2_vs_CombFC$sq_comb_FC
    View(temp_Co2_vs_CombFC[,c('sq_comb_FC','comb_FC')])

    lm(temp_Co2_vs_CombFC$CO2_em ~ temp_Co2_vs_CombFC$comb_FC + I((temp_Co2_vs_CombFC$comb_FC)^2))
    
    mod_Co2Em_vs_sq_CombFC = lm(temp_Co2_vs_CombFC$CO2_em ~ temp_Co2_vs_CombFC$comb_FC + I((temp_Co2_vs_CombFC$comb_FC)^2))
    summary(mod_Co2Em_vs_sq_CombFC)
    
    cor(temp_Co2_vs_CombFC$CO2_em, temp_Co2_vs_CombFC$comb_FC)
    cor(temp_Co2_vs_CombFC$CO2_em, temp_Co2_vs_CombFC$sq_comb_FC)
    plot(mod_Co2Em_vs_sq_CombFC)
    
    summary(mod_Co2Em_vs_sq_CombFC$residuals)
    mod_Co2Em_vs_sq_CombFC$residuals
    

    names(summary(mod_Co2Em_vs_sq_CombFC))
    
    #R-squared
    summary(mod_Co2Em_vs_sq_CombFC)$r.squared
    
    #p-values
    glance(mod_Co2Em_vs_sq_CombFC)
    summary(mod_Co2Em_vs_sq_CombFC)$coefficients[,4]
    
    format(summary(mod_Co2Em_vs_sq_CombFC)$coefficients[,4][2], scientific = TRUE)
    format(summary(mod_Co2Em_vs_sq_CombFC)$coefficients[,4][2], scientific = FALSE)
    
    anova(mod_Co2Em_vs_sq_CombFC)    
    
    #graph
    colnames(temp_Co2_vs_CombFC)
    
    ggplot(temp_Co2_vs_CombFC, aes(comb_FC,CO2_em)) +
      geom_point() +
      geom_smooth(method = lm, level=0.95, se =TRUE)

        
#-----------------------------------------------------------------------------------------

  #CO2 Emission vs Cylinders
    

    #Data for lr testc
    colnames(sample_CO2)
    temp_Co2_vs_noCylinders <- sample_CO2[,c("CO2_em" , "no_of_cylinders")]
    View(temp_Co2_vs_noCylinders)
    
    #Linear Model
    mod_Co2Em_vs_noCylinders <- lm(temp_Co2_vs_noCylinders$CO2_em ~ temp_Co2_vs_noCylinders$no_of_cylinders)
    summary(mod_Co2Em_vs_noCylinders)
    
    cor(sample_CO2$CO2_em, sample_CO2$comb_FC)
    plot(mod_Co2Em_vs_CombFC)
    
    summary(mod_Co2Em_vs_noCylinders$residuals)
    #mod_Co2Em_vs_noCylinders$residuals
    
    names(summary(mod_Co2Em_vs_noCylinders))
    summary(mod_Co2Em_vs_noCylinders)$r.squared
    
    glance(mod_Co2Em_vs_noCylinders)
    summary(mod_Co2Em_vs_noCylinders)$coefficients[,4]
    
    anova(mod_Co2Em_vs_noCylinders)
    
    
    #Using Model for analysis
    temp_Co2_vs_noCylinders
    temp_Co2_vs_noCylinders$pred_CO2emission <- predict(mod_Co2Em_vs_noCylinders)
    temp_Co2_vs_noCylinders$residuals <- residuals(mod_Co2Em_vs_noCylinders)
    temp_Co2_vs_CombFC$res_squ <- (mod_Co2Em_vs_CombFC$residuals)^2 
    
    View(temp_Co2_vs_CombFC)    
    
#-----------------------------------------------------------------------------------------
    
    