# Prerequisites

    library(data.table)
    library(olsrr)
    
    sample_CO2 <- read.csv("./data/Sample_CO2Emission.csv")
    
    View(sample_CO2[,c(13,9,10,11,6,5,8)])
    colnames(sample_CO2)
  
    sub_samp <- sample_CO2[,c(13,9,10,11,6,5,8)]
    View(sub_samp)
    
    sub_samp[,5]
    
  
    fit.all <- lm(CO2.Emissions..g.km. ~ .,data = sub_samp)  
    summary(fit.all)  
    anova(fit.all)

  
  # Selecting variables
      step(fit.all, direction = "backward")  
      
      step(fit.all, direction = "forward", scope = formula(fit.all))
      
      m1 = ols_step_both_p(fit.all, details = TRUE)
      plot(m1)
    
      m2 = ols_step_all_possible(fit.all)
      View(m2)
        
      ols_step_forward_p(fit.all)
      ols_step_backward_p(fit.all)
      
      ols_step_forward_aic(fit.all)
      ols_step_backward_aic(fit.all)
      
    #  , details = TRUE
      
      colnames(sub_samp)
      