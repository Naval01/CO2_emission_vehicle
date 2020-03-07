#Analyzing CO2 Emission

    library(data.table)
    
    sample_CO2 <- read.csv("./data/Sample_CO2Emission.csv")
    typeof(sample_CO2$CO2.Emissions..g.km.)
    
    
    #Statistical Analysis
    
    mean(sample_CO2$CO2.Emissions..g.km.)
    median(sample_CO2$CO2.Emissions..g.km.)
    
    var(sample_CO2$CO2.Emissions..g.km.)
    sd(sample_CO2$CO2.Emissions..g.km.)
    
    stat = summary(sample_CO2$CO2.Emissions..g.km.)
    stat
    
    iqr <-  stat[5] - stat[2] 
    iqr
    
    lower_lim <- stat[2] - 1.5*iqr
    upper_lim <- stat[5] + 1.5*iqr
    lower_lim
    upper_lim
    
    outlier_lower <- d[which(d < lower.lim)]
    outlier_upper <- d[which(d > upper.lim)]
    
    
    #Histogram of CO2 Emission
    
    ggplot(data = sample_CO2) + 
      geom_histogram(mapping=aes(x = CO2.Emissions..g.km., fill="red"),  binwidth=5, origin=0) +
      theme_update() +
      scale_y_continuous(name = "Number Of Cars") +
    #  scale_x_continuous(name = "CO2 emissions (g/km)" , breaks = seq(min(sample_CO2$CO2.Emissions..g.km.), max(sample_CO2$CO2.Emissions..g.km.),50)) +
      scale_fill_manual(name = "Color", values = "LIGHTBLUE", labels(NULL)) +
      xlab("CO2 emissions (g/km)") +
      ggtitle("Analyzing CO2 Emissions")
    
    
    #Box Plot
    
    ggplot(data = sample_CO2) + 
      scale_y_continuous(name = "CO2 emissions (g/km)") +
      geom_boxplot(mapping=aes(y = CO2.Emissions..g.km.)) +
      scale_x_continuous(name = NULL,) +
      theme_update() + 
      ggtitle("Analyzing TIme Spent by students in Games")
    
    boxplot(sample_CO2$CO2.Emissions..g.km.,
            main = "CO2 emissions (g/km)")
    
    