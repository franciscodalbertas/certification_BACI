
# encontrando declividade media entre propriedades

control_ma <- read.csv("control_ma.csv")
treatment_ma <- read.csv("treatment_ma.csv")

ma <- rbind(control_ma,treatment_ma)

control_ce <- read.csv("control_ce.csv")
treatment_ce <- read.csv("treatment_ce.csv")

ce <- rbind(control_ce,treatment_ce)

library(dplyr)

slope_ma <- ma %>% 
  summarise(mean=mean(mean_slope),sd=sd(mean_slope))

slope_ce <- ce %>% 
  summarise(mean=mean(mean_slope),sd=sd(mean_slope))
