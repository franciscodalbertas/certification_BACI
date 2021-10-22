#===============================================================================

# certification effect on land-use variables

#===============================================================================

#==== packages =================================================================

library(dplyr)
library(tidyr)

#===============================================================================


# combining control and treatment for two datasets (MA and CE)

ce <- rbind(read.csv('control_ce.csv'),read.csv('treatment_ce.csv'))
ma <- rbind(read.csv('control_ma.csv'),read.csv('treatment_ma.csv'))

# columns of interest

c <- c("COD_IMOVEL","treatment","distance","weights")

ce <- ce%>% select(c)
ma <- ma%>% select(c)
