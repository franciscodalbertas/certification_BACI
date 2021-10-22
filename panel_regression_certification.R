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

# picking temporal data for the selected properties

p <- dirname(getwd()) # parental folder

path_full_data <- "metricas\\melted_tables" # path to access metrics calculated 
                                            # for all properties

l <- list.files(file.path(p,path_full_data),full.names = T) #listing all the files

f <- function(x)read.csv(x,row.names = 1)

#---- deforestation ------------------------------------------------------------

def <- lapply(l[1:3],f) # data on deforestation

## columns to keep from the metrics

nm <- c("COD_IMOVEL","year","def_rate_1")

f2 <- function(x) x%>% select(nm)

def2 <- lapply(def,f2)

def_df <- do.call(rbind,def2)

all_properties <- unique(c(ce$COD_IMOVEL,ma$COD_IMOVEL))

## subset deforestation for selected properties
def_df_s <-def_df[def_df$COD_IMOVEL %in% all_properties,]

rm(def,def_df,def2)
#---- regeneration -------------------------------------------------------------

reg <- lapply(l[15:17],f) # data on deforestation

## columns to keep from the metrics

nm <- c("COD_IMOVEL","year","prop_reg")

f2 <- function(x) x%>% select(nm)

reg2 <- lapply(reg,f2)

reg_df <- do.call(rbind,reg2)

all_properties <- unique(c(ce$COD_IMOVEL,ma$COD_IMOVEL))

## subset reg for selected properties
reg_df_s <-reg_df[reg_df$COD_IMOVEL %in% all_properties,]

rm(reg,reg_df,reg2)

#---- prop cover ---------------------------------------------------------------

pveg <- lapply(l[12:14],f) # data on deforestation

## columns to keep from the metrics

nm <- c("COD_IMOVEL","year","prop_cover")

f2 <- function(x) x%>% select(nm)

pveg2 <- lapply(pveg,f2)

pveg_df <- do.call(rbind,pveg2)

all_properties <- unique(c(ce$COD_IMOVEL,ma$COD_IMOVEL))

## subset pveg for selected properties
pveg_df_s <-pveg_df[pveg_df$COD_IMOVEL %in% all_properties,]

rm(pveg,pveg_df,pveg2)

# merging into one data frame

temp_data <- cbind(def_df_s,reg_df_s[,3],pveg_df_s[,3])

names(temp_data)[4:5] <- c("reg_rate","p_veg")
