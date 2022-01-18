#===============================================================================

# calculate deforestation based on data from collection 5!

#===============================================================================

#==== packages =================================================================

library(dplyr)
library(tidyr)
library(sf)
library(ggpubr)

#===============================================================================

p <- dirname(getwd())


#-------------------------------------------------------------------------------
# abrindo regeneracao 
#-------------------------------------------------------------------------------

arquivos <- list.files(file.path(p,"GEE_col6"),pattern = ".csv",full.names = T)

# dados propriedades certificadas

reg_cert <- read.csv(arquivos[8])

reg_cert$treatment <- "certified"

# substituindo nomes

names(reg_cert)[12:41] <- paste0(seq(1988,2017,1),"_reg")

summary(reg_cert)

reg_ncert <- read.csv(arquivos[9])

reg_ncert$treatment <- "non certified"

names(reg_ncert)[12:41] <- paste0(seq(1988,2017,1),"_reg")

names(reg_ncert)[3] <- "COD_IMOVEL"

# precisa da area das propriedades!

# limite propriedades

cont <- st_read(file.path(p,"limites_propriedades","controle.shp"))

sf::sf_use_s2(FALSE)

cont$area_im <- as.numeric(st_area(cont))/10^4

reg_ncert2 <- left_join(x = reg_ncert,y = cont[,c(1,16)],by=c("COD_IMOVEL"="COD_IMO"))

names(reg_ncert2)[2] <- "COD_ESTADO"

# colunas pra manter!

k <- names(reg_cert)[c(2:4,8,12:41,45)]


reg_cert2 <- reg_cert %>% select(k)

reg_ncert2 <- reg_ncert2 %>% select(k)

reg <- rbind(reg_cert2,reg_ncert2)

names(reg)

reg_m <- reg %>% 
  pivot_longer(cols =c(5:34),names_to = "year",values_to = "area_reg" ) %>%
  separate(col = year,into = c("year","discard"),sep = "_")

reg_m <- reg_m[,-7]

summary(reg_m$area_reg[reg_m$treatment=="certified"])
summary(reg_m$area_reg[reg_m$treatment=="non certified"])

# convertendo pra ha

reg_m$area_reg <- reg_m$area_reg/10^4

rm(cont,reg,reg_cert,reg_cert2,reg_ncert,reg_ncert2)

summary(reg_m)


# taxa de regeneracao em relacao a área da propriedade!



reg_m$reg_rate <- reg_m$area_reg/reg_m$area_im

ggdensity(data = reg_m,x = "reg_rate",color = "treatment")

summary(reg_m$reg_rate)


write.csv(reg_m,"regeneration.csv",row.names = F)
