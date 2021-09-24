
# author: "Francisco d'Albertas"

#===============================================================================

# proportion vegetation

#===============================================================================

# calculate for each property the proportion of vegetation in relation to the 
# necessary area to achieve 20% vegetation cover and comply with the forest
# code

#===== pacotes =================================================================


library(reshape2)
library(dplyr)



#====== vegetation cover data ==================================================


d <- dirname(getwd())

# opening veg cover data for certified farms

fold.nam <- "metricas"

veg_cert <- read.csv(file.path(d,fold.nam,'metricas_cobveg_certificadas.csv'))

# opening veg cover data for non-certified farms

veg_ncert <- read.csv(file.path(d,fold.nam,'metricas_cobveg_n_certificadas.csv'))

# eliminating duplicated farms

veg_cert <- veg_cert[!duplicated(veg_cert$COD_IMOVEL),]

veg_ncert <- veg_ncert[!duplicated(veg_ncert$COD_IMO),]

# opening matched farms data (will use to so select farms)

df <- read.csv('matched_properties_no_log.csv')

# subseting vegetation cover data for matched farms


veg_cert <- veg_cert[veg_cert$COD_IMOVEL %in% df$COD_IMOVEL,]

veg_ncert <- veg_ncert[veg_ncert$COD_IMO %in% df$COD_IMOVEL,]


# fixing data: the certified vegetation farm has no area info. will take that from
# the df data

area_cert <- df[df$treatment=="certified",][,c(1,8)]


# combining data

veg_cert2 <- merge(veg_cert,area_cert,by="COD_IMOVEL")

# fixing column names

veg_cert2$area_im.x <- veg_cert2$area_im.y

# removing duplicated column and name

veg_cert2 <- veg_cert2[,-42]; names(veg_cert2)[10] <- "area_im"


# removing duplicated df

veg_cert <- veg_cert2; rm(veg_cert2)

# adding treatment info.

veg_cert$treatment <- "certified"
veg_ncert$treatment <- "n_certified"

# changing df format

names(veg_ncert)

veg_cert_m <- melt(veg_cert,measure.vars = c(21:41))
veg_ncert_m <- melt(veg_ncert,measure.vars = c(26:46))



# keeping variables of interest

names(veg_ncert_m)[1] <- "COD_IMOVEL"


var <- c("COD_IMOVEL","area_im","treatment", "variable","value")

veg_cert_m <- veg_cert_m %>% select(var)
veg_ncert_m <- veg_ncert_m %>% select(var)


# transforming area to ha

veg_ncert_m$area_im <- veg_ncert_m$area_im/10^4

# combining dfs

veg <- rbind(veg_cert_m,veg_ncert_m)

# separating year


veg <- veg %>% separate(variable, c("var",'var2',"year")) 

veg <- veg[,-c(4,5)]

# renaming value column

names(veg)[5] <- "veg_ha"

# calculating area equivalent to 20%

veg$area_20 <- veg$area_im*0.2

# calculating proportion

veg$prop_cover <- veg$veg_ha/veg$area_im

