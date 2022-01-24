
library(ggpubr)
library(tidyr)
library(sf)

apps_cover <- read.csv(file.path(p,"GEE_col6","apps_forest_cover_prop_col5.csv"))

apps_m <- pivot_longer(apps_cover,cols = c(3:37))

apps_m <-apps_m %>% 
  select(names(apps_m)[c(2,3,5,6)])%>%
  separate(col = name,into = c("discard","year"))

apps_m <- apps_m[,-3]

apps_d <- apps_m[duplicated(paste0(apps_m$COD_IMOVEL,apps_m$treatment,apps_m$year)),]


write.csv(apps_m,"app_forest_cover.csv",row.names = F)


ggboxplot(apps_m,x = "year",y = "value",fill="treatment")


apps <- st_read(file.path(p,"limites_propriedades","APPS.shp"))

