#==== pacotes =================================================================

library(sf)
library(dplyr)
library(stringr)

#=============================================================================



# tem q puxar os shapes de APP  e cruzar tudo de novo pelo jeito, pra ter
# os limites pra subir no gee


# pra MG e SP

p <- dirname(getwd())


c <- st_read(file.path(p,"limites_propriedades","tratamento.shp"))

mun_c <- unique(c$COD_MUN)

lista_mun <- list.dirs(file.path(p,"SICAR","MG_unzip_APP"))
lista_mun_s <- grep(paste(mun_c,collapse="|"),lista_mun, value=TRUE)

# faltou 4 municipios, de SP!

sf::sf_use_s2(FALSE)# resolve spherical geometry

#==== tratamento ===============================================================


# funcao pra rodar no lapply!

f <- function(i){                    
                    
app <- st_read(i)

app_int <- st_intersection(app,propriedades)
app_agg <- app_int%>%
  group_by(COD_IMOVEL) %>%
  summarise()
#gc()
rm(app,app_int)
return(app_agg)
}

propriedades <- c

app_lista <- lapply(lista_mun_s,f) 

apps_MG_tratamento <- do.call(rbind,app_lista)

apps_MG_pol <-apps_MG_tratamento %>% 
  filter(grepl("POLYGON", st_geometry_type(geometry)))

apps_MG_geom <-apps_MG_tratamento %>% 
  filter(grepl("GEOMETRYCOLLECTION", st_geometry_type(geometry)))

apps_MG_geom_ex <- st_collection_extract(apps_MG_geom, "POLYGON")%>%
  group_by(COD_IMOVEL) %>%
  summarise()

# combine data back aqui deve dar diferenca e aparecer repeticoes!!

app_MG_F <- rbind(apps_MG_pol,apps_MG_geom_ex)


app_MG_Fagg <- app_MG_F  %>%
  group_by(COD_IMOVEL) %>%
  summarise()


nrow(app_MG_Fagg) #419


st_write(app_MG_Fagg,file.path(p,"limites_propriedades","APPS_MG_tratamento.shp"),
         append = F)

rm(app,app_agg,app_int,app_lista,app_MG_F,apps_MG,apps_MG_geom,apps_MG_geom_ex,
   apps_MG_line,apps_MG_pol)


#==== controle =================================================================

# pegar car propriedades pos matching!

df <- read.csv("data_for_panel_regression.csv")%>% 
  filter(treatment == "non certified")

car <- unique(df$COD_IMOVEL)


t <- st_read(file.path(p,"limites_propriedades","controle.shp"))%>%
  filter(COD_IMO %in% car)

names(t)[1] <- "COD_IMOVEL"


propriedades <- t


app_lista_nc <- lapply(lista_mun_s,f) 


apps_MG_nc <- do.call(rbind,app_lista_nc)


apps_MG_pol_nc <-apps_MG_nc %>% 
  filter(grepl("POLYGON", st_geometry_type(geometry)))

apps_MG_geom_nc <-apps_MG_nc %>% 
  filter(grepl("GEOMETRYCOLLECTION", st_geometry_type(geometry)))

apps_MG_geom_ex_nc <- st_collection_extract(apps_MG_geom_nc, "POLYGON")%>%
  group_by(COD_IMOVEL) %>%
  summarise()

# combine data back

app_MG_F_nc <- rbind(apps_MG_pol_nc,apps_MG_geom_ex_nc)

# aqui tem q juntar de novo!

app_MG_F_ncagg <-app_MG_F_nc  %>%
  group_by(COD_IMOVEL) %>%
  summarise()


nrow(app_MG_F_ncagg)#1733

st_write(app_MG_F_ncagg,file.path(p,"limites_propriedades","APPS_MG_controle.shp"),
         append = F)

#==== falta fazer pra SP =======================================================

# tratamento

lista_mun_sp <- list.dirs(file.path(p,"SICAR","SP_unzip_APP"))
lista_mun_s_sp <- grep(paste(mun_c,collapse="|"),lista_mun_sp, value=TRUE)

propriedades=c

app_lista_c <- lapply(lista_mun_s_sp,f) 

apps_SP_c <- do.call(rbind,app_lista_c) 


apps_SP_pol_c <-apps_SP_c %>% 
  filter(grepl("POLYGON", st_geometry_type(geometry)))

nrow(apps_SP_pol_c)


st_write(apps_SP_pol_c,file.path(p,"limites_propriedades","APPS_SP_tratamento.shp"),append = F)

# controle

propriedades=t

app_lista_nc <- lapply(lista_mun_s_sp,f) 


apps_SP_nc <- do.call(rbind,app_lista_nc) 

apps_SP_pol_nc <-apps_SP_nc %>% 
  filter(grepl("POLYGON", st_geometry_type(geometry)))

apps_SP_geom_nc <-apps_SP_nc %>% 
  filter(grepl("GEOMETRYCOLLECTION", st_geometry_type(geometry)))

apps_SP_geom_ex_nc <- st_collection_extract(apps_SP_geom_nc, "POLYGON")%>%
  group_by(COD_IMOVEL) %>%
  summarise()

# combine data back

appSP_F_nc <- rbind(apps_SP_pol_nc,apps_SP_geom_ex_nc)

appSP_F_ncagg <- appSP_F_nc %>%
  group_by(COD_IMOVEL)%>%
  summarise()


nrow(appSP_F_ncagg)

st_write(appSP_F_ncagg,file.path(p,"limites_propriedades","APPS_SP_controle.shp"),
         append = F)

#==== combinando tudo em 2 shapes unicos =======================================


shapes <- list.files(file.path(p,"limites_propriedades"),full.names = T,
                     pattern = ".shp")

f2 <- function(x)read_sf(x)

apps_cont <- lapply(shapes[c(2,4)],f2)

apps__cont_f <- do.call(rbind,apps_cont)


apps_treat <- lapply(shapes[c(3,5)],f2)

apps__treat_f <- do.call(rbind,apps_treat)

apps__treat_f$treatment <- "certified"

apps__cont_f$treatment <- "non certified"

apps_f <- rbind(apps__cont_f,apps__treat_f)

length(unique(apps_f$COD_IMOVEL))

apps_f_d <- apps_f[duplicated(apps_f$COD_IMOVEL),]#zero!

apps_f_d2 <- apps_f %>% filter(apps_f$COD_IMOVEL %in% apps_f_d$COD_IMOVEL)

apps_f_d2$area <- as.numeric(st_area(apps_f_d2))


st_geometry(apps_f_d2) <- NULL


# tem 3 porra de imovel repetido!!

# parece q nao rolou um merge decente. o lance eh refazer tudo pra converir

# se tiver tudo certo, deveriam ter :



st_write(apps_f,file.path(p,"limites_propriedades","APPS.shp"),append = F)

