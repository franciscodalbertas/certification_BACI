#===============================================================================

# figures with anticipation effects!

#===============================================================================


#==== packages =================================================================

library(plm)
library(lmtest)
library(multiwayvcov)
library(sandwich)
library(clubSandwich)
library(stargazer)
library(ggplot2)
library(Rcpp)
library(tidyr)
library(dplyr)
library(grid)
library(gridExtra)
library(kableExtra)
library(knitr)
library(DT)
library(xtable)
library(pander)
library(Hmisc)
library(ggthemes)
library(did)
library(ggpubr)

#===============================================================================

#Data

data_cert <- read.csv("data_for_panel_regression.csv")

data_cert$first_year <- as.integer(substring(data_cert$Date.Issued, 1, 4))

# convert first year to NA

data_cert$first_year[is.na(data_cert$Date.Issued)] <- 0 

dcert <- data_cert[data_cert$year > 2004, ]

dcert$id <- as.numeric(as.factor(dcert$COD_IMOVEL))

dcert$certification_cat <- as.factor(dcert$certification_cat)

# dividindo em ma e ce

dma <- dcert[dcert$biome=="Atlantic Forest",]

dce <- dcert[dcert$biome=="Cerrado",]


# pegar dado de modulos fiscais

mf <- read.csv(file.path(dirname(getwd()),"certification_BACI","data_for_matching.csv"))

dma2 <- left_join(dma,mf[,c(1,13)]) %>% filter(area_im_mf>=4)

dce2 <- left_join(dce,mf[,c(1,13)]) %>% filter(area_im_mf>=4)


# df de proporcao de veg nas apps

apps <- read.csv("app_forest_cover.csv")

# adicionando a planilha de dados so com medias e grandes!


apps_ma <- apps %>% 
  filter(COD_IMOVEL %in% dma2$COD_IMOVEL)

dma2_app <- left_join(dma2,apps_ma)

dma2_app<- dma2_app %>% 
  mutate(papp = replace(value, is.na(value), 0))

# cerrado

apps_ce<- apps %>% 
  filter(COD_IMOVEL %in% dce2$COD_IMOVEL)

dce2_app <- left_join(dce2,apps_ce)

dce2_app <- dce2_app %>% 
  mutate(papp = replace(value, is.na(value), 0))

# Calculando o deficit de vegetacao, ao inves de usar a a prop. de veg!



################################################################################

# 1 ano de latencia

################################################################################


dcert_l <- data_cert[data_cert$year > 1999, ]

dcert_l$id <- as.numeric(as.factor(dcert_l$COD_IMOVEL))

dcert_l$certification_cat <- as.factor(dcert_l$certification_cat)

# dividindo em ma e ce

dma_l <- dcert_l[dcert_l$biome=="Atlantic Forest",]

dce_l <- dcert_l[dcert_l$biome=="Cerrado",]


# olhando 1 anos pra tras como periodo de latencia

dce_l <- dce_l %>% mutate( first_year_l1= first_year-1)

dma_l <- dma_l %>% mutate( first_year_l1= first_year-1)

# adiconando dados de compliance


dma2_l <- left_join(dma_l,mf[,c(1,13)]) %>% filter(area_im_mf>=4)

dce2_l <- left_join(dce_l,mf[,c(1,13)]) %>% filter(area_im_mf>=4)


# adicionando a planilha de dados so com medias e grandes!

dma2_app_l <- left_join(dma2_l,apps_ma)

dma2_app_l<- dma2_app_l %>% 
  mutate(papp = replace(value, is.na(value), 0))

# cerrado

apps_ce_l<- apps %>% 
  filter(COD_IMOVEL %in% dce2_l$COD_IMOVEL)

dce2_app_l <- left_join(dce2_l,apps_ce_l)

dce2_app_l <- dce2_app_l %>% 
  mutate(papp = replace(value, is.na(value), 0))


library(sf)

apps <- st_read(file.path("G:\\Meu Drive\\Doutorado\\cap3\\",
                          "limites_propriedades","APPS.shp"))

# ajeitando pra MA

apps_ma <- apps %>% filter(COD_IMOVEL %in% dma2_app_l$COD_IMOVEL)

sf::sf_use_s2(FALSE)

apps_ma$APP_area_ha <- as.numeric(st_area(apps_ma)/10^4)

st_geometry(apps_ma) <- NULL

dma2_app_l2 <- left_join(dma2_app_l,apps_ma)

dma2_app_l2$APP_area_ha[is.na(dma2_app_l2$APP_area_ha)] <- 0

# ajeitando pro CE

apps_ce <- apps %>% filter(COD_IMOVEL %in% dce2_app_l$COD_IMOVEL)

apps_ce$APP_area_ha <- as.numeric(st_area(apps_ce)/10^4)

st_geometry(apps_ce) <- NULL

dce2_app_l2 <- left_join(dce2_app_l,apps_ce)

dce2_app_l2$APP_area_ha[is.na(dce2_app_l2$APP_area_ha)] <- 0

###############################################################################

# MA

# calculando deficit fora de APP

dma2_app_l2 <- dma2_app_l2 %>% 
  mutate(deficit_noAPP= (area_im* 0.2)- (area_im* p_veg)- (APP_area_ha)) %>%
  mutate(deficit_noAPP = replace(deficit_noAPP, deficit_noAPP<0, 0)) %>%
  mutate(deficit_noAPP_prop= deficit_noAPP/area_im)

# cerrado


# calculando deficit fora de APP

dce2_app_l2 <- dce2_app_l2 %>% 
  mutate(deficit_noAPP= (area_im* 0.2)- (area_im* p_veg)- (APP_area_ha)) %>%
  mutate(deficit_noAPP = replace(deficit_noAPP, deficit_noAPP<0, 0)) %>%
  mutate(deficit_noAPP_prop= deficit_noAPP/area_im)



#---- modelos ------------------------------------------------------------------

# dados com todas as propriedades: dma_l
# dados so com medias e grandes: dma2_app_l

#### desmatamento Mata Atlantica ###############################################


ml_ma_01 <- att_gt(yname = "desm_rate_ly",
                   gname = "first_year_l1",
                   idname = "id",
                   tname = "year",
                   xformla = ~ 1,
                   panel = TRUE,
                   control_group = "notyettreated",
                   data = dma_l)

ag_ma_01 <- aggte(ml_ma_01, type = "dynamic", min_e = -11, max_e = 8)

# grafico do modelo agregado

ma_01_g <- ggdid(ag_ma_01 )+
  geom_vline(xintercept=-0.5, linetype="dashed",color = "black", size=1)+
  geom_hline(yintercept=0, linetype="dashed",color = "red", size=1)+
  theme_classic()

ma_def_l1 <- ggpar(ma_01_g,
                  legend = "none",
                  xlab = "time (years)",
                  ylab = "deforestation rate",
                  main = "",
                  font.x=c(7,"bold"),
                  font.y=c(7,"bold"),
                  font.tickslab=(5))

#### regeneracao Mata Atlantica ################################################

ml_ma_02 <- att_gt(yname = "reg_rate",
                   gname = "first_year_l1",
                   idname = "id",
                   tname = "year",
                   xformla = ~ 1,
                   panel = TRUE,
                   control_group = "notyettreated",
                   data = dma_l)

ag_ma_02 <- aggte(ml_ma_02, type = "dynamic", min_e = -11, max_e = 8)

# grafico do modelo agregado

ma_02_g <- ggdid(ag_ma_02 )+
  geom_vline(xintercept=-0.5, linetype="dashed",color = "black", size=1)+
  geom_hline(yintercept=0, linetype="dashed",color = "red", size=1)+
  theme_classic()

ma_reg_l1 <- ggpar(ma_02_g,
                   legend = "none",
                   xlab = "time (years)",
                   ylab = "regeneration rate",
                   main = "",
                   font.x=c(7,"bold"),
                   font.y=c(7,"bold"),
                   font.tickslab=(5))

#### deficit veg Mata Atlantica ################################################

ml_ma_03 <- att_gt(yname = "deficit_noAPP_prop",
                   gname = "first_year_l1",
                   idname = "id",
                   tname = "year",
                   xformla = ~ 1,
                   panel = TRUE,
                   control_group = "notyettreated",
                   data = dma2_app_l2)

ag_ma_03 <- aggte(ml_ma_03, type = "dynamic", min_e = -11, max_e = 8)

ma_03_g  <- ggdid(ag_ma_03)+
  geom_vline(xintercept=-0.5, linetype="dashed",color = "black", size=1)+
  geom_hline(yintercept=0, linetype="dashed",color = "red", size=1)+
  theme_classic()

# grafico do modelo agregado

ma_deficit_l1 <- ggpar(ma_03_g,
                 legend = "none",
                 xlab = "time (years)",
                 ylab = "vegetation deficit",
                 main = "",
                 font.x=c(7,"bold"),
                 font.y=c(7,"bold"),
                 font.tickslab=(5))

### APP ########################################################################

ml_ma_04 <- att_gt(yname = "papp",
                   gname = "first_year_l1",
                   idname = "id",
                   tname = "year",
                   xformla = ~ 1,
                   panel = TRUE,
                   control_group = "notyettreated",
                   data = dma2_app_l2)

ag_ma_04 <- aggte(ml_ma_04, type = "dynamic", min_e = -11, max_e = 8)

# grafico do modelo agregado

ma_04_g  <- ggdid(ag_ma_04)+
  geom_vline(xintercept=-0.5, linetype="dashed",color = "black", size=1)+
  geom_hline(yintercept=0, linetype="dashed",color = "red", size=1)+
  theme_classic()


ma_app_l1 <- ggpar(ma_04_g,
                  legend = "none",
                  xlab = "time (years)",
                  ylab = "APP vegetation",
                  main = "",
                  font.x=c(7,"bold"),
                  font.y=c(7,"bold"),
                  font.tickslab=(5))

#### grafico em painel com 3 plots (menos app) #################################

anticipation_1 <- ggarrange(ma_def_l1,ma_reg_l1,ma_deficit_l1,ncol=3,labels = "auto")


ggsave(filename = file.path("figures","panel_antic_1year_ma.jpeg"),
       plot = anticipation_1,width = 18,height = 6,units = "cm")

################################################################################

# 2 ano de latencia

################################################################################

# olhando 2 anos pra tras como periodo de latencia

# df completo
dce_l <- dce_l %>% mutate( first_year_l2= first_year-2)

dma_l <- dma_l %>% mutate( first_year_l2= first_year-2)

# df de compliance

dma2_app_l2 <- dma2_app_l2 %>% mutate( first_year_l2= first_year-2)

dce2_app_l2 <- dce2_app_l2  %>% mutate( first_year_l2= first_year-2)

#### desmatamento Mata Atlantica ###############################################


ml_ma_05 <- att_gt(yname = "desm_rate_ly",
                   gname = "first_year_l2",
                   idname = "id",
                   tname = "year",
                   xformla = ~ 1,
                   panel = TRUE,
                   control_group = "notyettreated",
                   data = dma_l)

ag_ma_05 <- aggte(ml_ma_05, type = "dynamic", min_e = -11, max_e = 8)

# grafico do modelo agregado

ma_05_g <- ggdid(ag_ma_05 )+
  geom_vline(xintercept=-0.5, linetype="dashed",color = "black", size=1)+
  geom_hline(yintercept=0, linetype="dashed",color = "red", size=1)+
  theme_classic()

ma_def_l2 <- ggpar(ma_05_g,
                   legend = "none",
                   xlab = "time (years)",
                   ylab = "deforestation rate",
                   main = "",
                   font.x=c(7,"bold"),
                   font.y=c(7,"bold"),
                   font.tickslab=(5))

#### regeneracao Mata Atlantica ################################################

ml_ma_06 <- att_gt(yname = "reg_rate",
                   gname = "first_year_l2",
                   idname = "id",
                   tname = "year",
                   xformla = ~ 1,
                   panel = TRUE,
                   control_group = "notyettreated",
                   data = dma_l)

ag_ma_06 <- aggte(ml_ma_06, type = "dynamic", min_e = -11, max_e = 8)

# grafico do modelo agregado

ma_06_g <- ggdid(ag_ma_06 )+
  geom_vline(xintercept=-0.5, linetype="dashed",color = "black", size=1)+
  geom_hline(yintercept=0, linetype="dashed",color = "red", size=1)+
  theme_classic()

ma_reg_l2 <- ggpar(ma_06_g,
                   legend = "none",
                   xlab = "time (years)",
                   ylab = "regeneration rate",
                   main = "",
                   font.x=c(7,"bold"),
                   font.y=c(7,"bold"),
                   font.tickslab=(5))

#### deficit veg Mata Atlantica ################################################

ml_ma_07 <- att_gt(yname = "deficit_noAPP_prop",
                   gname = "first_year_l2",
                   idname = "id",
                   tname = "year",
                   xformla = ~ 1,
                   panel = TRUE,
                   control_group = "notyettreated",
                   data = dma2_app_l2)

ag_ma_07 <- aggte(ml_ma_07, type = "dynamic", min_e = -11, max_e = 8)

ma_07_g  <- ggdid(ag_ma_07)+
  geom_vline(xintercept=-0.5, linetype="dashed",color = "black", size=1)+
  geom_hline(yintercept=0, linetype="dashed",color = "red", size=1)+
  theme_classic()

# grafico do modelo agregado

ma_deficit_l2 <- ggpar(ma_07_g,
                       legend = "none",
                       xlab = "time (years)",
                       ylab = "vegetation deficit",
                       main = "",
                       font.x=c(7,"bold"),
                       font.y=c(7,"bold"),
                       font.tickslab=(5))
### APP ########################################################################

ml_ma_08 <- att_gt(yname = "papp",
                   gname = "first_year_l2",
                   idname = "id",
                   tname = "year",
                   xformla = ~ 1,
                   panel = TRUE,
                   control_group = "notyettreated",
                   data = dma2_app_l2)

ag_ma_08 <- aggte(ml_ma_08, type = "dynamic", min_e = -11, max_e = 8)

# grafico do modelo agregado

ma_08_g  <- ggdid(ag_ma_08)+
  geom_vline(xintercept=-0.5, linetype="dashed",color = "black", size=1)+
  geom_hline(yintercept=0, linetype="dashed",color = "red", size=1)+
  theme_classic()


ma_app_l2 <- ggpar(ma_08_g,
                   legend = "none",
                   xlab = "time (years)",
                   ylab = "APP vegetation",
                   main = "",
                   font.x=c(7,"bold"),
                   font.y=c(7,"bold"),
                   font.tickslab=(5))

#### grafico em painel com 3 plots (menos app) #################################

anticipation_2 <- ggarrange(ma_def_l2,ma_reg_l2,ma_deficit_l2,ncol=3,labels = "auto")


ggsave(filename = file.path("figures","panel_antic_2year_ma.jpeg"),
       plot = anticipation_2,width = 18,height = 6,units = "cm")

################################################################################

# 3 ano de latencia

################################################################################

# olhando 2 anos pra tras como periodo de latencia

# df completo
dce_l <- dce_l %>% mutate( first_year_l3= first_year-3)

dma_l <- dma_l %>% mutate( first_year_l3= first_year-3)

# df de compliance

dma2_app_l2 <- dma2_app_l2 %>% mutate( first_year_l3= first_year-3)

dce2_app_l2 <- dce2_app_l2  %>% mutate( first_year_l3= first_year-3)

#### desmatamento Mata Atlantica ###############################################


ml_ma_09 <- att_gt(yname = "desm_rate_ly",
                   gname = "first_year_l3",
                   idname = "id",
                   tname = "year",
                   xformla = ~ 1,
                   panel = TRUE,
                   control_group = "notyettreated",
                   data = dma_l)

ag_ma_09 <- aggte(ml_ma_09, type = "dynamic", min_e = -11, max_e = 8)

# grafico do modelo agregado

ma_09_g <- ggdid(ag_ma_09 )+
  geom_vline(xintercept=-0.5, linetype="dashed",color = "black", size=1)+
  geom_hline(yintercept=0, linetype="dashed",color = "red", size=1)+
  theme_classic()

ma_def_l3 <- ggpar(ma_09_g,
                   legend = "none",
                   xlab = "time (years)",
                   ylab = "deforestation rate",
                   main = "",
                   font.x=c(7,"bold"),
                   font.y=c(7,"bold"),
                   font.tickslab=(5))
#### regeneracao Mata Atlantica ################################################

ml_ma_10 <- att_gt(yname = "reg_rate",
                   gname = "first_year_l3",
                   idname = "id",
                   tname = "year",
                   xformla = ~ 1,
                   panel = TRUE,
                   control_group = "notyettreated",
                   data = dma_l)

ag_ma_10 <- aggte(ml_ma_10, type = "dynamic", min_e = -11, max_e = 8)

# grafico do modelo agregado

ma_10_g <- ggdid(ag_ma_10 )+
  geom_vline(xintercept=-0.5, linetype="dashed",color = "black", size=1)+
  geom_hline(yintercept=0, linetype="dashed",color = "red", size=1)+
  theme_classic()

ma_reg_l3 <- ggpar(ma_10_g,
                   legend = "none",
                   xlab = "time (years)",
                   ylab = "regeneration rate",
                   main = "",
                   font.x=c(7,"bold"),
                   font.y=c(7,"bold"),
                   font.tickslab=(5))

#### deficit veg Mata Atlantica ################################################

ml_ma_11 <- att_gt(yname = "deficit_noAPP_prop",
                   gname = "first_year_l3",
                   idname = "id",
                   tname = "year",
                   xformla = ~ 1,
                   panel = TRUE,
                   control_group = "notyettreated",
                   data = dma2_app_l2)

ag_ma_11 <- aggte(ml_ma_11, type = "dynamic", min_e = -11, max_e = 8)

ma_11_g  <- ggdid(ag_ma_11)+
  geom_vline(xintercept=-0.5, linetype="dashed",color = "black", size=1)+
  geom_hline(yintercept=0, linetype="dashed",color = "red", size=1)+
  theme_classic()

# grafico do modelo agregado

ma_deficit_l3 <- ggpar(ma_11_g,
                       legend = "none",
                       xlab = "time (years)",
                       ylab = "vegetation deficit",
                       main = "",
                       font.x=c(7,"bold"),
                       font.y=c(7,"bold"),
                       font.tickslab=(5))

### APP ########################################################################

ml_ma_12 <- att_gt(yname = "papp",
                   gname = "first_year_l3",
                   idname = "id",
                   tname = "year",
                   xformla = ~ 1,
                   panel = TRUE,
                   control_group = "notyettreated",
                   data = dma2_app_l2)

ag_ma_12 <- aggte(ml_ma_12, type = "dynamic", min_e = -11, max_e = 8)

# grafico do modelo agregado

ma_12_g  <- ggdid(ag_ma_12)+
  geom_vline(xintercept=-0.5, linetype="dashed",color = "black", size=1)+
  geom_hline(yintercept=0, linetype="dashed",color = "red", size=1)+
  theme_classic()


ma_app_l3 <- ggpar(ma_12_g,
                   legend = "none",
                   xlab = "time (years)",
                   ylab = "APP vegetation",
                   main = "",
                   font.x=c(7,"bold"),
                   font.y=c(7,"bold"),
                   font.tickslab=(5))

### grafico em painel com 3 plots (menos app) #################################

anticipation_3 <- ggarrange(ma_def_l3,ma_reg_l3,ma_deficit_l3,ncol=3,labels = "auto")


ggsave(filename = file.path("figures","panel_antic_3year_ma.jpeg"),
       plot = anticipation_3,width = 18,height = 6,units = "cm")


### plotando antecipacao pra APP ###############################################



app_ant <- ggarrange(ma_app_l1,ma_app_l2,ma_app_l3,nrow = 3,labels = "auto")

ggsave(filename = file.path("figures","panel_antic_apps_ma.jpeg"),
       plot = app_ant,width = 6,height = 15,units = "cm")

#---- modelos cerrado ----------------------------------------------------------

################################################################################

# 1 ano de latencia

################################################################################


#### desmatamento Cerrado ######################################################


ml_ce_01 <- att_gt(yname = "desm_rate_ly",
                   gname = "first_year_l1",
                   idname = "id",
                   tname = "year",
                   xformla = ~ 1,
                   panel = TRUE,
                   control_group = "notyettreated",
                   data = dce_l)

ag_ce_01 <- aggte(ml_ce_01, type = "dynamic", min_e = -11, max_e = 8)

# grafico do modelo agregado

ce_01_g <- ggdid(ag_ce_01 )+
  geom_vline(xintercept=-0.5, linetype="dashed",color = "black", size=1)+
  geom_hline(yintercept=0, linetype="dashed",color = "red", size=1)+
  theme_classic()

ce_def_l1 <- ggpar(ce_01_g,
                   legend = "none",
                   xlab = "time (years)",
                   ylab = "deforestation rate",
                   main = "",
                   font.x=c(7,"bold"),
                   font.y=c(7,"bold"),
                   font.tickslab=(5))

#### regeneracao Cerrado #######################################################

ml_ce_02 <- att_gt(yname = "reg_rate",
                   gname = "first_year_l1",
                   idname = "id",
                   tname = "year",
                   xformla = ~ 1,
                   panel = TRUE,
                   control_group = "notyettreated",
                   data = dce_l)

ag_ce_02 <- aggte(ml_ce_02, type = "dynamic", min_e = -11, max_e = 8)

# grafico do modelo agregado

ce_02_g <- ggdid(ag_ce_02 )+
  geom_vline(xintercept=-0.5, linetype="dashed",color = "black", size=1)+
  geom_hline(yintercept=0, linetype="dashed",color = "red", size=1)+
  theme_classic()

ce_reg_l1 <- ggpar(ce_02_g,
                   legend = "none",
                   xlab = "time (years)",
                   ylab = "regeneration rate",
                   main = "",
                   font.x=c(7,"bold"),
                   font.y=c(7,"bold"),
                   font.tickslab=(5))

#### deficit veg Cerrado #######################################################

ml_ce_03 <- att_gt(yname = "deficit_noAPP_prop",
                   gname = "first_year_l1",
                   idname = "id",
                   tname = "year",
                   xformla = ~ 1,
                   panel = TRUE,
                   control_group = "notyettreated",
                   data = dce2_app_l2)

ag_ce_03 <- aggte(ml_ce_03, type = "dynamic", min_e = -11, max_e = 8)

ce_03_g  <- ggdid(ag_ce_03)+
  geom_vline(xintercept=-0.5, linetype="dashed",color = "black", size=1)+
  geom_hline(yintercept=0, linetype="dashed",color = "red", size=1)+
  theme_classic()

# grafico do modelo agregado

ce_deficit_l1 <- ggpar(ce_03_g,
                       legend = "none",
                       xlab = "time (years)",
                       ylab = "vegetation deficit",
                       main = "",
                       font.x=c(7,"bold"),
                       font.y=c(7,"bold"),
                       font.tickslab=(5))

### APP ########################################################################

ml_ce_04 <- att_gt(yname = "papp",
                   gname = "first_year_l1",
                   idname = "id",
                   tname = "year",
                   xformla = ~ 1,
                   panel = TRUE,
                   control_group = "notyettreated",
                   data = dce2_app_l2)

ag_ce_04 <- aggte(ml_ce_04, type = "dynamic", min_e = -11, max_e = 8)

# grafico do modelo agregado

ce_04_g  <- ggdid(ag_ce_04)+
  geom_vline(xintercept=-0.5, linetype="dashed",color = "black", size=1)+
  geom_hline(yintercept=0, linetype="dashed",color = "red", size=1)+
  theme_classic()


ce_app_l1 <- ggpar(ce_04_g,
                   legend = "none",
                   xlab = "time (years)",
                   ylab = "APP vegetation",
                   main = "",
                   font.x=c(7,"bold"),
                   font.y=c(7,"bold"),
                   font.tickslab=(5))

#### grafico em painel com 4 plots e APP #######################################

anticipation_1_ce <- ggarrange(ce_def_l1,ce_reg_l1,ce_deficit_l1,ce_app_l1,labels = "auto")


ggsave(filename = file.path("figures","panel_antic_1year_ce.jpeg"),
       plot = anticipation_1_ce,width = 14,height = 12,units = "cm")

################################################################################

# 2 ano de latencia

################################################################################


#### desmatamento Cerrado ######################################################


ml_ce_05 <- att_gt(yname = "desm_rate_ly",
                   gname = "first_year_l2",
                   idname = "id",
                   tname = "year",
                   xformla = ~ 1,
                   panel = TRUE,
                   control_group = "notyettreated",
                   data = dce_l)

ag_ce_05 <- aggte(ml_ce_05, type = "dynamic", min_e = -11, max_e = 8)

# grafico do modelo agregado

ce_05_g <- ggdid(ag_ce_05 )+
  geom_vline(xintercept=-0.5, linetype="dashed",color = "black", size=1)+
  geom_hline(yintercept=0, linetype="dashed",color = "red", size=1)+
  theme_classic()

ce_def_l2 <- ggpar(ce_05_g,
                   legend = "none",
                   xlab = "time (years)",
                   ylab = "deforestation rate",
                   main = "",
                   font.x=c(7,"bold"),
                   font.y=c(7,"bold"),
                   font.tickslab=(5))

#### regeneracao Cerrado #######################################################

ml_ce_06 <- att_gt(yname = "reg_rate",
                   gname = "first_year_l2",
                   idname = "id",
                   tname = "year",
                   xformla = ~ 1,
                   panel = TRUE,
                   control_group = "notyettreated",
                   data = dce_l)

ag_ce_06 <- aggte(ml_ce_06, type = "dynamic", min_e = -11, max_e = 8)

# grafico do modelo agregado

ce_06_g <- ggdid(ag_ce_06 )+
  geom_vline(xintercept=-0.5, linetype="dashed",color = "black", size=1)+
  geom_hline(yintercept=0, linetype="dashed",color = "red", size=1)+
  theme_classic()

ce_reg_l2 <- ggpar(ce_06_g,
                   legend = "none",
                   xlab = "time (years)",
                   ylab = "regeneration rate",
                   main = "",
                   font.x=c(7,"bold"),
                   font.y=c(7,"bold"),
                   font.tickslab=(5))

#### deficit veg Cerrado #######################################################

ml_ce_07 <- att_gt(yname = "deficit_noAPP_prop",
                   gname = "first_year_l2",
                   idname = "id",
                   tname = "year",
                   xformla = ~ 1,
                   panel = TRUE,
                   control_group = "notyettreated",
                   data = dce2_app_l2)

ag_ce_07 <- aggte(ml_ce_07, type = "dynamic", min_e = -11, max_e = 8)

ce_07_g  <- ggdid(ag_ce_07)+
  geom_vline(xintercept=-0.5, linetype="dashed",color = "black", size=1)+
  geom_hline(yintercept=0, linetype="dashed",color = "red", size=1)+
  theme_classic()

# grafico do modelo agregado

ce_deficit_l2 <- ggpar(ce_07_g,
                       legend = "none",
                       xlab = "time (years)",
                       ylab = "vegetation deficit",
                       main = "",
                       font.x=c(7,"bold"),
                       font.y=c(7,"bold"),
                       font.tickslab=(5))

### APP ########################################################################

ml_ce_08 <- att_gt(yname = "papp",
                   gname = "first_year_l2",
                   idname = "id",
                   tname = "year",
                   xformla = ~ 1,
                   panel = TRUE,
                   control_group = "notyettreated",
                   data = dce2_app_l2)

ag_ce_08 <- aggte(ml_ce_08, type = "dynamic", min_e = -11, max_e = 8)

# grafico do modelo agregado

ce_08_g  <- ggdid(ag_ce_08)+
  geom_vline(xintercept=-0.5, linetype="dashed",color = "black", size=1)+
  geom_hline(yintercept=0, linetype="dashed",color = "red", size=1)+
  theme_classic()


ce_app_l2 <- ggpar(ce_08_g,
                   legend = "none",
                   xlab = "time (years)",
                   ylab = "APP vegetation",
                   main = "",
                   font.x=c(7,"bold"),
                   font.y=c(7,"bold"),
                   font.tickslab=(5))

#### grafico em painel com 4 plots e APP #######################################

anticipation_2_ce <- ggarrange(ce_def_l2,ce_reg_l2,ce_deficit_l2,ce_app_l2,labels = "auto")


ggsave(filename = file.path("figures","panel_antic_2year_ce.jpeg"),
       plot = anticipation_2_ce,width = 14,height = 12,units = "cm")

################################################################################

# 3 ano de latencia

################################################################################


#### desmatamento Cerrado ######################################################


ml_ce_09 <- att_gt(yname = "desm_rate_ly",
                   gname = "first_year_l3",
                   idname = "id",
                   tname = "year",
                   xformla = ~ 1,
                   panel = TRUE,
                   control_group = "notyettreated",
                   data = dce_l)

ag_ce_09 <- aggte(ml_ce_09, type = "dynamic", min_e = -11, max_e = 8)

# grafico do modelo agregado

ce_09_g <- ggdid(ag_ce_09 )+
  geom_vline(xintercept=-0.5, linetype="dashed",color = "black", size=1)+
  geom_hline(yintercept=0, linetype="dashed",color = "red", size=1)+
  theme_classic()

ce_def_l3 <- ggpar(ce_09_g,
                   legend = "none",
                   xlab = "time (years)",
                   ylab = "deforestation rate",
                   main = "",
                   font.x=c(7,"bold"),
                   font.y=c(7,"bold"),
                   font.tickslab=(5))

#### regeneracao Cerrado #######################################################

ml_ce_10 <- att_gt(yname = "reg_rate",
                   gname = "first_year_l3",
                   idname = "id",
                   tname = "year",
                   xformla = ~ 1,
                   panel = TRUE,
                   control_group = "notyettreated",
                   data = dce_l)

ag_ce_10 <- aggte(ml_ce_10, type = "dynamic", min_e = -11, max_e = 8)

# grafico do modelo agregado

ce_10_g <- ggdid(ag_ce_10 )+
  geom_vline(xintercept=-0.5, linetype="dashed",color = "black", size=1)+
  geom_hline(yintercept=0, linetype="dashed",color = "red", size=1)+
  theme_classic()

ce_reg_l3 <- ggpar(ce_10_g,
                   legend = "none",
                   xlab = "time (years)",
                   ylab = "regeneration rate",
                   main = "",
                   font.x=c(7,"bold"),
                   font.y=c(7,"bold"),
                   font.tickslab=(5))

#### deficit veg Cerrado #######################################################

ml_ce_11 <- att_gt(yname = "deficit_noAPP_prop",
                   gname = "first_year_l3",
                   idname = "id",
                   tname = "year",
                   xformla = ~ 1,
                   panel = TRUE,
                   control_group = "notyettreated",
                   data = dce2_app_l2)

ag_ce_11 <- aggte(ml_ce_11, type = "dynamic", min_e = -11, max_e = 8)

ce_11_g  <- ggdid(ag_ce_11)+
  geom_vline(xintercept=-0.5, linetype="dashed",color = "black", size=1)+
  geom_hline(yintercept=0, linetype="dashed",color = "red", size=1)+
  theme_classic()

# grafico do modelo agregado

ce_deficit_l3 <- ggpar(ce_11_g,
                       legend = "none",
                       xlab = "time (years)",
                       ylab = "vegetation deficit",
                       main = "",
                       font.x=c(7,"bold"),
                       font.y=c(7,"bold"),
                       font.tickslab=(5))

### APP ########################################################################

ml_ce_12 <- att_gt(yname = "papp",
                   gname = "first_year_l3",
                   idname = "id",
                   tname = "year",
                   xformla = ~ 1,
                   panel = TRUE,
                   control_group = "notyettreated",
                   data = dce2_app_l2)

ag_ce_12 <- aggte(ml_ce_12, type = "dynamic", min_e = -11, max_e = 8)

# grafico do modelo agregado

ce_12_g  <- ggdid(ag_ce_12)+
  geom_vline(xintercept=-0.5, linetype="dashed",color = "black", size=1)+
  geom_hline(yintercept=0, linetype="dashed",color = "red", size=1)+
  theme_classic()


ce_app_l3 <- ggpar(ce_12_g,
                   legend = "none",
                   xlab = "time (years)",
                   ylab = "APP vegetation",
                   main = "",
                   font.x=c(7,"bold"),
                   font.y=c(7,"bold"),
                   font.tickslab=(5))

#### grafico em painel com 4 plots e APP #######################################

anticipation_3_ce <- ggarrange(ce_def_l3,ce_reg_l3,ce_deficit_l3,ce_app_l3,labels = "auto")


ggsave(filename = file.path("figures","panel_antic_3year_ce.jpeg"),
       plot = anticipation_3_ce,width = 14,height = 12,units = "cm")

