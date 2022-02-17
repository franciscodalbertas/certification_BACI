#===============================================================================

# figures with main results to insert in the manuscript

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


data_cert$first_year[is.na(data_cert$Date.Issued)] <- 0 # convert first year to NA

dcert <- data_cert[data_cert$year > 2004, ]

dcert$id <- as.numeric(as.factor(dcert$COD_IMOVEL))

dcert$certification_cat <- as.factor(dcert$certification_cat)

# dividindo em ma e ce

dma <- dcert[dcert$biome=="Atlantic Forest",]

dce <- dcert[dcert$biome=="Cerrado",]


# substituir pveg por pdeficit!





#---- Desmatamento -------------------------------------------------------------

################################################################################

# Mata Atlântica 

################################################################################

m1cs_ma <- att_gt(yname = "desm_rate_ly",
                  gname = "first_year",
                  idname = "id",
                  tname = "year",
                  xformla = ~ 1,
                  panel = TRUE,
                  control_group = "notyettreated",
                  data = dma)

ag_m1_ma <- aggte(m1cs_ma, type = "dynamic", min_e = -11, max_e = 8)


#### tabela do modelo agregado ###############################################

tabela_ma_desm <- as.data.frame(capture.output(summary(ag_m1_ma)))

tabela_ma_desm_1 <- data.frame(tabela_ma_desm[15:32,])

names(tabela_ma_desm_1) <- "output"


write.csv(tabela_ma_desm_1,"tables/desm_ma_pd.csv",row.names = F)



#### grafico do modelo agregado ###############################################


ma_def <- ggdid(ag_m1_ma)+
  geom_vline(xintercept=-0.5, linetype="dashed",color = "black", size=1)+
  geom_hline(yintercept=0, linetype="dashed",color = "red", size=1)+
  theme_classic()#+
  #rotate_x_text(angle = 20)

ma_def <- ggpar(ma_def,
      legend = "none",
      xlab = "time (years)",
      ylab = "deforestation rate",
      main = "",
      font.x=c(7,"bold"),
      font.y=c(7,"bold"),
      font.tickslab=(5))


################################################################################

# cerrado

################################################################################


m1cs_ce <- att_gt(yname = "desm_rate_ly",
                  gname = "first_year",
                  idname = "id",
                  tname = "year",
                  xformla = ~ 1,
                  panel = TRUE,
                  control_group = "notyettreated",
                  data = dce)


ag_m1_ce <- aggte(m1cs_ce, type = "dynamic", min_e = -11, max_e = 8)


#### tabela do modelo agregado ###############################################

tabela_ce_desm <- as.data.frame(capture.output(summary(ag_m1_ce)))

tabela_ce_desm_1 <- data.frame(tabela_ce_desm[15:34,])

names(tabela_ce_desm_1) <- "output"

write.csv(tabela_ce_desm_1,"tables/desm_ce_pd.csv",row.names = F)

#### grafico do modelo agregado ###############################################

ce_def <- ggdid(ag_m1_ce)+
  geom_vline(xintercept=-0.5, linetype="dashed",color = "black", size=1)+
  geom_hline(yintercept=0, linetype="dashed",color = "red", size=1)+
  theme_classic()#+
  #rotate_x_text(angle = 20)

ce_def <- ggpar(ce_def,
                legend = "none",
                xlab = "time (years)",
                ylab = "deforestation rate",
                main = "",
                font.x=c(7,"bold"),
                font.y=c(7,"bold"),
                font.tickslab=(5))



#---- Regeneracao  -------------------------------------------------------------


###############################################################################

# MA

################################################################################

m3cs_ma <- att_gt(yname = "reg_rate",
                  gname = "first_year",
                  idname = "id",
                  tname = "year",
                  xformla = ~ 1,
                  panel = TRUE,
                  control_group = "notyettreated",
                  data = dma)


ag_m3_ma <- aggte(m3cs_ma, type = "dynamic", min_e = -11, max_e = 8)

#### tabela do modelo agregado ###############################################

tabela_ma_reg <- as.data.frame(capture.output(summary(ag_m3_ma)))

tabela_ma_reg_1 <- data.frame(tabela_ma_reg[15:32,])

names(tabela_ma_reg_1) <- "output"

write.csv(tabela_ma_reg_1,"tables/reg_ma_pd.csv",row.names = F)

#### grafico do modelo agregado ###############################################


ma_reg <- ggdid(ag_m3_ma)+
  geom_vline(xintercept=-0.5, linetype="dashed",color = "black", size=1)+
  geom_hline(yintercept=0, linetype="dashed",color = "red", size=1)+
  theme_classic()#+
  #rotate_x_text(angle = 20)


ma_reg <- ggpar(ma_reg,
                legend = "none",
                xlab = "time (years)",
                ylab = "regeneration rate",
                main = "",
                font.x=c(7,"bold"),
                font.y=c(7,"bold"),
                font.tickslab=(5))
################################################################################

# CE

################################################################################

m4cs_ce <- att_gt(yname = "reg_rate",
                  gname = "first_year",
                  idname = "id",
                  tname = "year",
                  xformla = ~ 1,
                  panel = TRUE,
                  control_group = "notyettreated",
                  data = dce)

ag_m4_ce <- aggte(m4cs_ce, type = "dynamic", min_e = -13, max_e = 8)

#### tabela do modelo agregado ###############################################

tabela_ce_reg <- as.data.frame(capture.output(summary(ag_m4_ce)))

tabela_ce_reg_1 <- data.frame(tabela_ce_reg[15:34,])

names(tabela_ce_reg_1) <- "output"

write.csv(tabela_ce_reg_1,"tables/reg_ce_pd.csv",row.names = F)

#### grafico do modelo agregado ###############################################


ce_reg <- ggdid(ag_m4_ce)+
  geom_vline(xintercept=-0.5, linetype="dashed",color = "black", size=1)+
  geom_hline(yintercept=0, linetype="dashed",color = "red", size=1)+
  theme_classic()#+
  #rotate_x_text(angle = 20)

ce_reg <- ggpar(ce_reg,
                legend = "none",
                xlab = "time (years)",
                ylab = "regeneration rate",
                main = "",
                font.x=c(7,"bold"),
                font.y=c(7,"bold"),
                font.tickslab=(5))


#---- compliance  -------------------------------------------------------------

###############################################################################

# MA

##############################################################################


#Nesse caso considerei apenas propriedades medias e grandes nas analises

# pegar dado de modulos fiscais

mf <- read.csv(file.path(dirname(getwd()),"certification_BACI","data_for_matching.csv"))

dma2 <- left_join(dma,mf[,c(1,13)]) %>% filter(area_im_mf>=4)

dce2 <- left_join(dce,mf[,c(1,13)]) %>% filter(area_im_mf>=4)


# desisti de calcular razao de veg nativa em relacao aos 20%

#dma2$veg_ratio <- (dma2$p_veg*dma2$area_im)/(dma2$area_im*0.02)

#dce2$veg_ratio <- (dce2$p_veg*dce2$area_im)/(dce2$area_im*0.02)

m5cs_ma <- att_gt(yname = "p_veg",
                  gname = "first_year",
                  idname = "id",
                  tname = "year",
                  xformla = ~ 1,
                  panel = TRUE,
                  control_group = "notyettreated",
                  data = dma2)

ag_m5_ma <- aggte(m5cs_ma, type = "dynamic", min_e = -11, max_e = 8)

#### tabela do modelo agregado ###############################################

tabela_ma_pveg <- as.data.frame(capture.output(summary(ag_m5_ma)))

tabela_ma_pveg_1 <- data.frame(tabela_ma_pveg[15:32,])

names(tabela_ma_pveg_1) <- "output"

write.csv(tabela_ma_pveg_1,"tables/pveg_ma_pd.csv",row.names = F)

#### grafico do modelo agregado ###############################################


ma_pveg <- ggdid(ag_m5_ma)+
  geom_vline(xintercept=-0.5, linetype="dashed",color = "black", size=1)+
  geom_hline(yintercept=0, linetype="dashed",color = "red", size=1)+
  theme_classic()#+
  #rotate_x_text(angle = 20)

ma_pveg <- ggpar(ma_pveg,
                legend = "none",
                xlab = "time (years)",
                ylab = "vegetation cover",
                main = "",
                font.x=c(7,"bold"),
                font.y=c(7,"bold"),
                font.tickslab=c(5))

################################################################################

# CE

################################################################################

m6cs_ce <- att_gt(yname = "p_veg",
                  gname = "first_year",
                  idname = "id",
                  tname = "year",
                  xformla = ~ 1,
                  panel = TRUE,
                  control_group = "notyettreated",
                  data = dce2)

ag_m6_ce <- aggte(m6cs_ce, type = "dynamic", min_e = -11, max_e = 8)


#### tabela do modelo agregado ###############################################

tabela_ce_pveg <- as.data.frame(capture.output(summary(ag_m6_ce)))

tabela_ce_pveg_1 <- data.frame(tabela_ce_pveg[15:34,])

names(tabela_ce_pveg_1) <- "output"

write.csv(tabela_ce_pveg_1,"tables/pveg_ce_pd.csv",row.names = F)

#### grafico do modelo agregado ###############################################


ce_pveg <- ggdid(ag_m6_ce)+
  geom_vline(xintercept=-0.5, linetype="dashed",color = "black", size=1)+
  geom_hline(yintercept=0, linetype="dashed",color = "red", size=1)+
  theme_classic()#+
  #rotate_x_text(angle = 20)

ce_pveg <- ggpar(ce_pveg,
                 legend = "none",
                 xlab = "time (years)",
                 ylab = "vegetation cover",
                 main = "",
                 font.x=c(7,"bold"),
                 font.y=c(7,"bold"),
                 font.tickslab=(5))



#---- APPs ---------------------------------------------------------------------

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

################################################################################

# Mata Atlantica

################################################################################

m9cs_ma <- att_gt(yname = "papp",
                  gname = "first_year",
                  idname = "id",
                  tname = "year",
                  xformla = ~ 1,
                  panel = TRUE,
                  control_group = "notyettreated",
                  data = dma2_app)

ag_m9_ma <- aggte(m9cs_ma, type = "dynamic", min_e = -11, max_e = 8)

#### tabela do modelo agregado ###############################################

tabela_ma_app <- as.data.frame(capture.output(summary(ag_m9_ma)))

tabela_ma_app_1 <- data.frame(tabela_ma_app[15:32,])

names(tabela_ma_app_1) <- "output"

write.csv(tabela_ma_app_1,"tables/app_ma_pd.csv",row.names = F)

#### grafico do modelo agregado ###############################################

m9g_ma <- ggdid(ag_m9_ma)+
  geom_vline(xintercept=-0.5, linetype="dashed",color = "black", size=1)+
  geom_hline(yintercept=0, linetype="dashed",color = "red", size=1)+
  theme_classic()


ma_app <- ggpar(m9g_ma,
                 legend = "none",
                 xlab = "time (years)",
                 ylab = "APP vegetation cover",
                 main = "",
                 font.x=c(7,"bold"),
                 font.y=c(7,"bold"),
                 font.tickslab=(5))

################################################################################

# Cerrado

################################################################################

m9cs_ce <- att_gt(yname = "papp",
                  gname = "first_year",
                  idname = "id",
                  tname = "year",
                  xformla = ~ 1,
                  panel = TRUE,
                  control_group = "notyettreated",
                  data = dce2_app)
#(m3_group <- aggte(m3cs, type = "group"))

ag_m9_ce <- aggte(m9cs_ce, type = "dynamic", min_e = -11, max_e = 8)

#### tabela do modelo agregado ###############################################

tabela_ce_app <- as.data.frame(capture.output(summary(ag_m9_ce)))

tabela_ce_app_1 <- data.frame(tabela_ce_app[15:34,])

names(tabela_ce_app_1) <- "output"

write.csv(tabela_ce_app_1,"tables/app_ce_pd.csv",row.names = F)

#### grafico do modelo agregado ###############################################

m9g_ce <- ggdid(ag_m9_ce)+
  geom_vline(xintercept=-0.5, linetype="dashed",color = "black", size=1)+
  geom_hline(yintercept=0, linetype="dashed",color = "red", size=1)+
  theme_classic()

ce_app <- ggpar(m9g_ce,
               legend = "none",
               xlab = "time (years)",
               ylab = "APP vegetation cover",
               main = "",
               font.x=c(7,"bold"),
               font.y=c(7,"bold"),
               font.tickslab=(5))





#===============================================================================

# avaliando vegetacao sem app (proxy pra RL)


# nesse caso fazer area de deficit menos area de APP. Usar o df menor de medias
# e grandes

###############################################################################

# !!!  a area de APP nao ta certa! precisaria corrigir isso. !!!!
# pq tem proporcao app associada a area 0 de app.

# pegar de novo area de app?

library(sf)

apps <- st_read(file.path("G:\\Meu Drive\\Doutorado\\cap3\\",
                          "limites_propriedades","APPS.shp"))

# ajeitando pra MA

apps_ma <- apps %>% filter(COD_IMOVEL %in% dma2_app$COD_IMOVEL)

sf::sf_use_s2(FALSE)

apps_ma$APP_area_ha <- as.numeric(st_area(apps_ma)/10^4)

st_geometry(apps_ma) <- NULL

dma2_app_2 <- left_join(dma2_app,apps_ma)

dma2_app_2$APP_area_ha[is.na(dma2_app_2$APP_area_ha)] <- 0

# ajeitando pro CE

apps_ce <- apps %>% filter(COD_IMOVEL %in% dce2_app$COD_IMOVEL)

apps_ce$APP_area_ha <- as.numeric(st_area(apps_ce)/10^4)

st_geometry(apps_ce) <- NULL

dce2_app_2 <- left_join(dce2_app,apps_ce)

dce2_app_2$APP_area_ha[is.na(dce2_app_2$APP_area_ha)] <- 0


###############################################################################

# MA

# calculando deficit fora de APP

dma2_app_2 <- dma2_app_2 %>% 
  mutate(deficit_noAPP= (area_im* 0.2)- (area_im* p_veg)- (APP_area_ha)) %>%
  mutate(deficit_noAPP = replace(deficit_noAPP, deficit_noAPP<0, 0)) %>%
  mutate(deficit_noAPP_prop= deficit_noAPP/area_im)

# cerrado


# calculando deficit fora de APP

dce2_app_2 <- dce2_app_2 %>% 
  mutate(deficit_noAPP= (area_im* 0.2)- (area_im* p_veg)- (APP_area_ha)) %>%
  mutate(deficit_noAPP = replace(deficit_noAPP, deficit_noAPP<0, 0)) %>%
  mutate(deficit_noAPP_prop= deficit_noAPP/area_im)

###############################################################################

# Mata Atlantica

################################################################################

m14cs_ma <- att_gt(yname = "deficit_noAPP_prop",
                   gname = "first_year",
                   idname = "id",
                   tname = "year",
                   xformla = ~ 1,
                   panel = TRUE,
                   control_group = "notyettreated",
                   data = dma2_app_2)

ag_m14_ma <- aggte(m14cs_ma, type = "dynamic", min_e = -11, max_e = 8)

#### tabela do modelo agregado ###############################################

tabela_ma_def <- as.data.frame(capture.output(summary(ag_m14_ma)))

tabela_ma_def_1 <- data.frame(tabela_ma_def[15:32,])

names(tabela_ma_def_1) <- "output"

write.csv(tabela_ma_def_1,"tables/def_ma_pd.csv",row.names = F)

#### grafico do modelo agregado ###############################################

m14g_ma <- ggdid(ag_m14_ma)+
  geom_vline(xintercept=-0.5, linetype="dashed",color = "black", size=1)+
  geom_hline(yintercept=0, linetype="dashed",color = "red", size=1)+
  theme_classic()


ma_rl_l <- ggpar(m14g_ma,
                  legend = "none",
                  xlab = "time (years)",
                  ylab = "vegetation deficit",
                  main = "",
                  font.x=c(7,"bold"),
                  font.y=c(7,"bold"),
                  font.tickslab=(5))

################################################################################

# Cerrado

################################################################################

m14cs_ce <- att_gt(yname = "deficit_noAPP_prop",
                   gname = "first_year",
                   idname = "id",
                   tname = "year",
                   xformla = ~ 1,
                   panel = TRUE,
                   control_group = "notyettreated",
                   data = dce2_app_2)
#(m3_group <- aggte(m3cs, type = "group"))

ag_m14_ce <- aggte(m14cs_ce, type = "dynamic", min_e = -11, max_e = 8)

#### tabela do modelo agregado ###############################################

tabela_ce_def <- as.data.frame(capture.output(summary(ag_m14_ce)))

tabela_ce_def_1 <- data.frame(tabela_ce_def[15:34,])

names(tabela_ce_def_1) <- "output"

write.csv(tabela_ce_def_1,"tables/def_ce_pd.csv",row.names = F)

#### grafico do modelo agregado ###############################################

m14g_ce <- ggdid(ag_m14_ce)+
  geom_vline(xintercept=-0.5, linetype="dashed",color = "black", size=1)+
  geom_hline(yintercept=0, linetype="dashed",color = "red", size=1)+
  theme_classic()

ce_rl_l <- ggpar(m14g_ce,
                  legend = "none",
                  xlab = "time (years)",
                  ylab = "vegetation deficit",
                  main = "",
                  font.x=c(7,"bold"),
                  font.y=c(7,"bold"),
                  font.tickslab=(5))


# # salvar apenas analise deficit
# 
# library(egg)
# 
# detach("package:egg", unload=TRUE)
# 
# fig_deficitt <- ggarrange(ma_rl_l,ce_rl_l,ncol=2)
# 
# 
# ggsave(filename = file.path("figures","veg_deficit_latency.jpeg"),
#        plot = fig_deficitt,width = 14,height = 6,units = "cm")
# 

#---- figura em painel com todos os resultados! --------------------------------

# acho que fica melhor separar em MA e CE (substitui pelo deficit!)

panel_ma <- ggarrange(ma_def,ma_reg,ma_rl_l,ma_app,labels = "auto")

panel_ce <- ggarrange(ce_def,ce_reg,ce_rl_l,ce_app,labels = "auto")

ggsave(filename = file.path("figures","panel_regression_results_ma_deficit.jpeg"),plot = panel_ma,width = 14,
       height = 12,units = "cm")

ggsave(filename = file.path("figures","panel_regression_results_ce_deficit.jpeg"),plot = panel_ce,width = 14,
       height = 12,units = "cm")


ggsave(filename = file.path("figures","panel_regression_results_ce_deficit.jpeg"),plot = panel_ce,width = 14,
       height = 12,units = "cm")
