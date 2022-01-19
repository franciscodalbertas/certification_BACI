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


# titulo pra legenda da figura: average effect by lenght of exposure on 
# deforestation(a)....

#---- Desmatamento -------------------------------------------------------------

#########################

# Mata Atlântica 

#########################

m1cs_ma <- att_gt(yname = "desm_rate_ly",
                  gname = "first_year",
                  idname = "id",
                  tname = "year",
                  xformla = ~ 1,
                  panel = TRUE,
                  control_group = "notyettreated",
                  data = dma)


ag_m1_ma <- aggte(m1cs_ma, type = "dynamic", min_e = -11, max_e = 8)


ma_def <- ggdid(ag_m1_ma)+
  geom_vline(xintercept=-0.5, linetype="dashed",color = "black", size=1)+
  geom_hline(yintercept=0, linetype="dashed",color = "red", size=1)+
  theme_classic()#+
  #rotate_x_text(angle = 20)

ma_def <- ggpar(ma_def,
      legend = "none",
      xlab = "time (years)",
      ylab = "average effect on deforestation",
      main = "",
      font.x=c(7,"bold"),
      font.y=c(7,"bold"),
      font.tickslab=(5))

# cerrado


m1cs_ce <- att_gt(yname = "desm_rate_ly",
                  gname = "first_year",
                  idname = "id",
                  tname = "year",
                  xformla = ~ 1,
                  panel = TRUE,
                  control_group = "notyettreated",
                  data = dce)


ag_m1_ce <- aggte(m1cs_ce, type = "dynamic", min_e = -11, max_e = 8)

ce_def <- ggdid(ag_m1_ce)+
  geom_vline(xintercept=-0.5, linetype="dashed",color = "black", size=1)+
  geom_hline(yintercept=0, linetype="dashed",color = "red", size=1)+
  theme_classic()#+
  #rotate_x_text(angle = 20)

ce_def <- ggpar(ce_def,
                legend = "none",
                xlab = "time (years)",
                ylab = "average effect on deforestation",
                main = "",
                font.x=c(7,"bold"),
                font.y=c(7,"bold"),
                font.tickslab=(5))



#---- Regeneracao  -------------------------------------------------------------

# MA

m3cs_ma <- att_gt(yname = "reg_rate",
                  gname = "first_year",
                  idname = "id",
                  tname = "year",
                  xformla = ~ 1,
                  panel = TRUE,
                  control_group = "notyettreated",
                  data = dma)


ag_m3_ma <- aggte(m3cs_ma, type = "dynamic", min_e = -11, max_e = 8)

ma_reg <- ggdid(ag_m3_ma)+
  geom_vline(xintercept=-0.5, linetype="dashed",color = "black", size=1)+
  geom_hline(yintercept=0, linetype="dashed",color = "red", size=1)+
  theme_classic()#+
  #rotate_x_text(angle = 20)


ma_reg <- ggpar(ma_reg,
                legend = "none",
                xlab = "time (years)",
                ylab = "average effect on regeneration",
                main = "",
                font.x=c(7,"bold"),
                font.y=c(7,"bold"),
                font.tickslab=(5))


# CE

m4cs_ce <- att_gt(yname = "reg_rate",
                  gname = "first_year",
                  idname = "id",
                  tname = "year",
                  xformla = ~ 1,
                  panel = TRUE,
                  control_group = "notyettreated",
                  data = dce)

ag_m4_ce <- aggte(m4cs_ce, type = "dynamic", min_e = -13, max_e = 8)

ce_reg <- ggdid(ag_m4_ce)+
  geom_vline(xintercept=-0.5, linetype="dashed",color = "black", size=1)+
  geom_hline(yintercept=0, linetype="dashed",color = "red", size=1)+
  theme_classic()#+
  #rotate_x_text(angle = 20)

ce_reg <- ggpar(ce_reg,
                legend = "none",
                xlab = "time (years)",
                ylab = "average effect on regeneration",
                main = "",
                font.x=c(7,"bold"),
                font.y=c(7,"bold"),
                font.tickslab=(5))


#---- compliance  -------------------------------------------------------------


# MA



#Nesse caso considerei apenas propriedades medias e grandes nas analises

# pegar dado de modulos fiscais

mf <- read.csv(file.path(dirname(getwd()),"certification_BACI","data_for_matching.csv"))

dma2 <- left_join(dma,mf[,c(1,13)]) %>% filter(area_im_mf>=4)

dce2 <- left_join(dce,mf[,c(1,13)]) %>% filter(area_im_mf>=4)

# calculando razao de veg nativa em relacao aos 20%

dma2$veg_ratio <- (dma2$p_veg*dma2$area_im)/(dma2$area_im*0.02)

dce2$veg_ratio <- (dce2$p_veg*dce2$area_im)/(dce2$area_im*0.02)

m5cs_ma <- att_gt(yname = "veg_ratio",
                  gname = "first_year",
                  idname = "id",
                  tname = "year",
                  xformla = ~ 1,
                  panel = TRUE,
                  control_group = "notyettreated",
                  data = dma2)

ag_m5_ma <- aggte(m5cs_ma, type = "dynamic", min_e = -11, max_e = 8)


ma_pveg <- ggdid(ag_m5_ma)+
  geom_vline(xintercept=-0.5, linetype="dashed",color = "black", size=1)+
  geom_hline(yintercept=0, linetype="dashed",color = "red", size=1)+
  theme_classic()#+
  #rotate_x_text(angle = 20)

ma_pveg <- ggpar(ma_pveg,
                legend = "none",
                xlab = "time (years)",
                ylab = "average effect on vegetation cover",
                main = "",
                font.x=c(7,"bold"),
                font.y=c(7,"bold"),
                font.tickslab=c(5))


# CE

m6cs_ce <- att_gt(yname = "p_veg",
                  gname = "first_year",
                  idname = "id",
                  tname = "year",
                  xformla = ~ 1,
                  panel = TRUE,
                  control_group = "notyettreated",
                  data = dce2)

ag_m6_ce <- aggte(m6cs_ce, type = "dynamic", min_e = -11, max_e = 8)

ce_pveg <- ggdid(ag_m6_ce)+
  geom_vline(xintercept=-0.5, linetype="dashed",color = "black", size=1)+
  geom_hline(yintercept=0, linetype="dashed",color = "red", size=1)+
  theme_classic()#+
  #rotate_x_text(angle = 20)

ce_pveg <- ggpar(ce_pveg,
                 legend = "none",
                 xlab = "time (years)",
                 ylab = "average effect on vegetation cover",
                 main = "",
                 font.x=c(7,"bold"),
                 font.y=c(7,"bold"),
                 font.tickslab=(5))





# figura em painel com todos os resultados!

# acho que fica melhor separar em MA e CE

panel_ma <- ggarrange(ma_def,ma_reg,ma_pveg,ncol = 3,labels = "auto")

panel_ce <- ggarrange(ce_def,ce_reg,ce_pveg,ncol = 3,labels = "auto")

ggsave(filename = file.path("figures","panel_regression_results_ma.jpeg"),plot = panel_ma,width = 19,
       height = 6,units = "cm")

ggsave(filename = file.path("figures","panel_regression_results_ce.jpeg"),plot = panel_ce,width = 19,
       height = 6,units = "cm")


