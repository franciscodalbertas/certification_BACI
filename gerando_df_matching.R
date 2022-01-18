################################################################################
#### pacotes
################################################################################

library(sf)
library(dplyr)
library(tidyr)

#################################################################################

# Os dados estarao no github (mas preciso eliminar o CAR antes)

# pasta raiz
p <- dirname(getwd())

# pasta com as planilhas longitudinais

d <- "metricas/melted_tables"

# listando arquvivos

planilhas <- list.files(file.path(p,d),full.names = T)

################################################################################
#### gerando a planilha de dados nao temporais!
################################################################################

# *** OBS *** : uma vez gerada, nao preciso rodar esse trecho novamente salvo
# mudanças nos dados

# abrindo propriedades certificadas:

# p_propriedades <- file.path(p,"limites_propriedades")
#
# treatment <- st_read(file.path(p_propriedades,"tratamento.shp"))
#
# # excluindo info. espacial
#
# st_geometry(treatment) <- NULL
#
#
# control <- st_read(file.path(p_propriedades,"controle.shp"))
#
#
# control <- st_make_valid(control)
# control$area_im <- as.numeric(st_area(control)/10^4)
#
# # excluindo info. espacial
#
# st_geometry(control) <- NULL
#
# # adicionando coluna tratamento
#
# treatment$treatment <- "certified"
#
# names(control)[1] <- "COD_IMOVEL"
#
# control$treatment <- "non certified"
#
# # selecionando colunas
#
# nomes <- c("COD_IMOVEL","areaRL_m","areaAPP_m","treatment","area_im")
#
# df <- rbind(treatment %>% select(nomes),control %>% select(nomes))
# 
# #################################
# # transformando rl em binario
# ################################
# df$LR_bin <- NA
# 
# df$LR_bin[df$areaRL_m>0] <- 1
# df$LR_bin[df$areaRL_m==0] <- 0
# 
# ################################
# # calculando proporcao APP
# ################################
# 
# # area app em ha
# 
# df$propAPP <- (df$areaAPP_m/10^4)/df$area_im
# 
# 
# # tratamento tem valores estranhos de proporcao de app. excluir!
# 
# bizarro <- df[df$propAPP>1,]
# 
# df <- df[!df$COD_IMOVEL %in% bizarro$COD_IMOVEL,]
# 
# #######################################################################
# #### atribuindo declividade 
# #######################################################################
# 
# # planilhas correspondestes a slope
# 
# slope <- lapply(planilhas[c(5,6,7)],FUN = function(x)read.csv(x)) 
# 
# # 1 e 2 sao das certidicadas
# 
# slope_cer <- rbind(slope[[1]][c(2,13)],slope[[2]][c(1,14)])
# 
# slope_treat <- slope[[3]][c(2,17)]
# 
# names(slope_treat)[1] <- "COD_IMOVEL"
# 
# slope2 <- rbind(slope_cer,slope_treat)
# 
# # combinando
# 
# df2 <- left_join(df,slope2)
# 
# summary(df2) # 48 NAs!!
# 
# nas <- df2[is.na(df2$mean_slope),] # nao calculei pra essas (deixar de fora por
# # enquanto)
# 
# df2 <- df2[!is.na(df2$mean_slope),]
# 
# ##############################################################
# #### adicionando bioma
# #############################################################
# 
# 
# p_b <- file.path(p,"metricas")
# 
# bioma <- read.csv(file.path(p_b,"biomas_1.csv"))
# 
# df3 <- left_join(df2,bioma) 
# 
# ###############################################
# #### modulos fiscais
# ###############################################
# 
# p_mf <- file.path(p,"tabela_modulo_fiscal")
# 
# mf <- read.csv(file.path(p_mf,"mf_84Mun.csv"))
# 
# head(mf)
# 
# # criando coluna com cod_mun no df
# 
# df4 <- df3 %>% separate(col = COD_IMOVEL,into = c("UF","MUN","CAR"))
# 
# df4 <- cbind(df3,df4[,c(2)])
# 
# names(df4)[11] <- "code_muni"
# 
# mf$code_muni <- as.character(mf$code_muni)
# 
# df4 <- left_join(df4,mf) 
# 
# head(df4)
# 
# # calculando area em mf 
# 
# df4$area_im_mf <- df4$area_im/df4$MF.ha.
# 
# summary(df4$area_im_mf) # tem mtos NAs!
# 
# 
# ######################################################
# #### planilha com dados nao anuais
# #######################################################
# 
# # salvando os dados. Falta combinar com os dados anuais
# 
# names(df4)
# 
# nomes <- names(df4)[c(1,4,5,6,7,8,10,11,12,13,16,17)] 
# 
# df5 <- df4 %>% select(nomes)
# 
# write.csv(df5,"dados_n_temporais.csv",row.names = F)

# removendo dados

#rm(df,df2,df3,df4m,bioma,biomas_rep,bizarrom,certificadas,control,mf,nas,Nas,slope,
#   slope_cer,slope_treat,slope2,treatment,bizarro,df4,zeros)


#### agregando dados temporais


df <- read.csv("dados_n_temporais.csv")

head(df)

##############################################
#### desmatamento agregado 
##############################################

# abrindo dados de desmatamento

# pasta com as planilhas longitudinais

d <- "certification_Baci"


desm <- read.csv(file.path(p,d,"vegetation_deforestation.csv"))

# somando taxa desmatamento

desm_acc <- desm %>%
  filter( year<=2009&year>=2004)%>%
  group_by(COD_IMOVEL)%>%
  summarise(desm_ac=sum(desm_rate_ly))



df2 <- left_join(df,desm_acc) # gera NAS! pq?

length(df2$COD_IMOVEL[df2$treatment=="certified"]) # 537, ok!


rm(desm_acc,desm_cert,desm_cert_acc,desm_cert_s,desm_df,desm_df,desm_ncert,
   desm_s,dup,faltantes,nas,treatment)

#############################################################
#### regeneracao agregada 
############################################################

reg <- read.csv(file.path(p,d,"regeneration.csv"))

reg_s <- subset(reg_df,subset = year<=2009&year>=2004)

# somando taxa desmatamento

reg_acc <- reg %>% 
  filter(year<=2009&year>=2004)%>%
  group_by(COD_IMOVEL)%>%
  summarise(reg_ac=sum(reg_rate))

df3 <- left_join(df2,reg_acc) 

length(unique(df3$COD_IMOVEL[df3$treatment=="certified"])) #OK

#################################
#### proporcao vegetacao nativa
##################################

fc <- read.csv(file.path(p,d,"forest_cover_proportion.csv"))

fc_2009 <- fc %>%
  filter( year==2009)

df4 <- left_join(df3,fc_2009[,c(1,6)]) 

names(df4)[15] <- "prop_veg_09"

#################################
#### proporcao pastagem
##################################

past <- read.csv(file.path(p,d,"pasture_cover_proportion.csv"))


pasture_2009 <- past %>%
  filter( year==2009)

df5 <- left_join(df4,pasture_2009[,c(1,6)]) 

names(df5)[16] <- "prop_past_09"

write.csv(df5,"data_for_matching.csv",row.names = F)




