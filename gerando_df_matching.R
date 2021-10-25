################################################################################
#### pacotes
################################################################################

library(sf)
library(dplyr)
library(tidyr)

#################################################################################
# Os dados estão todos no seguinte caminho (substituir por link do drive!):

# link: https://drive.google.com/drive/folders/1J4SWKgCICBbMZX2WeANjvuIg_jz3Mu_l?usp=sharing

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

rm(df,df2,df3,df4m,bioma,biomas_rep,bizarrom,certificadas,control,mf,nas,Nas,slope,
   slope_cer,slope_treat,slope2,treatment,bizarro,df4,zeros)
#### agregando dados temporais


df <- read.csv("dados_n_temporais.csv")

##################################
#### desmatamento agregado 
##################################

# abrindo dados de desmatamento

f <- function(x)read.csv(x,row.names = 1)

desm <- lapply(planilhas[1:3],f)

names(desm[[1]])
names(desm[[2]])

desm_cert <- rbind(desm[[1]][,c(3,5,6,7,8,9,10,11)],desm[[2]][c(2,4,5,6,7,8,9,10)])

desm_cert$treatment <- "certified" # corrigindo categoria

desm_ncert <- desm[[3]][,c(3,5,6,7,8,9,10,11)]

summary(as.factor(desm_ncert$treatment))

desm_df <- rbind(desm_cert,desm_ncert)

length(unique(desm_df$COD_IMOVEL[desm_df$treatment=="certified"])) 


#subset intervalo (2004-2009)

desm_s <- subset(desm_df,subset = year<=2009&year>=2004)

# somando taxa desmatamento

desm_acc <- desm_s %>% 
  group_by(COD_IMOVEL)%>%
  summarise(desm_ac=sum(def_rate_1))


length(unique(df$COD_IMOVEL[df$treatment=="certified"]))

df2 <- left_join(desm_acc,df) # gera NAS! pq?


nas <- df2[is.na(df2$area_im),] # 322 q nao batem!

faltantes <- nas[nas$COD_IMOVEL %in% df$COD_IMOVEL,] # nenhuma dessas esta no df!

# sao propriedades q estao no desmatamento acumulado mas nao no df!

df3 <- df2[!is.na(df2$area_im),] # isso elimina NAs

length(df3$COD_IMOVEL[df3$treatment=="certified"]) # 537, ok!


rm(desm,desm_acc,desm_cert,desm_cert_acc,desm_cert_s,desm_df,desm_df,desm_ncert,
   desm_s,dup,faltantes,nas,treatment)

##################################
#### regeneracao agregada 
##################################
reg <- lapply(planilhas[15:17],f)

nomes <- names(reg[[1]])[c(2,4,5,6,7)]

reg_cert <- rbind(reg[[1]]%>% select(nomes),reg[[2]]%>% select(nomes))

reg_df <- rbind(reg_cert,reg[[3]]%>% select(nomes))

#subset intervalo (2004-2009)

reg_s <- subset(reg_df,subset = year<=2009&year>=2004)

# somando taxa desmatamento

reg_acc <- reg_s %>% 
  group_by(COD_IMOVEL)%>%
  summarise(reg_ac=sum(prop_reg))

df4 <- left_join(df3,reg_acc) # 16 NAs em reg_ac

summary(df4)
summary(reg_acc$reg_ac)
length(unique(df4$COD_IMOVEL[df4$treatment=="certified"])) #OK


df4 <- df4[!is.na(df4$reg_ac),]

length(unique(df4$COD_IMOVEL)) # OK

#################################
#### proporcao vegetacao nativa
##################################

# com os dados de desmatamento, da pra pegar prop veg

prop_veg_2009 <- desm_s[desm_s$year==2009,]

length(unique(prop_veg_2009$COD_IMOVEL))

str(prop_veg_2009$COD_IMOVEL)
str(df4$COD_IMOVEL)

summary(prop_veg_2009)

df5 <- left_join(df4,prop_veg_2009[,c(1,5)],by="COD_IMOVEL") 

names(df5)[15] <- "prop_veg_09"

#################################
#### proporcao pastagem
##################################

past1 <- read.csv(planilhas[9])
past2 <- read.csv(planilhas[10])
past2$prop_past <- (past2$past_ha/2)/past2$area_im
past3 <- read.csv(planilhas[11])
nomes <- names(past1)[c(2,5,6,7)]
names(past2)
past_cert <- rbind(past1%>%select(nomes),past2%>%select(nomes))
past_df <- rbind(past_cert,past3%>%select(nomes))

#subset 2009

past_s <- subset(past_df,subset = year==2009)

df6 <- left_join(df5,past_s[,c(1,4)],by="COD_IMOVEL") 

summary(df6)

names(df6)[16] <- "prop_past_09"

write.csv(df6,"data_for_matching.csv",row.names = F)