#==== packages =================================================================

library(dplyr)
library(tidyr)

#===============================================================================


# combining control and treatment for two datasets (MA and CE)

ce <- rbind(read.csv('control_ce_ratio1_1.csv.'),read.csv('treatment_ce_ratio1_1.csv'))
ma <- rbind(read.csv('control_ma_ratio1_1.csv'),read.csv('treatment_ma_ratio1_1.csv'))

# columns of interest

c <- c("COD_IMOVEL","treatment","distance","weights")

ce <- ce%>% select(c)
ma <- ma%>% select(c)

# picking temporal data for the selected properties

p <- dirname(getwd()) # parental folder


path_full_data <- "certification_BACI" # path to access metrics calculated 

#---- deforestation ------------------------------------------------------------

def <- read.csv(file.path(p,path_full_data,"vegetation_deforestation.csv"))

#def <- lapply(l[33],f) # data on deforestation

## columns to keep from the metrics

nm <- c("COD_IMOVEL","year","desm_rate_ly")

all_properties <- unique(c(ce$COD_IMOVEL,ma$COD_IMOVEL))

## subset deforestation for selected properties

def_s <- def %>% filter(COD_IMOVEL %in% all_properties)
  
rm(def,def_df,def2)

#---- regeneration -------------------------------------------------------------

reg <- read.csv(file.path(p,path_full_data,"regeneration.csv"))

## columns to keep from the metrics

nm <- c("COD_IMOVEL","year","prop_reg")

## subset reg for selected properties

reg_s <-reg %>% filter(COD_IMOVEL %in% all_properties)

rm(reg)

#---- prop cover ---------------------------------------------------------------

pveg <- read.csv(file.path(p,path_full_data,"forest_cover_proportion.csv"))

## columns to keep from the metrics

nm <- c("COD_IMOVEL","year","prop_cover")

## subset pveg for selected properties


pveg_s <-pveg %>% 
  filter(COD_IMOVEL %in% all_properties)%>%
  filter(year>=1988&year<=2017) # keep same years from the other data

rm(pveg)

# merging into one data frame

temp_data <- cbind(def_s[,c(1:3,5:7,13)],reg_s[,8],pveg_s[,6])

names(temp_data)[8:9] <- c("reg_rate","p_veg")

# exporting data

write.csv(temp_data,"response_variables_ratio1_1.csv",row.names = F)

#==== tendencias temporais =====================================================

# atribuindo dados 

ma_2 <- left_join(ma,temp_data)
ma_2$biome <- "Atlantic Forest"
ce_2 <- left_join(ce,temp_data)
ce_2$biome <- "Cerrado"

df_full <- rbind(ma_2,ce_2)


summary(df_full)

#==== agregando data da certificacao nos dados certificados ====================

# dados certificacao

ra <- read.csv(file.path(getwd(),"propriedades_certificadas.csv"))

df_full_s <- df_full[df_full$COD_IMOVEL %in% ra$COD_IMOVEL,]

length(unique(df_full$COD_IMOVEL[df_full$treatment=="certified"])) #531!!
length(unique(df_full_s$COD_IMOVEL)) # so tem 525!!
length(unique(ra$COD_IMOVEL)) # 531 

################################################################################

car_ra <- ra$COD_IMOVEL

car_df <- unique(df_full$COD_IMOVEL[df_full$treatment=="certified"])

diff <- car_ra[!car_ra %in% car_df] 

# pq tem 6 q nao tao??? alias, tinha q ter 537 no df full nao 531! algum erro previo
# pode ser as q nao pareou, pq efetivamente tiveram 6 q nao parearam!                                 


################################################################################


df_full_s2 <- left_join(df_full_s,ra)

# falta atribuir dados pros car novos da cooxupe

car_cooxupe <- df_full[!df_full$COD_IMOVEL %in% ra$COD_IMOVEL,]
car_cooxupe <- car_cooxupe[car_cooxupe$treatment=="certified",]

names(car_cooxupe)
names(df_full_s2)

'%nin%' <- Negate('%in%')

nomes_cooxupe <- names(df_full_s2)[10:34]

# sao 180 linhas q precisa # conferir aqui qual o problema!

summary( df_full_s2[df_full_s2$cert=="Cooxupe_CerradoMG",10:34])


ra_cooxupe_faltante <- df_full_s2[df_full_s2$cert=="Cooxupe_CerradoMG",10:34]

ra_cooxupe_faltante <- ra_cooxupe_faltante[1:180,]

# juntando com info. das propriedades

car_cooxupe <- cbind(car_cooxupe,ra_cooxupe_faltante)


car_cooxupe_f <- car_cooxupe %>% select(names(df_full_s2))

# gerando df completo

df_full_s3 <- rbind(df_full_s2,car_cooxupe_f)

summary(df_full_s3)

names(df_full_s3)

df_full_s3 <- df_full_s3[,-13]

# falta inserir nao certificados

#selecionar <- names(df_full_s3)[c(1:9,11:12,16:19,21:24,29:32)]


#df_full_s3 <- df_full_s3%>% select(selecionar)


# colocar colunas extras no controle como NA


df_full_s_control <- df_full[df_full$treatment=="non certified",] 


names(df_full_s_control); names(df_full_s3)

nomes_faltantes <-names(df_full_s3) [ ! names(df_full_s3)%in% names(df_full_s_control)]


m <- matrix(NA, ncol = length(nomes_faltantes), nrow = nrow(df_full_s_control))


aditional_var <- as.data.frame(m)

names(aditional_var) <- nomes_faltantes


df_full_s_control <- cbind(df_full_s_control,aditional_var)

# selecionae2 <- names(df_full_s_control)
# 
# df_full_s4 <- df_full_s3 %>% select(selecionae2)


names(df_full_s_control)
names(df_full_s3)


final_df <- rbind(df_full_s3,df_full_s_control)

summary(final_df)

# APP e RL com varios NAs!! pode ter so faltado converter pra 0


summary(df_full_s3) #OK, sem NAs 
summary(df_full_s_control) # ta sem esse dado!!!mas seria bacana ter, pelo
                           # menos proporcao de APP?? discutir isso depois!

# por enquanto excluir esses dados


keep <- names(final_df)[c(1:16,23,30,31)]

final_df2 <- final_df %>% select(keep)

summary(final_df2)

write.csv(final_df2,"data_for_panel_regression_ratio1_1.csv",row.names = F)


#==== criando coluna de antes e depois =========================================

# pras certificadas é mais facil


str(final_df)

final_df2$Date.Issued <- as.Date(final_df2$Date.Issued)


format(as.Date(final_df2$Date.Issued),"%Y")

# antes =0; depois =1


final_df2$certification_cat[final_df2$treatment=="certified"&
                             format(final_df2$Date.Issued,"%Y")<=final_df2$year] <- 
  1


final_df2$certification_cat[final_df2$treatment=="certified"&
                             format(final_df2$Date.Issued,"%Y")>final_df2$year] <- 
  0


################################################################################

## OBS : talvez eu precisa fazer um matching pra cada conta

# We define a fixed effect factor, years after treatment (YAT), 
# which represents the number of years since the action

# como calcular anos pos tratamento pros controle?

# boa ref:
# https://mixtape.scunning.com/difference-in-differences.html
# indica como ex esse paper q eh exatamente oq eu faço:
# https://www.nber.org/system/files/working_papers/w26081/w26081.pdf

# a solucao: no caso do controle, a coluna de impacto fica com valor 0
# pra todos. e eu ploto os coeficientes dos anos, igual na figura. Muito legal!

################################################################################

final_df2$certification_cat[final_df2$treatment=="non certified"] <- 
  0

#==== calculando tempo do evento ===============================================

# os valores devem ser centrados em 0, contendo valores negativos( before) e 
# positivos (after)

# nesse caso,  fica como zero pros controle tb

# valor zero controle

final_df2$event_time[final_df2$treatment=="non certified"] <- 0


# ano referencia tratamento
final_df2$event_time[final_df2$treatment=="certified"&
                      format(final_df2$Date.Issued,"%Y")==final_df2$year] <- 0


# ano referencia tratamento
final_df2$event_time[final_df2$treatment=="certified"&
                      format(final_df2$Date.Issued,"%Y")==final_df2$year] <- 0


final_df2$event_time[final_df2$treatment=="certified"&
                      format(final_df2$Date.Issued,"%Y")!=final_df2$year] <- 
  final_df2$year[final_df2$treatment=="certified"&
                  format(final_df2$Date.Issued,"%Y")!=final_df2$year]-
  as.integer(format(final_df2$Date.Issued,"%Y"))[final_df2$treatment=="certified"&
                              format(final_df2$Date.Issued,"%Y")!=final_df2$year]



summary(final_df2$event_time)

hist(final_df2$event_time)

write.csv(final_df,"data_for_panel_regression.csv_ratio1_1",row.names = F)

