#==== packages =================================================================

library(dplyr)
library(tidyr)

#===============================================================================


# combining control and treatment for two datasets (MA and CE)

ce <- rbind(read.csv('control_ce.csv'),read.csv('treatment_ce.csv'))
ma <- rbind(read.csv('control_ma.csv'),read.csv('treatment_ma.csv'))

# columns of interest

c <- c("COD_IMOVEL","treatment","distance","weights")

ce <- ce%>% select(c)
ma <- ma%>% select(c)

# picking temporal data for the selected properties

p <- dirname(getwd()) # parental folder

path_full_data <- "metricas\\melted_tables" # path to access metrics calculated 
# for all properties

l <- list.files(file.path(p,path_full_data),full.names = T) #listing all the files

f <- function(x)read.csv(x,row.names = 1)

#---- deforestation ------------------------------------------------------------

def <- lapply(l[1:3],f) # data on deforestation

## columns to keep from the metrics

nm <- c("COD_IMOVEL","year","def_rate_1")

f2 <- function(x) x%>% select(nm)

def2 <- lapply(def,f2)

def_df <- do.call(rbind,def2)

all_properties <- unique(c(ce$COD_IMOVEL,ma$COD_IMOVEL))

## subset deforestation for selected properties
def_df_s <-def_df[def_df$COD_IMOVEL %in% all_properties,]

rm(def,def_df,def2)
#---- regeneration -------------------------------------------------------------

reg <- lapply(l[15:17],f) # data on deforestation

## columns to keep from the metrics

nm <- c("COD_IMOVEL","year","prop_reg")

f2 <- function(x) x%>% select(nm)

reg2 <- lapply(reg,f2)

reg_df <- do.call(rbind,reg2)

all_properties <- unique(c(ce$COD_IMOVEL,ma$COD_IMOVEL))

## subset reg for selected properties
reg_df_s <-reg_df[reg_df$COD_IMOVEL %in% all_properties,]

rm(reg,reg_df,reg2)

#---- prop cover ---------------------------------------------------------------

pveg <- lapply(l[12:14],f) # data on deforestation

## columns to keep from the metrics

nm <- c("COD_IMOVEL","year","prop_cover")

f2 <- function(x) x%>% select(nm)

pveg2 <- lapply(pveg,f2)

pveg_df <- do.call(rbind,pveg2)

all_properties <- unique(c(ce$COD_IMOVEL,ma$COD_IMOVEL))

## subset pveg for selected properties
pveg_df_s <-pveg_df[pveg_df$COD_IMOVEL %in% all_properties,]

rm(pveg,pveg_df,pveg2)

# merging into one data frame

temp_data <- cbind(def_df_s,reg_df_s[,3],pveg_df_s[,3])

names(temp_data)[4:5] <- c("reg_rate","p_veg")

# exporting data

write.csv(temp_data,"response_variables.csv",row.names = F)

#==== tendencias temporais =====================================================

# atribuindo dados 

ma_2 <- left_join(ma,temp_data)
ma_2$biome <- "Atlantic Forest"
ce_2 <- left_join(ce,temp_data)
ce_2$biome <- "Cerrado"
df_full <- rbind(ma_2,ce_2)


summary(df_full)

#==== agregando data da certificacao nos dados certificados ====================


#### OBS #######################################################################
# eu nao entendo como usar o controle uma vez q mesmo calculando anos antes e
# depois, cada contrato se refere a um conjunto diferente de anos

# tem q ter um df pra cada contrato, pelo jeito.

################################################################################

# dados certificacao

ra <- read.csv(file.path(p,"dados_Imaflora_RA_clean","propriedades_certificadas.csv"))

df_full_s <- df_full[df_full$COD_IMOVEL %in% ra$COD_IMOVEL,]
df_full_s2 <- left_join(df_full_s,ra)

# falta atribuir dados pros car novos da cooxupe

car_cooxupe <- df_full[!df_full$COD_IMOVEL %in% ra$COD_IMOVEL,]
car_cooxupe <- car_cooxupe[car_cooxupe$treatment=="certified",]

names(car_cooxupe)
names(df_full_s2)

'%nin%' <- Negate('%in%')

nomes_cooxupe <- names(df_full_s2)[10:34]

# sao 180 linhas q precisa


ra_cooxupe_faltante <- df_full_s2[df_full_s2$cert=="Cooxupe_CerradoMG",10:34]

ra_cooxupe_faltante <- ra_cooxupe_faltante[1:180,]

# juntando com info. das propriedades

car_cooxupe <- cbind(car_cooxupe,ra_cooxupe_faltante)

# gerando df completo

df_full_s3 <- rbind(df_full_s2,car_cooxupe)

summary(df_full_s3)

# falta inserir nao certificados

selecionar <- names(df_full_s3)[c(1:9,11:12,16:19,21:24,29:32)]


df_full_s3 <- df_full_s3%>% select(selecionar)


# colocar colunas extras no controle como NA


df_full_s_control <- df_full[df_full$treatment=="non certified",] 


names(df_full_s_control); names(df_full_s3)



m <- matrix(NA, ncol = length(names(df_full_s3)[c(11,16:23)]), nrow = nrow(df_full_s_control))


aditional_var <- as.data.frame(m)

names(aditional_var) <- names(df_full_s3)[c(11,16:23)]


df_full_s_control <- cbind(df_full_s_control,aditional_var)


selecionae2 <- names(df_full_s_control)

df_full_s4 <- df_full_s3 %>% select(selecionae2)


final_df <- rbind(df_full_s4,df_full_s_control)

# salvando

write.csv(final_df,"data_for_panel_regression.csv",row.names = F)


#==== criando coluna de antes e depois =========================================

# pras certificadas é mais facil


str(final_df)

final_df$Date.Issued <- as.Date(final_df$Date.Issued)

?as.Date

format(as.Date(final_df$Date.Issued),"%Y")

# antes =0; depois =1


final_df$certification_cat[final_df$treatment=="certified"&
                             format(final_df$Date.Issued,"%Y")<=final_df$year] <- 
  1


final_df$certification_cat[final_df$treatment=="certified"&
                             format(final_df$Date.Issued,"%Y")>final_df$year] <- 
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

final_df$certification_cat[final_df$treatment=="non certified"] <- 
  0

#==== calculando tempo do evento ===============================================

# os valores devem ser centrados em 0, contendo valores negativos( before) e 
# positivos (after)

# nesse caso,  fica como zero pros controle tb

# valor zero controle

final_df$event_time[final_df$treatment=="non certified"] <- 0


# ano referencia tratamento
final_df$event_time[final_df$treatment=="certified"&
                      format(final_df$Date.Issued,"%Y")==final_df$year] <- 0


# ano referencia tratamento
final_df$event_time[final_df$treatment=="certified"&
                      format(final_df$Date.Issued,"%Y")==final_df$year] <- 0


final_df$event_time[final_df$treatment=="certified"&
                      format(final_df$Date.Issued,"%Y")!=final_df$year] <- 
  final_df$year[final_df$treatment=="certified"&
                  format(final_df$Date.Issued,"%Y")!=final_df$year]-
  as.integer(format(final_df$Date.Issued,"%Y"))[final_df$treatment=="certified"&
                              format(final_df$Date.Issued,"%Y")!=final_df$year]



summary(final_df$event_time)
hist(final_df$event_time)

getwd()

write.csv(final_df,"data_for_panel_regression.csv",row.names = F)
