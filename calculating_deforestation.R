#===============================================================================

# calculate deforestation based on data from collection 5!

#===============================================================================

#==== packages =================================================================

library(dplyr)
library(tidyr)
library(sf)
library(ggpubr)

#===============================================================================

# obs

# parece que o dado de cobertura vegetacao ta errado. mas o de desmatamento
# ta certo!

# tem q ser algum erro

# pasta raiz

p <- dirname(getwd())

#-------------------------------------------------------------------------------
# abrindo cob veg 
#-------------------------------------------------------------------------------

arquivos <- list.files(file.path(p,"GEE_col6"),pattern = ".csv",full.names = T
                       )
veg_cert <- read.csv(arquivos[4])

veg_cert$treatment <- "certified"

# substituindo nomes

names(veg_cert)[12:46] <- paste0(seq(1985,2019,1),"_cobveg")

summary(veg_cert)

veg_ncert <- read.csv(arquivos[5])

veg_ncert$treatment <- "non certified"

names(veg_ncert)[12:46] <- paste0(seq(1985,2019,1),"_cobveg")

names(veg_ncert)[3] <- "COD_IMOVEL"

# precisa da area das propriedades!

# limite propriedades

cont <- st_read(file.path(p,"limites_propriedades","controle.shp"))

sf::sf_use_s2(FALSE)

cont$area_im <- as.numeric(st_area(cont))/10^4

veg_ncert2 <- left_join(x = veg_ncert,y = cont[,c(1,16)],by=c("COD_IMOVEL"="COD_IMO"))

names(veg_ncert2)[2] <- "COD_ESTADO"

# colunas pra manter!

k <- names(veg_cert)[c(2:4,8,12:46,50)]


veg_cert2 <- veg_cert %>% select(k)

veg_ncert2 <- veg_ncert2 %>% select(k)

veg <- rbind(veg_cert2,veg_ncert2)

veg_m <- veg %>% 
  pivot_longer(cols =c(5:39),names_to = "year",values_to = "area_veg" ) %>%
  separate(col = year,into = c("year","discard"),sep = "_")

veg_m <- veg_m[,-7]

summary(veg_m$area_veg[veg_m$treatment=="certified"])
summary(veg_m$area_veg[veg_m$treatment=="non certified"])

veg_m$area_veg <- veg_m$area_veg/10^4

rm(cont,veg,veg_cert,veg_cert2,veg_ncert,veg_ncert2)

summary(veg_m$area_veg/veg_m$area_im)

# tem umas areas que ficam maiores q 1. pra proporcao, melhor eh calcular a 
# parte...mto estranho que aconteca, mas acontece!

hist(veg_m$area_veg/veg_m$area_im)

teste <- veg_m[veg_m$area_veg/veg_m$area_im>1.1,]

#2 propriedade2 com area desprezivel, da pra descartar!

# 124 m^2

min(veg_m$area_im[veg_m$treatment=="certified"]) # 0.76 ha eh o menor valor. vou
# excluir o resto pra ver se melhora

summary(veg_m$area_im[veg_m$treatment=="certified"])
hist(veg_m$area_im[veg_m$treatment=="certified"])

#-------------------------------------------------------------------------------
# abrindo desmatamento
#-------------------------------------------------------------------------------

desm_c <- read.csv(arquivos[1])
desm_nc <- read.csv(arquivos[2])

names(desm_c)[12:41] <- paste0(seq(1988,2017,1),"_deforestation")
names(desm_nc)[14:43] <- paste0(seq(1988,2017,1),"_deforestation")
names(desm_nc)[3] <- "COD_IMOVEL"


k2 <- names(desm_c)[c(3,12:41)]

desm_c2 <- desm_c %>% select(k2)

desm_c2$treatment <- "certified"

desm_nc2 <- desm_nc %>% select(k2)

desm_nc2$treatment <- "non certified"
 
desm <- rbind(desm_c2,desm_nc2) 

desm_m <- desm %>% 
  pivot_longer(cols =c(2:31),names_to = "year",values_to = "desm" ) %>%
  separate(col = year,into = c("year","discard"),sep = "_")

desm_m <- desm_m[,-4]

# transformando valor desmatamento em ha 

desm_m$desm <- desm_m$desm/10^4


summary(desm_m$desm) # tem q ver se ta certo

hist(desm_m$desm[desm_m$desm<100])

# se um pixel tem 0.09 ha, pela logica, desmatamento menor que 1 pixel deveria
# ser aproximado pra 0

length(desm_m$desm[desm_m$desm<0.09&desm_m$desm>0]) 

df <- left_join(desm_m,veg_m)

summary(desm_m)
summary(veg_m)
summary(df)

# filtrando desmatamentos  <0.09 =0

df$desm_filt <- df$desm
df$desm_filt[df$desm_filt<0.09] <- 0

df$desm_rate <- df$desm_filt/df$area_veg

# qndo desmatamento=0, taxa tem q ser 0

df$desm_rate[df$desm_filt==0] <- 0

summary(df$desm_rate) 

# agora parece so ter problemas com algumas propriedades certificadas!!
# sera algum erro nos dados??!

valores_absurdos <- df[df$desm_rate>1.4,]



# as metricas tao certas, os erros devem estar associados a ter cafe e o desmatamento
# pegar essas areas. Conferi # os valores batem com as imagens!


length(unique(valores_absurdos$COD_IMOVEL)) #76 imoveis.eh mto! so pra certifi
# cados! preciso rever os dados pra certificados! recalcular eles!


# ao que parece, os valores mto altos estao associados a erros de classificacao
# mas como cuidar de todos? olhar propriedade a propriedade?
# padrao atual e/ou, historico das propriedades: so um valor absurdo de desmata-
# mento ou regeneracao

summary(valores_absurdos$area_veg)

################################################################################

# decisoes a tomar: pra contemplar os valores bizarros

################################################################################


# se o valor de vegetacao for < 1/2 pixel, considerar que o desmatamento eh zero!

df$desm_rate_ed <- df$desm_rate


df$desm_rate_ed[df$area_veg<=(0.09/2)] <- 0

# checando efeito

valores_absurdos <- df[df$desm_rate>1.4,]

####################################

# plotar valores problematicos

####################################

# plotar desmatamento desss propriedades em painel! mas sao 78! mta coisa


codigos_problema <- valores_absurdos$COD_IMOVEL


desm_m_s <- df  %>% filter(COD_IMOVEL %in% codigos_problema )%>% 
  arrange(desc(COD_IMOVEL))



# dividir em grupos!
# 76 propriedades, cada uma com 30 linhas
# quero dividir em 10


# ta errado os grupos! mas ajustar e avaliar!!!

2280/10

# cada propriedade sao 30 linhas!

2280

# assim nao funciona, pq tem q pegar de 30 em 30
# completo!


length(rep(1:10, times=1, each =300))

x <- rep(1:10, times=1, each =300)
x <- x[1:2280]


desm_m_s$group <- x

desm_m_s_g <- desm_m_s %>%
  group_by(group)%>%
  group_split()


# sao todos praticamente picos isolados de 1 ano só!

ggline(desm_m_s_g[[1]],x = "year",y = "desm_rate_ed")+facet_grid("COD_IMOVEL")+ 
  rotate_x_text(angle = 90)

ggline(desm_m_s_g[[2]],x = "year",y = "desm_rate_ed")+facet_grid("COD_IMOVEL")+ 
  rotate_x_text(angle = 90)


ggline(desm_m_s_g[[3]],x = "year",y = "desm_rate_ed")+facet_grid("COD_IMOVEL")+ 
  rotate_x_text(angle = 90)

ggline(desm_m_s_g[[4]],x = "year",y = "desm_rate_ed")+facet_grid("COD_IMOVEL")+ 
  rotate_x_text(angle = 90)


ggline(desm_m_s_g[[5]],x = "year",y = "desm_rate_ed")+facet_grid("COD_IMOVEL")+ 
  rotate_x_text(angle = 90)


ggline(desm_m_s_g[[6]],x = "year",y = "desm_rate_ed")+facet_grid("COD_IMOVEL")+ 
  rotate_x_text(angle = 90)

ggline(desm_m_s_g[[7]],x = "year",y = "desm_rate_ed")+facet_grid("COD_IMOVEL")+ 
  rotate_x_text(angle = 90)

ggline(desm_m_s_g[[8]],x = "year",y = "desm_rate_ed")+facet_grid("COD_IMOVEL")+ 
  rotate_x_text(angle = 90)

# olhar pros mais problematicos!

desm_m_s2 <- df %>% filter(COD_IMOVEL %in% desm_m_s$COD_IMOVEL[desm_m_s$desm_rate_ed>10])%>% 
  arrange(desc(COD_IMOVEL))

ggline(desm_m_s2,x = "year",y = "desm_rate_ed")+facet_grid("COD_IMOVEL")+ 
  rotate_x_text(angle = 90)


# uma analise mais detalhada dos casos extremos sugere ser melhor calcular desma-
# tamento baseado no ano anterior!!!

# ainda assim, sao claramente erros relacionados ao cafe! tanto mata qnto desm
# ( o ideal seria filtrar variacoes absurdas de regeneracao por ex...)

# acho q vou ter que fazer pra comparar!


################################################################################

# desmatamento ano anterior

################################################################################


lista_car <- unique(df$COD_IMOVEL)
lista_car_absurdos <- unique(valores_absurdos$COD_IMOVEL)
lista_anos <- seq(1989,2017,1)


lista_dfs <- list()

c <- 0

for(i in 1:length(lista_car)){
  c <- c+1
  s <- df %>%filter(COD_IMOVEL ==lista_car[i])
  s$desm_rate_ly <- NA
  for(y in 1:29){
    
    s$desm_rate_ly[s$year==1988] <- NA
    s$desm_rate_ly[s$year==lista_anos[y]] <- s$desm[s$year==lista_anos[y]]/s$area_veg[s$year==lista_anos[y]-1]
    lista_dfs[[c]] <- s
  }
  print(i)
}

df2 <- do.call(rbind,lista_dfs)

# valores antes absurdos ficam corretos!
valores_absurdos3 <- df2 %>% 
  filter(desm_rate_ed>1.5)

# fica bem melhor! so propriedades nao-certificadas sairiam!
valores_absurdos4 <- df2 %>% 
  filter(desm_rate_ly>1.5)


length(unique(valores_absurdos4$COD_IMOVEL)) # 15 propriedades!

# falta converter NAN em 0s


df2$desm_rate_ly[is.nan(df2$desm_rate_ly)] <- 0


hist(df2$desm_rate_ly[df2$desm_rate_ly<1.2])

df2%>% filter(desm_rate_ly <1.2)%>%
  ggdensity(x ="desm_rate_ly" ,col="treatment" )

write.csv(df2,"vegetation_deforestation.csv",row.names = F)
