#===============================================================================

# figura manuscrito com municipios certificados e distribuicao dos contratos

#===============================================================================


#==== pacotes ==================================================================

library(sf)
library(geobr)
library(ggplot2)
library(viridis)
library(ggpubr)
library(ggspatial)
library(dplyr)
library(patchwork)
library(lubridate)
library(dplyr)
library(scales)

#===============================================================================

# limites Br

Br <- read_country()     

Biomes <- read_biomes(year = 2019, simplified = TRUE, showProgress = TRUE)%>%
  filter(code_biome==3|code_biome==4)



# criando area de zoom 

box = c(xmin = -50, ymin = -23, xmax = -40, ymax = -14)              

ps <- list(rbind(c(-50, -23),c(-40,-23),c(-40,-14),c(-50, -14),c(-50, -23)))
box <- st_polygon(ps)
box2 <- st_sfc(box,crs =st_crs(Br) )

Limites_BR <- ggplot()+
  geom_sf(data = Br,color=NA)+
  geom_sf(data=Biomes,fill=c("gray","darkgray"))+
  geom_sf(data = box2,fill = NA, color = "black", size = 1.2)+
  theme_void()+theme(plot.background = element_rect(fill = "white"))
# municipios

mun <- read_municipality(year=2019)

# abrindo dados de certificação 

path <- file.path("D:\\Doutorado\\cap3\\SICAR_IMAFLORA_unificado")

cert <- st_read(paste(path,"propriedades_certificadas_SICAR.shp",sep="/"))

# tabulando frequencia municipios 

freq <- as.data.frame(table(cert$COD_MUN))

# transformando codigo mun em caracter

mun$code_muni <- as.character(mun$code_muni)
freq$Var1 <- as.character(freq$Var1)

# juntando os dados

mun2 <- merge(mun,freq,by.x="code_muni",by.y="Var1",all.x=T)


# transformando NA em 0

mun2$Freq[is.na(mun2$Freq)] <- 0

#==== figura 1 =================================================================

# plotando apenas municipios diferentes de 0

mun2 <- subset(mun2,subset = Freq>0)


# Remove plot axis

no_axis <- theme(axis.title=element_blank(),
                 axis.text=element_blank(),
                 axis.ticks=element_blank())

# recortando municipios usando limite box2

mun_crop <- st_crop(x = mun,box2)

# plotando mun cert 

municipios_certificados <- ggplot() +
  geom_sf(data = Br,color=NA)+
  #geom_sf(data=mun_crop,fill="NA",color="white")+
  geom_sf(data=Biomes,fill=c("gray","darkgray"))+
  geom_sf(data=mun2,aes( fill=Freq),color="black") +
  coord_sf(xlim = c(-49, -40), 
           ylim = c(-23, -15), 
           expand = T)+
  scale_fill_viridis(name="n farms")+
  annotation_scale(location="br")+
  annotation_north_arrow(location = "tr", 
  which_north = "true", 
  style = north_arrow_fancy_orienteering)+
  theme(legend.position="top")+
  theme_pubclean() 




# como sobrepor grafico!!! patchwork!!!!

final <- municipios_certificados + inset_element(Limites_BR, left = 0, 
       bottom = 0.8, right = 0.2, top = 1)

#==== figura 2 =================================================================


p <- dirname(getwd())


df <- read.csv(file.path(p,"dados_Imaflora_RA_clean",
                         "propriedades_certificadas.csv"),row.names = 1)


# grafico da distribuicao de fazendas por grupo


grupos <- as.data.frame(table(df$cert))

grupos$label <- as.character(as.numeric(as.factor(grupos$Var1)))

# 27 grupos!

p1 <- grupos %>% 
  arrange(Freq) %>%
  mutate(label=factor(label, levels=label))%>%
  ggplot(aes(x=label, y=Freq)) +
  geom_bar(stat="identity") +
  coord_flip() +
  theme_bw() +
  xlab("")+
  ylab("number of farms involved")+
  theme_minimal()+
  theme(axis.title.y=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank(),panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank())

# plotar os mesmos com as linhas do tempo!!!

periodo <-df[,c(3,22,23)]

periodo <- unique(periodo)

periodo$In <- as.Date(periodo$Date.Issued)
periodo$Fn <- as.Date(periodo$Expiration.Date)

names(grupos)[1] <- "cert"

periodo <- left_join(periodo,grupos, by="cert")



periodo$label <- as.factor(as.numeric(as.factor(periodo$cert)))
#---- plotando o grafico -----------------------------------------------------

duracao <- periodo %>% 
  arrange(Freq) %>%
  mutate(label=factor(label, levels=label))%>% 
  ggplot() +
  geom_segment(data = periodo, aes(x = In, y = cert, xend = as.Date('2019-12-31'),
                                   yend = cert), size = 2)+
  scale_x_date( breaks=date_breaks("1 year"),labels = date_format("%y")) + 
  xlab('period under certification')+
  theme_pubr()+
  theme(axis.line.y=element_blank(),
        axis.text.y=element_blank(),axis.ticks=element_blank(),
        axis.title.y=element_blank(),legend.title = element_blank())+
  theme(legend.position="right")

library(egg)

figuras <- ggarrange(p1,duracao,ncol=2)


final2 <- final/figuras


detach("package:egg", unload=TRUE)

ggarrange(final,figuras,heights = c(4,1))


ggsave(filename = "figures/study_site.jpg",plot = final2,width = 21,height = 20,
       units = "cm",)

