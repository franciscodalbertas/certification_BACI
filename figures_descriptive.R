#==============================================================================

library(ggpubr)
library(dplyr)
library(ggsci)

#==============================================================================


#Data

data_cert <- read.csv("data_for_panel_regression.csv")

data_cert$first_year <- as.integer(substring(data_cert$Date.Issued, 1, 4))

data_cert$first_year[is.na(data_cert$Date.Issued)] <- 0 # convert first year to NA

dcert <- data_cert[data_cert$year > 2004, ]

#dcert$id <- as.numeric(as.factor(dcert$COD_IMOVEL))

dcert$certification_cat <- as.factor(dcert$certification_cat)

# dividindo em ma e ce

dma <- dcert[dcert$biome=="Atlantic Forest",]

dce <- dcert[dcert$biome=="Cerrado",]

# data em q maior parte propriedades foi certificada

summary(as.Date(dma$Date.Issued)) # 2010-2013

summary(as.Date(dce$Date.Issued)) # 2009-2011


# pAPP veg!
 
apps <- read.csv("app_forest_cover.csv")

 apps_ma <- apps %>% 
   filter(COD_IMOVEL %in% dma$COD_IMOVEL)

 dma<- left_join(dma,apps_ma)
 
 dma<- dma %>% 
   mutate(papp = replace(value, is.na(value), 0))
 
# cerrado
 
apps_ce<- apps %>% 
   filter(COD_IMOVEL %in% dce$COD_IMOVEL)
 
dce <- left_join(dce,apps_ce)
 
dce <- dce %>% 
   mutate(papp = replace(value, is.na(value), 0))

 
#unindo dataframes
 
df <- rbind(dma,dce)


# adicionando area de app! pra descontar do deficit!

# aqui tem q levar em consideracao mf! pra filtrar!mas pode ser depois

library(sf)

apps <- st_read(file.path("G:\\Meu Drive\\Doutorado\\cap3\\",
                          "limites_propriedades","APPS.shp"))

apps_f <- apps %>% filter(COD_IMOVEL %in% df$COD_IMOVEL)

sf::sf_use_s2(FALSE)

apps_f$APP_area_ha <- as.numeric(st_area(apps_f)/10^4)

st_geometry(apps_f) <- NULL

df_app <- left_join(df,apps_f)

df_app$APP_area_ha[is.na(df_app$APP_area_ha)] <- 0

################################################################################

# continuar!

#---------------------------------------------------------------

# desmatamento

#---------------------------------------------------------------

# faz com dados completos!

df$treatment <- factor(df$treatment, levels = c("non certified","certified"))


g <-df%>% filter(year>2003&year<=2017)%>%
  ggplot(aes(x = as.factor(year), y = desm_rate_ly, 
  color = treatment, group = interaction(as.factor(year), treatment))) +
  #coord_flip() +
  coord_cartesian(ylim = c(0,0.5)) +
  #scale_color_uchicago() +
  labs(x = NULL, y = "deforestation rate") +
  theme(
    legend.position = "none",
    axis.title = element_text(size = 16),
    axis.text.x = element_text(family = "Roboto Mono", size = 12),
    panel.grid = element_blank()) +
  scale_x_discrete()+
  annotate(geom="rect",
            xmin=5, 
            xmax=9, 
            ymin=0, 
            ymax=0.5,
            fill="yellow",
            alpha=0.2,
            color=NA)


desm_points <- g +
  geom_jitter(colour = "gray",size = 1, alpha = 0.1, width = 0.2) +
  stat_summary(fun = mean, geom = "point", size = 2,stroke = 0.5,shape=21)+
  theme_classic()+
  facet_wrap(~biome)+
  #scale_x_discrete(labels=label_x)
  theme(strip.background = element_blank())



desm_points <- ggpar(desm_points,
                 legend = "none",
                 #xlab = "time (years)",
                 #ylab = "average effect on vegetation cover",
                 main = "",
                 font.x=c(7,"bold"),
                 font.y=c(7,"bold"),
                 font.tickslab=(5),
                 x.text.angle = 90)

#-------------------------------------------------------------------------------

# regeneracao

#------------------------------------------------------------------------------


g2 <- df%>% filter(year>2003&year<=2017)%>%
  ggplot(aes(x = as.factor(year), y = reg_rate, 
             color = treatment, group = interaction(as.factor(year), treatment))) +
  #coord_flip() +
  coord_cartesian(ylim = c(0,0.5)) +
  #scale_color_uchicago() +
  labs(x = NULL, y = "regeneration rate") +
  theme(
    legend.position = "none",
    axis.title = element_text(size = 16),
    axis.text.x = element_text(family = "Roboto Mono", size = 12),
    panel.grid = element_blank())+
  scale_x_discrete()+
  annotate(geom="rect",
           xmin=5, 
           xmax=9, 
           ymin=0, 
           ymax=0.5,
           fill="yellow",
           alpha=0.2,
           color=NA)

reg_points <- g2 +
  geom_jitter(colour = "gray",size = 1, alpha = 0.1, width = 0.2) +
  stat_summary(fun = mean, geom = "point", size = 2,stroke =0.5,shape=21)+
  theme_classic()+facet_wrap(~biome)+
  theme(strip.background = element_blank())
  #scale_x_discrete(labels=label_x)+

reg_points <- ggpar(reg_points,
                     legend = "none",
                     #xlab = "time (years)",
                     #ylab = "average effect on vegetation cover",
                     main = "",
                     font.x=c(7,"bold"),
                     font.y=c(7,"bold"),
                     font.tickslab=(5),
                     x.text.angle = 90)


#-------------------------------------------------------------------------------

# veg cover

#-------------------------------------------------------------------------------

# aqui faria sentido focar apenas nas medias e grandes!

mf <- read.csv(file.path(dirname(getwd()),"certification_BACI","data_for_matching.csv"))

df_app2 <- left_join(df_app,mf[,c(1,13)]) %>% filter(area_im_mf>=4)

df_app2$treatment <- factor(df_app2$treatment, levels = c("non certified","certified"))

g3 <- df_app2%>% filter(year>2003&year<=2017)%>%
  ggplot(aes(x = as.factor(year), y = p_veg, 
             color = treatment, group = interaction(as.factor(year), treatment))) +
  #coord_flip() +
  coord_cartesian(ylim = c(0,0.9)) +
  #scale_color_uchicago() +
  labs(x = NULL, y = "vegetation cover proportion") +
  theme(
    legend.position = "none",
    axis.title = element_text(size = 16),
    axis.text.x = element_text(family = "Roboto Mono", size = 12),
    panel.grid = element_blank())+
  scale_x_discrete()+
  annotate(geom="rect",
           xmin=5, 
           xmax=9, 
           ymin=0, 
           ymax=0.95,
           fill="yellow",
           alpha=0.2,
           color=NA)


pveg_points <- g3 +
  geom_jitter(colour = "gray",size = 1, alpha = 0.1, width = 0.2) +
  stat_summary(fun = mean, geom = "point", size = 2,stroke = 0.5,shape=21)+
  theme_classic()+facet_wrap(~biome)+
  theme(strip.background = element_blank())
    #scale_x_discrete(labels=label_x)
  

pveg_points <- ggpar(pveg_points,
                    legend = "none",
                    #xlab = "time (years)",
                    #ylab = "average effect on vegetation cover",
                    main = "",
                    font.x=c(7,"bold"),
                    font.y=c(7,"bold"),
                    font.tickslab=(5),
                    x.text.angle = 90)


#--------------------------------------------------------------------

# papp

#---------------------------------------------------------------------


g4 <- df_app2%>% filter(year>2003&year<=2017)%>%
  ggplot(aes(x = as.factor(year), y = papp, 
             color = treatment, group = interaction(as.factor(year), treatment))) +
  #coord_flip() +
  coord_cartesian(ylim = c(0,0.9)) +
  #scale_color_uchicago() +
  labs(x = NULL, y = "app cover proportion") +
  theme(
    legend.position = "none",
    axis.title = element_text(size = 16),
    axis.text.x = element_text(family = "Roboto Mono", size = 12),
    panel.grid = element_blank())+
  scale_x_discrete()+
  annotate(geom="rect",
           xmin=5, 
           xmax=9, 
           ymin=0, 
           ymax=0.95,
           fill="yellow",
           alpha=0.2,
           color=NA)


app_points <- g4 +
  geom_jitter(colour = "gray",size = 1, alpha = 0.1, width = 0.2) +
  stat_summary(fun = mean, geom = "point", size = 2,stroke = 0.5,shape=21)+
  theme_classic()+facet_wrap(~biome)+
  theme(strip.background = element_blank())
#scale_x_discrete(labels=label_x)

app_points <- ggpar(app_points,
                     legend = "none",
                     #xlab = "time (years)",
                     #ylab = "average effect on vegetation cover",
                     main = "",
                     font.x=c(7,"bold"),
                     font.y=c(7,"bold"),
                     font.tickslab=(5),
                     x.text.angle = 90)


#--------------------------------------------------------------------

# proportional deficit

#---------------------------------------------------------------------

# calculando deficit fora de APP

df_app2 <- df_app2 %>% 
  mutate(deficit_noAPP= (area_im* 0.2)- (area_im* p_veg)- (APP_area_ha)) %>%
  mutate(deficit_noAPP = replace(deficit_noAPP, deficit_noAPP<0, 0)) %>%
  mutate(deficit_noAPP_prop= deficit_noAPP/area_im)


g5 <- df_app2%>% filter(year>2003&year<=2017)%>%
  ggplot(aes(x = as.factor(year), y = deficit_noAPP_prop, 
             color = treatment, group = interaction(as.factor(year), treatment))) +
  #coord_flip() +
  coord_cartesian(ylim = c(0,0.2)) +
  #scale_color_uchicago() +
  labs(x = NULL, y = "vegetation deficit") +
  theme(
    legend.position = "none",
    axis.title = element_text(size = 16),
    axis.text.x = element_text(family = "Roboto Mono", size = 12),
    panel.grid = element_blank())+
  scale_x_discrete()+
  annotate(geom="rect",
           xmin=5, 
           xmax=9, 
           ymin=0, 
           ymax=0.25,
           fill="yellow",
           alpha=0.2,
           color=NA)


deficit_points <- g5 +
  geom_jitter(colour = "gray",size = 1, alpha = 0.1, width = 0.2) +
  stat_summary(fun = mean, geom = "point", size = 2,stroke = 0.5,shape=21)+
  theme_classic()+facet_wrap(~biome)+
  theme(strip.background = element_blank())
#scale_x_discrete(labels=label_x)

deficit_points <- ggpar(deficit_points,
                    legend = "none",
                    #xlab = "time (years)",
                    #ylab = "average effect on vegetation cover",
                    main = "",
                    font.x=c(7,"bold"),
                    font.y=c(7,"bold"),
                    font.tickslab=(5),
                    x.text.angle = 90)











#-------------------------------------------------------------------------

# painel antigo
panel <- ggarrange(desm_points,reg_points,pveg_points,app_points,common.legend = T,
                   labels = "auto",ncol = 2,nrow = 2)

# painel com deficit

painel_deficit <- ggarrange(desm_points,reg_points,deficit_points,app_points,common.legend = T,
                            labels = "auto",ncol = 2,nrow = 2)


# essa era antes de ter a faixa!

ggsave(filename = file.path("figures","descriptive_figure.jpeg"),plot = panel,width = 14,
       height = 12,units = "cm")

# versao com faixa de adocao certificacao

ggsave(filename = file.path("figures","descriptive_figure_comfaixa.jpeg"),plot = panel,width = 14,
       height = 12,units = "cm")


ggsave(filename = file.path("figures","descriptive_figure_comfaixa_deficit.jpeg"),plot = painel_deficit,width = 14,
       height = 12,units = "cm")

# salvando figura de cobertura de vegetacao como suplementar!


ggsave(filename =file.path("figures","descriptive_figure_comfaixa_pveg.jpeg"),plot =pveg_points,
       width = 6,height = 6, units = "cm")
