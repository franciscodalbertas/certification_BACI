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

dcert$id <- as.numeric(as.factor(dcert$COD_IMOVEL))

dcert$certification_cat <- as.factor(dcert$certification_cat)

# dividindo em ma e ce

dma <- dcert[dcert$biome=="Atlantic Forest",]

dce <- dcert[dcert$biome=="Cerrado",]

# data em q maior parte propriedades foi certificada

summary(as.Date(dma$Date.Issued))


# # desmatamento
# 
# desm_ma <- dma%>% filter(year>2003&year<=2017)%>%
#   ggerrorplot(x = "year",y="desm_rate_ly",col="treatment", ylim = c(0, 0.1))+
#   ylab("deforestation rate")
# 
# 
# desm_ce <- dce%>% filter(year>2003&year<=2017)%>%
#   ggerrorplot(x = "year",y="desm_rate_ly",col="treatment", ylim = c(0, 0.1))+
#   ylab("deforestation rate")
# 
# # regeneracao
# 
# reg_ma <- dma%>% filter(year>2003&year<=2017)%>%
#   ggerrorplot(x = "year",y="reg_rate",col="treatment", ylim = c(0, 0.1))+
#   ylab("regeneration rate")
# 
# 
# reg_ce <- dce%>% filter(year>2003&year<=2017)%>%
#   ggerrorplot(x = "year",y="reg_rate",col="treatment", ylim = c(0, 0.1))+
#   ylab("regeneration rate")
# 
# # p_veg
# 
# pveg_ma <- dma%>% filter(year>2003&year<=2017)%>%
#   ggerrorplot(x = "year",y="p_veg",col="treatment", ylim = c(0, 0.5))+
#   ylab("vegetation cover ratio")
# 
# 
# pveg_ce <- dce%>% filter(year>2003&year<=2017)%>%
#   ggerrorplot(x = "year",y="p_veg",col="treatment", ylim = c(0, 0.5))+
#   ylab("vegetation cover proportion")
# 
# 
# pAPP veg!
 
apps <- read.csv("app_forest_cover.csv")

 apps_ma <- apps %>% 
   filter(COD_IMOVEL %in% dma$COD_IMOVEL)

 dma<- left_join(dma,apps_ma)
# 
 dma<- dma %>% 
   mutate(papp = replace(value, is.na(value), 0))
# 
# # cerrado
# 
apps_ce<- apps %>% 
   filter(COD_IMOVEL %in% dce$COD_IMOVEL)
# 
 dce <- left_join(dce,apps_ce)
# 
 dce <- dce %>% 
   mutate(papp = replace(value, is.na(value), 0))
# 
# 
#unindo dataframes
 
df <- rbind(dma,dce)

################################################################################

#--------------------

# desmatamento

#--------------------



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
            xmin=6, 
            xmax=9, 
            ymin=0, 
            ymax=0.5,
            fill="red",
            alpha=0.05,
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

#--------------------

# regeneracao

#--------------------


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
           xmin=6, 
           xmax=9, 
           ymin=0, 
           ymax=0.5,
           fill="red",
           alpha=0.05,
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


#--------------------

# veg cover

#--------------------


g3 <- df%>% filter(year>2003&year<=2017)%>%
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
           xmin=6, 
           xmax=9, 
           ymin=0, 
           ymax=0.95,
           fill="red",
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


#--------------------

# papp

#--------------------


g4 <- df%>% filter(year>2003&year<=2017)%>%
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
           xmin=6, 
           xmax=9, 
           ymin=0, 
           ymax=0.95,
           fill="red",
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


panel <- ggarrange(desm_points,reg_points,pveg_points,app_points,common.legend = T,
                   labels = "auto",ncol = 2,nrow = 2)

# versao com faixa de adocao certificacao

ggsave(filename = file.path("figures","descriptive_figure.jpeg"),plot = panel,width = 14,
       height = 12,units = "cm")

ggsave(filename = file.path("figures","descriptive_figure_comfaixa.jpeg"),plot = panel,width = 14,
       height = 12,units = "cm")

