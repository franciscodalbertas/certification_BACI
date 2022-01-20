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

# desmatamento

desm_ma <- dma%>% filter(year>2003&year<=2017)%>%
  ggerrorplot(x = "year",y="desm_rate_ly",col="treatment", ylim = c(0, 0.1))+
  ylab("deforestation rate")


desm_ce <- dce%>% filter(year>2003&year<=2017)%>%
  ggerrorplot(x = "year",y="desm_rate_ly",col="treatment", ylim = c(0, 0.1))+
  ylab("deforestation rate")

# regeneracao

reg_ma <- dma%>% filter(year>2003&year<=2017)%>%
  ggerrorplot(x = "year",y="reg_rate",col="treatment", ylim = c(0, 0.1))+
  ylab("regeneration rate")


reg_ce <- dce%>% filter(year>2003&year<=2017)%>%
  ggerrorplot(x = "year",y="reg_rate",col="treatment", ylim = c(0, 0.1))+
  ylab("regeneration rate")

# p_veg

pveg_ma <- dma%>% filter(year>2003&year<=2017)%>%
  ggerrorplot(x = "year",y="p_veg",col="treatment", ylim = c(0, 0.5))+
  ylab("vegetation cover ratio")


pveg_ce <- dce%>% filter(year>2003&year<=2017)%>%
  ggerrorplot(x = "year",y="p_veg",col="treatment", ylim = c(0, 0.5))+
  ylab("vegetation cover ratio")


#### pensando em gerar uma figura só


df <- rbind(dma,dce)

# errorplot

desm <- df%>% filter(year>2003&year<=2017)%>%
  ggerrorplot(x = "year",y="desm_rate_ly",col="treatment", ylim = c(0, 0.1))+
  ylab("deforestation rate")+facet_grid("biome")


reg <- df%>% filter(year>2003&year<=2017)%>%
  ggerrorplot(x = "year",y="reg_rate",col="treatment", ylim = c(0, 0.1))+
  ylab("regeneration rate")+facet_grid("biome")


# violin plot

desm_v <- df%>% filter(year>2003&year<=2017)%>%
  ggviolin(x = "year",y="desm_rate_ly",col="treatment")+
  ylab("deforestation rate")+facet_grid("biome")


reg_v <- df%>% filter(year>2003&year<=2017)%>%
  ggviolin(x = "year",y="reg_rate",col="treatment")+
  ylab("regeneration rate")+facet_grid("biome")

# boxplot

desm_b <- df%>% filter(year>2003&year<=2017)%>%
  ggboxplot(x = "year",y="desm_rate_ly",col="treatment")+
  ylab("deforestation rate")+facet_grid("biome")


reg_b <- df%>% filter(year>2003&year<=2017)%>%
  ggboxplot(x = "year",y="reg_rate",col="treatment")+
  ylab("regeneration rate")+facet_grid("biome")


# hist

df%>% filter(year>2003&year<=2017)%>%
  ggplot(mapping = aes(x = as.factor(year), y = desm_rate_ly,colour=desm_rate_ly)) +
  # plotting jittered points
  geom_jitter(size = 3, alpha = 0.5, width = 0.15)+facet_grid("biome")+
  scale_color_gradient()+
  theme_classic()



df%>% filter(year>2003&year<=2017)%>%
  ggdotplot(x = "year",y="desm_rate_ly",col="treatment")+
  ylab("deforestation rate")+facet_grid("biome")


ggdotplot()

# como fazer esse grafico mas pra 2 grupos!!?? 

df%>% filter(year>2003&year<=2017)%>%
ggplot(aes(x=as.factor(year),y=desm_rate_ly)) + 
  geom_jitter(colour="lightblue", alpha=0.5, width=0.1) +
  geom_point(stat="summary", fun.y="mean") + 
  geom_errorbar(stat="summary", fun.data="mean_se", fun.args = list(mult = 1.96), width=0) +
  labs(x="Manufacturer", y="Mileage per gallon of fuel (mean + 95%CI)") +
  theme_bw()

# misturando com o de baixo!!eh algo por ai!!

df%>% filter(year>2003&year<=2017)%>%
ggplot (aes(y = desm_rate_ly, x = as.factor(year), fill = treatment, color = treatment, group = interaction(as.factor(year), treatment))) +
  #geom_boxplot(alpha = 0.1, width=0.75) +
  geom_point(position = position_dodge(width=0.75))


df%>% filter(year>2003&year<=2017)%>%
  ggplot(aes(x=as.factor(year),y=desm_rate_ly, 
    fill = treatment, color = treatment, group = interaction(as.factor(year), treatment))) + 
  geom_point(colour="lightblue", alpha=0.5, width=0.1,position = position_jitterdodge(jitter.width=0.85)) +
  geom_point(stat="summary", fun.y="mean",position = position_dodge(width=0.75)) + 
  #geom_errorbar(stat="summary", fun.data="mean_se", fun.args = list(mult = 1.96), width=0) +
  labs(x="Manufacturer", y="Mileage per gallon of fuel (mean + 95%CI)") +
  theme_bw()

#quase. mas ficaria melhor com jitter
#position_jitterdodge(jitter.width=0.85)

df%>% filter(year>2003&year<=2017)%>%
ggplot(aes(y = desm_rate_ly, x = as.factor(year), fill = treatment, color = treatment, group = interaction(as.factor(year), treatment))) +
  geom_boxplot(alpha = 0.1, width=0.75) +
  geom_jitter(position = position_jitterdodge(jitter.width=0.85))


###########################################################################

# melhor solucao ate agora 

###########################################################################


label_x <- seq(5,17,1)


g <-df%>% filter(year>2003&year<=2017)%>%
  ggplot(aes(x = as.factor(year), y = desm_rate_ly,fill = treatment, 
  color = treatment, group = interaction(as.factor(year), treatment))) +
  #coord_flip() +
  coord_cartesian(ylim = c(0,0.1)) +
  scale_color_uchicago() +
  labs(x = NULL, y = "Student to teacher ratio") +
  theme(
    legend.position = "none",
    axis.title = element_text(size = 16),
    axis.text.x = element_text(family = "Roboto Mono", size = 12),
    panel.grid = element_blank()
  )



g+
  geom_boxplot(color = "gray60", outlier.shape = NA) +
  geom_point(size = 3, alpha = 0.15)+
  scale_x_discrete(labels=label_x)

#g + geom_jitter(size = 2, alpha = 0.25, width = 0.2,, outlier.shape = NA)



p2 <- df%>% filter(year>2003&year<=2017)%>% 
  ggplot( aes(x = year,y=desm_rate_ly )) + 
  geom_boxplot(outlier.shape = NA) +
  scale_y_continuous(limits = c(0,0.1))


p2  # no outliers plotted, range shifted

ggplot(df ,aes(x = as.factor(year),y=desm_rate_ly,colour=treatment )) + 
  geom_boxplot(outlier.shape = NA)+
  coord_cartesian(ylim = c(0,0.1))

df%>% filter(year>2003&year<=2017)%>% 
ggboxplot(x="year",y="desm_rate_ly")+ 
  geom_boxplot(outlier.shape = NA)+
  coord_cartesian(ylim = c(0,0.00001))

# tentativa eliminando outliers!!!


sd <- sd()

df%>% filter(year>2003&year<=2017)%>% 
  filter(desm_rate_ly<=2*sd(desm_rate_ly))%>%
  ggboxplot(x = "year",y="desm_rate_ly",col="treatment")+
  ylab("deforestation rate")+facet_grid("biome")

summary(df$desm_rate_ly)
2*sd(df$desm_rate_ly)

# dado tem mto zero, fica uma merda mesmo assim!!

df%>% filter(year==2005)%>% 
gghistogram(x = "desm_rate_ly")+
  coord_flip()


################################################################################

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
    panel.grid = element_blank()
  )





desm_points <- g +
  geom_jitter(colour = "gray",size = 1, alpha = 0.1, width = 0.2) +
  stat_summary(fun = mean, geom = "point", size = 2,stroke = 0.5,shape=21)+
  theme_classic()+facet_wrap(~biome)#+
  #scale_x_discrete(labels=label_x)


desm_points <- ggpar(desm_points,
                 legend = "none",
                 #xlab = "time (years)",
                 #ylab = "average effect on vegetation cover",
                 main = "",
                 font.x=c(7,"bold"),
                 font.y=c(7,"bold"),
                 font.tickslab=(5),
                 x.text.angle = 90)



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
    panel.grid = element_blank()
  )

reg_points <- g2 +
  geom_jitter(colour = "gray",size = 1, alpha = 0.1, width = 0.2) +
  stat_summary(fun = mean, geom = "point", size = 2,stroke =0.5,shape=21)+
  theme_classic()+facet_wrap(~biome)#+
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



g3 <- df%>% filter(year>2003&year<=2017)%>%
  ggplot(aes(x = as.factor(year), y = p_veg, 
             color = treatment, group = interaction(as.factor(year), treatment))) +
  #coord_flip() +
  coord_cartesian(ylim = c(0,0.9)) +
  #scale_color_uchicago() +
  labs(x = NULL, y = "vegetation cover ratio") +
  theme(
    legend.position = "none",
    axis.title = element_text(size = 16),
    axis.text.x = element_text(family = "Roboto Mono", size = 12),
    panel.grid = element_blank()
  )


pveg_points <- g3 +
  geom_jitter(colour = "gray",size = 1, alpha = 0.1, width = 0.2) +
  stat_summary(fun = mean, geom = "point", size = 2,stroke = 0.5,shape=21)+
  theme_classic()+facet_wrap(~biome)#+
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



panel <- ggarrange(desm_points,reg_points,pveg_points,common.legend = T,labels = "auto",ncol = 3)


ggsave(filename = file.path("figures","descriptive_figure.jpeg"),plot = panel,width = 19,
       height = 8,units = "cm")

