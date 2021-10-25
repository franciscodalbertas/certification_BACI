#===============================================================================

# certification effect on land-use variables

#===============================================================================

#==== packages =================================================================

library(dplyr)
library(tidyr)

#===============================================================================


# reading the data

write.csv(temp_data,"response_variables.csv",row.names = F)


#==== tendencias temporais =====================================================

# atribuindo dados 

ma_2 <- left_join(ma,temp_data)
ma_2$biome <- "Atlantic Forest"
ce_2 <- left_join(ce,temp_data)
ce_2$biome <- "Cerrado"
df_full <- rbind(ma_2,ce_2)

library(doBy)
library(ggplot2)
library(ggpubr)

av <- summaryBy(data = df_full,def_rate_1+reg_rate+p_veg~treatment+year+biome) 


def_year <- ggplot(data = av, 
            aes(x = year, 
                y = def_rate_1.mean,
                group=treatment,
                colour=treatment))+
  #geom_point(aes(colour=treatment),alpha = 0.05)+
  geom_line(size=0.8)+
  #geom_ribbon(aes(x = year, ymin = lo.m01, ymax = up.m01), alpha = 1)+
  #facet_wrap(~COD_IMOVEL, scales = "free") +
  xlab("Year")+
  ylab("deforestation rate")+
  theme_bw()+
  ylim(0,0.1)+
  facet_grid("biome")
  #scale_x_discrete(labels=c(seq(97,99,1),seq(0,18,1)))#+
  #geom_vline(xintercept = 11,linetype = "dashed",colour="red",size=1)

reg_year <- ggplot(data = av, 
                   aes(x = year, 
                       y = reg_rate.mean,
                       group=treatment,
                       colour=treatment))+
  #geom_point(aes(colour=treatment),alpha = 0.05)+
  geom_line(size=0.8)+
  #geom_ribbon(aes(x = year, ymin = lo.m01, ymax = up.m01), alpha = 1)+
  #facet_wrap(~COD_IMOVEL, scales = "free") +
  xlab("Year")+
  ylab("regeneration rate")+
  theme_bw()+
  ylim(0,0.1)+
  facet_grid("biome")

summary(df_full$p_veg)

# padrão bizarro na razao de cobertura!

prop_veg_year <- ggplot(data = av, 
                   aes(x = year, 
                       y = p_veg.mean,
                       group=treatment,
                       colour=treatment))+
  #geom_point(aes(colour=treatment),alpha = 0.05)+
  geom_line(size=0.8)+
  #geom_ribbon(aes(x = year, ymin = lo.m01, ymax = up.m01), alpha = 1)+
  #facet_wrap(~COD_IMOVEL, scales = "free") +
  xlab("Year")+
  ylab("vegetation cover ratio")+
  theme_bw()+
  #ylim(0,1)+
  facet_grid("biome")

#==== agregando data da certificacao nos dados certificados ====================


# eu nao entendo como usar o controle uma vez q mesmo calculando anos antes e
# depois, cada contrato se refere a um conjunto diferente de anos

# tem q ter um df pra cada contrato, pelo jeito.

# teste com cooxupe (falta as propriedades novas 6!)

# dados certificacao

ra <- read.csv(file.path(p,"dados_Imaflora_RA_clean","propriedades_certificadas.csv"))

df_full_s <- df_full[df_full$COD_IMOVEL %in% ra$COD_IMOVEL,]
df_full_s2 <- left_join(df_full_s,ra)

df_full_s2_coox <- df_full_s2[df_full_s2$cert=="Cooxupe_CerradoMG",]

# 2012 - periodo corresponde entao a 2007- 2017

df_full_s2_coox <- df_full_s2_coox[df_full_s2_coox$year<=2017&df_full_s2_coox$year>=2005,]

# controle correspondente

cont_cooxupe <- df_full[df_full$treatment=="non certified"&df_full$year<=2017&
                          df_full$year>=2005&df_full$biome=="Cerrado",]


cooxupe <- rbind(df_full_s2_coox[,c(1,2,5,6:9)],cont_cooxupe[,c(1,2,5:9)])

av_c <- summaryBy(data = cooxupe,def_rate_1+reg_rate+p_veg~treatment+year+biome) 



def_year_c <- ggplot(data = av_c, 
                     aes(x = year, 
                         y = def_rate_1.mean,
                         group=treatment,
                         colour=treatment))+
  #geom_point(aes(colour=treatment),alpha = 0.05)+
  geom_line(size=0.8)+
  #geom_ribbon(aes(x = year, ymin = lo.m01, ymax = up.m01), alpha = 1)+
  #facet_wrap(~COD_IMOVEL, scales = "free") +
  xlab("Year")+
  ylab("deforestation rate")+
  theme_bw()+
  ylim(0,0.1)+
  facet_grid("biome")
#scale_x_discrete(labels=c(seq(97,99,1),seq(0,18,1)))#+
#geom_vline(xintercept = 11,linetype = "dashed",colour="red",size=1)


reg_year_c <- ggplot(data = av_c, 
                     aes(x = year, 
                         y = reg_rate.mean,
                         group=treatment,
                         colour=treatment))+
  #geom_point(aes(colour=treatment),alpha = 0.05)+
  geom_line(size=0.8)+
  #geom_ribbon(aes(x = year, ymin = lo.m01, ymax = up.m01), alpha = 1)+
  #facet_wrap(~COD_IMOVEL, scales = "free") +
  xlab("Year")+
  ylab("reg rate")+
  theme_bw()+
  ylim(0,0.1)+
  facet_grid("biome")
#scale_x_discrete(labels=c(seq(97,99,1),seq(0,18,1)))#+
#geom_vline(xintercept = 11,line

prop_veg_year_c <- ggplot(data = av_c, 
                        aes(x = year, 
                            y = p_veg.mean,
                            group=treatment,
                            colour=treatment))+
  #geom_point(aes(colour=treatment),alpha = 0.05)+
  geom_line(size=0.8)+
  #geom_ribbon(aes(x = year, ymin = lo.m01, ymax = up.m01), alpha = 1)+
  #facet_wrap(~COD_IMOVEL, scales = "free") +
  xlab("Year")+
  ylab("vegetation cover ratio")+
  theme_bw()+
  #ylim(0,1)+
  facet_grid("biome")
