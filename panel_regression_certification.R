#===============================================================================

# certification effect on land-use variables

#===============================================================================

#==== packages =================================================================

<<<<<<< HEAD
library(plm)
library(clubSandwich)
library(ggplot2)
library(ggpubr)
#===============================================================================

df <- read.csv('data_for_panel_regression.csv')

# subset apenas alguns anos, usando o tempo decorrido

# data pra ter -5

ano_ma <- min(df$year[df$event_time>=-5&df$treatment=="certified"&
                        df$biome=="Atlantic Forest"])

df2 <- df[df$year>=2004,]

head(df)
summary(df)
summary(df$treatment)
summary(df2$year)

df2$treatment <- as.factor(df2$treatment)
df2$certification_cat <- as.factor(df2$certification_cat)
df2$year <- as.factor(df2$year)

df2$event_time <- as.factor(df2$event_time)

str(df2)
names(df2)

# ==== Regressão em painel =====================================================

# nao esta estimando coeficientes da interacao. tem algo errado!

m02$model
  
##### desmatamento MA ##########################################################


dfma <- df2[df2$biome=="Atlantic Forest"&df2$year!=2004,]

summary(dfma$treatment)

summary(df$certification_cat[df$treatment=="non certified"])

m01 <- plm(formula = def_rate_1 ~ treatment*certification_cat, data = dfma, 
      effect="individual",model = "within", index = c("COD_IMOVEL","year"))

summary(m01)

#---- obs------------------------------------------------------------------------
# duplicate couples (id-time): ha valores duplicados de imoveis com o event_time=0
# uma vez que realmente,tudo q eh nao certificado por ex tem valor 0. teria q nao
# indexar pelo imovel...mas eh meio merda isso. O artigo de referencia era bem di
# ferente o modelo acho! 

# Pra resolver eh so remover as duplicatas de propriedades
# nao certificadas com relacao ao tempo , q eh sempre 0! mas ai fode o esquema 
# pq o valor da variavel resposta nao eh igual! NAO DA PRA FAZER ISSO! poderia
# indexar nao pelo codigo imovel mas pela comb de codigo com ano...
# nao da certo....acho q ali ainda tem q ser year...mas em algum lugar tem q 
# entrar o tempo discorrido...
#-------------------------------------------------------------------------------

# tentando formula do artigo de ref!

# event_time como fator!

m02 <- plm(formula = def_rate_1 ~ treatment*event_time, data = dfma, 
           effect="individual",model = "within", index = c("COD_IMOVEL","year"))



summary(m02)
# pra plotar seria coloar os coeficientes + IC. so q aqui sao soh os coef. e 
# tem jeito de considerar o N, q nao sei ainda

coef <-coef_test(m02, vcovCR(m02, cluster = dfma$COD_IMOVEL, type = "CR1S"))

coef$variables <- row.names(coef)

# plotando como os desgramado fizeram

ma_p <- ggplot(coef, aes(x=variables, y=beta)) + 
  geom_errorbar(aes(ymin=beta-SE, ymax=beta+SE), width=.1) +
  #geom_line(position=pd) +
  geom_point()+
  geom_hline(yintercept=0, linetype="dashed",color = "red", size=1)+
  scale_x_discrete(labels=seq(-13,8,1))+
  xlab("")+
  geom_vline(xintercept=15, linetype="dashed",color = "black", size=1)+
  theme_classic()+
  ggtitle("Atlantic Forest") 

# desmatamento CE 

dfce <- df2[df2$biome=="Cerrado",]

m03 <- plm(formula = def_rate_1 ~ treatment*event_time, data = dfce, 
           effect="individual",model = "within", index = c("COD_IMOVEL","year"))

coef_ce <-coef_test(m03, vcovCR(m03, cluster = dfce$COD_IMOVEL, type = "CR1S"))

coef_ce$variables <- row.names(coef_ce)

ce_p <- ggplot(coef_ce, aes(x=variables, y=beta)) + 
  geom_errorbar(aes(ymin=beta-SE, ymax=beta+SE), width=.1) +
  #geom_line(position=pd) +
  geom_point()+
  geom_hline(yintercept=0, linetype="dashed",color = "red", size=1)+
  scale_x_discrete(labels=seq(-14,8,1))+
  xlab("")+
  geom_vline(xintercept=15, linetype="dashed",color = "black", size=1)+
  theme_classic()+
  ggtitle("Cerrado")


ggarrange(ma_p,ce_p,nrow = 2)

##### reg MA ###################################################################


m04 <- plm(formula = reg_rate ~ treatment*event_time, data = dfma, 
           effect="individual",model = "within", index = c("COD_IMOVEL","year"))

summary(m04)


coef_ma_reg <-coef_test(m04, vcovCR(m04, cluster = dfma$COD_IMOVEL, type = "CR1S"))

coef_ma_reg$variables <- row.names(coef_ma_reg)

ma_p_reg <- ggplot(coef_ma_reg, aes(x=variables, y=beta)) + 
  geom_errorbar(aes(ymin=beta-SE, ymax=beta+SE), width=.1) +
  #geom_line(position=pd) +
  geom_point()+
  geom_hline(yintercept=0, linetype="dashed",color = "red", size=1)+
  scale_x_discrete(labels=seq(-14,8,1))+
  xlab("")+
  geom_vline(xintercept=15, linetype="dashed",color = "black", size=1)+
  theme_classic()+
  ggtitle("Atlantic Forest")


##### reg CE ###################################################################


m05 <- plm(formula = reg_rate ~ treatment*event_time, data = dfce, 
           effect="individual",model = "within", index = c("COD_IMOVEL","year"))

summary(m05)


coef_ce_reg <-coef_test(m05, vcovCR(m05, cluster = dfce$COD_IMOVEL, type = "CR1S"))

coef_ce_reg$variables <- row.names(coef_ce_reg)

ce_p_reg <- ggplot(coef_ce_reg, aes(x=variables, y=beta)) + 
  geom_errorbar(aes(ymin=beta-SE, ymax=beta+SE), width=.1) +
  #geom_line(position=pd) +
  geom_point()+
  geom_hline(yintercept=0, linetype="dashed",color = "red", size=1)+
  scale_x_discrete(labels=seq(-14,8,1))+
  xlab("")+
  geom_vline(xintercept=15, linetype="dashed",color = "black", size=1)+
  theme_classic()+
  ggtitle("Cerrado")

ggarrange(ma_p_reg,ce_p_reg,nrow = 2)

##### prop veg ma ##############################################################


m06 <- plm(formula = p_veg ~ treatment*event_time, data = dfma, 
           effect="individual",model = "within", index = c("COD_IMOVEL","year"))

summary(m06)


coef_ma_pveg <-coef_test(m06, vcovCR(m06, cluster = dfma$COD_IMOVEL, type = "CR1S"))

coef_ma_pveg$variables <- row.names(coef_ma_pveg)

ma_p_veg <- ggplot(coef_ma_pveg, aes(x=variables, y=beta)) + 
  geom_errorbar(aes(ymin=beta-SE, ymax=beta+SE), width=.1) +
  #geom_line(position=pd) +
  geom_point()+
  geom_hline(yintercept=0, linetype="dashed",color = "red", size=1)+
  scale_x_discrete(labels=seq(-14,8,1))+
  xlab("")+
  geom_vline(xintercept=15, linetype="dashed",color = "black", size=1)+
  theme_classic()+
  ggtitle("Atlantic Forest")


summary(dfma$p_veg[dfma$biome=="Atlantic Forest"]) 

##### prop veg ce ##############################################################

m07 <- plm(formula = p_veg ~ treatment*event_time, data = dfce, 
           effect="individual",model = "within", index = c("COD_IMOVEL","year"))

summary(m07)


coef_ce_pveg <-coef_test(m07, vcovCR(m07, cluster = dfce$COD_IMOVEL, type = "CR1S"))

coef_ce_pveg$variables <- row.names(coef_ce_pveg)

ce_p_veg <- ggplot(coef_ce_pveg, aes(x=variables, y=beta)) + 
  geom_errorbar(aes(ymin=beta-SE, ymax=beta+SE), width=.1) +
  #geom_line(position=pd) +
  geom_point()+
  geom_hline(yintercept=0, linetype="dashed",color = "red", size=1)+
  scale_x_discrete(labels=seq(-14,8,1))+
  xlab("")+
  geom_vline(xintercept=15, linetype="dashed",color = "black", size=1)+
  theme_classic()+
  ggtitle("Cerrado")

ggarrange(ma_p_veg,ce_p_veg,nrow = 2)
