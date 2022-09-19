
# pacotes

library(sf)
library(dplyr)
library(tidyr)
library(plm)
require(MatchIt)
require(optmatch)
library(ggpubr)
library(scales)
library(RColorBrewer)

# pasta raiz

p <- dirname(getwd())

df <- read.csv("data_for_matching.csv")
names(df)[8] <- "biome"

summary(as.factor(df$biome[df$treatment=="certified"]))

# converter RL to factor

df$LR_bin <- as.factor(df$LR_bin)

# transformando area em variavel categorica

df$area_ct <- NA

df$area_ct[df$area_im_mf<=4] <- "s"

df$area_ct[df$area_im_mf>4&df$area_im_mf<=15] <- "m"

df$area_ct[df$area_im_mf>15] <- "l"

df$area_ct <- as.factor(df$area_ct)

#Distribuição da proporcao de pastagens de forma categorica:

df$prop_past_09_ct <- cut(df$prop_past_09,breaks = c(-0.1,0.25,1.1)  )

# Aqui gera uma matrix aleatória que a gente pode repetir
set.seed(1)
rand<-sample(nrow(df))
# #rand
data.rand1<-df[rand, ]
#head(data.rand1)
# 
data.rand1$treatment <- as.factor(data.rand1$treatment)
data.rand1$treatment <- relevel(data.rand1$treatment, ref = "non certified")

# formula com area im cat

formula6 <- treatment ~ desm_ac + LR_bin + propAPP +area_ct + prop_veg_09  + prop_past_09_ct+ mean_slope + reg_ac

# separando biomas

df_ma <- subset(data.rand1,subset = biome=="MATA ATLÂNTICA")
df_ce <- subset(data.rand1,subset = biome=="CERRADO")

# matching

#aqui vai definir o intervalo de valores de caliper a serem usados

caliper_values<-c(NULL,seq(0.25,1.00,0.25))

#aqui separa o dado que tem apenas o tratamento

treat_ma<-df_ma[df_ma$treatment=="certified", ]
# variaveis usadas
variaveis <- names(df_ma)[c(2,5,6,7,14,15,17,18)]

#aqui cria dois objetos para salvar os resultados
mean_difs<-data.frame()
treated_matched<-data.frame()

for(caliper in caliper_values){
  #MA
  nn_ma <-matchit(formula6, data=df_ma, method="nearest", ratio=5, caliper = caliper,replace = T)
  output <-summary(nn_ma, standardize = TRUE)
  #aqui vai extrair os valores dos resultados de mean diff das tabelas Summary of balance for matched data e da tabela Percent Balance Improvement
  mean_difs_output<-as.data.frame(cbind(rownames(output$sum.matched),output$sum.matched[,3],output$reduction[,1]))
  #aqui insere o valor de caliper, a formula do modelo e um id numerico do modelo
  mean_difs_output$caliper<-caliper
  #aqui vai criar um outro data frame com os valores de "Treated Matched" da tabela Sample sizes
  treated_matched_output<-cbind(output$nn[2,2],caliper)
  # cria colunas vazias para inserir os resultados de valor do teste T, p-value, intervalos de confianca e media dos valroes de x e y
  #mean_difs_output[,5:10]<-NA
  #aqui extrai os valores do grupo controle
  control_output<-match.data(nn_ma, group="control")
  # teste t das variaveis
  
  # tem q fazer isso abaixo pra todas -- pensar num loop tb
  #lista_testes <- list()
  # for (variables in variaveis){
  #   t_test<-t.test(treat_ma[,grep(variables,colnames(treat_ma))], control_output[,(grep(variables,colnames(control_output)))])
  #   mean_difs_output[which(mean_difs_output==variables),5:10]<-cbind(t_test$statistic,t_test$p.value,t_test$conf.int[1],t_test$conf.int[2],t_test$estimate[1],t_test$estimate[2])
  #   
  #  }
  #aqui vai juntar os resultados desta rodada do loop com os resultados das rodadas anteriores
  mean_difs<-rbind(mean_difs,mean_difs_output)
  treated_matched<-rbind(treated_matched,treated_matched_output)
}

#agora vai inserir os nomes das colunas dos data frames contendo os resultados
colnames(mean_difs)<-c("Variable","SD_Mean_diff_matched","Mean_diff_improvement","Caliper")

colnames(treated_matched)<-c("Treated_matched","Caliper")


# plotando resultados dos testes de caliper


nn_ce <-matchit(formula6, data=df_ce, method="nearest", ratio=5, caliper = 0.25,replace = T)

plot(Treated_matched~Caliper, data=treated_matched,col="red", ylim=c(50,100))

points(y=treated_matched,x=treated_matched,col="darkorchid")


#Altitude
plot(SD_Mean_diff_matched~Caliper,data=mean_difs,col="red", ylim=c(-1,1), ylab="SD Mean Difference", main="Mean altitud")

points(y=mean_difs,x=mean_difs, col="darkorchid")

mean_difs$shape <- NA

mean_difs$shape[grep(pattern = "LR",x =mean_difs$Variable)] <- "LR"
mean_difs$shape[grep(pattern = "area",x =mean_difs$Variable)] <- "area"
mean_difs$shape[grep(pattern = "past",x =mean_difs$Variable)] <- "prop.pasture"
mean_difs$shape[grep(pattern = "veg",x =mean_difs$Variable)] <- "prop.veg"
mean_difs$shape[grep(pattern = "desm",x =mean_difs$Variable)] <- "desm."
mean_difs$shape[grep(pattern = "APP",x =mean_difs$Variable)] <- "prop.APP"
mean_difs$shape[grep(pattern = "lope",x =mean_difs$Variable)] <- "slope"
mean_difs$shape[grep(pattern = "reg",x =mean_difs$Variable)] <- "reg."


# FALTA FAZER COM CALIPER NULO

# PRA FICAR SHOW DE BOLA


mean_difs <- mean_difs[mean_difs$Variable!="distance",]

ggplot(mean_difs,aes(x=Caliper,y=as.numeric(SD_Mean_diff_matched),colour=shape) )+
  #geom_point(shape = 21,size = 3,alpha = 1/2,position=position_dodge(width=0.1))+
  geom_point(size = 3,alpha = 1/2)+
  ylim(-1,1)+
  geom_hline(yintercept=0.2, linetype="dashed", color = "red")+
  geom_hline(yintercept=-0.2, linetype="dashed", color = "red")+
  geom_hline(yintercept=0, linetype="dashed", color = "black")+
  ylab("standard mean difference")+
  xlab("caliper")+
  scale_color_brewer(palette = "Accent")+
  theme_classic()

