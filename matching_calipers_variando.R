
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

library(readr)
df <- read_csv("data_for_matching.csv", 
                              locale = locale(encoding = "WINDOWS-1252"))

# df <- read.csv("data_for_matching.csv")
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

###############################################################################

# MA

###############################################################################



for(caliper in caliper_values){
  
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

colnames(mean_difs)<-c("Variable","SD_Mean_diff_matched","Caliper")

colnames(treated_matched)<-c("Treated_matched","Caliper")

# FALTA FAZER COM CALIPER NULO

mean_difs_null<-data.frame()
treated_matched_null<-data.frame()


  #MA
  nn_ma_null <-matchit(formula6, data=df_ma, method="nearest", ratio=5,replace = T)
  output_null <-summary(nn_ma_null, standardize = TRUE)
  #aqui vai extrair os valores dos resultados de mean diff das tabelas Summary of balance for matched data e da tabela Percent Balance Improvement
  mean_difs_output_null<-as.data.frame(cbind(rownames(output_null$sum.matched),output_null$sum.matched[,3],output_null$reduction[,1]))
  #aqui insere o valor de caliper, a formula do modelo e um id numerico do modelo
  mean_difs_output_null$caliper<-0
  #aqui vai criar um outro data frame com os valores de "Treated Matched" da tabela Sample sizes
  treated_matched_output_null<-cbind(output_null$nn[2,2],0)
  # cria colunas vazias para inserir os resultados de valor do teste T, p-value, intervalos de confianca e media dos valroes de x e y
  #mean_difs_output[,5:10]<-NA
  #aqui extrai os valores do grupo controle
  control_output_null<-match.data(nn_ma_null, group="control")
  # teste t das variaveis
  
  # tem q fazer isso abaixo pra todas -- pensar num loop tb
  #lista_testes <- list()
  # for (variables in variaveis){
  #   t_test<-t.test(treat_ma[,grep(variables,colnames(treat_ma))], control_output[,(grep(variables,colnames(control_output)))])
  #   mean_difs_output[which(mean_difs_output==variables),5:10]<-cbind(t_test$statistic,t_test$p.value,t_test$conf.int[1],t_test$conf.int[2],t_test$estimate[1],t_test$estimate[2])
  #   
  #  }
  #aqui vai juntar os resultados desta rodada do loop com os resultados das rodadas anteriores
  mean_difs_null<-rbind(mean_difs_null,mean_difs_output_null)
  treated_matched_null<-rbind(treated_matched_null,treated_matched_output_null)

  colnames(mean_difs_null)<-c("Variable","SD_Mean_diff_matched","Caliper")
  
  colnames(treated_matched_null)<-c("Treated_matched","Caliper")  
  
  
mean_difs_f <- rbind(mean_difs,mean_difs_null)
  
mean_difs_f <- mean_difs_f[mean_difs_f$Variable!="distance",]

# agregando modelo sem caliper



mean_difs_f$shape <- NA

mean_difs_f$shape[grep(pattern = "LR",x =mean_difs_f$Variable)] <- "LR"
mean_difs_f$shape[grep(pattern = "area",x =mean_difs_f$Variable)] <- "area"
mean_difs_f$shape[grep(pattern = "past",x =mean_difs_f$Variable)] <- "prop.pasture"
mean_difs_f$shape[grep(pattern = "veg",x =mean_difs_f$Variable)] <- "prop.veg"
mean_difs_f$shape[grep(pattern = "desm",x =mean_difs_f$Variable)] <- "desm."
mean_difs_f$shape[grep(pattern = "APP",x =mean_difs_f$Variable)] <- "prop.APP"
mean_difs_f$shape[grep(pattern = "lope",x =mean_difs_f$Variable)] <- "slope"
mean_difs_f$shape[grep(pattern = "reg",x =mean_difs_f$Variable)] <- "reg."


figure_changing_caliper_ma <- ggplot(mean_difs_f,aes(x=Caliper,y=as.numeric(SD_Mean_diff_matched),colour=shape) )+
  #geom_point(shape = 21,size = 3,alpha = 1/2,position=position_dodge(width=0.1))+
  geom_point(size = 3,alpha = 1/2)+
  #ylim(-1,1)+
  geom_hline(yintercept=0.25, linetype="dashed", color = "red")+
  geom_hline(yintercept=-0.25, linetype="dashed", color = "red")+
  geom_hline(yintercept=0, linetype="dashed", color = "black")+
  ylab("standard mean difference")+
  xlab("caliper")+
  scale_color_brewer(palette = "Accent",name="matching variables")+
  scale_y_continuous(breaks = seq(-1.0,1.0,by=0.25),limits = c(-1,1))+
  #ylim(-1,1)+
  theme_classic()

figure_changing_caliper_ma <- ggpar(figure_changing_caliper_ma,
                 main = "",
                 font.x=c(7,"bold"),
                 font.y=c(7,"bold"),
                 font.tickslab=(5))


###############################################################################

# CE

###############################################################################


mean_difs_CE<-data.frame()
treated_matched_CE<-data.frame()

for(caliper in caliper_values){
  
  nn_ce <-matchit(formula6, data=df_ce, method="nearest", ratio=5, caliper = caliper,replace = T)
  output <-summary(nn_ce, standardize = TRUE)
  #aqui vai extrair os valores dos resultados de mean diff das tabelas Summary of balance for matched data e da tabela Percent Balance Improvement
  mean_difs_output<-as.data.frame(cbind(rownames(output$sum.matched),output$sum.matched[,3],output$reduction[,1]))
  #aqui insere o valor de caliper, a formula do modelo e um id numerico do modelo
  mean_difs_output$caliper<-caliper
  #aqui vai criar um outro data frame com os valores de "Treated Matched" da tabela Sample sizes
  treated_matched_output<-cbind(output$nn[2,2],caliper)
  # cria colunas vazias para inserir os resultados de valor do teste T, p-value, intervalos de confianca e media dos valroes de x e y
  #mean_difs_output[,5:10]<-NA
  #aqui extrai os valores do grupo controle
  control_output<-match.data(nn_ce, group="control")
  # teste t das variaveis
  
  # tem q fazer isso abaixo pra todas -- pensar num loop tb
  #lista_testes <- list()
  # for (variables in variaveis){
  #   t_test<-t.test(treat_ma[,grep(variables,colnames(treat_ma))], control_output[,(grep(variables,colnames(control_output)))])
  #   mean_difs_output[which(mean_difs_output==variables),5:10]<-cbind(t_test$statistic,t_test$p.value,t_test$conf.int[1],t_test$conf.int[2],t_test$estimate[1],t_test$estimate[2])
  #   
  #  }
  #aqui vai juntar os resultados desta rodada do loop com os resultados das rodadas anteriores
  mean_difs_CE<-rbind(mean_difs_CE,mean_difs_output)
  treated_matched_CE<-rbind(treated_matched_CE,treated_matched_output)
}

#agora vai inserir os nomes das colunas dos data frames contendo os resultados

colnames(mean_difs_CE)<-c("Variable","SD_Mean_diff_matched","Caliper")

colnames(treated_matched_CE)<-c("Treated_matched","Caliper")

# FALTA FAZER COM CALIPER NULO

mean_difs_null_ce<-data.frame()
treated_matched_null_ce<-data.frame()

nn_ce_null <-matchit(formula6, data=df_ce, method="nearest", ratio=5,replace = T)
output_null <-summary(nn_ce_null, standardize = TRUE)
#aqui vai extrair os valores dos resultados de mean diff das tabelas Summary of balance for matched data e da tabela Percent Balance Improvement
mean_difs_output_null<-as.data.frame(cbind(rownames(output_null$sum.matched),output_null$sum.matched[,3],output_null$reduction[,1]))
#aqui insere o valor de caliper, a formula do modelo e um id numerico do modelo
mean_difs_output_null$caliper<-0
#aqui vai criar um outro data frame com os valores de "Treated Matched" da tabela Sample sizes
treated_matched_output_null<-cbind(output_null$nn[2,2],0)
# cria colunas vazias para inserir os resultados de valor do teste T, p-value, intervalos de confianca e media dos valroes de x e y
#mean_difs_output[,5:10]<-NA
#aqui extrai os valores do grupo controle
control_output_null<-match.data(nn_ce_null, group="control")
# teste t das variaveis

# tem q fazer isso abaixo pra todas -- pensar num loop tb
#lista_testes <- list()
# for (variables in variaveis){
#   t_test<-t.test(treat_ma[,grep(variables,colnames(treat_ma))], control_output[,(grep(variables,colnames(control_output)))])
#   mean_difs_output[which(mean_difs_output==variables),5:10]<-cbind(t_test$statistic,t_test$p.value,t_test$conf.int[1],t_test$conf.int[2],t_test$estimate[1],t_test$estimate[2])
#   
#  }
#aqui vai juntar os resultados desta rodada do loop com os resultados das rodadas anteriores
mean_difs_null_ce<-rbind(mean_difs_null_ce,mean_difs_output_null)
treated_matched_null_ce<-rbind(treated_matched_null_ce,treated_matched_output_null)

colnames(mean_difs_null_ce)<-c("Variable","SD_Mean_diff_matched","Caliper")

colnames(treated_matched_null_ce)<-c("Treated_matched","Caliper")  


mean_difs_f_ce <- rbind(mean_difs_CE,mean_difs_null_ce)

mean_difs_f_ce <- mean_difs_f_ce[mean_difs_f_ce$Variable!="distance",]

mean_difs_f_ce$shape <- NA

mean_difs_f_ce$shape[grep(pattern = "LR",x =mean_difs_f_ce$Variable)] <- "LR"
mean_difs_f_ce$shape[grep(pattern = "area",x =mean_difs_f_ce$Variable)] <- "area"
mean_difs_f_ce$shape[grep(pattern = "past",x =mean_difs_f_ce$Variable)] <- "prop.pasture"
mean_difs_f_ce$shape[grep(pattern = "veg",x =mean_difs_f_ce$Variable)] <- "prop.veg"
mean_difs_f_ce$shape[grep(pattern = "desm",x =mean_difs_f_ce$Variable)] <- "desm."
mean_difs_f_ce$shape[grep(pattern = "APP",x =mean_difs_f_ce$Variable)] <- "prop.APP"
mean_difs_f_ce$shape[grep(pattern = "lope",x =mean_difs_f_ce$Variable)] <- "slope"
mean_difs_f_ce$shape[grep(pattern = "reg",x =mean_difs_f_ce$Variable)] <- "reg."

figure_changing_caliper_ce <- ggplot(mean_difs_f_ce,aes(x=Caliper,y=as.numeric(SD_Mean_diff_matched),colour=shape) )+
  #geom_point(shape = 21,size = 3,alpha = 1/2,position=position_dodge(width=0.1))+
  geom_point(size = 3,alpha = 1/2)+
  #ylim(-1,1)+
  geom_hline(yintercept=0.25, linetype="dashed", color = "red")+
  geom_hline(yintercept=-0.25, linetype="dashed", color = "red")+
  geom_hline(yintercept=0, linetype="dashed", color = "black")+
  ylab("standard mean difference")+
  xlab("caliper")+
  scale_color_brewer(palette = "Accent",name="matching variables")+
  scale_y_continuous(breaks = seq(-1.0,1.0,by=0.25),limits = c(-1,1))+
  #ylim(-1,1)+
  theme_classic()

figure_changing_caliper_ce <- ggpar(figure_changing_caliper_ce,
                                    main = "",
                                    font.x=c(7,"bold"),
                                    font.y=c(7,"bold"),
                                    font.tickslab=(5))


final_caliper_plot <- ggarrange(figure_changing_caliper_ma,figure_changing_caliper_ce,labels = "auto",common.legend = T)
