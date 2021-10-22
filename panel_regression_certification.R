#===============================================================================

# certification effect on land-use variables

#===============================================================================

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
