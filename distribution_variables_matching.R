#===============================================================================

# histogramas com as variáveis do matching
# deve haver sobreposição dos dados!!

#===============================================================================

#====== pacotes ===============================================================

library(dplyr)
# plotar como ggplot!
library(ggpubr)

#==============================================================================


# pasta raiz

p <- dirname(getwd())


# dados

df <- read.csv("data_for_matching.csv")

names(df)[8] <- "biome"

df$biome[df$biome=="CERRADO"] <- "Cerrado"
df$biome[df$biome=="MATA ATLÂNTICA"] <- "Atlantic Forest"

summary(as.factor(df$biome))


# converter RL to factor

df$LR_bin <- as.factor(df$LR_bin)

# transformando area em variavel categorica

df$area_ct <- NA

df$area_ct[df$area_im_mf<=4] <- "s"

df$area_ct[df$area_im_mf>4&df$area_im_mf<=15] <- "m"

df$area_ct[df$area_im_mf>15] <- "l"

df$area_ct <- as.factor(df$area_ct)

# range valores modulos fiscais

df%>% group_by(area_ct)%>%
  summarise(min(area_im),max(area_im))

# pasture como categorico

df$prop_past_09_ct <- cut(df$prop_past_09,breaks = c(-0.1,0.25,1.1)  )

#==== variaveis do matching ===================================================

# natural vegetation cover [15] ;
# pasture cover [18]; 
# the sum of deforestation rates between 2004-2009 [2]; 
# the sum of regeneration rates between 2004-2009 [14]; 
# presence/absence of Legal Reserve [5]; 
# property area [17]; 
# mean slope [7]

#===============================================================================

# natural vegetation cover [15] ------------------------------------------------


veg_density <- ggdensity(data = df,x = "prop_veg_09",color = "treatment")+
  facet_wrap("biome")+
  theme(strip.background = element_blank())


veg_density <- ggpar(veg_density,
                     xlab = "Natural vegetation cover",
                     #ylab = "average effect on vegetation cover",
                     main = "",
                     font.x=c(7,"bold"),
                     font.y=c(7,"bold"),
                     font.tickslab=(5)
                      )


# pasture cover ----------------------------------------------------------------


#  x tem q ser numerico!Plotar valores numericos?


past_density<- ggdensity(data = df,
    x = "prop_past_09",
    color = "treatment")+
    facet_wrap("biome")+
    theme(strip.background = element_blank())


past_density <- ggpar(past_density,
                     xlab = "Pasture cover",
                     #ylab = "average effect on vegetation cover",
                     main = "",
                     font.x=c(7,"bold"),
                     font.y=c(7,"bold"),
                     font.tickslab=(5)
)


# deforestation ----------------------------------------------------------------


#  x tem q ser numerico!Plotar valores numericos?


summary(df$desm_ac)

def_density<- ggdensity(data = df,
                         x = "desm_ac",
                         color = "treatment")+
  facet_wrap("biome")+
  theme(strip.background = element_blank())


def_density <- ggpar(def_density,
                      xlab = "Sum of deforestation rates",
                      #ylab = "average effect on vegetation cover",
                      main = "",
                      font.x=c(7,"bold"),
                      font.y=c(7,"bold"),
                      font.tickslab=(5)
)


# regeneration  ----------------------------------------------------------------


#  x tem q ser numerico!Plotar valores numericos?

reg_density<- ggdensity(data = df,
                        x = "reg_ac",
                        color = "treatment")+
  facet_wrap("biome")+
  theme(strip.background = element_blank())


reg_density <- ggpar(reg_density,
                     xlab = "Sum of regeneration rates",
                     #ylab = "average effect on vegetation cover",
                     main = "",
                     font.x=c(7,"bold"),
                     font.y=c(7,"bold"),
                     font.tickslab=(5)
)

# LR  ----------------------------------------------------------------


#  x tem q ser numerico!Plotar valores numericos?



LR <- df %>%
  count(LR_bin, treatment,biome) %>%
  group_by(treatment,biome) %>%          # now required with changes to dplyr::count()
  mutate(prop = prop.table(n))

names(LR)[1] <- "Presence of LR declared"

LR_bar<- ggbarplot(data = LR,
          fill = "Presence of LR declared",
          y= "prop",  
          x = "treatment")+
          facet_wrap("biome")+
          theme(strip.background = element_blank())


LR_bar <- ggpar(LR_bar,
                     #xlab = "Sum of regeneration rates",
                     #ylab = "average effect on vegetation cover",
                     main = "",
                     font.x=c(7,"bold"),
                     font.y=c(7,"bold"),
                     font.tickslab=(5),
                     font.legend = (7)
)




# property area ----------------------------------------------------------------

area_density<- ggdensity(data = df,
                        x = "area_im",
                        color = "treatment")+
  facet_wrap("biome")+
  theme(strip.background = element_blank())


area_density <- ggpar(area_density,
                     xlab = "Property area (ha)",
                     #ylab = "average effect on vegetation cover",
                     main = "",
                     font.x=c(7,"bold"),
                     font.y=c(7,"bold"),
                     font.tickslab=(5)
)

#slope ----------------------------------------------------------------

slope_density<- ggdensity(data = df,
  x = "mean_slope",
  color = "treatment")+
  facet_wrap("biome")+
  theme(strip.background = element_blank())


slope_density <- ggpar(slope_density,
                      xlab = "Mean slope (degrees)",
                      #ylab = "average effect on vegetation cover",
                      main = "",
                      font.x=c(7,"bold"),
                      font.y=c(7,"bold"),
                      font.tickslab=(5))

#==== combinando em figura única ===============================================


continuas <- ggarrange(veg_density,def_density,reg_density,area_density,slope_density,
          common.legend = T,ncol = 2,nrow = 3)



ggsave(filename = file.path("figures","density_plot_var_match_cont.jpeg"),
       plot = continuas,width = 16,height = 20,units = "cm")


ggsave(filename = file.path("figures","barplot_LR_match_cont.jpeg"),
       plot = LR_bar,width = 8,height =8,units = "cm")
