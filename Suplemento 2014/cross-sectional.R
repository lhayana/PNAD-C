require(plyr)
library('dplyr')
library(lmtest)
library(ggplot2)
library("survey")
library("ggsurvey")
library("questionr")
library(nortest)

data = read.csv2("C:/Users/Lhayana/Documents/GitHub/PNAD-C/Suplemento 2014/Dados/dados.csv")

#transformando escolaridade em anos de estudo
data$anos_estudo_pai = ifelse(data$escolaridade_pai == 1, 1,
                      ifelse(data$escolaridade_pai %in% c(2, 3), 2,
                        ifelse(data$escolaridade_pai == 4, 8,
                          ifelse(data$escolaridade_pai == 5, 12,
                            ifelse(data$escolaridade_pai == 6, 13,
                             ifelse(data$escolaridade_pai == 7, 14,
                               ifelse(data$escolaridade_pai == 8, 15,
                                ifelse(data$escolaridade_pai == 9, 19,
                                  ifelse(data$escolaridade_pai == 10, 21,
                                    ifelse(data$escolaridade_pai == 11, NA,
                                      ifelse(data$escolaridade_pai == 12, 0, NA)
                                    ))))))))))

data$anos_estudo_mae = ifelse(data$escolaridade_mae == 1, 1,
                          ifelse(data$escolaridade_mae %in% c(2, 3), 2,
                                 ifelse(data$escolaridade_mae == 4, 8,
                                        ifelse(data$escolaridade_mae == 5, 12,
                                               ifelse(data$escolaridade_mae == 6, 13,
                                                      ifelse(data$escolaridade_mae == 7, 14,
                                                             ifelse(data$escolaridade_mae == 8, 15,
                                                                    ifelse(data$escolaridade_mae == 9, 19,
                                                                           ifelse(data$escolaridade_mae == 10, 21,
                                                                                  ifelse(data$escolaridade_mae == 11, NA,
                                                                                         ifelse(data$escolaridade_mae == 12, 0, NA)
                                                                                  ))))))))))

data$anos_estudo_filho = ifelse(data$anos_estudo_filho == 1, 0,
                                ifelse(data$anos_estudo_filho == 2, 1,
                                     ifelse(data$anos_estudo_filho == 3, 2,
                                            ifelse(data$anos_estudo_filho == 4, 3,
                                                   ifelse(data$anos_estudo_filho == 5, 4,
                                                          ifelse(data$anos_estudo_filho == 6, 5,
                                                                 ifelse(data$anos_estudo_filho == 7, 6,
                                                                        ifelse(data$anos_estudo_filho == 8, 7,
                                                                               ifelse(data$anos_estudo_filho == 9, 8,
                                                                                      ifelse(data$anos_estudo_filho == 10, 9,
                                                                                             ifelse(data$anos_estudo_filho == 11, 10,
                                                                                                    ifelse(data$anos_estudo_filho == 12, 11,
                                                                                                           ifelse(data$anos_estudo_filho == 13,12,
                                                                                                                  ifelse(data$anos_estudo_filho == 14,13,
                                                                                                                         ifelse(data$anos_estudo_filho == 15,14,
                                                                                                                                ifelse(data$anos_estudo_filho == 16,15,
                                                                                                                                       ifelse(data$anos_estudo_filho == 17,NA, NA)
                                                                                      ))))))))))))))))

data$dummy_cor = ifelse(data$cor %in% c(2, 6), 1, 0)
data$dummy_zona = ifelse(data$zona == 2, 0, 1)
data$log_renda = log(data$renda_dom)
data$dummy_pais_desemp = ifelse(data$pai_empreg == 1, 1,
                                ifelse(data$mae_empreg == 1, 1, 0))
data = filter(data, morava_uf_aos15==1)
data$dummy_sexo = ifelse(data$sex == 4, 0, 1)
data$etnia <- ifelse(data$cor == 4, 8, data$cor)

# nordeste = filter(data, UF==21 | UF==22 | UF==23 | UF==24 | UF==25 | UF==26 | UF==27 | UF==28 | UF==29)
rn = filter(data, UF==24)

pop_types <- data.frame(V4609 = unique(rn$V4609), Freq = unique(rn$V4609))

prestratified_design <-
  svydesign(id = ~ V4618,
            strata = ~ Estrato ,
            data = rn ,
            weights = ~ peso ,
            na.rm = TRUE,
            nest = TRUE)

gc()

dados_estratificados <-
  postStratify(design = prestratified_design,
               strata = ~ V4609,
               population = pop_types)

rn_com_pai = subset(dados_estratificados, morava_pai==1)
#rn_com_pai = filter(rn, morava_pai==1)

svyplot(renda_dom~anos_estudo_pai, design=rn_com_pai)
svyplot(anos_estudo_pai~log_renda, design=rn_com_pai)

ggsurvey(rn_com_pai) +
  aes(x = anos_estudo_pai) +
  geom_bar()

ggsurvey(rn_com_pai) +
  aes(x = log_renda) +
  geom_bar()

modelo = svyglm(log_renda ~ anos_estudo_pai + anos_estudo_filho + age +  factor(etnia) + factor(zona) , design = rn_com_pai)
summary(modelo)
shapiro.test(modelo$residuals)

modelo = svyglm(log_renda ~ anos_estudo_mae + anos_estudo_filho + age + factor(etnia) + factor(zona), design = rn_com_mae)
summary(modelo)
shapiro.test(modelo$residuals)


####### NORDESTE ########
nordeste = filter(data, UF==21 | UF==22 | UF==23 | UF==24 | UF==25 | UF==26 | UF==27 | UF==28 | UF==29)

pop_types <- data.frame(V4609 = unique(nordeste$V4609), Freq = unique(nordeste$V4609))

prestratified_design <-
  svydesign(id = ~ V4618,
            strata = ~ Estrato ,
            data = nordeste ,
            weights = ~ peso ,
            na.rm = TRUE,
            nest = TRUE)

gc()

dados_estratificados <-
  postStratify(design = prestratified_design,
               strata = ~ V4609,
               population = pop_types)

nordeste_com_pai = subset(dados_estratificados, morava_pai==1)
#nordeste_com_pai = filter(nordeste, morava_pai==1)

svyplot(renda_dom~anos_estudo_pai, design=nordeste_com_pai)
svyplot(log_renda~anos_estudo_pai, design=nordeste_com_pai)

ggsurvey(nordeste_com_pai) +
  aes(x = anos_estudo_pai) +
  geom_bar()

ggsurvey(nordeste_com_pai) +
  aes(x = (anos_estudo_pai)^2, y=log_renda) +
  geom_point()+                                      
  stat_smooth(method = "lm", 
              formula = y ~ x, 
              geom = "smooth") 

modelo = svyglm(log_renda ~ anos_estudo_pai + anos_estudo_filho + factor(UF) + factor(cor) + factor(zona), design = nordeste_com_pai)
summary(modelo)
ad.test(modelo$residuals)

