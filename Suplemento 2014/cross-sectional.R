require(plyr)
library('dplyr')
library(lmtest)

data = read.csv2("C:/Users/Lhayana/Documents/GitHub/PNAD-C/Suplemento 2014/Dados/dados.csv")

nordeste = filter(data, UF==21 | UF==22 | UF==23 | UF==24 | UF==25 | UF==26 | UF==27 | UF==28 | UF==29)
rn = filter(data, UF==24)

rn_com_pai = filter(rn, morava_pai==1)
reg_esc_pai = lm(renda_dom ~ escolaridade_pai, data=rn_com_pai)
summary(reg_esc_pai)

rn_com_mae = filter(rn, morava_mae==1)
reg_esc_mae = lm(renda_dom ~ escolaridade_mae, data=rn_com_mae)
summary(reg_esc_mae)

rn_com_pai$cor = ifelse(rn_com_pai$cor == 8, 4, rn_com_pai$cor) #juntando pretos e pardos em uma só categoria
reg_esc_pai_etnia = lm(renda_dom ~ escolaridade_pai + factor(cor), data = rn_com_pai)
summary(reg_esc_pai_etnia)

shapiro.test(reg_esc_pai_etnia$residuals) #teste de normalidade
dwtest(reg_esc_pai_etnia) 

#ne_com_pai = filter(nordeste, morava_pai==1)
#ne_com_pai$cor = ifelse(ne_com_pai$cor == 8, 4, ne_com_pai$cor) #juntando pretos e pardos em uma só categoria
#reg_esc_pai_etnia_ne = lm(renda_dom ~ escolaridade_pai + factor(cor), data = ne_com_pai)
#summary(reg_esc_pai_etnia_ne)




#########################################################
# library(plm)
# library(lmtest)
# library(ggplot2)
# library(ggfortify)
# ne_com_pai$cor = as.factor(ne_com_pai$cor)
# ne_com_pai$UF = as.factor(ne_com_pai$UF)
# panel_data = pdata.frame(ne_com_pai, index = c("X"))
# modelo = plm(renda_dom ~ escolaridade_pai + cor + UF, data = panel_data, model = "pooling")
# summary(modelo)
# 
# bptest(modelo, studentize = FALSE, data = panel_data)
# 
# coef(modelo)
# 
# resids <- data.frame(residual = residuals(modelo), attr(residuals(modelo), "index"))
# ggplot(resids, aes(x = X, y = residual)) +
#   geom_point()
# 
# residuos <- resid(modelo)
# set.seed(123)
# amostra_indices <- sample(1:nrow(panel_data), size = 100, replace = FALSE)
# residuos_amostra <- residuos[amostra_indices]
# plot(residuos_amostra, main = "Gráfico de Dispersão dos Resíduos (Amostra)", 
#      xlab = "Índice Observacional", ylab = "Resíduos")
