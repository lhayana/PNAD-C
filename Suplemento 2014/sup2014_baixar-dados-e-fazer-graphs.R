# SUPLEMENTO: https://www.ibge.gov.br/estatisticas/sociais/trabalho/19898-suplementos-pnad3.html?edicao=17983&t=downloads

#devtools::install_github("lhayana/microdadosBrasil")
library('microdadosBrasil')
library('dplyr')
library(dbplyr)
library("survey")
library("srvyr")
library(tidyr)
library(ggplot2)
library(scales)
library(RColorBrewer)
library(survey)
options(survey.lonely.psu = "adjust")


pessoas = read_PNAD('pessoas', 2014, file = path.expand("C:/Users/Lhayana/Documents/GitHub/PNAD-C/Suplemento 2014/Dados/PES2014.txt"))
domicilios = read_PNAD('domicilios', 2014, file = path.expand("C:/Users/Lhayana/Documents/GitHub/PNAD-C/Suplemento 2014/Dados/DOM2014.txt"))
# V4742 = rendimento dom. per capita
pessoas = select(pessoas,UF,V0102,V0103,V4729,V32039,V0404,V4742,V0305,V32001,V32004,V32010,V32012, V32024, V32026,V4743,V4803,V9611,V8005,V0302, V32015, V32029)
domicilios = select(domicilios, V0102,V0103,V4602,UPA,V4609, V4617, V4626, V4618)

pessoas = filter(pessoas, V0305==2) #Selecionados para o Suplemento de Mobilidade Sócio-Ocupacional

base = merge(x=pessoas,y=domicilios,by=c("V0102","V0103"))
base$V4609 = as.numeric(base$V4609)

base$V4742 = sub("^0+", "", base$V4742) #tira zeros da esquerda  
base$V4742 = as.integer(base$V4742)
base$V4602 = as.numeric(base$V4602)
base$V32039 = as.numeric(base$V32039)
 
base = filter(base, V0305==2) #Selecionados para o Suplemento de Mobilidade Sócio-Ocupacional
base = rename(base, Estrato = V4617, peso = V4729, cor = V0404, renda_dom=V4742, renda_faixa=V4743, morava_uf_aos15=V32001, zona=V32004, morava_pai=V32010, escolaridade_pai=V32012, morava_mae=V32024, escolaridade_mae=V32026,anos_estudo_filho=V4803, anos_exp = V9611, age=V8005, sex=V0302, pai_empreg=V32015, mae_empreg=V32029)
class(base$V4609)
rm(pessoas,domicilios)

write.csv2(base, "C:/Users/Lhayana/Documents/GitHub/PNAD-C/Suplemento 2014/Dados/dados.csv")

# pop_types <- data.frame(V4609 = unique(base$V4609), Freq = unique(base$V4609))
# prestratified_design <-
#   svydesign(id = ~ V4618,
#             strata = ~ Estrato ,
#             data = base ,
#             weights = ~ peso ,
#             na.rm = TRUE,
#             nest = TRUE)
# gc()
# dados_estratificados <-
#   postStratify(design = prestratified_design,
#                strata = ~ V4609,
#                population = pop_types)
# 
# rm(prestratified_design)
# gc()

################ GRÁFICOS ##################################################

################ PAI ########################################

#ESCOLARIDADE DO PAI

base = mutate(base, escolaridade_pai = case_when((escolaridade_pai == "01") ~ "Fundamental ou inferior",
                                                 (escolaridade_pai == "02") ~ "Fundamental ou inferior",
                                                 (escolaridade_pai == "03") ~ "Fundamental ou inferior",
                                                 (escolaridade_pai == "04") ~ "Fundamental ou inferior",
                                                 (escolaridade_pai == "05") ~ "Fundamental ou inferior",
                                                 (escolaridade_pai == "06") ~ "Fundamental ou inferior",
                                                 (escolaridade_pai == "07") ~ "Fundamental ou inferior",
                                                 (escolaridade_pai == "08") ~ "Médio",
                                                 (escolaridade_pai == "09") ~ "Superior",
                                                 (escolaridade_pai == "10") ~ "Mestrado/Doutorado",
                                                 (escolaridade_pai == "11") ~ NA,
                                                 (escolaridade_pai == "12") ~ "Nenhum",
))

base$escolaridade_pai <- factor(base$escolaridade_pai,
                  levels = c("Nenhum","Fundamental ou inferior", "Médio", "Superior", "Mestrado/Doutorado")) #reordenando

# Escolaridade do pai por contagem populacional

base %>% #contagem populacional, nao amostral, pois está com peso
  filter(morava_pai == 1) %>%
  filter(is.na(escolaridade_pai) == FALSE) %>%
  ggplot(aes(x = escolaridade_pai, weight = peso)) +
  geom_bar(fill="#6495ED") +
  scale_y_continuous(labels =
                       scales::number_format(
                         accuracy = NULL,
                         scale = 1,
                         prefix = "",
                         suffix = "",
                         big.mark = ".",
                         decimal.mark = ",",
                         trim = TRUE)) +
  xlab("Escolaridade do Pai") +
  ylab("Número de pessoas") +
  scale_x_discrete()+
  labs(
    title = "Contagem Populacional por Escolaridade do Pai",
    subtitle = "Brasil, 2014",
    caption = "Fonte: IBGE, Suplemento de mobilidade da PNAD, 2014.")

# Renda média por escolaridade do pai

base %>% 
  filter(morava_pai == 1) %>%
  filter(is.na(escolaridade_pai) == FALSE) %>%
  ggplot(aes(x = escolaridade_pai, y=renda_dom, weight = peso)) +
  geom_bar(fill="#48D1CC", position = "dodge",
           stat = "summary",
           fun = "mean") +
  scale_y_continuous(labels =
                       scales::number_format(
                         accuracy = NULL,
                         scale = 1,
                         prefix = "",
                         suffix = "",
                         big.mark = ".",
                         decimal.mark = ",",
                         trim = TRUE)) +
  xlab("Escolaridade do Pai") +
  ylab("Renda Média") +
  scale_x_discrete()+
  labs(
    title = "Renda Média por Escolaridade do Pai",
    subtitle = "Brasil, 2014",
    caption = "Fonte: IBGE, Suplemento de mobilidade da PNAD, 2014.")

# Renda média por escolaridade do pai e área

base %>% 
  filter(morava_pai == 1) %>%
  filter(is.na(escolaridade_pai) == FALSE) %>%
  filter(zona != "") %>%
  ggplot(aes(x = escolaridade_pai, y=renda_dom, weight = peso, fill=zona)) +
  geom_bar(position = "dodge",
           stat = "summary",
           fun = "mean") +
  scale_y_continuous(labels =
                       scales::number_format(
                         accuracy = NULL,
                         scale = 1,
                         prefix = "",
                         suffix = "",
                         big.mark = ".",
                         decimal.mark = ",",
                         trim = TRUE)) +
  xlab("Escolaridade do Pai") +
  ylab("Renda Média") +
  scale_x_discrete()+
  labs(
    title = "Renda Média por Escolaridade do Pai e Zona de Moradia",
    subtitle = "Brasil, 2014",
    caption = "Fonte: IBGE, Suplemento de mobilidade da PNAD, 2014.")+
  scale_fill_discrete(name = "Zona",labels=c("Urbana","Rural"))

# População por etnia e escolaridade do pai

base = mutate(base, cor = case_when((cor == "2") ~ "Branca",
                                                 (cor == "4") ~ "Preta/Parda",
                                                 (cor == "6") ~ "Outras",
                                                 (cor == "8") ~ "Preta/Parda",
                                                 (cor == "0") ~ "Outras",
                                                 (cor == "9") ~ "Outras"))

base$cor <- factor(base$cor, levels = c("Branca","Preta/Parda", "Outras")) #reordenando

base %>% 
  filter(morava_pai == 1) %>%
  filter(is.na(escolaridade_pai) == FALSE) %>%
  filter(is.na(cor) == FALSE) %>%
  ggplot(aes(x = cor, weight = peso, fill=escolaridade_pai)) +
  geom_bar(position = "dodge") +
  scale_y_continuous(labels =
                       scales::number_format(
                         accuracy = NULL,
                         scale = 1,
                         prefix = "",
                         suffix = "",
                         big.mark = ".",
                         decimal.mark = ",",
                         trim = TRUE)) +
  xlab("Etnia") +
  ylab("Número de Pessoas") +
  scale_x_discrete()+
  labs(
    title = "Contagem Populacional por Etnia e Escolaridade do Pai",
    subtitle = "Brasil, 2014",
    caption = "Fonte: IBGE, Suplemento de mobilidade da PNAD, 2014.")+
  scale_fill_brewer(name = "Escolaridade do Pai", palette = "RdYlBu")

# Renda média por etnia e escolaridade do pai

base = mutate(base, cor = case_when((cor == "2") ~ "Branca",
                                    (cor == "4") ~ "Preta/Parda",
                                    (cor == "6") ~ "Outras",
                                    (cor == "8") ~ "Preta/Parda",
                                    (cor == "0") ~ "Outras",
                                    (cor == "9") ~ "Outras"))

base$cor <- factor(base$cor, levels = c("Branca","Preta/Parda", "Outras")) #reordenando

base %>% 
  filter(morava_pai == 1) %>%
  filter(is.na(escolaridade_pai) == FALSE) %>%
  filter(is.na(cor) == FALSE) %>%
  ggplot(aes(x = cor, y=renda_dom, weight = peso, fill=escolaridade_pai)) +
  geom_bar(position = "dodge",
           stat = "summary",
           fun = "mean") +
  scale_y_continuous(labels =
                       scales::number_format(
                         accuracy = NULL,
                         scale = 1,
                         prefix = "",
                         suffix = "",
                         big.mark = ".",
                         decimal.mark = ",",
                         trim = TRUE)) +
  xlab("Etnia") +
  ylab("Renda média") +
  scale_x_discrete()+
  labs(
    title = "Renda média por Etnia e Escolaridade do Pai",
    subtitle = "Brasil, 2014",
    caption = "Fonte: IBGE, Suplemento de mobilidade da PNAD, 2014.")+
  scale_fill_brewer(name = "Escolaridade do Pai", palette = "RdYlBu")

#Faixa de renda e escolaridade do pai

base = mutate(base, renda_faixa = case_when((renda_faixa == "00") ~ "Até ¼ salário mínimo",
                                    (renda_faixa == "01") ~ "Até ¼ salário mínimo",
                                    (renda_faixa == "02") ~ "Entre ¼ e ½ salário mínimo",
                                    (renda_faixa == "03") ~ "Entre ½ e 1 salário mínimo",
                                    (renda_faixa == "04") ~ "Entre 1 e 2 salário mínimo",
                                    (renda_faixa == "05") ~ "Entre 2 e 3 salário mínimo",
                                    (renda_faixa == "06") ~ "Entre 3 e 5 salário mínimo",
                                    (renda_faixa == "07") ~ "Mais de 5 salários mínimos",
                                    (renda_faixa == "99") ~ NA))

base %>%
  filter(morava_pai == 1) %>%
  filter(is.na(escolaridade_pai) == FALSE) %>%
  filter(is.na(renda_faixa) == FALSE) %>%
  ggplot(aes(x = escolaridade_pai, weight = peso, fill = renda_faixa)) +
  geom_bar(position = "dodge") +
  scale_y_continuous(
    labels =
      scales::number_format(
        accuracy = NULL,
        scale = 1,
        prefix = "",
        suffix = "",
        big.mark = ".",
        decimal.mark = ",",
        trim = TRUE
      )
  ) +
  xlab("Escolaridade do Pai") +
  ylab("Número de Pessoas") +
  scale_x_discrete() +
  labs(title = "Contagem Populacional por Faixa de Renda e Escolaridade do Pai",
       subtitle = "Brasil, 2014",
       caption = "Fonte: IBGE, Suplemento de mobilidade da PNAD, 2014.") +
  scale_fill_brewer(name = "Faixa de Renda", palette = "RdYlBu")



# CRIANDO TABELAS PERCENTUAIS
etnia_b = base %>%
  filter(morava_pai == 1 & cor == "Branca" & is.na(escolaridade_pai) == FALSE) %>%
  count(escolaridade_pai, wt = peso) %>%
  mutate(freq = n/sum(n)) %>%
  select("escolaridade_pai","freq")

etnia_p = base %>%
  filter(morava_pai == 1 & cor == "Preta/Parda" & is.na(escolaridade_pai) == FALSE) %>%
  count(escolaridade_pai, wt = peso) %>%
  mutate(freq = n/sum(n)) %>%
  select("escolaridade_pai","freq")

etnia_o = base%>%
  filter(morava_pai == 1 & cor == "Outras" & is.na(escolaridade_pai) == FALSE) %>%
  count(escolaridade_pai, wt = peso) %>%
  mutate(freq = n/sum(n)) %>%
  select("escolaridade_pai","freq")

#Gráfico Escolaridade do Pai - Etnia Branca
etnia_b %>%
  ggplot(aes(x = "", y = freq, fill = escolaridade_pai, label=scales::percent(freq))) +
  geom_bar(stat = "identity", width = 1, position = "dodge")+
  xlab("") +
  ylab("") +
  labs(
    title = "Escolaridade do Pai - Etnia Branca",
    subtitle = "Brasil, 2014",
    caption = "Fonte: IBGE, Suplemento de mobilidade da PNAD, 2014.")+
  scale_y_continuous(labels = scales::percent,
                     breaks = scales::pretty_breaks(n = 4))+
  scale_fill_brewer(name = "Escolaridade do Pai", palette = "RdYlBu")+
  geom_text(
    aes(label = sprintf("%1.1f%%", freq*100)),
    position = position_dodge(0.99),
    vjust = -.25,
    size=3.2,
    color="#363636",
  )

#Gráfico Escolaridade do Pai - Etnia Preta/Parda
etnia_p %>%
  ggplot(aes(x = "", y = freq, fill = escolaridade_pai, label=scales::percent(freq))) +
  geom_bar(stat = "identity", width = 1, position = "dodge")+
  xlab("") +
  ylab("") +
  labs(
    title = "Escolaridade do Pai - Etnia Preta/Parda",
    subtitle = "Brasil, 2014",
    caption = "Fonte: IBGE, Suplemento de mobilidade da PNAD, 2014.")+
  scale_y_continuous(labels = scales::percent,
                     breaks = scales::pretty_breaks(n = 4))+
  scale_fill_brewer(name = "Escolaridade do Pai", palette = "RdYlBu")+
  geom_text(
    aes(label = sprintf("%1.1f%%", freq*100)),
    position = position_dodge(0.99),
    vjust = -.25,
    size=3.2,
    color="#363636",
  )

#Gráfico Escolaridade do Pai - Outras Etnias
etnia_o %>%
  ggplot(aes(x = "", y = freq, fill = escolaridade_pai, label=scales::percent(freq))) +
  geom_bar(stat = "identity", width = 1, position = "dodge")+
  xlab("") +
  ylab("") +
  labs(
    title = "Escolaridade do Pai - Outras Etnias",
    subtitle = "Brasil, 2014",
    caption = "Fonte: IBGE, Suplemento de mobilidade da PNAD, 2014.")+
  scale_y_continuous(labels = scales::percent,
                     breaks = scales::pretty_breaks(n = 4))+
  scale_fill_brewer(name = "Escolaridade do Pai", palette = "RdYlBu")+
  geom_text(
    aes(label = sprintf("%1.1f%%", freq*100)),
    position = position_dodge(0.99),
    vjust = -.25,
    size=3.2,
    color="#363636",
  )

################ MÃE ########################################

#ESCOLARIDADE DA MÃE

base = mutate(base, escolaridade_mae = case_when((escolaridade_mae == "01") ~ "Fundamental ou inferior",
                                                 (escolaridade_mae == "02") ~ "Fundamental ou inferior",
                                                 (escolaridade_mae == "03") ~ "Fundamental ou inferior",
                                                 (escolaridade_mae == "04") ~ "Fundamental ou inferior",
                                                 (escolaridade_mae == "05") ~ "Fundamental ou inferior",
                                                 (escolaridade_mae == "06") ~ "Fundamental ou inferior",
                                                 (escolaridade_mae == "07") ~ "Fundamental ou inferior",
                                                 (escolaridade_mae == "08") ~ "Médio",
                                                 (escolaridade_mae == "09") ~ "Superior",
                                                 (escolaridade_mae == "10") ~ "Mestrado/Doutorado",
                                                 (escolaridade_mae == "11") ~ NA,
                                                 (escolaridade_mae == "12") ~ "Nenhum",
))

base$escolaridade_mae <- factor(base$escolaridade_mae,
                                levels = c("Nenhum","Fundamental ou inferior", "Médio", "Superior", "Mestrado/Doutorado")) #reordenando

# Escolaridade da mãe por contagem populacional

base %>% #contagem populacional, nao amostral, pois está com peso
  filter(morava_mae == 1) %>%
  filter(is.na(escolaridade_mae) == FALSE) %>%
  ggplot(aes(x = escolaridade_mae, weight = peso)) +
  geom_bar(fill="#6495ED") +
  scale_y_continuous(labels =
                       scales::number_format(
                         accuracy = NULL,
                         scale = 1,
                         prefix = "",
                         suffix = "",
                         big.mark = ".",
                         decimal.mark = ",",
                         trim = TRUE)) +
  xlab("Escolaridade da Mãe") +
  ylab("Número de pessoas") +
  scale_x_discrete()+
  labs(
    title = "Contagem Populacional por Escolaridade da Mãe",
    subtitle = "Brasil, 2014",
    caption = "Fonte: IBGE, Suplemento de mobilidade da PNAD, 2014.")

# Renda média por escolaridade da mãe

base %>% 
  filter(morava_mae == 1) %>%
  filter(is.na(escolaridade_mae) == FALSE) %>%
  ggplot(aes(x = escolaridade_mae, y=renda_dom, weight = peso)) +
  geom_bar(fill="#48D1CC", position = "dodge",
           stat = "summary",
           fun = "mean") +
  scale_y_continuous(labels =
                       scales::number_format(
                         accuracy = NULL,
                         scale = 1,
                         prefix = "",
                         suffix = "",
                         big.mark = ".",
                         decimal.mark = ",",
                         trim = TRUE)) +
  xlab("Escolaridade da Mãe") +
  ylab("Renda Média") +
  scale_x_discrete()+
  labs(
    title = "Renda Média por Escolaridade da Mãe",
    subtitle = "Brasil, 2014",
    caption = "Fonte: IBGE, Suplemento de mobilidade da PNAD, 2014.")

# Renda média por escolaridade da mãe e área

base %>% 
  filter(morava_mae == 1) %>%
  filter(is.na(escolaridade_mae) == FALSE) %>%
  filter(zona != "") %>%
  ggplot(aes(x = escolaridade_mae, y=renda_dom, weight = peso, fill=zona)) +
  geom_bar(position = "dodge",
           stat = "summary",
           fun = "mean") +
  scale_y_continuous(labels =
                       scales::number_format(
                         accuracy = NULL,
                         scale = 1,
                         prefix = "",
                         suffix = "",
                         big.mark = ".",
                         decimal.mark = ",",
                         trim = TRUE)) +
  xlab("Escolaridade da Mãe") +
  ylab("Renda Média") +
  scale_x_discrete()+
  labs(
    title = "Renda Média por Escolaridade da Mãe e Zona de Moradia",
    subtitle = "Brasil, 2014",
    caption = "Fonte: IBGE, Suplemento de mobilidade da PNAD, 2014.")+
  scale_fill_discrete(name = "Zona",labels=c("Urbana","Rural"))

# População por etnia e escolaridade da mãe

base = mutate(base, cor = case_when((cor == "2") ~ "Branca",
                                    (cor == "4") ~ "Preta/Parda",
                                    (cor == "6") ~ "Outras",
                                    (cor == "8") ~ "Preta/Parda",
                                    (cor == "0") ~ "Outras",
                                    (cor == "9") ~ "Outras"))

base$cor <- factor(base$cor, levels = c("Branca","Preta/Parda", "Outras")) #reordenando

base %>% 
  filter(morava_mae == 1) %>%
  filter(is.na(escolaridade_mae) == FALSE) %>%
  filter(is.na(cor) == FALSE) %>%
  ggplot(aes(x = cor, weight = peso, fill=escolaridade_mae)) +
  geom_bar(position = "dodge") +
  scale_y_continuous(labels =
                       scales::number_format(
                         accuracy = NULL,
                         scale = 1,
                         prefix = "",
                         suffix = "",
                         big.mark = ".",
                         decimal.mark = ",",
                         trim = TRUE)) +
  xlab("Etnia") +
  ylab("Número de Pessoas") +
  scale_x_discrete()+
  labs(
    title = "Contagem Populacional por Etnia e Escolaridade da Mãe",
    subtitle = "Brasil, 2014",
    caption = "Fonte: IBGE, Suplemento de mobilidade da PNAD, 2014.")+
  scale_fill_brewer(name = "Escolaridade da Mãe", palette = "RdYlBu")

# Renda média por etnia e escolaridade da mãe

base = mutate(base, cor = case_when((cor == "2") ~ "Branca",
                                    (cor == "4") ~ "Preta/Parda",
                                    (cor == "6") ~ "Outras",
                                    (cor == "8") ~ "Preta/Parda",
                                    (cor == "0") ~ "Outras",
                                    (cor == "9") ~ "Outras"))

base$cor <- factor(base$cor, levels = c("Branca","Preta/Parda", "Outras")) #reordenando

base %>% 
  filter(morava_mae == 1) %>%
  filter(is.na(escolaridade_mae) == FALSE) %>%
  filter(is.na(cor) == FALSE) %>%
  ggplot(aes(x = cor, y=renda_dom, weight = peso, fill=escolaridade_mae)) +
  geom_bar(position = "dodge",
           stat = "summary",
           fun = "mean") +
  scale_y_continuous(labels =
                       scales::number_format(
                         accuracy = NULL,
                         scale = 1,
                         prefix = "",
                         suffix = "",
                         big.mark = ".",
                         decimal.mark = ",",
                         trim = TRUE)) +
  xlab("Etnia") +
  ylab("Renda média") +
  scale_x_discrete()+
  labs(
    title = "Renda média por Etnia e Escolaridade da Mãe",
    subtitle = "Brasil, 2014",
    caption = "Fonte: IBGE, Suplemento de mobilidade da PNAD, 2014.")+
  scale_fill_brewer(name = "Escolaridade da Mãe", palette = "RdYlBu")

base %>%
  filter(morava_mae == 1) %>%
  filter(is.na(escolaridade_mae) == FALSE) %>%
  filter(is.na(renda_faixa) == FALSE) %>%
  ggplot(aes(x = escolaridade_mae, weight = peso, fill = renda_faixa)) +
  geom_bar(position = "dodge") +
  scale_y_continuous(
    labels =
      scales::number_format(
        accuracy = NULL,
        scale = 1,
        prefix = "",
        suffix = "",
        big.mark = ".",
        decimal.mark = ",",
        trim = TRUE
      )
  ) +
  xlab("Escolaridade da Mãe") +
  ylab("Número de Pessoas") +
  scale_x_discrete() +
  labs(title = "Contagem Populacional por Faixa de Renda e Escolaridade da Mãe",
       subtitle = "Brasil, 2014",
       caption = "Fonte: IBGE, Suplemento de mobilidade da PNAD, 2014.") +
  scale_fill_brewer(name = "Faixa de Renda", palette = "RdYlBu")

################### Quantis de Renda #########################

quantile(base$renda_dom, na.rm = TRUE, probs = c(0,0.2,0.4,0.6,0.8, 1))

base$Quintile = base %>% mutate(renda_dom =
                     case_when(renda_dom <= 362 ~ "1", 
                               (renda_dom > 362) & (renda_dom<=629) ~ "2",
                               (renda_dom > 629) & (renda_dom<=900) ~ "3",
                               (renda_dom > 900) & (renda_dom<=1500) ~ "4",
                               (renda_dom > 1500) & (renda_dom<=100000) ~ "5"))

################### GEOLOCALIZAÇÕES #########################

geo = read_state(code_state="all", year=2020)

brasil= data.frame(code_state=c(geo$code_state), 
                   abbrev_state=c(geo$abbrev_state), 
                   name_state=c(geo$name_state), 
                   name_region=c(geo$name_region))

base = base %>% left_join(brasil, by=c('UF'='code_state'))


######### GRÁFICOS POR REGIÃO ############

# Escolaridade do Pai por Região

base %>%
  filter(morava_pai == 1) %>%
  filter(is.na(escolaridade_pai) == FALSE) %>%
  filter(is.na(name_region) == FALSE) %>%
  ggplot(aes(x = name_region, weight = peso, fill = escolaridade_pai)) +
  geom_bar(position = "dodge") +
  scale_y_continuous(
    labels =
      scales::number_format(
        accuracy = NULL,
        scale = 1,
        prefix = "",
        suffix = "",
        big.mark = ".",
        decimal.mark = ",",
        trim = TRUE
      )
  ) +
  xlab("Região") +
  ylab("Número de Pessoas") +
  scale_x_discrete() +
  labs(title = "Escolaridade do Pai por Região",
       subtitle = "Brasil, 2014",
       caption = "Fonte: IBGE, Suplemento de mobilidade da PNAD, 2014.") +
  scale_fill_brewer(name = "Escolaridade do Pai", palette = "RdYlBu")

# Escolaridade da Mãe por Região

base %>%
  filter(morava_mae == 1) %>%
  filter(is.na(escolaridade_mae) == FALSE) %>%
  filter(is.na(name_region) == FALSE) %>%
  ggplot(aes(x = name_region, weight = peso, fill = escolaridade_mae)) +
  geom_bar(position = "dodge") +
  scale_y_continuous(
    labels =
      scales::number_format(
        accuracy = NULL,
        scale = 1,
        prefix = "",
        suffix = "",
        big.mark = ".",
        decimal.mark = ",",
        trim = TRUE
      )
  ) +
  xlab("Região") +
  ylab("Número de Pessoas") +
  scale_x_discrete() +
  labs(title = "Escolaridade da Mãe por Região",
       subtitle = "Brasil, 2014",
       caption = "Fonte: IBGE, Suplemento de mobilidade da PNAD, 2014.") +
  scale_fill_brewer(name = "Escolaridade da Mãe", palette = "RdYlBu")
