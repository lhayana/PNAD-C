# SUPLEMENTO: https://www.ibge.gov.br/estatisticas/sociais/trabalho/19898-suplementos-pnad3.html?edicao=17983&t=downloads

library('microdadosBrasil')
library('dplyr')
library(dbplyr)
library("survey")
library("srvyr")
library(tidyr)
library(ggplot2)
library(scales)
library(RColorBrewer)


pessoas = read_PNAD('pessoas', 2014, file = path.expand("C:/Users/Lhayana/Documents/pnad_2014_data/suplemento/PES2014.txt"))
domicilios = read_PNAD('domicilios', 2014, file = path.expand("C:/Users/Lhayana/Documents/pnad_2014_data/suplemento/DOM2014.txt"))
# V4742 = rendimento dom. per capita
pessoas = select(pessoas,UF,V0102,V0103,V32039,V0404,V4742,V0305,V32001,V32004,V32010,V32012, V32024, V32026,V4743)
domicilios = select(domicilios, V0102,V0103,V4602,UPA)

base = merge(x=pessoas,y=domicilios,by=c("V0102","V0103"))

base$V4742 = sub("^0+", "", base$V4742) #tira zeros da esquerda  
base$V4742 = as.integer(base$V4742)
base$V4602 = as.numeric(base$V4602)
base$V32039 = as.numeric(base$V32039)

base = filter(base, V0305==2) #Selecionados para o Suplemento de Mobilidade Sócio-Ocupacional
base = rename(base, Estrato = V4602, peso = V32039, cor = V0404, renda_dom=V4742, renda_faixa=V4743, morava_uf_aos15=V32001, zona=V32004, morava_pai=V32010, escolaridade_pai=V32012, morava_mae=V32024, escolaridade_mae=V32026)

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

