# SUPLEMENTO: https://www.ibge.gov.br/estatisticas/sociais/trabalho/19898-suplementos-pnad3.html?edicao=17983&t=downloads

library('microdadosBrasil')
library('dplyr')
library(dbplyr)
library("survey")
library("srvyr")
library(tidyr)
library(ggplot2)

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

base %>% #contagem populacional, nao amostral, pois está com peso
  filter(UF == 24) %>%
  ggplot(aes(renda_dom, weight = peso)) +
  geom_freqpoly(binwidth = 998)
  
