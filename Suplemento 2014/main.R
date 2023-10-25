library('microdadosBrasil')
library('dplyr')
library("survey")
library("srvyr")
library(tidyr)
library(survey)
options(survey.lonely.psu = "adjust")

#########COLETA E ESTRATIFICAÇÃO###########

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

rm(pessoas,domicilios)

write.csv2(base, "C:/Users/Lhayana/Documents/GitHub/PNAD-C/Suplemento 2014/Dados/dados.csv")

pop_types <- data.frame(V4609 = unique(base$V4609), Freq = unique(base$V4609))

prestratified_design <-
  svydesign(id = ~ V4618,
            strata = ~ Estrato ,
            data = base ,
            weights = ~ peso ,
            na.rm = TRUE,
            nest = TRUE)

gc()

dados_estratificados <-
  postStratify(design = prestratified_design,
               strata = ~ V4609,
               population = pop_types)

rm(prestratified_design)
gc()
class(dados_estratificados )

#########RIO GRANDE DO NORTE###########

options(OutDec=",",
        scipen=100,
        digits=4,
        big.mark = ".")

rn = subset(dados_estratificados, UF == 24)

svytotal(~sex,
         rn,
         na.rm = T)

x = svyglm(as.numeric(renda_dom) ~ as.numeric(anos_estudo_filho), design = rn)
summary(x)
