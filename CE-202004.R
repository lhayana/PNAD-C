library(PNADcIBGE)

get_pnadc(2020, quarter = 4, labels=TRUE, savedir="C:/Users/pc/Downloads/PNADC_R")

PNAD202004 = read_pnadc("C:/Users/pc/Downloads/PNADC_R/PNADC_042020.txt", 
                        "C:/Users/pc/Downloads/PNADC_R/input_PNADC_trimestral.txt", vars = NULL)

#Criando um objeto contendo apenas essas variáveis
reduz <- c("UF", "UPA","Estrato", "VD4001", "VD4002", "V2009", "V2007", "VD4016", "VD4009","V1028")
reduz_pnad204 <- PNAD202004[reduz]

#Para trabalhar com as variáveis precisaremos transformar alguns de character(string)em numéricas
reduz_pnad204$UF=as.numeric(reduz_pnad204$UF)
reduz_pnad204$UPA=as.numeric(reduz_pnad204$UPA)
reduz_pnad204$Estrato=as.numeric(reduz_pnad204$Estrato)
reduz_pnad204$VD4001=as.numeric(reduz_pnad204$VD4001)
reduz_pnad204$VD4002=as.numeric(reduz_pnad204$VD4002)
reduz_pnad204$V2007=as.numeric(reduz_pnad204$V2007)
reduz_pnad204$VD4009=as.numeric(reduz_pnad204$VD4009)

#Adicionando os rótulos criando uma nova variável (condat)
table(reduz_pnad204$VD4001)
reduz_pnad204$condat<-factor(reduz_pnad204$VD4001,labels=c('PEA', 'Inativos'))
100*prop.table(table(reduz_pnad204$condat))

#Adicionando os rótulos criando uma nova variável (condocup)
reduz_pnad204$condocup<-factor(reduz_pnad204$VD4002,labels=c('Ocupadas', 'Desocupadas'))
100*prop.table(table(reduz_pnad204$condocup))

#Variável idade (V2009)
summary(reduz_pnad204$V2009)

#Construindo faixas etárias (IDADECAT)
reduz_pnad204$IDADECAT <- factor(cut(reduz_pnad204$V2009, breaks=c(14,20,25,30,35,40,45,50,55,60,65,112), labels=c("14-19","20-24","25-29","30-34","35-39","40-44","45-49", "50-54","55-59", "60-64","65+"),right=FALSE))
table(reduz_pnad204$IDADECAT)
100*prop.table(table(reduz_pnad204$IDADECAT))

#Variável sexo (V2007)
reduz_pnad204$sexo<-factor(reduz_pnad204$V2007,labels=c('Homens', 'Mulheres'))
100*prop.table(table(reduz_pnad204$sexo))

#Criando uma variável para expressar trabalhadores sem/com carteira de trabalho assinada (trabcarteira)
reduz_pnad204$trabcarteira[reduz_pnad204$VD4009==2 | reduz_pnad204$VD4009==4] <- "Sem carteira"

reduz_pnad204$trabcarteira[reduz_pnad204$VD4009==1 | reduz_pnad204$VD4009==3 | reduz_pnad204$VD4009==5 | 
                            reduz_pnad204$VD4009==6 | reduz_pnad204$VD4009==7 | reduz_pnad204$VD4009==8 |
                            reduz_pnad204$VD4009==9]<- "com carteira/outro"

table(reduz_pnad204$trabcarteira)

library("survey")

###############################################
#Dados do Brasil
###############################################

sample.pnadc <- svydesign(ids = ~UPA, strata = ~Estrato, weights = ~V1028, data = reduz_pnad204 , na.rm=TRUE, nest = TRUE)
# para evitar erro "has only one PSU at stage 1"
options(survey.lonely.psu = "adjust")

#a)  Condição de atividade - Brasil
svymean(~factor(condat), design=sample.pnadc, na.rm=TRUE)
#Representação em gráfico
a<-svymean(~factor(condat), design=sample.pnadc, na.rm=TRUE)
barplot(a*100, names.arg=c("PEA","Inativos"), col="purple", 
        ylim=c(0,100), main="Condição de atividade, Brasil, 4o tri 2020.",
        sub="Fonte: PNAD C, 4o tri 2020.")

#b)  Condição de ocupação  - Brasil
svymean(~factor(condocup), design=sample.pnadc, na.rm=TRUE)
#Representação em gráfico
b<-svymean(~factor(condocup), design=sample.pnadc, na.rm=TRUE)
barplot(b*100, names.arg=c("Ocupadas","Desocupadas"), col="grey", 
        ylim=c(0,100), main="Condição de ocupação, Brasil, 4o tri 2020.",
        sub="Fonte: PNAD C, 4o tri 2020.")

#c)  Posição na ocupação  - Brasil
svymean(~factor(trabcarteira), design=sample.pnadc, na.rm=TRUE)
#Representação em gráfico
c<-svymean(~factor(trabcarteira), design=sample.pnadc, na.rm=TRUE)
barplot(b*100, names.arg=c("Com carteira/outro","Sem carteira"), col="green", 
        ylim=c(0,100), main="Posição na ocupação, Brasil, 4o tri 2020.",
        sub="Fonte: PNAD C, 4o tri 2020.",
        xlab="Sem carteira: setor privado+empreg.doméstico",
        font.main=1, font.lab=4, font.sub=4)

#d)  Renda média por sexo  - Brasil
svyby(~VD4016, ~sexo, sample.pnadc, svymean, na.rm=TRUE)
#Representação em gráfico
svyboxplot(~VD4016~factor(sexo, 
                          labels=c('Homens','Mulheres')),
           design=sample.pnadc, xlab='PNAD C 4o tri 2020', ylab='R$', col="red",main="Boxplot - Rendimento mensal por sexo, Brasil, 4o tri 2020", 
           font.main = 4, col.main = "black", cex.main = 0.8, outliers=TRUE,
           ylim =c (0, 8000))

#e)  Renda média por idade  - Brasil
svyby(~VD4016, ~IDADECAT, sample.pnadc, svymean, na.rm=TRUE)
svyboxplot(~VD4016~factor(IDADECAT), 
           design=sample.pnadc, xlab='PNAD C 4o tri 2020', ylab='R$', col="orange",main="Boxplot - Rendimento mensal por grupo etário, Brasil, 4o tri 2020", 
           font.main = 12, col.main = "black", cex.main = 0.9, outliers=TRUE,
           ylim =c (0, 10000))

#f)  Renda média por posição na ocupação  - Brasil
svyby(~VD4016, ~trabcarteira, sample.pnadc, svymean, na.rm=TRUE)
svyboxplot(~VD4016~factor(trabcarteira), 
           design=sample.pnadc, xlab='PNAD C 4o tri 2020', ylab='R$', col="brown",main="Boxplot - Rendimento mensal por posição na ocupação, Brasil, 4o tri 2020", 
           font.main = 12, col.main = "black", cex.main = 0.9, outliers=TRUE,
           ylim =c (0, 10000))



###############################################
#Dados apenas do CE
###############################################

CE <-reduz_pnad204[reduz_pnad204$UF==23,]

#Para recompor o plano amostral para o Ceará
subamostra <- svydesign(ids = ~UPA, strata = ~Estrato, weights = ~V1028, data = CE, na.rm=TRUE, nest = TRUE)

#Para evitar erro "has only one PSU at stage 1"
options(survey.lonely.psu = "adjust")

#a)  Condição de atividade - CE
svymean(~factor(condat), design=subamostra, na.rm=TRUE)
#Representação em gráfico
a<-svymean(~factor(condat), design=subamostra, na.rm=TRUE)
barplot(a*100, names.arg=c("PEA","Inativos"), col="purple", 
        ylim=c(0,100), main="Condição de atividade, Ceará, 4º tri 2020.",
        sub="Fonte: PNAD Contínua, 4º tri 2020.")

#b)  Condição de ocupação  - CE
svymean(~factor(condocup), design=subamostra, na.rm=TRUE)
#Representação em gráfico
b<-svymean(~factor(condocup), design=subamostra, na.rm=TRUE)
barplot(b*100, names.arg=c("Ocupadas","Desocupadas"), col="grey", 
        ylim=c(0,100), main="Condição de ocupação, Ceará, 4º tri 2020.",
        sub="Fonte: PNAD Contínua, 4º tri 2020.")

#c)  Posição na ocupação  - CE
svymean(~factor(trabcarteira), design=subamostra, na.rm=TRUE)
#Representação em gráfico
c<-svymean(~factor(trabcarteira), design=subamostra, na.rm=TRUE)
barplot(b*100, names.arg=c("Com carteira/outro","Sem carteira"), col="green", 
        ylim=c(0,100), main="Posição na ocupação, Ceará, 4º tri 2020.",
        sub="Fonte: PNAD Contínua, 4º tri 2020.",
        xlab="Sem carteira: setor privado+empreg.doméstico",
        font.main=1, font.lab=4, font.sub=4)

#d)  Renda média por sexo  - CE
svyby(~VD4016, ~sexo, subamostra, svymean, na.rm=TRUE)
#Representação em gráfico
svyboxplot(~VD4016~factor(sexo, 
                          labels=c('Homens','Mulheres')),
           design=subamostra, xlab='PNAD Contínua 4º tri 2020', ylab='R$', col="red",main="Boxplot - Rendimento mensal por sexo, Ceará, 4º tri 2020", 
           font.main = 4, col.main = "black", cex.main = 0.8, outliers=TRUE,
           ylim =c (0, 8000))

#e)  Renda média por idade  - CE
svyby(~VD4016, ~IDADECAT, subamostra, svymean, na.rm=TRUE)
svyboxplot(~VD4016~factor(IDADECAT), 
           design=subamostra, xlab='PNAD Contínua 4º tri 2020', ylab='R$', col="orange",main="Boxplot - Rendimento mensal por grupo etário, Ceará, 4º tri 2020", 
           font.main = 12, col.main = "black", cex.main = 0.9, outliers=TRUE,
           ylim =c (0, 10000))

#f)  Renda média por posição na ocupação  - CE
svyby(~VD4016, ~trabcarteira, subamostra, svymean, na.rm=TRUE)
svyboxplot(~VD4016~factor(trabcarteira), 
           design=subamostra, xlab='PNAD Contínua 4º tri 2020', ylab='R$', col="brown",main="Boxplot - Rendimento mensal por posição na ocupação, Ceará, 4º tri 2020", 
           font.main = 12, col.main = "black", cex.main = 0.9, outliers=TRUE,
           ylim =c (0, 10000))


