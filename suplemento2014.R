# SUPLEMENTO: https://www.ibge.gov.br/estatisticas/sociais/trabalho/19898-suplementos-pnad3.html?edicao=17983&t=downloads

library('microdadosBrasil')
library('dplyr')

pessoas = read_PNAD('pessoas', 2014, file = path.expand("C:/Users/Lhayana/Documents/pnad_2014_data/suplemento/PES2014.txt"))
domicilios = read_PNAD('domicilios', 2014, file = path.expand("C:/Users/Lhayana/Documents/pnad_2014_data/suplemento/DOM2014.txt"))

pessoas = select(suplemento,UF,V0102,V0103,V0404,V0502,V4750,V4742,V4718,V4719,V4720,V4721,V4722,V0305,V32000,V32001,V32002)
domicilios = select(domicilios, V0102,V0103,V4602,UPA)
