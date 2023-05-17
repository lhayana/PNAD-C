# SUPLEMENTO: https://www.ibge.gov.br/estatisticas/sociais/trabalho/19898-suplementos-pnad3.html?edicao=17983&t=downloads

library('microdadosBrasil')

suplemento = read_PNAD('pessoas', 2014, file = path.expand("C:/Users/Lhayana/Documents/pnad_2014_data/suplemento/PES2014.txt"))

suplemento$UF = substr(suplemento$V0102,1,2)
