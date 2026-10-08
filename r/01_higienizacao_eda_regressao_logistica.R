
#instalando pacotes necessários
install.packages("tidyverse") #para manipulação de banco de dados e usar o comando %>%
install.packages("readr") #para leitura de dados em excel
install.packages("dplyr") # para agregação dos dados e usar o comando %>%
install.packages("data.table") # para exportar o arquivo
install.packages("readxl")
install.packages("Information") #para calculo do IV
install.packages("knitr") # para transformar em tabela
install.packages("lubridate") #para tratar a variavel horario
install.packages("smbinning") #para categorização com optimal binning
install.packages("car")  # para calcular o VIF pra ver se tem multicolinearidade
install.packages("ROCR")         # para calculo do KS e AUC
install.packages("cutpointr") #para calcular o ponto de cortein
install.packages("doBy") #para anlisar o summary cruzando duas variaveis



#habilitando os pacotes
library(tidyverse) 
library(readr) 
library(dplyr) 
library(data.table) 
library(readxl)
library(Information)
library(knitr)
library(lubridate)
library(smbinning)
library(car)
library(ROCR)
library(cutpointr)
library(doBy)

#mapear diretorio
# Baixe os arquivos datatran2022.csv e datatran2023 do portal de Dados Abertos da PRF
# (https://www.gov.br/prf/pt-br/acesso-a-informacao/dados-abertos/dados-abertos-da-prf)
# e salve-os na pasta data/raw/ do repositório. Execute o script a partir da raiz do repositório.
setwd("data/raw/")


#lendo os arquivos
df2022 <- read.csv2("datatran2022.csv",  encoding = "UTF-8")
df2023 <- read.csv2("datatran2023_ate_nov.csv", encoding = "latin1")

#unificando bases
df_unificado <- bind_rows(df2022,df2023)


########################################  ANALISE PARA IDENTIFICAR A REPRESENTATIVIDADE POR UF E BR  ############################################################

#Analise Estado e BR - MG é o Estado com mais ocorrências um total de 16.368 registros (13,04% das ocorrencias de 2022 a 2023)
#E a BR 381 de MG é a que possui mais ocorrências um total de 4.821 (29,49%) de 2022 a 2023. 

#Estado
uf <- table(df_unificado$uf)
uf <- sort(uf, decreasing = TRUE)
porcentagens <- prop.table(uf) * 100


tabela_final <- data.frame(
  Categoria = names(uf),
  Frequencia_Porcentagem = paste(uf, " (", round(porcentagens, 2), "%)", sep = "")
)


kable(tabela_final, format = "markdown", col.names = c("Categoria", "Frequência / Percentual"))


#MG > BR
br_mg <- table(df_unificado$br[df_unificado$uf == "MG"])
br <- sort(br_mg, decreasing = TRUE)
porcentagens <- prop.table(br) * 100


tabela_final <- data.frame(
  Categoria = names(br),
  Frequencia_Porcentagem = paste(br, " (", round(porcentagens, 2), "%)", sep = "")
)


kable(tabela_final, format = "markdown", col.names = c("Categoria", "Frequência / Percentual"))



######################################## HIGIENIZACAO BASE TOTAL - TRATAMENTO (INCLUSÃO, ALTERAÇÃO, REMOÇÃO DE VARIÁVEIS) ############################################################


# convertendo coluna data_inversa para o tipo data
df_unificado$data_inversa <- as.Date(df_unificado$data_inversa, format = "%d/%m/%Y")

# Adicionar a coluna ano_mes_ocorrencia 
df_unificado$ano_mes_ocorrencia <- format(df_unificado$data_inversa, format = "%Y-%m")

# Adicionar a coluna mês_ocorrencia 
df_unificado$mes_ocorrencia <- format(df_unificado$data_inversa, format = "%m")

# Adicionar a coluna horario_agrupado
df_unificado$horario <- as.POSIXct(df_unificado$horario, format = "%H:%M")
intervalos <- c(-Inf, 6, 12, 18, 24)
df_unificado$periodo_dia <- cut(hour(df_unificado$horario), breaks = intervalos, labels = c("Madrugada", "Manhã", "Tarde", "Noite"), include.lowest = TRUE)


# Adicionar a coluna "Região" com base nas regras
df_unificado$Regiao <- ifelse(df_unificado$uf %in% c('DF', 'GO', 'MT', 'MS'), 'Centro-Oeste',
                              ifelse(df_unificado$uf %in% c('AL', 'BA', 'CE', 'MA', 'PB','PE', 'PI', 'RN', 'SE'), 'Nordeste',
                                     ifelse(df_unificado$uf %in% c('AC', 'AP', 'AM', 'PA', 'RO', 'RR','TO'), 'Norte',
                                            ifelse(df_unificado$uf %in% c('SP', 'RJ', 'MG', 'ES'), 'Sudeste',
                                                   ifelse(df_unificado$uf %in% c('PR', 'SC', 'RS'), 'Sul', 'NA')))))

# Adicionar a coluna "causa_acidente_agg" com base nas regras
df_unificado$causa_acidente_agg <- ifelse(df_unificado$causa_acidente %in% c('Velocidade Incompatível', 'Ausência de reação do condutor', 'Reação tardia ou ineficiente do condutor', 'Acessar a via sem observar a presença dos outros veículos','Transitar na contramão', 'Condutor Dormindo',
                                                                             'Ultrapassagem Indevida', 'Ingestão de álcool pelo condutor', 'Pedestre andava na pista', 'Condutor deixou de manter distância do veículo da frente','Manobra de mudança de faixa','Pedestre cruzava a pista fora da faixa',
                                                                             'Conversão proibida', 'Entrada inopinada do pedestre','Desrespeitar a preferência no cruzamento', 'Transitar no acostamento','Trafegar com motocicleta (ou similar) entre as faixas', 'Mal súbito do condutor','Retorno proibido',
                                                                             'Ingestão de álcool e/ou substâncias psicoativas pelo pedestre', 'Condutor usando celular', 'Estacionar ou parar em local proibido','Frear bruscamente','Carga excessiva e/ou mal acondicionada', 'Ingestão de álcool ou de substâncias psicoativas pelo pedestre',
                                                                             'Ingestão de substâncias psicoativas pelo condutor', 'Suicídio (presumido)','Transitar na calçada', 'Transtornos Mentais (exceto suicidio)', 'Modificação proibida', 'Faróis desregulados','Participar de racha','Curva acentuada',
                                                                             'Condutor desrespeitou a iluminação vermelha do semáforo', 'Deixar de acionar o farol da motocicleta (ou similar)','Transitar no Acostamento', 'Obstrução Via tentativa Assalto'), 'Fator Humano',
                                          ifelse(df_unificado$causa_acidente %in% c('Demais falhas mecânicas ou elétricas', 'Avarias e/ou desgaste excessivo no pneu', 'Problema com o freio', 'Sistema de drenagem ineficiente', 'Problema na suspensão','Redutor de velocidade em desacordo'), 'Fator Veicular',
                                                 ifelse(df_unificado$causa_acidente %in% c('Chuva', 'Animais na Pista', 'Acesso irregular', 'Pista esburacada', 'Afundamento ou ondulação no pavimento', 'Demais falhas na via', 'Acumulo de água sobre o pavimento','Objeto estático sobre o leito carroçável',
                                                                                           'Acumulo de areia ou detritos sobre o pavimento', 'Ausência de sinalização', 'Acumulo de óleo sobre o pavimento', 'Área urbana sem a presença de local apropriado para a travessia de pedestres', 'Obras na pista', 'Iluminação deficiente',
                                                                                           'Deficiência do Sistema de Iluminação/Sinalização', 'Obstrução na via', 'Acostamento em desnível', 'Declive acentuado', 'Sinalização mal posicionada','Falta de acostamento', 'Falta de elemento de contenção que evite a saída do leito carroçável',
                                                                                           'Desvio temporário', 'Demais Fenômenos da natureza', 'Pista em desnível', 'Restrição de visibilidade em curvas verticais', 'Neblina', 'Fumaça', 'Restrição de visibilidade em curvas horizontais', 'Sinalização encoberta', 'Faixas de trânsito com largura insuficiente',
                                                                                           'Pista Escorregadia','Semáforo com defeito'), 'Fator Via/ Natureza', 'NA')))


# Adicionar a coluna "Zona" com base nas regras
df_unificado$zona <- ifelse(df_unificado$uso_solo == "Sim" , 'Urbano',
                            ifelse(df_unificado$uso_solo == "Não", 'Rural','NA'))

# Adicionar a coluna "houve mortos ou feridos graves?" VARIAVEL TARGET
df_unificado$houve_mortos_ou_feridos_graves <- ifelse(df_unificado$mortos > 0 | df_unificado$feridos_graves > 0 , '1','0')


# Adicionar a coluna "tipo_acidente_categ" com base nas regras - agreguei por colisão, atropelamento, capotamento, engavetamento, tombamento, saída leito, outros
df_unificado$tipo_acidente_categ <- ifelse(df_unificado$tipo_acidente %in% c('Colisão com objeto', 'Colisão traseira', 'Colisão lateral mesmo sentido', 'Colisão frontal','Colisão lateral sentido oposto', 'Colisão transversal'), 'Colisão',
                                           ifelse(df_unificado$tipo_acidente %in% c('Atropelamento de Animal', 'Atropelamento de Pedestre' ), 'Atropelamento',
                                                  ifelse(df_unificado$tipo_acidente %in% c('Capotamento'), 'Capotamento',
                                                         ifelse(df_unificado$tipo_acidente %in% c('Engavetamento'), 'Engavetamento',
                                                                ifelse(df_unificado$tipo_acidente %in% c('Tombamento'), 'Tombamento',
                                                                       ifelse(df_unificado$tipo_acidente %in% c('Saída de leito carroçável'), 'Saída de leito carroçável',
                                                                              ifelse(df_unificado$tipo_acidente %in% c('Derramamento de carga','Incêndio','Eventos atípicos', 'Queda de ocupante de veículo' ), 'Outros','NA')))))))







# removendo coluna que não vou utilizar, reduzi de 33 variaveis para 19 variaveis , total 125.563 registros
df_ajust <- df_unificado[, -c(4,7,8,9,10,11,12,17,19,20,21,22,23,24)] 

# COLUNAS REMOVIDAS MOTIVO:
# HORARIO: removi pois irei trabalhar com a informação agregado mensal/
# KM e MUNICIPIO: removi pois o objetivo é trabalhar apenas com MG > BR 381 
# CAUSA_ACIDENTE e TIPO_ACIDENTE: removi pois foi feito uma categorização
# CLASSIFICAÇÃO_ACIDENTE: removi, pois iria causa confusão com a variavel target
# FASE_DIA: removi pois estava inconsistente com horario, porém com base no horario foi feito uma agregação pra identificar a fase_dia correta
# USO_SOLO: removi pois foi classificado em uma nova variavel como Urbano ou Rural, utilizei o dicionário de dados para a classificação
# QTD_MORTOS, QTD_FERIDOS_LEVES, QTD_FERIDOS_GRAVES, QTD_FERIDOS, QTD_ILESOS, QTD_IGNORADOS: removi par anão causar confusão com a variavel target


########################################  HIGIENIZANDO A BASE - ANALISE MISSING VALUE - BASE TOTAL   ############################################################


# ANALISAR O MISSING VALUE DAS VARIAVEIS QUANTITATIVAS
# Embora tenha NA em BR representa cerca de 0,27% do brasil , e usarei somente a br 381 então não irei tratar pois não afeta o meu problema de negócio

NAS <- round(colSums(is.na(df_ajust))*100/nrow(df_ajust),2)
NAS
NAS[NAS>0]


# Alterar a coluna "tracado_via" com base nas regras - manter o NA e avaliar na analise exploratoria
df_ajust$tracado_via <- ifelse(df_ajust$tracado_via == "Não Informado" , 'NA', df_ajust$tracado_via)


#Avaliar o NA na coluna tracado_via - representa 19,93% de NA da base total com 25.024 registros. Reanalizar quando filtrar por MG> br 381, provavel que afetará bastante

tracado_via <- table(df_ajust$tracado_via)
tracado_via <- sort(tracado_via, decreasing = TRUE)
porcentagens <- prop.table(tracado_via) * 100


tabela_final <- data.frame(
  Categoria = names(tracado_via),
  Frequencia_Porcentagem = paste(tracado_via, " (", round(porcentagens, 2), "%)", sep = "")
)


kable(tabela_final, format = "markdown", col.names = c("Categoria", "Frequência / Percentual"))



# Alterar a coluna "sentido_via" com base nas regras - manter o NA e avaliar na analise exploratoria
df_ajust$sentido_via <- ifelse(df_ajust$sentido_via == "Não Informado" , 'NA', df_ajust$sentido_via)


#Avaliar o NA na coluna sentido_via - representa 0,27% de NA da base total com 343 registros. Reanalizar quando filtrar por MG> br 381, provavel que não afetará

sentido_via <- table(df_ajust$sentido_via)
sentido_via <- sort(sentido_via, decreasing = TRUE)
porcentagens <- prop.table(sentido_via) * 100


tabela_final <- data.frame(
  Categoria = names(sentido_via),
  Frequencia_Porcentagem = paste(sentido_via, " (", round(porcentagens, 2), "%)", sep = "")
)


kable(tabela_final, format = "markdown", col.names = c("Categoria", "Frequência / Percentual"))



# Alterar a coluna "condicao_metereologica" com base nas regras - manter o NA e avaliar na analise exploratoria
df_ajust$condicao_metereologica <- ifelse(df_ajust$condicao_metereologica == "Ignorado" , 'NA', df_ajust$condicao_metereologica)


#Avaliar o NA na coluna condicao_metereologica - representa 1,29% de NA da base total com 1616 registros. Reanalizar quando filtrar por MG> br 381

condicao_metereologica <- table(df_ajust$condicao_metereologica)
condicao_metereologica <- sort(condicao_metereologica, decreasing = TRUE)
porcentagens <- prop.table(condicao_metereologica) * 100


tabela_final <- data.frame(
  Categoria = names(condicao_metereologica),
  Frequencia_Porcentagem = paste(condicao_metereologica, " (", round(porcentagens, 2), "%)", sep = "")
)


kable(tabela_final, format = "markdown", col.names = c("Categoria", "Frequência / Percentual"))



######################################## FILTRANDO A BASE PARA O RECAORTE DESEJADO ############################################################


#filtrando dados para obter somente ocorrências de acidente de transito na Rodovia no Estado de MG, na BR 381, de jan/22 a out/23 (TOTAL DE 4605 REGISTROS E 19 VARIAVEIS)
df_filtrado <- subset(df_ajust, df_ajust$uf == 'MG'& df_ajust$br == '381' & df_ajust$ano_mes_ocorrencia != "2023-11")


######################################## EXPORTANDO A BASE PARA O RECAORTE DESEJADO ############################################################

#exportando o arquivo
write.csv(df_filtrado , "base_acidente_transito_MG_381_v2.csv", row.names = FALSE)

######################################## ANALISE EXPLORATÓRIA - MISSING VALUE ############################################################

# MISSING VALUE NAS VARIAVEIS QUANTITATIVAS: não tem 
NAS <- round(colSums(is.na(df_filtrado))*100/nrow(df_filtrado),2)
NAS
NAS[NAS>0]



#Avaliar o NA na coluna tracado_via - representa 30,88% de NA  (1422 registros) da base de MG br 381. como o NA esta acima de 30%, irei remover

tracado_via <- table(df_filtrado$tracado_via)
tracado_via <- sort(tracado_via, decreasing = TRUE)
porcentagens <- prop.table(tracado_via) * 100


tabela_final <- data.frame(
  Categoria = names(tracado_via),
  Frequencia_Porcentagem = paste(tracado_via, " (", round(porcentagens, 2), "%)", sep = "")
)


kable(tabela_final, format = "markdown", col.names = c("Categoria", "Frequência / Percentual"))



#Avaliar o NA na coluna sentido_via - Não contém NA

sentido_via <- table(df_filtrado$sentido_via)
sentido_via <- sort(sentido_via, decreasing = TRUE)
porcentagens <- prop.table(sentido_via) * 100


tabela_final <- data.frame(
  Categoria = names(sentido_via),
  Frequencia_Porcentagem = paste(sentido_via, " (", round(porcentagens, 2), "%)", sep = "")
)


kable(tabela_final, format = "markdown", col.names = c("Categoria", "Frequência / Percentual"))



#Avaliar o NA na coluna condicao_metereologica - representa 1,85% de NA com 85 registros. Vou manter na base uma vez q já esta categorizado

condicao_metereologica <- table(df_filtrado$condicao_metereologica)
condicao_metereologica <- sort(condicao_metereologica, decreasing = TRUE)
porcentagens <- prop.table(condicao_metereologica) * 100


tabela_final <- data.frame(
  Categoria = names(condicao_metereologica),
  Frequencia_Porcentagem = paste(condicao_metereologica, " (", round(porcentagens, 2), "%)", sep = "")
)


kable(tabela_final, format = "markdown", col.names = c("Categoria", "Frequência / Percentual"))



########################### ANALISE EXPLORATORIA - SINTETIZANDO DADOS QUALITATIVOS ##############################


#variavel explicativa- ano_mes_ocorrencia
#mantém um comportamento linear ao longo dos meses, com destaque para dez/22 que teve a maior qtd de ocorrencia com cerca de 4% a 5% de ocorrencia por mês.
tb_abs_ano_mes_ocorrencia <- (table(df_filtrado$ano_mes_ocorrencia))
tb_abs_ano_mes_ocorrencia
kable(tb_abs_ano_mes_ocorrencia, format = "markdown")

tb_relat_ano_mes_ocorrencia<- prop.table(table(df_filtrado$ano_mes_ocorrencia))
tb_relat_ano_mes_ocorrencia
kable(tb_relat_ano_mes_ocorrencia, format = "markdown")

tb_percen_ano_mes_ocorrencia <- (prop.table(table(df_filtrado$ano_mes_ocorrencia)))* 100 
tb_percen_ano_mes_ocorrencia
kable(tb_percen_ano_mes_ocorrencia, format = "markdown")

barplot(table(df_filtrado$ano_mes_ocorrencia))
barplot(tb_percen_ano_mes_ocorrencia)


#variavel explicativa- mes_ocorrencia
#mês 7 tem uma representatividade maior de 10%, e como tem apenas 1 intervalo de periodo para os meses 11 e 12 consequentemente ficam mais baixos.
tb_abs_mes_ocorrencia <- (table(df_filtrado$mes_ocorrencia))
tb_abs_mes_ocorrencia
kable(tb_abs_mes_ocorrencia, format = "markdown")

tb_relat_mes_ocorrencia<- prop.table(table(df_filtrado$mes_ocorrencia))
tb_relat_mes_ocorrencia
kable(tb_relat_mes_ocorrencia, format = "markdown")

tb_percen_mes_ocorrencia <- (prop.table(table(df_filtrado$mes_ocorrencia)))* 100 
tb_percen_mes_ocorrencia
kable(tb_percen_mes_ocorrencia, format = "markdown")

barplot(table(df_filtrado$mes_ocorrencia))
barplot(tb_percen_mes_ocorrencia)



#variavel explicativa- dia_semana
#Domingo é levemente mais representativo  com 17% e 787 ocorrencias
tb_abs_dia_semana <- (table(df_filtrado$dia_semana))
tb_abs_dia_semana
kable(tb_abs_dia_semana, format = "markdown")

tb_relat_dia_semana<- prop.table(table(df_filtrado$dia_semana))
tb_relat_dia_semana
kable(tb_relat_dia_semana, format = "markdown")

tb_percen_dia_semana <- (prop.table(table(df_filtrado$dia_semana)))* 100 
tb_percen_dia_semana
kable(tb_percen_dia_semana, format = "markdown")

barplot(table(df_filtrado$dia_semana))
barplot(tb_percen_dia_semana)


#variavel explicativa- periodo_dia
#No periodo da Tarde é onde tem mais ocorrencias com 33% e 1559 ocorrencias
tb_abs_periodo_dia <- (table(df_filtrado$periodo_dia))
tb_abs_periodo_dia
kable(tb_abs_periodo_dia, format = "markdown")

tb_relat_periodo_dia<- prop.table(table(df_filtrado$periodo_dia))
tb_relat_periodo_dia
kable(tb_relat_periodo_dia, format = "markdown")

tb_percen_periodo_dia <- (prop.table(table(df_filtrado$periodo_dia)))* 100 
tb_percen_periodo_dia
kable(tb_percen_periodo_dia, format = "markdown")

barplot(table(df_filtrado$periodo_dia))
barplot(tb_percen_periodo_dia)



#variavel explicativa- causa_acidente_agg
# mais de 80,08% dos casos ocorrem devido fator humano! totalizando 3.688 ocorrências
tb_abs_causa_acidente_agg <- (table(df_filtrado$causa_acidente_agg))
tb_abs_causa_acidente_agg
kable(tb_abs_causa_acidente_agg, format = "markdown")

tb_relat_causa_acidente_agg <- prop.table(table(df_filtrado$causa_acidente_agg))
tb_relat_causa_acidente_agg
kable(tb_relat_causa_acidente_agg, format = "markdown")

tb_percen_causa_acidente_agg <- (prop.table(table(df_filtrado$causa_acidente_agg)))* 100 
tb_percen_causa_acidente_agg
kable(tb_percen_causa_acidente_agg, format = "markdown")

barplot(table(df_filtrado$causa_acidente_agg))
barplot(tb_percen_causa_acidente_agg)


#variavel explicativa- tipo_acidente_categ
#colisão representa 49,42% totalizando 2.276 ocorrências) 
tb_abs_tipo_acidente_categ <- (table(df_filtrado$tipo_acidente_categ))
tb_abs_tipo_acidente_categ
kable(tb_abs_tipo_acidente_categ, format = "markdown")

tb_relat_tipo_acidente_categ <- prop.table(table(df_filtrado$tipo_acidente_categ))
tb_relat_tipo_acidente_categ
kable(tb_relat_tipo_acidente_categ, format = "markdown")

tb_percen_tipo_acidente_categ <- (prop.table(table(df_filtrado$tipo_acidente_categ)))* 100 
tb_percen_tipo_acidente_categ
kable(tb_percen_tipo_acidente_categ, format = "markdown")

barplot(table(df_filtrado$tipo_acidente_categ))
barplot(tb_percen_tipo_acidente_categ)

#variavel explicativa- sentido_via
# Sentido via decrescente é levente mais representativo com 50,44% (2.323 ocorrências)
tb_abs_sentido_via<- (table(df_filtrado$sentido_via))
tb_abs_sentido_via
kable(tb_abs_sentido_via, format = "markdown")

tb_relat_sentido_via<- prop.table(table(df_filtrado$sentido_via))
tb_relat_sentido_via
kable(tb_relat_sentido_via, format = "markdown")

tb_percen_sentido_via <- (prop.table(table(df_filtrado$sentido_via)))* 100 
tb_percen_sentido_via
kable(tb_percen_sentido_via, format = "markdown")

barplot(table(df_filtrado$sentido_via))
barplot(tb_percen_sentido_via)



#variavel explicativa- condicao_meteorologica
# Quando a condição meteorologica é de Céu claro é responsavel por 59% das ocorrencias (2.721), ja chuva e nublado somados representam 29%
tb_abs_condicao_meteoro <- (table(df_filtrado$condicao_metereologica))
tb_abs_condicao_meteoro
kable(tb_abs_condicao_meteoro, format = "markdown")

tb_relat_condicao_meteoro <- prop.table(table(df_filtrado$condicao_metereologica))
tb_relat_condicao_meteoro
kable(tb_relat_condicao_meteoro, format = "markdown")


tb_percen_condicao_meteoro <- (prop.table(table(df_filtrado$condicao_metereologica)))*100 
tb_percen_condicao_meteoro
kable(tb_percen_condicao_meteoro, format = "markdown")


barplot(table(df_filtrado$condicao_metereologica))
barplot(tb_percen_condicao_meteoro)

#variavel explicativa- tipo_pista
# Quando a o tipo de pista é dupla é responsavel por + de 68% das ocorrencias, totalizando 3.164 ocorrências
tb_abs_tipo_pista <- (table(df_filtrado$tipo_pista))
tb_abs_tipo_pista
kable(tb_abs_tipo_pista, format = "markdown")

tb_relat_tipo_pista<- prop.table(table(df_filtrado$tipo_pista))
tb_relat_tipo_pista
kable(tb_relat_tipo_pista, format = "markdown")


tb_percen_tipo_pista <- (prop.table(table(df_filtrado$tipo_pista)))*100 
tb_percen_tipo_pista
kable(tb_percen_tipo_pista, format = "markdown")


barplot(table(df_filtrado$tipo_pista))
barplot(tb_percen_tipo_pista)

#variavel explicativa- tracado_via
# Cerca de 41% é traçado via reta, porem cerca de 30% NA, acho q neste caso vale remover esta variavel , POR ENQUANTO MANTER NO MODELO< POREM SE FICAR RUIM RETIRAR POIS O IV DELE É DE 0,03 
tb_abs_tracado_via<- (table(df_filtrado$tracado_via))
tb_abs_tracado_via
kable(tb_abs_tracado_via, format = "markdown")

tb_relat_tracado_via<- prop.table(table(df_filtrado$tracado_via))
tb_relat_tracado_via
kable(tb_relat_tracado_via, format = "markdown")


tb_percen_tracado_via <- (prop.table(table(df_filtrado$tracado_via)))*100 
tb_percen_tracado_via
kable(tb_percen_tracado_via, format = "markdown")


barplot(table(df_filtrado$tracado_via))
barplot(tb_percen_tracado_via)

#variavel explicativa- zona
# Cerca de 66% da zona é rural  totalizando 1.528
tb_abs_zona <- (table(df_filtrado$zona))
tb_abs_zona
kable(tb_abs_zona, format = "markdown")

tb_relat_zona <- prop.table(table(df_filtrado$zona))
tb_relat_zona
kable(tb_relat_zona, format = "markdown")


tb_percen_zona <- (prop.table(table(df_filtrado$zona)))*100 
tb_percen_zona
kable(tb_percen_zona, format = "markdown")


barplot(table(df_filtrado$zona))
barplot(tb_percen_zona)


#variavel explicativa- houve_mortos_ou_feridos_graves
# Cerca de 24% de ocorrência onde tiveram feridos graves ou mortes, totalizando 1.112 
tb_abs_houve_mortos_ou_feridos_graves <- (table(df_filtrado$houve_mortos_ou_feridos_graves))
tb_abs_houve_mortos_ou_feridos_graves
kable(tb_abs_houve_mortos_ou_feridos_graves, format = "markdown")

tb_relat_houve_mortos_ou_feridos_graves <- prop.table(table(df_filtrado$houve_mortos_ou_feridos_graves))
tb_relat_houve_mortos_ou_feridos_graves
kable(tb_relat_houve_mortos_ou_feridos_graves, format = "markdown")


tb_percen_houve_mortos_ou_feridos_graves <- (prop.table(table(df_filtrado$houve_mortos_ou_feridos_graves)))*100 
tb_percen_houve_mortos_ou_feridos_graves
kable(tb_percen_houve_mortos_ou_feridos_graves, format = "markdown")


barplot(table(df_filtrado$houve_mortos_ou_feridos_graves))
barplot(tb_percen_houve_mortos_ou_feridos_graves)


########################### ANALISE EXPLORATORIA - SINTETIZANDO DADOS QUANTITATIVAS ##############################


# Variavel quantitativa - 'pessoas'
# 50% das ocorrências tiveram pelo menos 2 pessoas envolvidas, 25% das ocorrencias tiveram pelos menos 3 pessoas envolvidas, enquanto o máximo é de 54 e tem outlier 
summary(df_filtrado$pessoas)
boxplot(df_filtrado$pessoas,   
        col = "darkturquoise",
        main = "Boxplot Pessoas")
hist(df_filtrado$pessoas,
     col = "darkturquoise",
     main = "Histograma Pessoas")

#desvio padrao --
sd(df_filtrado$pessoas)

#coeficiente padrao  -- -- tem uma variabilidade muito alta em relação a média.
cv_pessoas <- (sd(df_filtrado$pessoas)/mean(df_filtrado$pessoas))*100
cv_pessoas




# Variavel quantitativa - 'veiculos'
# 50% da base são de 1 ocorrencia, 25% de 2 ocorrencias, enquanto o máximo é de 11 e tem outlier 
summary(df_filtrado$veiculos)
boxplot(df_filtrado$veiculos,   
        col = "darkturquoise",
        main = "Boxplot Veiculos")
hist(df_filtrado$veiculos,
     col = "darkturquoise",
     main = "Histograma Veiculos")

#desvio padrao -- 
sd(df_filtrado$veiculos)

#coeficiente padrao  -- Tem variabilidade em relação  média, porém é uma variabildiade menor do que a de quantidade de pessoas. 
cv_veiculos <- (sd(df_filtrado$veiculos)/mean(df_filtrado$veiculos))*100
cv_veiculos


##################################### ANALISE BIVARIADA - SINTETIZANDO DADOS QUALITATIVAS ############################################

#Quali x Quali - ano_mes_ocorrencia X houve_mortos_ou_feridos_graves?
# em média 24% das ocorrências com target = 1, somente abr/22 e mar/23 tiveram um comportamento atípico com 30%
par(mfrow = c(1,1))

biv_ano_mes_ocorrencia_abs <- table(df_filtrado$houve_mortos_ou_feridos_graves, df_filtrado$ano_mes_ocorrencia)
biv_ano_mes_ocorrencia_abs_t <- t(biv_ano_mes_ocorrencia_abs)
biv_ano_mes_ocorrencia_abs_t
biv_ano_mes_ocorrencia_perc <- prop.table(biv_ano_mes_ocorrencia_abs, margin = 2) * 100  # Calcular por coluna (ano_mes_ocorrencia)
biv_ano_mes_ocorrencia_perc_t <- t(biv_ano_mes_ocorrencia_perc)
biv_ano_mes_ocorrencia_perc_t
barplot(biv_ano_mes_ocorrencia_perc, beside = FALSE, col = c("darkturquoise", "gray"), legend.text = TRUE)


#Quali x Quali - mes_ocorrencia X houve_mortos_ou_feridos_graves?
# os meses 03 e 07 tem um representatividade levemente acima dos demais periodos quando a ocorrências com target é  = 1, sendo de 26% +100
par(mfrow = c(1,1))

biv_mes_ocorrencia_abs <- table(df_filtrado$houve_mortos_ou_feridos_graves, df_filtrado$mes_ocorrencia)
biv_mes_ocorrencia_abs_t <- t(biv_mes_ocorrencia_abs)
biv_mes_ocorrencia_abs_t
biv_mes_ocorrencia_perc <- prop.table(biv_mes_ocorrencia_abs, margin = 2) * 100  # Calcular por coluna (ano_mes_ocorrencia)
biv_mes_ocorrencia_perc_t <- t(biv_mes_ocorrencia_perc)
biv_mes_ocorrencia_perc_t
barplot(biv_mes_ocorrencia_perc, beside = FALSE, col = c("darkturquoise", "gray"), legend.text = TRUE)


#Quali x Quali - dia_semana X houve_mortos_ou_feridos_graves?
# sabado tem um representatividade levemente acima dos demais periodos do dia quando a ocorrências com target é  = 1, sendo de 27% (192)
par(mfrow = c(1,1))

biv_dia_semana_abs <- table(df_filtrado$houve_mortos_ou_feridos_graves, df_filtrado$dia_semana)
biv_dia_semana_abs_t <- t(biv_dia_semana_abs)
biv_dia_semana_abs_t
biv_dia_semana_perc <- prop.table(biv_dia_semana_abs, margin = 2) * 100  # Calcular por coluna (ano_mes_ocorrencia)
biv_dia_semana_perc_t <- t(biv_dia_semana_perc)
biv_dia_semana_perc_t
barplot(biv_dia_semana_perc, beside = FALSE, col = c("darkturquoise", "gray"), legend.text = TRUE)



#Quali x Quali - periodo_dia X houve_mortos_ou_feridos_graves?
# Madrugada e noite tem um representatividade levemente acima dos demais periodos do dia quando a ocorrências com target é  = 1, sendo (26% em média 220)
par(mfrow = c(1,1))

biv_periodo_dia_abs <- table(df_filtrado$houve_mortos_ou_feridos_graves, df_filtrado$periodo_dia)
biv_periodo_dia_abs_t <- t(biv_periodo_dia_abs)
biv_periodo_dia_abs_t
biv_periodo_dia_perc <- prop.table(biv_periodo_dia_abs, margin = 2) * 100  # Calcular por coluna (ano_mes_ocorrencia)
biv_periodo_dia_perc_t <- t(biv_periodo_dia_perc)
biv_periodo_dia_perc_t
barplot(biv_periodo_dia_perc, beside = FALSE, col = c("darkturquoise", "gray"), legend.text = TRUE)



#Quali x Quali - causa_acidente_agg X houve_mortos_ou_feridos_graves?
# causa acidente Fator Humano e via/natureza tem cerca de 21 a 25% de ocorrências com target = 1 cada, sendo 950  seguido de 116 ocorrências.
biv_causa_acidente_agg_abs <- table(df_filtrado$houve_mortos_ou_feridos_graves, df_filtrado$causa_acidente_agg)
biv_causa_acidente_agg_abs_t <- t(biv_causa_acidente_agg_abs)
biv_causa_acidente_agg_abs_t
biv_causa_acidente_agg_perc <- prop.table(biv_causa_acidente_agg_abs, margin = 2) * 100  # Calcular por coluna (ano_mes_ocorrencia)
biv_causa_acidente_agg_perc_t <- t(biv_causa_acidente_agg_perc)
biv_causa_acidente_agg_perc_t
barplot(biv_causa_acidente_agg_perc, beside = FALSE, col = c("darkturquoise", "gray"), legend.text = TRUE)


#Quali x Quali - tipo_acidente X houve_mortos_ou_feridos_graves?
# o tipo de acidente atropelamento tem um comportamento bem expressivo para ocorrências com target = 1, sendo 60% (129), posteriormente clisão com 26%(599)
biv_tipo_acidente_categ_abs <- table(df_filtrado$houve_mortos_ou_feridos_graves, df_filtrado$tipo_acidente_categ)
biv_tipo_acidente_categ_abs_t <- t(biv_tipo_acidente_categ_abs)
biv_tipo_acidente_categ_abs_t
biv_tipo_acidente_categ_perc <- prop.table(biv_tipo_acidente_categ_abs, margin = 2) * 100  # Calcular por coluna (ano_mes_ocorrencia)
biv_tipo_acidente_categ_perc_t <- t(biv_tipo_acidente_categ_perc)
biv_tipo_acidente_categ_perc_t
barplot(biv_tipo_acidente_categ_perc, beside = FALSE, col = c("darkturquoise", "gray"), legend.text = TRUE)


#Quali x Quali - sentido_via X houve_mortos_ou_feridos_graves?
# independente do sentido da via (crescente ou decrescente) o comportamento é o mesmo para ocorrências com target = 1, sendo em torno de 24%
biv_sentido_via_abs <- table(df_filtrado$houve_mortos_ou_feridos_graves, df_filtrado$sentido_via)
biv_sentido_via_abs_t <- t(biv_sentido_via_abs)
biv_sentido_via_abs_t
biv_sentido_via_perc <- prop.table(biv_sentido_via_abs, margin = 2) * 100  # Calcular por coluna (ano_mes_ocorrencia)
biv_sentido_via_perc_t <- t(biv_sentido_via_perc)
biv_sentido_via_perc_t
barplot(biv_sentido_via_perc, beside = FALSE, col = c("darkturquoise", "gray"), legend.text = TRUE)


#Quali x Quali - condicao_metereologica X houve_mortos_ou_feridos_graves?
# Embora Nevoeiro/Neblina represente 32% das ocorrências com target = 1, em top1 temos o NA(missing value), após conifrmar o IV desta variavel, provavelmente será removida do modelo.
biv_condicao_metereologica_abs <- table(df_filtrado$houve_mortos_ou_feridos_graves, df_filtrado$condicao_metereologica)
biv_condicao_metereologica_abs_t <- t(biv_condicao_metereologica_abs)
biv_condicao_metereologica_abs_t
biv_condicao_metereologica_perc <- prop.table(biv_condicao_metereologica_abs, margin = 2) * 100  # Calcular por coluna (ano_mes_ocorrencia)
biv_condicao_metereologica_perc_t <- t(biv_condicao_metereologica_perc)
biv_condicao_metereologica_perc_t
barplot(biv_condicao_metereologica_perc, beside = FALSE, col = c("darkturquoise", "gray"), legend.text = TRUE)


#Quali x Quali - tipo_pista X houve_mortos_ou_feridos_graves?
# Tipo de pista Simples é responsável por 38% das ocorrências com target = 1, diferente da base total que o destaque era para pista Dupla.
biv_tipo_pista_abs <- table(df_filtrado$houve_mortos_ou_feridos_graves, df_filtrado$tipo_pista)
biv_tipo_pista_abs_t <- t(biv_tipo_pista_abs)
biv_tipo_pista_abs_t
biv_tipo_pista_perc <- prop.table(biv_tipo_pista_abs, margin = 2) * 100  # Calcular por coluna (ano_mes_ocorrencia)
biv_tipo_pista_perc_t <- t(biv_tipo_pista_perc)
biv_tipo_pista_perc_t
barplot(biv_tipo_pista_perc, beside = FALSE, col = c("darkturquoise", "gray"), legend.text = TRUE)


#Quali x Quali - tracado_via X houve_mortos_ou_feridos_graves?
# quando o traçado_via é ponte 50% das ocorrências são target = 1, porém devido o alto volume de NA na bse, essa variável será removida.
biv_tracado_via_abs <- table(df_filtrado$houve_mortos_ou_feridos_graves, df_filtrado$tracado_via)
biv_tracado_via_abs_t <- t(biv_tracado_via_abs)
biv_tracado_via_abs_t
biv_tracado_via_perc <- prop.table(biv_tracado_via_abs, margin = 2) * 100  # Calcular por coluna (ano_mes_ocorrencia)
biv_tracado_via_perc_t <- t(biv_tracado_via_perc)
biv_tracado_via_perc_t
barplot(biv_tracado_via_perc, beside = FALSE, col = c("darkturquoise", "gray"), legend.text = TRUE)


#Quali x Quali - zona X houve_mortos_ou_feridos_graves?
# independente da ZOna, temos cerca de 23 a 25% de ocorrências com target = 1
biv_zona_abs <- table(df_filtrado$houve_mortos_ou_feridos_graves, df_filtrado$zona)
biv_zona_abs_t <- t(biv_zona_abs)
biv_zona_abs_t
biv_zona_perc <- prop.table(biv_zona_abs, margin = 2) * 100  # Calcular por coluna (ano_mes_ocorrencia)
biv_zona_perc_t <- t(biv_zona_perc)
biv_zona_perc_t
barplot(biv_zona_perc, beside = FALSE, col = c("darkturquoise", "gray"), legend.text = TRUE)



##################################### ANALISE BIVARIADA - SINTETIZANDO DADOS QUANTITATIVAS ############################################

#Quali x Quanti - pessoas X houve_mortes_ou_feridos_graves?
# 50% da base teve 2 pessoas envolvidas quando o target = 1, já quando o target = 0 25% da base tem cerca de 1 pessoa envolvida
boxplot(df_filtrado$pessoas~df_filtrado$houve_mortos_ou_feridos_graves, 
        col="darkturquoise")



#summary
df_pessoas <- data.frame(df_filtrado[, c("pessoas", "houve_mortos_ou_feridos_graves")])

custom_summary <- function(x) c(
  mean = mean(x),
  sd = sd(x),
  min = min(x),
  q1 = quantile(x, 0.25),
  median = median(x),
  q3 = quantile(x, 0.75),
  max = max(x)
)

summary_pessoas <- summaryBy(pessoas ~ houve_mortos_ou_feridos_graves, data = df_pessoas, FUN = custom_summary)

print(summary_pessoas)


#Quali x Quanti - veiculos X houve_mortes_ou_feridos_graves?
#50% da base pode ter até 2 veículos envolvidos quando o target = 1, já quando o target = 0 somente 1 veiculo envolvido.
boxplot(df_filtrado$veiculos~df_filtrado$houve_mortos_ou_feridos_graves, 
        col="darkturquoise")


#summary
df_veiculos <- data.frame(df_filtrado[, c("veiculos", "houve_mortos_ou_feridos_graves")])

custom_summary <- function(x) c(
  mean = mean(x),
  sd = sd(x),
  min = min(x),
  q1 = quantile(x, 0.25),
  median = median(x),
  q3 = quantile(x, 0.75),
  max = max(x)
)

summary_veiculos <- summaryBy(veiculos ~ houve_mortos_ou_feridos_graves, data = df_veiculos, FUN = custom_summary)

print(summary_veiculos)



# removendo colunas que não vou utilizar nos modelos tracado_via pois o NA esta muito alto

#df_filtrado com 18 variaveis e 4605 registros
df_filtrado1 <- df_filtrado[, -c(9)] 



##################################### IV - RELEVANCIA DAS VARIAVEIS EXPLICATIVAS ##########################################

# Convertendo a variável dependente para numérica 
df_filtrado1$houve_mortos_ou_feridos_graves <- as.numeric(df_filtrado1$houve_mortos_ou_feridos_graves)


#verificar o IV para saber a relevância das variaveis para o modelo  - ficaram 7 variaveis relevantes pro modelo
IV_df_filtrado <- create_infotables(data = df_filtrado1[ ,c(2:18)],
                                    y = "houve_mortos_ou_feridos_graves")
IV_df_filtrado$Summary


#Devido ao baixo IV menor do 0,02 remover do modelo: sentido_via, zona, mes_ocorrencia, periodo_dia, dia_semana. 
#As variaveis UF, BR, Região, ocorrencia não entram no IV, pois são unique, e nem a data inversa, pois é data



#################### SEPARAR A BASE EM TREINO E TESTE - HOLDOUT VALIDATION - df_filtrado1 #######################

#Divida a base em treino e teste
set.seed(123)
amostra = sort(sample(nrow(df_filtrado1), nrow(df_filtrado1)*.7))
treino<-df_filtrado1[amostra,]
teste<-df_filtrado1[-amostra,]

names(df_filtrado1)

##################################### MODELO - REMOVENDO QUEM TIVER P-VALOR ACIMA DE 0,05 - df_filtrado1 ############################################


# com a base treino
modelo1 <- glm(houve_mortos_ou_feridos_graves ~
                 pessoas +
                 veiculos + 
                 causa_acidente_agg +
                 condicao_metereologica +
                 tipo_pista +
                 ano_mes_ocorrencia +
                 tipo_acidente_categ, 
               family = binomial(link = 'logit'), data = treino)
summary(modelo1)


# com a base treino - retirando ano_mes_ocorrencia

modelo1 <- glm(houve_mortos_ou_feridos_graves ~
                 pessoas +
                 veiculos + 
                 causa_acidente_agg +
                 condicao_metereologica +
                 tipo_pista +
                # ano_mes_ocorrencia +
                tipo_acidente_categ,
               family = binomial(link = 'logit'), data = treino)
summary(modelo1)


# com a base treino - retirando condicao_metereologica
modelo1 <- glm(houve_mortos_ou_feridos_graves ~
                 pessoas +
                 veiculos + 
                 causa_acidente_agg +
                # condicao_metereologica +
                 tipo_pista +
                 # ano_mes_ocorrencia +
                 tipo_acidente_categ,
               family = binomial(link = 'logit'), data = treino)
summary(modelo1)




##################################### VIF - VERIFICAR MULTICOLINEARIDADE - df_filtrado1 ############################################

vif(modelo1) #base treino

#Não houve multicolinearidade



####################################### PREDICT ###########################################

#base treino 
treino$probabilidade = predict(modelo1, treino, type = "response")


#base teste
teste$probabilidade = predict(modelo1, teste, type = "response")


####################################### PONTO DE CORTE - df_filtrado1 ###########################################


# ponto de corte 4: para maximizar a sensibildiade e especificidade
ponto_corte1 <- cutpointr(treino, probabilidade, houve_mortos_ou_feridos_graves,
                          method = maximize_metric, metric = sum_sens_spec,
                          pos_class = 1, direction = ">=")
summary(ponto_corte1) 



####################################### PREDITO - df_filtrado1 ###########################################


#base_treino - usandoo cut_point
treino$predito <- as.factor(ifelse(treino$probabilidade > 0.2259605, 1, 0))


#base_teste - usandoo cut_point
teste$predito <- as.factor(ifelse(teste$probabilidade > 0.2259605, 1, 0))




####################################### AVALIAÇÃO DE DESEMPENHO - df_filtrado1 ###########################################


# Funcao para auxiliar no calculo de medidas de desempenho do modelo

desempenho <- function (base, real, predito, prob) {
  
  tabela <- table(real, predito)
  
  pred <- prediction(prob, real)
  perf <- performance(pred, "tpr", "fpr")
  
  acuracia       <- (sum(real == 0 & predito == 0) + sum(real == 1 & predito == 1)) / nrow(base)
  sensibilidade  <- sum(real == 1 & predito == 1) / (sum(real == 1 & predito == 0) + sum(real == 1 & predito == 1))
  especificidade <- sum(real == 0 & predito == 0) / (sum(real == 0 & predito == 0) + sum(real == 0 & predito == 1))
  ks             <- max(attr(perf, 'y.values')[[1]] - attr(perf, 'x.values')[[1]])
  auc            <- performance(pred, "auc")
  auc            <- auc@y.values[[1]]
  
  print(paste0("Acuracia = ", round(acuracia, 3)))
  print(paste0("Sensibilidade = ", round(sensibilidade, 3)))
  print(paste0("Especificidade = ", round(especificidade, 3)))
  print(paste0("KS = ", round(ks, 3)))
  print(paste0("AUC = ", round(auc, 3)))
  plot(perf)
  
}

# Desempenho geral do modelo - base treino, sensibilidade 63%, acuracia 62%, especificidade 61%, KS 24%, AUC 67%

desempenho(treino,
           treino$houve_mortos_ou_feridos_graves,
           treino$predito,
           treino$probabilidade)


# Desempenho geral do modelo - base teste, sensibilidade 67%, acuracia 62%, especificidade 60%, KS 30%, AUC 70%

desempenho(teste,
           teste$houve_mortos_ou_feridos_graves,
           teste$predito,
           teste$probabilidade)


##################### VALIDAÇÃO DO MODELO - OUT-OF-TIME VALIDATION - df_fitlrado1 #############################

# Leitura da base de validacao out of time - 216 registro com 19 variaveis

base_oot <- subset(df_ajust, df_ajust$uf == 'MG'& df_ajust$br == '381' & df_ajust$ano_mes_ocorrencia == "2023-11")

#removendo coluna tracado_via 
base_oot <- base_oot[, -c(9)]


#variavel categorica para numerica
base_oot$houve_mortos_ou_feridos_graves <- as.numeric(base_oot$houve_mortos_ou_feridos_graves)

# Aplicando o modelo
base_oot$probabilidade   <- predict(modelo1, base_oot, type = 'response')
base_oot$predito <- ifelse(base_oot$probabilidade > 0.2259605, 1, 0) #ponto de corte da base treino


#avaliacao modelo - base oot , sensibilidade 60%, acuracia 58%, especificidade 57%, KS 29%, AUC 65%
desempenho(base_oot,
           base_oot$houve_mortos_ou_feridos_graves,
           base_oot$predito,
           base_oot$probabilidade)


######## INCLUSÃO DESSA ETAPA NO DIA 21/09/2024, FAZ PARTE DA ENTREGA 3 ########

# Instalar e carregar o pacote necessário
if (!requireNamespace("ggplot2", quietly = TRUE)) {
  install.packages("ggplot2")
}
library(ggplot2)

# Função para plotar a matriz de confusão com rótulos
plot_confusion_matrix <- function(real, predito, cutoff) {
  # Criar a matriz de confusão
  conf_matrix <- table(Predito = predito, Real = real)
  
  # Criar um DataFrame para ggplot
  conf_matrix_df <- as.data.frame(conf_matrix)
  names(conf_matrix_df) <- c("Predito", "Real", "Frequencia")
  
  # Adicionar rótulos dos quadrantes
  conf_matrix_df$Quadrante <- factor(
    with(conf_matrix_df, ifelse(Predito == 0 & Real == 0, "TN",
                                ifelse(Predito == 1 & Real == 0, "FP",
                                       ifelse(Predito == 0 & Real == 1, "FN", "TP")))),
    levels = c("TN", "FP", "FN", "TP")
  )
  
  # Criar o gráfico da matriz de confusão
  ggplot(conf_matrix_df, aes(x = Predito, y = Real, fill = Frequencia)) +
    geom_tile() +
    geom_text(aes(label = Frequencia), vjust = 1) +
    scale_fill_gradient(low = "white", high = "blue") +
    labs(x = "Predito", y = "Real", fill = "Frequência") +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
}

# Aplicar a função às bases de treino, teste e oot
plot_confusion_matrix(treino$houve_mortos_ou_feridos_graves, treino$predito, 0.2259605)
plot_confusion_matrix(teste$houve_mortos_ou_feridos_graves, teste$predito, 0.2259605)
plot_confusion_matrix(base_oot$houve_mortos_ou_feridos_graves, base_oot$predito, 0.2259605)


# Função para calcular custos de prevenção
calcular_custo_prevencao <- function(real, predito) {
  custo_acidente <- 2600  # Custo médio por acidente grave
  custo_reparo_fn <- 3000  # Custo adicional para remediar falsos negativos (FN)
  custo_prevencao_fp <- 400  # Custo da mobilização preventiva em falsos positivos (FP)
    
  # Criar matriz de confusão
  conf_matrix <- table(Predito = predito, Real = real)
  
  # Recuperar os valores da matriz de confusão
  tn <- conf_matrix["0", "0"]
  fp <- conf_matrix["1", "0"]
  fn <- conf_matrix["0", "1"]
  tp <- conf_matrix["1", "1"]
  
  # Cálculo de custos
  custo_tp <- tp * custo_acidente
  custo_fn <- fn * custo_reparo_fn
  custo_fp <- fp * custo_prevencao_fp
  
  custo_sem_modelo <- (tp + fn) * custo_acidente
  custo_com_modelo <- custo_tp + custo_fn + custo_fp
  economia_efetiva <- custo_sem_modelo - custo_com_modelo  # Corrigido para refletir economia
  
  return(c(custo_sem_modelo, custo_com_modelo, economia_efetiva, tp, fn, fp, tn))
}

# Exemplo de uso da função
# Supondo que você tenha os dados reais e preditos
# y_train e y_pred_train_optimal são os dados de treino
# y_oot e y_pred_oot_optimal são os dados de OOT

# Calcular custos para os conjuntos de treino, teste e OOT
custo_train <- calcular_custo_prevencao(treino$houve_mortos_ou_feridos_graves, treino$predito)
custo_teste <- calcular_custo_prevencao(teste$houve_mortos_ou_feridos_graves, teste$predito)
custo_oot <- calcular_custo_prevencao(base_oot$houve_mortos_ou_feridos_graves, base_oot$predito)

# Exibir os resultados para o conjunto de treino
cat("Custo sem Modelo - Treino: R$", round(custo_train[1], 2), "\n")
cat("Custo com Modelo - Treino: R$", round(custo_train[2], 2), "\n")
cat("Economia Efetiva - Treino: R$", round(custo_train[3], 2), "\n")

# Exibir os resultados para o conjunto de teste
cat("Custo sem Modelo - Teste: R$", round(custo_teste[1], 2), "\n")
cat("Custo com Modelo - Teste: R$", round(custo_teste[2], 2), "\n")
cat("Economia Efetiva - Teste: R$", round(custo_teste[3], 2), "\n")

# Exibir os resultados para o conjunto OOT
cat("Custo Sem Modelo - OOT: R$", round(custo_oot[1], 2), "\n")
cat("Custo com Modelo - OOT: R$", round(custo_oot[2], 2), "\n")
cat("Economia Efetiva - OOT: R$", round(custo_oot[3], 2), "\n")


