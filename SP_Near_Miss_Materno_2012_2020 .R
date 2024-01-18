##############################################################################################################

###  Capta Bases SIH SP RD 2012 a 2019, seleciona  Mulheres de 10 a 49 anos e AIH tipo 1

## Data: 25/03/2022 

## ATUALIZADO: 22/06/2022

## Autor: Lana Meijinhos

###############################################################################################################

#### Chamando os pacotes

library(tidyverse)
library(magrittr)
library(lubridate)
library(httr)
library(getPass)
library(repr)
library(read.dbc)
library(RCurl)
library(curl)
library(jsonlite)
library(remotes)
library(microdatasus)
library(Rtools)
library(devtools)
library(data.table)
library(readr)
library(janitor)


### Diretório

# session > set working directory > choose directory > escolher o local que vai salvar as bases


#### Instalando o pacote "microdatasus"

   # install.packages("remotes")
   # remotes::install_github("rfsaldanha/microdatasus")


###### Baixando os bancos através do pacote "microdatasus":

  #### Período: 2012 a 2020

### Visto que cada ano tem mais de 1 milhão de registros, tem que baixar 1 de cada vez e depois 
    ### juntar tudo num arquivo só:


##### Baixar SIH-RD de 2012 a 2017 e filtrar somente mulheres de 10 a 49 anos:

  ### Optei por baixar cada ano e filtrar logo para que não fique muito pesado na hora de juntar tudo em um arquivo só

## SEXO = 3 (feminino) & COD_IDADE = 4 (em anos)

## toupper transforma no nome das variáveis para letra maiuscula

# 2012:

sih_12 <- fetch_datasus(year_start = 2012, month_start = 1, year_end = 2012, month_end = 12, uf = "SP", information_system = "SIH-RD")

sih_12 <- sih_12 %>%
  filter(SEXO == 3 & COD_IDADE == 4 & IDADE >=10 & IDADE <= 49)


# 2013:

sih_13 <- fetch_datasus(year_start = 2013, month_start = 1, year_end = 2013, month_end = 12, uf = "SP", information_system = "SIH-RD")


sih_13 <- sih_13 %>%
  filter(SEXO == 3 & COD_IDADE == 4 & IDADE >=10 & IDADE <= 49)


# 2014:

sih_14 <- fetch_datasus(year_start = 2014, month_start = 1, year_end = 2014, month_end = 12, uf = "SP", information_system = "SIH-RD")

sih_14 <- sih_14 %>%
  filter(SEXO == 3 & COD_IDADE == 4 & IDADE >=10 & IDADE <= 49)


# 2015:

sih_15 <- fetch_datasus(year_start = 2015, month_start = 1, year_end = 2015, month_end = 12, uf = "SP", information_system = "SIH-RD")

sih_15 <- sih_15 %>%
  filter(SEXO == 3 & COD_IDADE == 4 & IDADE >=10 & IDADE <= 49)


# 2016:

sih_16 <- fetch_datasus(year_start = 2016, month_start = 1, year_end = 2016, month_end = 12, uf = "SP", information_system = "SIH-RD")

sih_16 <- sih_16 %>%
  filter(SEXO == 3 & COD_IDADE == 4 & IDADE >=10 & IDADE <= 49)


# 2017:

sih_17 <- fetch_datasus(year_start = 2017, month_start = 1, year_end = 2017, month_end = 12, uf = "SP", information_system = "SIH-RD")

sih_17 <- sih_17 %>%
  filter(SEXO == 3 & COD_IDADE == 4 & IDADE >=10 & IDADE <= 49)


# 2018:

sih_18 <- fetch_datasus(year_start = 2018, month_start = 1, year_end = 2018, month_end = 12, uf = "SP", information_system = "SIH-RD")

sih_18 <- sih_18 %>%
  filter(SEXO == 3 & COD_IDADE == 4 & IDADE >=10 & IDADE <= 49)


# 2019:

sih_19 <- fetch_datasus(year_start = 2019, month_start = 1, year_end = 2019, month_end = 12, uf = "SP", information_system = "SIH-RD")

sih_19 <- sih_19 %>%
  filter(SEXO == 3 & COD_IDADE == 4 & IDADE >=10 & IDADE <= 49)


# 2020:

sih_20 <- fetch_datasus(year_start = 2020, month_start = 1, year_end = 2020, month_end = 12, uf = "SP", information_system = "SIH-RD")

sih_20 <- sih_20 %>%
  filter(SEXO == 3 & COD_IDADE == 4 & IDADE >=10 & IDADE <= 49)


#### Erro de alocaçãoo de memória:

memory.limit (9999999999)

gc()

##### Juntando tudo em um banco ?nico:

dados <- rbindlist(list(sih_12, sih_13, sih_14, sih_15, sih_16, sih_17, sih_18, sih_19, sih_20), fill = T)


### Tirando o IDENT == 5

dados <- dados %>%
  filter(IDENT != 5)


## Removendo os bancos do enviroment:

rm(sih_12, sih_13, sih_14, sih_15, sih_16, sih_17, sih_18, sih_19, sih_20)


### Salvando a base do SIH-RD de 2012 a 2019 de mulheres de 10 a 49 anos sem IDENT == 5:

#write.csv2(dados, "SP_Base_MIF_2012_2019.csv", row.names = F) ## ir mudando a sigla da UF no nome do arquivo


########################################################################################################

################### FLAG DE PROCEDIMENTOS REALIZADOS


### Criando uma lista com os procedimentos de interesse

procedimentos <- as.character(c("0201010011",
                              "0211040010",
                              "0211040061",
                              "0310010012",
                              "0310010020",
                              "0310010039",
                              "0310010047",
                              "0310010055",
                              "0303100010",
                              "0303100028",
                              "0303100036",
                              "0303100044",
                              "0303100052",
                              "0409060011",
                              "0409060054",
                              "0409060070",
                              "0411010018",
                              "0411010026",
                              "0411010034",
                              "0411010042",
                              "0411010050",
                              "0411010069",
                              "0411010077",
                              "0411010085",
                              "0411020013",
                              "0411020021",
                              "0411020030",
                              "0411020048",
                              "0411020056",
                              "0417010028",
                              "0417010010",
                              "0417010036"))

### Se o procedimento de interesse estiver contido na coluna PROC_REA, recebe 1, caso contrário, 0:

dados$FLAG_PROC_REA <- ifelse(dados$PROC_REA %in% procedimentos, 1, 0)

## Percebi que as vezes o 0 do comando acima fica como NA, então tudo que for NA na coluna recebe 0

dados$FLAG_PROC_REA[is.na(dados$FLAG_PROC_REA)] <- 0


################### FLAG DE PARTO

### Criando uma lista com os procedimentos relacionados a parto

parto <- as.character(c("0310010012",
                      "0310010039",
                      "0310010047",
                      "0310010055",
                      "0411010026",
                      "0411010034",
                      "0411010042"))


### Se os procedimentos de parto estiverem contidos na coluna PROC_REA, recebem 1, caso contrário, 0:

dados$FLAG_PARTO <- ifelse(dados$PROC_REA %in% parto, 1, 0)

dados$FLAG_PARTO[is.na(dados$FLAG_PARTO)] <- 0


################### FLAG DE DIAGNÓSTICO

### Temos 11 variáveis de diagnóstico: DIAG_PRINC, DIAG_SECUN, DIAGSEC1, DIAGSEC2, DIAGSEC3, DIAGSEC4,
 ## DIAGSEC5, DIAGSEC6, DIAGSEC7, DIAGSEC8 e DIAGSEC9

## Transformando todas as variáveis de diagnóstico em character

dados <- dados %>%
  mutate(across(contains('DIAG'), ~ as.character(.)))

dados$CID_MORTE <- as.character(dados$CID_MORTE)

dados$CID_ASSO <- as.character(dados$CID_ASSO)

dados$CID_ASSO <- as.character(dados$CID_ASSO)

dados$CID_NOTIF <- as.character(dados$CID_NOTIF)

#### Se as colunas de diagnóstico tiverem algum diagnóstico que começa com O, recebe 1, caso contrário, 0.

### 11/06/2022 - Adicionando as variáveis CID_MORTE, CID_ASSO e CID_NOTIF no critério

dados$FLAG_DIAGNOSTICO <- ifelse(startsWith(dados$DIAG_PRINC, "O") | 
                                 startsWith(dados$DIAG_SECUN, "O") |
                                 startsWith(dados$DIAGSEC1, "O") |
                                 startsWith(dados$DIAGSEC2, "O") |
                                 startsWith(dados$DIAGSEC3, "O") |
                                 startsWith(dados$DIAGSEC4, "O") |
                                 startsWith(dados$DIAGSEC5, "O") |
                                 startsWith(dados$DIAGSEC6, "O") |
                                 startsWith(dados$DIAGSEC7, "O") |
                                 startsWith(dados$DIAGSEC8, "O") |
                                 startsWith(dados$DIAGSEC9, "O") |
                                 startsWith(dados$CID_MORTE, "O") |
                                 startsWith(dados$CID_ASSO, "O") |
                                 startsWith(dados$CID_NOTIF, "O"), 1, 0)

dados$FLAG_DIAGNOSTICO[is.na(dados$FLAG_DIAGNOSTICO)] <- 0


##### 18/06/2022 - Criando FLAG restrito do grupo O para as variáveis DIAG_PRINC e DIAG_SECUN

dados$FLAG_DIAGNOSTICO_REST <- ifelse(startsWith(dados$DIAG_PRINC, "O") | 
                                      startsWith(dados$DIAG_SECUN, "O"), 1, 0)

dados$FLAG_DIAGNOSTICO_REST[is.na(dados$FLAG_DIAGNOSTICO_REST)] <- 0


######### Salvando a base completa com os flags

write.csv2(dados, "SP_MIF_Todos_Campos_Sem_Cluster_2012_2020.csv", row.names = F) ## ir mudando a sigla da UF no nome do arquivo


####### Subset do banco somente com os registros que apresentam 1 nos flags "FLAG_PROC_REA" OU "FLAG_DIAGNOSTICO"

dados1 <- dados %>%
  filter(FLAG_PROC_REA == 1 | FLAG_DIAGNOSTICO == 1)


####### Salvando a base que apresentou 1 nos flags "FLAG_PROC_REA" OU "FLAG_DIAGNOSTICO"

write.csv2(dados1, "SP_OBS_Todos_Campos_Sem_Cluster_2012_2020.csv", row.names = F) ## ir mudando a sigla da UF no nome do arquivo


####### Subset do banco somente com os registros que apresentam 1 nos flags "FLAG_PROC_REA" OU "FLAG_DIAGNOSTICO_REST"

dados2 <- dados %>%
  filter(FLAG_PROC_REA == 1 | FLAG_DIAGNOSTICO_REST == 1)


####### Salvando a base que apresentou 1 nos flags "FLAG_PROC_REA" OU "FLAG_DIAGNOSTICO_REST"

write.csv2(dados2, "SP_OBS_RESTRITO_Todos_Campos_Sem_Cluster_2012_2020.csv", row.names = F) ## ir mudando a sigla da UF no nome do arquivo


##########################################################################################################

########### Referências:

## PARTO:

# 0310010012 ASSIST?NCIA AO PARTO SEM DISTOCIA
# 0310010039 PARTO NORMAL
# 0310010047 PARTO NORMAL EM GESTACAO DE ALTO RISCO 
# 0310010055 PARTO NORMAL EM CENTRO DE PARTO NORMAL  
# 0411010026 PARTO CESARIANO EM GESTACAO DE ALTO RISCO 
# 0411010034 PARTO CESARIANO
# 0411010042 PARTO CESARIANO C/ LAQUEADURA TUBARIA


## DIAGN?STICO:

# Todos os diagnósticos do grupo XV - causas obstétricas (Grupo "O" = O00 a O99)



## PROCEDIMENTO REALIZADO:

# Procedimentos removidos - 22/06/2022
# 0202031179 Teste não treponêmico p/ detecção de sífilis em gestantes
# 0205020143 Ultrassonografia obstétrica
# 0205010059 Ultrassonografia doppler de fluxo obstétrico
# 0205020151 Ultrassonografia obstétrica com doppler colorido e pulsado
# 0214010040 Teste rápido para detecção de HIV em gestante
# 0301010145 Primeira consulta de pediatria ao recém-nascido
# 0801010039 Incentivo ao parto - PHPN (componente I)
# 0801010047 Incentivo ao registro civil de nascimento
# 0802010032 Diária de acompanhante de gestante c/ pernoite
# 0205020186	Ultrassonografia transvaginal (Removido em 02.05.2022)
# 0211070149	Emissoes otoacusticas evocadas p/ triagem auditiva (teste da orelhinha) (Removido em 02.05.2022)
# 0409060046	Curetagem semi?tica com ou sem dilata??o do colo uterino (Removido em 04.02.2022)
# 0409060062	Dilata??o de colo de ?tero (Removido em 02.05.2022)
# 0409060160	Histerorrafia (Removido em 02.05.2022)
# 0603040012 	Administra??o de Cabergolina 0,5mg (Removidos em 02.05.2022)
# 0603030017	Imunoglobulina anti- (Removidos em 02.05.2022)
# 0303160055  Tratamento de transtornos relacionados com a dura??o da gesta??o e com o crescimento fetal (Adicionado em 04.02.2022 e REMOVIDO EM 02.05.2022)


### Procedimentos ok:

# 0201010011	Amniocentese
# 0211040010	Amnioscopia
# 0211040061	Tococardiografia anteparto
# 0310010012	Assist?ncia ao parto sem dist?cia
# 0310010020	Atendimento ao rec?m-nascido no momento do nascimento
# 0310010039	Parto normal
# 0310010047	Parto normal em gesta??o de alto risco
# 0310010055	Parto normal em centro de parto normal (CPN)
# 0303100010	Tratamento de complica??es relacionadas predominantemente ao puerp?rio
# 0303100028	Tratamento de ecl?mpsia
# 0303100036	Tratamento de edema, protein?ria e transtornos hipertensivos na gravidez, parto e puerp?rio.
# 0303100044	Tratamento de intercorr?ncias cl?nicas na gravidez
# 0303100052	Tratamento de mola hidatiforme
# 0409060011 	Cerclagem de colo do ?tero
# 0409060054	Curetagem uterina em mola hidatiforme
# 0409060070	Esvaziamento de ?tero p?s-aborto por aspira??o manual intra-uterina (AMIU)
# 0411010018	Descolamento manual de placenta
# 0411010026	Parto cesariano em gesta??o de alto risco
# 0411010034	Parto cesariano
# 0411010042	Parto cesariano com laqueadura tub?ria
# 0411010050	Redu??o manual de invers?o uterina aguda p?s-parto
# 0411010069	Ressutura de episiorrafia p?s-parto 
# 0411010077	Sutura de lacera??es de trajeto p?lvico (no parto antes da admiss?o)
# 0411010085	Tratamento cir?rgico de invers?o uterina aguda p?s-parto
# 0411020013	Curetagem pos-abortamento/puerperal
# 0411020021	Embriotomia
# 0411020030	Histerectomia puerperal
# 0411020048	Tratamento cir?rgico de gravidez ect?pica
# 0411020056	Tratamento de outros transtornos maternos relacionados predominantemente ? gravidez
# 0417010028	Analgesia Obste??trica p/ Parto Normal
# 0417010010	Anestesia Obste??trica p/ Cesariana
# 0417010036	Anestesia Obste??trica p/ Cesariana em Gestac??a~o de Alto Risco


