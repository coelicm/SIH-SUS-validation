library(ggplot2)
library(rio)
library(dplyr)
library(plyr)
library(gtsummary)
library(patchwork)
library(ggrepel)
library(sf)
library(stringr)
library(data.table)
library(janitor)
library(readr)
library(haven)
library(kableExtra)
library(tidyr)
library(stringr)
library(magrittr)


## Abrindo a base do MMG

mmg <- read_sav("Triagens_Prontuarios_Estudo_Validação_06_01_2023.sav")


#PADRONIZA CNES PARA 7 DIGITOS

mmg$CNES <- str_pad(mmg$CNES, 7, pad = "0") 


# planilha com os períodos de cada hospital

periodos <- import("Hospitais MMG - CNES e Período_30_09_2022.xlsx")

## corrigindo algumas datas que não abriram no formato certo no R

periodos$DT_INICIO <- ifelse(periodos$CNES == "2615746", "10/01/2022", 
                             ifelse(periodos$CNES == "2361787", "19/04/2021", 
                                    ifelse(periodos$CNES == "2448521", "27/05/2021", periodos$DT_INICIO)))

periodos$DT_FIM <- ifelse(periodos$CNES == "2615746", "10/02/2022", 
                          ifelse(periodos$CNES == "2361787", "18/05/2021", 
                                 ifelse(periodos$CNES == "2319454", "11/05/2021", 
                                        ifelse(periodos$CNES == "2448521", "25/06/2021", periodos$DT_FIM))))

# transformando de character para date

periodos$DT_INICIO <- as.Date(periodos$DT_INICIO, "%d/%m/%Y")

periodos$DT_FIM <- as.Date(periodos$DT_FIM, "%d/%m/%Y")


## join do CNES de periodos com a base mif

periodos$CNES <- as.character(periodos$CNES)

periodos <- periodos %>%
  select(CNES, REGIAO, TIPO_UNIDADE, DT_INICIO, DT_FIM)

mmg <- mmg %>%
  filter(CNES %in% periodos$CNES)

mmg <- left_join(mmg, periodos, by = "CNES")

mmg$DATA_INTER <- if_else(is.na(mmg$ft_dt_inter_triagem), mmg$pront_data_internação, mmg$ft_dt_inter_triagem)

mmg <- mmg %>% 
  filter(DATA_INTER >= DT_INICIO & DATA_INTER <= DT_FIM)


#### Ajeitando os labels dos prontuarios

names(mmg) <- tolower(names(mmg))

mmg$pront_motivo_internação <- as.character(mmg$pront_motivo_internação)

mmg$pront_tipo_saida_mulher <- as.character(mmg$pront_tipo_saida_mulher)

summary.factor(mmg$pront_motivo_internação)

mmg <- mmg %>%
  mutate(pront_motivo_internação = dplyr::recode(pront_motivo_internação, 
                                                 "4" = "Aborto",
                                                 "1" = "Parto", 
                                                 "2" = "Parto", 
                                                 "3" = "Parto",
                                                 "5" = "Complicações na gestação",
                                                 "6" = "Complicações no puerpério"),
         pront_tipo_saida_mulher = dplyr::recode(pront_tipo_saida_mulher,
                                                 "1" = "Alta",
                                                 "2" = "Alta",
                                                 "5" = "Transferencia",
                                                 "3" = "Morte",
                                                 "4" = "Permanencia"))

mmg$motivo_internacao <- ifelse(mmg$ft_coletada == 0, mmg$pront_motivo_internação, mmg$motivo_internacao)

mmg$tipo_alta_mulher <- ifelse(mmg$ft_coletada == 0, mmg$pront_tipo_saida_mulher, mmg$tipo_alta_mulher)


mmg <- mmg %>%
  mutate(motivo_internacao = dplyr::recode(motivo_internacao, 
                                           "1" = "Parto",
                                           "2" = "Aborto", 
                                           "3" = "Complicações na gestação",
                                           "4" = "Complicações no puerpério",
                                           "5" = "Complicações no puerpério",
                                           "6" = "Complicações no puerpério"),
         tipo_alta_mulher = dplyr::recode(tipo_alta_mulher,
                                          "-1" = "Permanencia",
                                          "-2" = "Permanencia",
                                          "1" = "Alta",
                                          "2" = "Alta",
                                          "3" = "Transferencia",
                                          "4" = "Morte",
                                          "5" = "Permanencia"))


mmg$motivo_internacao <- ifelse(is.na(mmg$motivo_internacao), mmg$pront_motivo_internação, mmg$motivo_internacao)

mmg$tipo_alta_mulher <- ifelse(is.na(mmg$tipo_alta_mulher), mmg$pront_tipo_saida_mulher, mmg$tipo_alta_mulher)


rm(periodos)

cod_transfusao <- c("10831018",
                    "27630654",
                    "27930489",
                    "30530940",
                    "45030208",
                    "19730965",
                    "15730405",
                    "19330079",
                    "52730270",
                    "17530333",
                    "49430032",
                    "44230087",
                    "10230347",
                    "45030066",
                    "52330092",
                    "51730379",
                    "19730351",
                    "11130241",
                    "43630323",
                    "45030041")

mmg$cod_unico <- as.character(mmg$cod_unico)

mmg$mmg_transfusao <- ifelse(mmg$cod_unico %in% cod_transfusao, 1, mmg$mmg_transfusao)


cod_mmg <- c("52730270",
             "49430032",
             "44230087",
             "10230347",
             "45030066",
             "52330092",
             "51730379",
             "19730351",
             "11130241",
             "43630323",
             "45030041")

mmg$mmg_total <- ifelse(mmg$cod_unico %in% cod_mmg, 1, mmg$mmg_total)


### correções em registros 25/03/23


#código único 43730246 (tem critério hellp)

mmg$mmg_hellp <- ifelse(mmg$cod_unico == "43730246", 1, mmg$mmg_hellp)


#código único 52530047 (tem critério TMP>7 dias)

mmg$mmg_internprol <- ifelse(mmg$cod_unico == "52530047", 1, mmg$mmg_internprol)


#código único 45130353 (tem critério pré-eclampsia grave)

mmg$mmg_pegrave <- ifelse(mmg$cod_unico == "45130353", 1, mmg$mmg_pegrave)


#### Padrozinando para 0 e 1

mmg <- mmg %>%
  mutate_at(c(27:148), ~replace(., is.na(.), 0))


### Pegando apenas os registros com prontuário coletado MMG

mmg <- mmg %>%
  filter(pronturio_coletado == 1 & ft_morbidade == 1)


## ajeitando as datas

mmg$pront_data_internação <- as.Date(mmg$pront_data_internação, "%d/%m/%Y")

mmg$pront_data_saida <- as.Date(mmg$pront_data_saida, "%d/%m/%Y")

mmg$pront_dt_nasc <- as.Date(mmg$pront_dt_nasc, "%d/%m/%Y")


##########################################################################################################

### Base SIH

dados <- fread("Base OBS para Linkage - Novos Critérios_24.06.23.csv")


dados$NASC <- as.Date(dados$NASC, "%d/%m/%Y")

dados$DT_INTER <- as.Date(dados$DT_INTER, "%d/%m/%Y")

dados$DT_SAIDA <- as.Date(dados$DT_SAIDA, "%d/%m/%Y")

### Primeira AIH

dados <- dados %>% group_by(grupoaih) %>% filter(DT_INTER==min(DT_INTER))

###################################################################################################################

### Variáveis para usar base SIH

# NASC, DT_INTER, DT_SAIDA, CEP

### Variáveis para usar base MMG

# pront_dt_nasc, pront_data_internação, pront_dt_saida, pront_cep

#### Arrumando as variáveis

## Se o CEP for NA, recebe 0

mmg$pront_cep <- ifelse(is.na(mmg$pront_cep), 0, mmg$pront_cep)

dados$CEP <- ifelse(is.na(dados$CEP), 0, dados$CEP)

# cep com 8 digitos

dados$CEP <- str_pad(dados$CEP, 8, pad = "0")

mmg$pront_cep <- str_pad(mmg$pront_cep, 8, pad = "0")


##### Criando as chaves SIH

# Chave 1 - NASC + DT_INTER + DT_SAIDA + CEP + CNES

dados$chave1 <- paste0(dados$NASC, dados$DT_INTER, dados$DT_SAIDA, dados$CEP, dados$CNES)

dados <- dados[!duplicated(dados$chave1), ]

# Chave 2 - NASC + DT_INTER + DT_SAIDA + CNES

dados$chave2 <- paste0(dados$NASC, dados$DT_INTER, dados$DT_SAIDA, dados$CNES)

# Chave 3 - NASC + DT_INTER + CNES + CEP

dados$chave3 <- paste0(dados$NASC, dados$DT_INTER, dados$CNES, dados$CEP)

# Chave 4 - NASC + DT_INTER + CNES 

dados$chave4 <- paste0(dados$NASC, dados$DT_INTER, dados$CNES)

# Chave 5 - NASC +  DT_SAIDA + CNES 

dados$chave5 <- paste0(dados$NASC, dados$DT_SAIDA, dados$CNES)

##### Criando as chaves MMG

# Chave 1 - pront_dt_nasc + pront_data_internação + pront_data_saida + pront_cep + cnes

mmg$chave1 <- paste0(mmg$pront_dt_nasc, mmg$pront_data_internação, mmg$pront_data_saida, mmg$pront_cep, mmg$cnes)

# Chave 2 - pront_dt_nasc + pront_data_internação + pront_data_saida + cnes

mmg$chave2 <- paste0(mmg$pront_dt_nasc, mmg$pront_data_internação, mmg$pront_data_internação, mmg$cnes)

# Chave 3 - pront_dt_nasc + pront_data_internação + cnes + pront_cep

mmg$chave3 <- paste0(mmg$pront_dt_nasc, mmg$pront_data_internação, mmg$cnes, mmg$pront_cep)

# Chave 4 - pront_dt_nasc + pront_data_internação + cnes

mmg$chave4 <- paste0(mmg$pront_dt_nasc, mmg$pront_data_internação, mmg$cnes)

# Chave 5 - pront_dt_nasc + pront_data_saida + cnes

mmg$chave5 <- paste0(mmg$pront_dt_nasc, mmg$pront_data_saida, mmg$cnes)

#########################################################################################################

### Join chave 1

found1 <- inner_join(mmg, dados, by = "chave1")

notfound1 <- anti_join(mmg, dados, by = "chave1")

found1$NUM_CHAVE <- "1"

### Joinc chave 2

found2 <- inner_join(notfound1, dados, by = "chave2")

notfound2 <- anti_join(notfound1, dados, by = "chave2")

found2$NUM_CHAVE <- "2"

### Join chave 3

found3 <- inner_join(notfound2, dados, by = "chave3")

notfound3 <- anti_join(notfound2, dados, by = "chave3")

found3$NUM_CHAVE <- "3"


### Join chave 4

found4 <- inner_join(notfound3, dados, by = "chave4")

notfound4 <- anti_join(notfound3, dados, by = "chave4")

found4$NUM_CHAVE <- "4"


### Join chave 5

found5 <- inner_join(notfound4, dados, by = "chave5")

notfound5 <- anti_join(notfound4, dados, by = "chave5")

found5$NUM_CHAVE <- "5"


####### Juntando os achados em um arquivo só

found1 <- found1 %>% select(-starts_with("chave"))
found2 <- found2 %>% select(-starts_with("chave"))
found3 <- found3 %>% select(-starts_with("chave"))
found4 <- found4 %>% select(-starts_with("chave"))
found5 <- found5 %>% select(-starts_with("chave"))

found_all <- rbind(found1, found2, found3, found4, found5)


### Registros não encontrados:

notfound_all <- anti_join(mmg, found_all, by = "cod_unico") # 926

notfound_all <- notfound_all %>% select(-starts_with("chave"))


### AIHS duplicadas

duplicadas <- found_all %>%
  group_by(N_AIH) %>%
  tally %>%
  filter(n > 1)

# 15 AIHS duplicadas

# BASE PARA REVISÃO MANUAL

revisao <- found_all %>%
  filter(N_AIH %in% duplicadas$N_AIH)

write.csv2(revisao, "Base Revisão Manual.csv")


#### Abrindo base com a revisao manual feita

revisao <- import("Base Revisão Manual.xlsx")

### filtrando os não pares

nao_pares <- revisao %>%
  filter(MATCH == "NÃO PAR")

# removendo da base found_all

found_all$REMOVE <- ifelse(found_all$cod_unico %in% nao_pares$cod_unico, 1, 0)


### Pegando os não pares e colocando na base dos não achados

nao_pares <- found_all %>%
  filter(REMOVE == 1)

nao_pares <- nao_pares[,1:153]

notfound_all <- rbind(notfound_all, nao_pares) # 941


### Base final achados

found_all <- found_all %>%
  filter(REMOVE == 0) ### 4.652


### Deixando apenas o banco dos encontrados

rm(aih, cod_unico, dados, duplicadas, found1, found2, found3, found4, found5,
   mmg, nao_pares, notfound_all, notfound1, notfound2, notfound3, notfound4, notfound5, revisao)


found_all <- found_all %>%
  select(-REMOVE)


### padronizando os NA para 0

found_all <- found_all %>%
  mutate_at(c(225:231, 236, 377:450), ~replace(., is.na(.), 0))


## ajeitando as variaveis do atoprof

found_all <- found_all %>%
  mutate_at(c(283:376), ~replace(., is.na(.), 0))


found_all <- found_all %>%
  mutate_at(c(283:376), ~replace(., .=="Sim", 1))


## salva base dos achados

write_excel_csv2(found_all, "Linkage MMG x SIH - encontrados_24.06.23.csv")

