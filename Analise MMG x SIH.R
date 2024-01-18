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


setwd("C:/Users/Lana Meijinhos/Desktop/Validação Gates")


# Abrindo a base do MMG e arrumando a base (filtro só 2021:


mmg <- read_sav("MMG_finalizados_para_validação_sem_não elegíveis.sav")

ms <- read_sav("Triagens_MS_19_09.sav")

ms[] <- lapply(ms[], as.character)

mmg[] <- lapply(mmg[], as.character)

mmg  <- bind_rows(mmg, ms)

limpar_base_mmg <- function(mmg){
  
  # Colocando todas as variáveis em uppercase
  
  names(mmg) <- toupper(names(mmg))
  
  # Removendo 2022
  
  mmg_para <- mmg %>%
    filter(CNES == "2615746")
  
  mmg_2021 <- mmg %>%
    filter(!grepl("2022", DATA_INTERNACAO_MULHER))
  
  mmg <- bind_rows(mmg_2021, mmg_para)
  
  ## Criando variável mês de internação/ano internação
  
  mmg$DATA_INTERNACAO_MULHER <- as.Date(mmg$DATA_INTERNACAO_MULHER)
  
  mmg$MES_INTERNACAO <- month(mmg$DATA_INTERNACAO_MULHER)
  
  mmg$ANO_INTERNACAO <- year(mmg$DATA_INTERNACAO_MULHER)
  
  mmg$MES_INTERNACAO <- paste0(mmg$MES_INTERNACAO, "/", mmg$ANO_INTERNACAO)
  
  
  ## Complicações
  
  # mmg$TEVE_COMPLICACAO <- rowSums(mmg[ ,c("COMPLICAÇÃO_HEMORRAGICA", "COMPLICAÇÃO_HIPERTENSIVA", "OUTRAS_COMPLICAÇÕES",
  #                                         "COMPLICAÇÃO_CIRURGICA")])
  # 
  # mmg$TEVE_COMPLICACAO <- ifelse(mmg$TEVE_COMPLICACAO == 0, "Não", "Sim")
  # 
  
  mmg$TEMPO_INTERNACAO_MULHER <- as.numeric(mmg$TEMPO_INTERNACAO_MULHER)
  
  mmg$TEMPO_PERMANENCIA_MAIOR_7 <- ifelse(mmg$TEMPO_INTERNACAO_MULHER > 7, "Sim", "Não")
  
  mmg$TEMPO_PERMANENCIA_MAIOR_7 <- ifelse(is.na(mmg$DATA_ALTA_MULHER), "Sim", mmg$TEMPO_PERMANENCIA_MAIOR_7)
  
  
  
  ## Recodificando os labels
  
  #mmg <- mmg %>% 
  # mutate(MOTIVO_INTERNACAO = as.character(MOTIVO_INTERNACAO),
  #       TIPO_ALTA_MULHER = as.character(TIPO_ALTA_MULHER),
  #      TEMPO_PERMANENCIA_MAIOR_7 = as.character(TEMPO_PERMANENCIA_MAIOR_7),
  #     UTI = as.character(UTI),
  #    FT_Q3 = as.character(FT_Q3),
  #   FT_Q34_2 = as.character(FT_Q34_2),
  #  FT_Q34 = as.character(FT_Q34),
  # TRANSFUSÃO = as.character(TRANSFUSÃO),
  # MMG = as.character(MMG))
  
  mmg <- mmg %>% 
    mutate(across(everything(), as.character))
  
  mmg <- mmg %>%
    mutate(MOTIVO_INTERNACAO = dplyr::recode(MOTIVO_INTERNACAO, 
                                             "1" = "Parto",
                                             "2" = "Aborto", 
                                             "3" = "Complicações na gestação",
                                             "4" = "Complicações no puerpério",
                                             "5" = "Complicações no puerpério",
                                             "6" = "Complicações no puerpério"),
           TIPO_ALTA_MULHER = dplyr::recode(TIPO_ALTA_MULHER,
                                            "-1" = "Alta",
                                            "-2" = "Alta",
                                            "1" = "Alta",
                                            "2" = "Alta",
                                            "3" = "Transferencia",
                                            "4" = "Morte",
                                            "5" = "Permanencia"),
           COMPLICAÇÃO_HEMORRAGICA = dplyr::recode(COMPLICAÇÃO_HEMORRAGICA,
                                                   "1" = "Sim",
                                                   "0" = "Não"),
           COMPLICAÇÃO_HIPERTENSIVA = dplyr::recode(COMPLICAÇÃO_HIPERTENSIVA,
                                                    "1" = "Sim",
                                                    "0" = "Não"),
           COMPLICAÇÃO_CIRURGICA = dplyr::recode(COMPLICAÇÃO_CIRURGICA,
                                                 "1" = "Sim",
                                                 "0" = "Não"),
           OUTRAS_COMPLICAÇÕES = dplyr::recode(OUTRAS_COMPLICAÇÕES,
                                               "1" = "Sim",
                                               "0" = "Não"),
           PROCEDIMENTO_RISCO = dplyr::recode(PROCEDIMENTO_RISCO,
                                              "1" = "Sim",
                                              "0" = "Não"),
           TEMPO_PERMANENCIA_MAIOR_7 = dplyr::recode(TEMPO_PERMANENCIA_MAIOR_7,
                                                     "1" = "Sim",
                                                     "0" = "Não"),
           UTI = dplyr::recode(UTI,
                               "1" = "Sim",
                               "0" = "Não"),
           FT_Q3 = dplyr::recode(FT_Q3,
                                 "1" = "Sim",
                                 "0" = "Não"),
           FT_Q34_2 = dplyr::recode(FT_Q34_2,
                                    "1" = "Sim",
                                    "0" = "Não"),
           FT_Q34 = dplyr::recode(FT_Q34,
                                  "1" = "Sim",
                                  "0" = "Não"),
           FT_Q7 = dplyr::recode(FT_Q7,
                                 "1" = "Sim",
                                 "0" = "Não"),
           FT_Q5 = dplyr::recode(FT_Q5,
                                 "1" = "Sim",
                                 "0" = "Não"),
           FT_Q25 = dplyr::recode(FT_Q25,
                                 "1" = "Sim",
                                 "0" = "Não"),
           FT_Q6_A = dplyr::recode(FT_Q6_A,
                                   "1" = "Sim",
                                   "0" = "Não"),
           FT_Q6_B = dplyr::recode(FT_Q6_B,
                                   "1" = "Sim",
                                   "0" = "Não"),
           FT_Q6_C = dplyr::recode(FT_Q6_C,
                                   "1" = "Sim",
                                   "0" = "Não"),
           FT_Q6_D = dplyr::recode(FT_Q6_D,
                                   "1" = "Sim",
                                   "0" = "Não"),
           FT_Q6_E = dplyr::recode(FT_Q6_E,
                                   "1" = "Sim",
                                   "0" = "Não"),
           FT_Q6_F = dplyr::recode(FT_Q6_F,
                                   "1" = "Sim",
                                   "0" = "Não"),
           FT_Q8 = dplyr::recode(FT_Q8,
                                 "1" = "Sim",
                                 "0" = "Não"),
           FT_Q9 = dplyr::recode(FT_Q9,
                                 "1" = "Sim",
                                 "0" = "Não"),
           FT_Q19 = dplyr::recode(FT_Q9,
                                 "1" = "Sim",
                                 "0" = "Não"),
           FT_Q10 = dplyr::recode(FT_Q10,
                                  "1" = "Sim",
                                  "0" = "Não"),
           FT_Q12 = dplyr::recode(FT_Q12,
                                  "1" = "Sim",
                                  "0" = "Não"),
           FT_Q13 = dplyr::recode(FT_Q13,
                                  "1" = "Sim",
                                  "0" = "Não"),
           FT_Q14 = dplyr::recode(FT_Q14,
                                  "1" = "Sim",
                                  "0" = "Não"),
           FT_Q17 = dplyr::recode(FT_Q17,
                                  "1" = "Sim",
                                  "0" = "Não"),
           FT_Q16 = dplyr::recode(FT_Q16,
                                  "1" = "Sim",
                                  "0" = "Não"),
           FT_Q33 = dplyr::recode(FT_Q33,
                                  "1" = "Sim",
                                  "0" = "Não"),
           FT_Q18 = dplyr::recode(FT_Q18,
                                  "1" = "Sim",
                                  "0" = "Não"),
           FT_Q21 = dplyr::recode(FT_Q21,
                                  "1" = "Sim",
                                  "0" = "Não"),
           FT_Q22 = dplyr::recode(FT_Q22,
                                  "1" = "Sim",
                                  "0" = "Não"),
           FT_Q23 = dplyr::recode(FT_Q23,
                                  "1" = "Sim",
                                  "0" = "Não"),
           FT_Q20 = dplyr::recode(FT_Q20,
                                  "1" = "Sim",
                                  "0" = "Não"),
           FT_Q26 = dplyr::recode(FT_Q26,
                                  "1" = "Sim",
                                  "0" = "Não"),
           FT_Q24 = dplyr::recode(FT_Q24,
                                  "1" = "Sim",
                                  "0" = "Não"),
           FT_Q27 = dplyr::recode(FT_Q27,
                                  "1" = "Sim",
                                  "0" = "Não"),
           FT_Q1 = dplyr::recode(FT_Q1,
                                 "1" = "Sim",
                                 "0" = "Não"),
           FT_Q2 = dplyr::recode(FT_Q2,
                                 "1" = "Sim",
                                 "0" = "Não"),
           FT_Q32 = dplyr::recode(FT_Q32,
                                 "1" = "Sim",
                                 "0" = "Não"),
           FT_Q29 = dplyr::recode(FT_Q29,
                                  "1" = "Sim",
                                  "0" = "Não"),
           FT_Q4 = dplyr::recode(FT_Q4,
                                 "1" = "Sim",
                                 "0" = "Não"),
           FT_Q15 = dplyr::recode(FT_Q15,
                                 "1" = "Sim",
                                 "0" = "Não"),
           FT_Q3 = dplyr::recode(FT_Q3,
                                 "1" = "Sim",
                                 "0" = "Não"),
           FT_Q35 = dplyr::recode(FT_Q35,
                                 "1" = "Sim",
                                 "0" = "Não"),
           FT_Q11 = dplyr::recode(FT_Q11,
                                  "1" = "Sim",
                                  "0" = "Não"),
           FT_Q11_2 = dplyr::recode(FT_Q11_2,
                                    "1" = "Sim",
                                    "0" = "Não"),
           TRANSFUSÃO = dplyr::recode(TRANSFUSÃO,
                                      "1" = "Sim",
                                      "0" = "Não"),
           MMG = dplyr::recode(MMG,
                               "1" = "Sim",
                               "0" = "Não"))
  
  mmg$MOTIVO_INTERNACAO[is.na(mmg$MOTIVO_INTERNACAO)] <- "Em branco"
  
  mmg$TIPO_ALTA_MULHER[is.na(mmg$TIPO_ALTA_MULHER)] <- "Em branco"
  
  mmg$UTI[is.na(mmg$UTI)] <- "Não"
  
  mmg$FT_Q3[is.na(mmg$FT_Q3)] <- "Não"
  
  mmg$FT_Q34_2[is.na(mmg$FT_Q34_2)] <- "Não"
  
  mmg$FT_Q34[is.na(mmg$FT_Q34)] <- "Não"
  
  mmg$TRANSFUSÃO[is.na(mmg$TRANSFUSÃO)] <- "Não"
  
  mmg$MMG[is.na(mmg$MMG)] <- "Não"
  
  names(mmg)
  
  summary.factor(mmg$MMG)
  
  mmg$MMG <- ifelse(mmg$TEMPO_PERMANENCIA_MAIOR_7 == "Sim" | mmg$UTI == "Sim", "Sim", mmg$MMG)
  
  
  mmg[, 28:67][is.na(mmg[, 28:67])] <- "Não"
  
  
  mmg$HEMORRAGIA_POS <- ifelse(mmg$FT_Q5 == "Sim" |
                                 mmg$FT_Q6_A == "Sim" |
                                 mmg$FT_Q6_B == "Sim" |
                                 mmg$FT_Q6_C == "Sim" |
                                 mmg$FT_Q6_D == "Sim" |
                                 mmg$FT_Q6_E == "Sim", "Sim", "Não")
  
  mmg$CNES <- str_pad(mmg$CNES, 7, pad = "0")
  
  return(mmg)
}

mmg <- limpar_base_mmg(mmg)


## Removendo as unidades que não vao participar

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


mmg <- mmg %>% 
  filter(DATA_INTERNACAO_MULHER >= DT_INICIO & DATA_INTERNACAO_MULHER <= DT_FIM)

rm(ms, periodos)

summary.factor(mmg$MMG)


## Lendo base SIH

sih <- fread("OBS_ESTUDO_VALIDACAO_COM_CRITERIOS_CPAV_NMM_FILTRADA_PERIODOS2.csv") 


#############################################################################################################

## Tabela - TOTAL INTERNAÇÕES


total_internacoes_tab <- function(sih, mmg){
  
primeira_AIH <- setDT(sih)[order(grupoaih, as.IDate(DT_INTER, "%m/%d/%Y"))][!duplicated(grupoaih)]
  

# Região Norte

norte_sih <- primeira_AIH %>%
  filter(REGIAO == "Região Norte") %>%
  group_by(TIPO_UNIDADE) %>%
  tally %>%
  adorn_totals("row")


norte_mmg <- mmg %>%
  filter(REGIAO == "Região Norte") %>%
  group_by(TIPO_UNIDADE) %>%
  tally %>%
  adorn_totals("row")


total_internacoes_norte <- full_join(norte_mmg, norte_sih, by = "TIPO_UNIDADE")

colnames(total_internacoes_norte) <- c("Tipo Unidade", "MMG", "SIH")


# Região Nordeste

nordeste_sih <- primeira_AIH %>%
  filter(REGIAO == "Região Nordeste") %>%
  group_by(TIPO_UNIDADE) %>%
  tally %>%
  adorn_totals("row")


nordeste_mmg <- mmg %>%
  filter(REGIAO == "Região Nordeste") %>%
  group_by(TIPO_UNIDADE) %>%
  tally %>%
  adorn_totals("row")


total_internacoes_nordeste <- full_join(nordeste_mmg, nordeste_sih, by = "TIPO_UNIDADE")

colnames(total_internacoes_nordeste) <- c("Tipo Unidade", "MMG", "SIH")


# Região Sul

sul_sih <- primeira_AIH %>%
  filter(REGIAO == "Região Sul") %>%
  group_by(TIPO_UNIDADE) %>%
  tally %>%
  adorn_totals("row")


sul_mmg <- mmg %>%
  filter(REGIAO == "Região Sul") %>%
  group_by(TIPO_UNIDADE) %>%
  tally %>%
  adorn_totals("row")


total_internacoes_sul <- full_join(sul_mmg, sul_sih, by = "TIPO_UNIDADE")

colnames(total_internacoes_sul) <- c("Tipo Unidade", "MMG", "SIH")


# Região Sudeste

sudeste_sih <- primeira_AIH %>%
  filter(REGIAO == "Região Sudeste") %>%
  group_by(TIPO_UNIDADE) %>%
  tally %>%
  adorn_totals("row")


sudeste_mmg <- mmg %>%
  filter(REGIAO == "Região Sudeste") %>%
  group_by(TIPO_UNIDADE) %>%
  tally %>%
  adorn_totals("row")


total_internacoes_sudeste <- full_join(sudeste_mmg, sudeste_sih, by = "TIPO_UNIDADE")

colnames(total_internacoes_sudeste) <- c("Tipo Unidade", "MMG", "SIH")


# Região Centro-Oeste

centro_oeste_sih <- primeira_AIH %>%
  filter(REGIAO == "Região Centro-Oeste") %>%
  group_by(TIPO_UNIDADE) %>%
  tally %>%
  adorn_totals("row")


centro_oeste_mmg <- mmg %>%
  filter(REGIAO == "Região Centro-Oeste") %>%
  group_by(TIPO_UNIDADE) %>%
  tally %>%
  adorn_totals("row")


total_internacoes_centro_oeste <- full_join(centro_oeste_mmg, centro_oeste_sih, by = "TIPO_UNIDADE")

colnames(total_internacoes_centro_oeste) <- c("Tipo Unidade", "MMG", "SIH")


# Brasil

br_sih <- primeira_AIH %>%
  group_by(TIPO_UNIDADE) %>%
  tally %>%
  adorn_totals("row")

br_mmg <- mmg %>%
  group_by(TIPO_UNIDADE) %>%
  tally %>%
  adorn_totals("row")


total_internacoes_br <- full_join(br_mmg, br_sih, by = "TIPO_UNIDADE")

colnames(total_internacoes_br) <- c("Tipo Unidade", "MMG", "SIH")


# Tabela 

tabelao_internacoes <- rbind(total_internacoes_norte, total_internacoes_nordeste, total_internacoes_sul,
                             total_internacoes_sudeste, total_internacoes_centro_oeste, total_internacoes_br)

tabela_internacoes <- kbl(tabelao_internacoes, align = "lcc") %>%
  kable_classic(full_width = F) %>%
  pack_rows("Região Norte", 1, 3) %>%
  pack_rows("Região Nordeste", 4, 6) %>%
  pack_rows("Região Sul", 7, 9) %>%
  pack_rows("Região Sudeste", 10, 12) %>%
  pack_rows("Região Centro-Oeste", 13, 15) %>%
  pack_rows("Brasil", 16, 18)

return(tabela_internacoes)
}

total_internacoes_tab(sih, mmg)


### Motivo da internação

motivo_internacao_tab <- function(sih, mmg){
  
  
  primeira_AIH <- setDT(sih)[order(grupoaih, as.IDate(DT_INTER, "%m/%d/%Y"))][!duplicated(grupoaih)]
  
  ##### Região Norte
  
  # parto
  
  
  parto_norte_sih <- primeira_AIH %>%
    filter(REGIAO == "Região Norte" & MOTIVO_INTERNACAO == "Parto") %>%
    group_by(TIPO_UNIDADE) %>%
    tally %>%
    adorn_totals("row")
  
  
  parto_norte_mmg <- mmg %>%
    filter(REGIAO == "Região Norte" & MOTIVO_INTERNACAO == "Parto") %>%
    group_by(TIPO_UNIDADE) %>%
    tally %>%
    adorn_totals("row")
  
  parto_norte <- full_join(parto_norte_mmg, parto_norte_sih, by = "TIPO_UNIDADE")
  
  colnames(parto_norte) <- c("Tipo Unidade", "MMG", "SIH")
  
  
  # Aborto
  
  aborto_norte_sih <- primeira_AIH %>%
    filter(REGIAO == "Região Norte" & MOTIVO_INTERNACAO == "Aborto") %>%
    group_by(TIPO_UNIDADE) %>%
    tally %>%
    adorn_totals("row")
  
  
  aborto_norte_mmg <- mmg %>%
    filter(REGIAO == "Região Norte" & MOTIVO_INTERNACAO == "Aborto") %>%
    group_by(TIPO_UNIDADE) %>%
    tally %>%
    adorn_totals("row")
  
  aborto_norte <- full_join(aborto_norte_mmg, aborto_norte_sih, by = "TIPO_UNIDADE")
  
  aborto_norte[is.na(aborto_norte)] <- 0
  
  colnames(aborto_norte) <- c("Tipo Unidade", "MMG", "SIH")
  
  
  # Complicações na gestação
  
  comp_gest_norte_sih <- primeira_AIH %>%
    filter(REGIAO == "Região Norte" & MOTIVO_INTERNACAO == "Complicações na gestação") %>%
    group_by(TIPO_UNIDADE) %>%
    tally %>%
    adorn_totals("row")
  
  
  comp_gest_norte_mmg <- mmg %>%
    filter(REGIAO == "Região Norte" & MOTIVO_INTERNACAO == "Complicações na gestação") %>%
    group_by(TIPO_UNIDADE) %>%
    tally %>%
    adorn_totals("row")
  
  comp_gest_norte <- full_join(comp_gest_norte_mmg, comp_gest_norte_sih, by = "TIPO_UNIDADE")
  
  comp_gest_norte[is.na(comp_gest_norte)] <- 0
  
  colnames(comp_gest_norte) <- c("Tipo Unidade", "MMG", "SIH")
  
  
  # Complicações no puerpério
  
  comp_puerp_norte_sih <- primeira_AIH %>%
    filter(REGIAO == "Região Norte" & MOTIVO_INTERNACAO == "Complicações no puerpério") %>%
    group_by(TIPO_UNIDADE) %>%
    tally %>%
    adorn_totals("row")
  
  
  comp_puerp_norte_mmg <- mmg %>%
    filter(REGIAO == "Região Norte" & MOTIVO_INTERNACAO == "Complicações no puerpério") %>%
    group_by(TIPO_UNIDADE) %>%
    tally %>%
    adorn_totals("row")
  
  comp_puerp_norte <- full_join(comp_puerp_norte_mmg, comp_puerp_norte_sih, by = "TIPO_UNIDADE")
  
  colnames(comp_puerp_norte) <- c("TIPO_UNIDADE", "MMG", "SIH")
  
  data <- data.frame(TIPO_UNIDADE = c("Unidades Privadas"),
                     MMG = c(0),
                     SIH = c(0))
  
  
  comp_puerp_norte <- rbind(data, comp_puerp_norte)
  
  comp_puerp_norte[is.na(comp_puerp_norte)] <- 0
  
  colnames(comp_puerp_norte) <- c("Tipo Unidade", "MMG", "SIH")
  
  
  # outros
  
  outros_norte_sih <- primeira_AIH %>%
    filter(REGIAO == "Região Norte" & MOTIVO_INTERNACAO == "Outros") %>%
    group_by(TIPO_UNIDADE) %>%
    tally %>%
    adorn_totals("row")
  
  
  outros_norte_mmg <- mmg %>%
    filter(REGIAO == "Região Norte" & MOTIVO_INTERNACAO == "Outros") %>%
    group_by(TIPO_UNIDADE) %>%
    tally %>%
    adorn_totals("row")
  
  outros_norte <- full_join(outros_norte_mmg, outros_norte_sih, by = "TIPO_UNIDADE")
  
  outros_norte[is.na(outros_norte)] <- 0
  
  colnames(outros_norte) <- c("Tipo Unidade", "MMG", "SIH")
  
  # Em branco
  
  branco_norte_sih <- primeira_AIH %>%
    filter(REGIAO == "Região Norte" & MOTIVO_INTERNACAO == "Em branco") %>%
    group_by(TIPO_UNIDADE) %>%
    tally %>%
    adorn_totals("row")
  
  
  branco_norte_mmg <- mmg %>%
    filter(REGIAO == "Região Norte" & MOTIVO_INTERNACAO == "Em branco") %>%
    group_by(TIPO_UNIDADE) %>%
    tally %>%
    adorn_totals("row")
  
  branco_norte <- full_join(branco_norte_mmg, branco_norte_sih, by = "TIPO_UNIDADE")
  
  branco_norte[is.na(branco_norte)] <- 0
  
  colnames(branco_norte) <- c("Tipo Unidade", "MMG", "SIH")
  
  motivo_norte <- parto_norte %>%
    full_join(aborto_norte, by = "Tipo Unidade") %>%
    full_join(comp_gest_norte, by = "Tipo Unidade") %>%
    full_join(comp_puerp_norte, by = "Tipo Unidade") %>%
    full_join(outros_norte, by = "Tipo Unidade") %>%
    full_join(branco_norte, by = "Tipo Unidade")
  
  colnames(motivo_norte) <- c("Tipo Unidade", "MMG", "SIH", "MMG", "SIH", "MMG", "SIH", "MMG", "SIH",
                              "MMG", "SIH", "MMG", "SIH")
  
  
  ##### Região Nordeste
  
  # parto
  
  
  parto_nordeste_sih <- primeira_AIH %>%
    filter(REGIAO == "Região Nordeste" & MOTIVO_INTERNACAO == "Parto") %>%
    group_by(TIPO_UNIDADE) %>%
    tally %>%
    adorn_totals("row")
  
  
  parto_nordeste_mmg <- mmg %>%
    filter(REGIAO == "Região Nordeste" & MOTIVO_INTERNACAO == "Parto") %>%
    group_by(TIPO_UNIDADE) %>%
    tally %>%
    adorn_totals("row")
  
  parto_nordeste <- full_join(parto_nordeste_mmg, parto_nordeste_sih, by = "TIPO_UNIDADE")
  
  colnames(parto_nordeste) <- c("Tipo Unidade", "MMG", "SIH")
  
  
  # Aborto
  
  aborto_nordeste_sih <- primeira_AIH %>%
    filter(REGIAO == "Região Nordeste" & MOTIVO_INTERNACAO == "Aborto") %>%
    group_by(TIPO_UNIDADE) %>%
    tally %>%
    adorn_totals("row")
  
  
  aborto_nordeste_mmg <- mmg %>%
    filter(REGIAO == "Região Nordeste" & MOTIVO_INTERNACAO == "Aborto") %>%
    group_by(TIPO_UNIDADE) %>%
    tally %>%
    adorn_totals("row")
  
  aborto_nordeste <- full_join(aborto_nordeste_mmg, aborto_nordeste_sih, by = "TIPO_UNIDADE")
  
  aborto_nordeste[is.na(aborto_nordeste)] <- 0
  
  colnames(aborto_nordeste) <- c("Tipo Unidade", "MMG", "SIH")
  
  
  # Complicações na gestação
  
  comp_gest_nordeste_sih <- primeira_AIH %>%
    filter(REGIAO == "Região Nordeste" & MOTIVO_INTERNACAO == "Complicações na gestação") %>%
    group_by(TIPO_UNIDADE) %>%
    tally %>%
    adorn_totals("row")
  
  
  comp_gest_nordeste_mmg <- mmg %>%
    filter(REGIAO == "Região Nordeste" & MOTIVO_INTERNACAO == "Complicações na gestação") %>%
    group_by(TIPO_UNIDADE) %>%
    tally %>%
    adorn_totals("row")
  
  comp_gest_nordeste <- full_join(comp_gest_nordeste_mmg, comp_gest_nordeste_sih, by = "TIPO_UNIDADE")
  
  comp_gest_nordeste[is.na(comp_gest_nordeste)] <- 0
  
  colnames(comp_gest_nordeste) <- c("Tipo Unidade", "MMG", "SIH")
  
  
  # Complicações no puerpério
  
  comp_puerp_nordeste_sih <- primeira_AIH %>%
    filter(REGIAO == "Região Nordeste" & MOTIVO_INTERNACAO == "Complicações no puerpério") %>%
    group_by(TIPO_UNIDADE) %>%
    tally %>%
    adorn_totals("row")
  
  
  comp_puerp_nordeste_mmg <- mmg %>%
    filter(REGIAO == "Região Nordeste" & MOTIVO_INTERNACAO == "Complicações no puerpério") %>%
    group_by(TIPO_UNIDADE) %>%
    tally %>%
    adorn_totals("row")
  
  comp_puerp_nordeste <- full_join(comp_puerp_nordeste_mmg, comp_puerp_nordeste_sih, by = "TIPO_UNIDADE")
  
  comp_puerp_nordeste[is.na(comp_puerp_nordeste)] <- 0
  
  colnames(comp_puerp_nordeste) <- c("Tipo Unidade", "MMG", "SIH")
  
  
  # outros
  
  outros_nordeste_sih <- primeira_AIH %>%
    filter(REGIAO == "Região Nordeste" & MOTIVO_INTERNACAO == "Outros") %>%
    group_by(TIPO_UNIDADE) %>%
    tally %>%
    adorn_totals("row")
  
  
  outros_nordeste_mmg <- mmg %>%
    filter(REGIAO == "Região Nordeste" & MOTIVO_INTERNACAO == "Outros") %>%
    group_by(TIPO_UNIDADE) %>%
    tally %>%
    adorn_totals("row")
  
  outros_nordeste <- full_join(outros_nordeste_mmg, outros_nordeste_sih, by = "TIPO_UNIDADE")
  
  outros_nordeste[is.na(outros_nordeste)] <- 0
  
  colnames(outros_nordeste) <- c("Tipo Unidade", "MMG", "SIH")
  
  # Em branco
  
  branco_nordeste_sih <- primeira_AIH %>%
    filter(REGIAO == "Região Nordeste" & MOTIVO_INTERNACAO == "Em branco") %>%
    group_by(TIPO_UNIDADE) %>%
    tally %>%
    adorn_totals("row")
  
  
  branco_nordeste_mmg <- mmg %>%
    filter(REGIAO == "Região Nordeste" & MOTIVO_INTERNACAO == "Em branco") %>%
    group_by(TIPO_UNIDADE) %>%
    tally %>%
    adorn_totals("row")
  
  branco_nordeste <- full_join(branco_nordeste_mmg, branco_nordeste_sih, by = "TIPO_UNIDADE")
  
  branco_nordeste[is.na(branco_nordeste)] <- 0
  
  colnames(branco_nordeste) <- c("Tipo Unidade", "MMG", "SIH")
  
  motivo_nordeste <- parto_nordeste %>%
    full_join(aborto_nordeste, by = "Tipo Unidade") %>%
    full_join(comp_gest_nordeste, by = "Tipo Unidade") %>%
    full_join(comp_puerp_nordeste, by = "Tipo Unidade") %>%
    full_join(outros_nordeste, by = "Tipo Unidade") %>%
    full_join(branco_nordeste, by = "Tipo Unidade")
  
  motivo_nordeste[is.na(motivo_nordeste)] <- 0
  
  colnames(motivo_nordeste) <- c("Tipo Unidade", "MMG", "SIH", "MMG", "SIH", "MMG", "SIH", "MMG", "SIH",
                                 "MMG", "SIH", "MMG", "SIH")
  
  
  ##### Região Sul
  
  # parto
  
  
  parto_sul_sih <- primeira_AIH %>%
    filter(REGIAO == "Região Sul" & MOTIVO_INTERNACAO == "Parto") %>%
    group_by(TIPO_UNIDADE) %>%
    tally %>%
    adorn_totals("row")
  
  
  parto_sul_mmg <- mmg %>%
    filter(REGIAO == "Região Sul" & MOTIVO_INTERNACAO == "Parto") %>%
    group_by(TIPO_UNIDADE) %>%
    tally %>%
    adorn_totals("row")
  
  parto_sul <- full_join(parto_sul_mmg, parto_sul_sih, by = "TIPO_UNIDADE")
  
  colnames(parto_sul) <- c("Tipo Unidade", "MMG", "SIH")
  
  
  # Aborto
  
  aborto_sul_sih <- primeira_AIH %>%
    filter(REGIAO == "Região Sul" & MOTIVO_INTERNACAO == "Aborto") %>%
    group_by(TIPO_UNIDADE) %>%
    tally %>%
    adorn_totals("row")
  
  
  aborto_sul_mmg <- mmg %>%
    filter(REGIAO == "Região Sul" & MOTIVO_INTERNACAO == "Aborto") %>%
    group_by(TIPO_UNIDADE) %>%
    tally %>%
    adorn_totals("row")
  
  aborto_sul <- full_join(aborto_sul_mmg, aborto_sul_sih, by = "TIPO_UNIDADE")
  
  aborto_sul[is.na(aborto_sul)] <- 0
  
  colnames(aborto_sul) <- c("Tipo Unidade", "MMG", "SIH")
  
  
  # Complicações na gestação
  
  comp_gest_sul_sih <- primeira_AIH %>%
    filter(REGIAO == "Região Sul" & MOTIVO_INTERNACAO == "Complicações na gestação") %>%
    group_by(TIPO_UNIDADE) %>%
    tally %>%
    adorn_totals("row")
  
  
  comp_gest_sul_mmg <- mmg %>%
    filter(REGIAO == "Região Sul" & MOTIVO_INTERNACAO == "Complicações na gestação") %>%
    group_by(TIPO_UNIDADE) %>%
    tally %>%
    adorn_totals("row")
  
  comp_gest_sul <- full_join(comp_gest_sul_mmg, comp_gest_sul_sih, by = "TIPO_UNIDADE")
  
  comp_gest_sul[is.na(comp_gest_sul)] <- 0
  
  colnames(comp_gest_sul) <- c("Tipo Unidade", "MMG", "SIH")
  
  
  # Complicações no puerpério
  
  comp_puerp_sul_sih <- primeira_AIH %>%
    filter(REGIAO == "Região Sul" & MOTIVO_INTERNACAO == "Complicações no puerpério") %>%
    group_by(TIPO_UNIDADE) %>%
    tally %>%
    adorn_totals("row")
  
  
  comp_puerp_sul_mmg <- mmg %>%
    filter(REGIAO == "Região Sul" & MOTIVO_INTERNACAO == "Complicações no puerpério") %>%
    group_by(TIPO_UNIDADE) %>%
    tally %>%
    adorn_totals("row")
  
  comp_puerp_sul <- full_join(comp_puerp_sul_mmg, comp_puerp_sul_sih, by = "TIPO_UNIDADE")
  
  comp_puerp_sul[is.na(comp_puerp_sul)] <- 0
  
  colnames(comp_puerp_sul) <- c("Tipo Unidade", "MMG", "SIH")
  
  
  # outros
  
  outros_sul_sih <- primeira_AIH %>%
    filter(REGIAO == "Região Sul" & MOTIVO_INTERNACAO == "Outros") %>%
    group_by(TIPO_UNIDADE) %>%
    tally %>%
    adorn_totals("row")
  
  
  outros_sul_mmg <- mmg %>%
    filter(REGIAO == "Região Sul" & MOTIVO_INTERNACAO == "Outros") %>%
    group_by(TIPO_UNIDADE) %>%
    tally %>%
    adorn_totals("row")
  
  outros_sul <- full_join(outros_sul_mmg, outros_sul_sih, by = "TIPO_UNIDADE")
  
  outros_sul[is.na(outros_sul)] <- 0
  
  colnames(outros_sul) <- c("Tipo Unidade", "MMG", "SIH")
  
  # Em branco
  
  branco_sul_sih <- primeira_AIH %>%
    filter(REGIAO == "Região Sul" & MOTIVO_INTERNACAO == "Em branco") %>%
    group_by(TIPO_UNIDADE) %>%
    tally %>%
    adorn_totals("row")
  
  
  branco_sul_mmg <- mmg %>%
    filter(REGIAO == "Região Sul" & MOTIVO_INTERNACAO == "Em branco") %>%
    group_by(TIPO_UNIDADE) %>%
    tally %>%
    adorn_totals("row")
  
  branco_sul <- full_join(branco_sul_mmg, branco_sul_sih, by = "TIPO_UNIDADE")
  
  colnames(branco_sul) <- c("TIPO_UNIDADE", "MMG", "SIH")
  
  data <- data.frame(TIPO_UNIDADE = c("Unidades Públicas"),
                     MMG = c(0),
                     SIH = c(0))
  
  
  branco_sul <- rbind(branco_sul, data)
  
  
  branco_sul[is.na(branco_sul)] <- 0
  
  colnames(branco_sul) <- c("Tipo Unidade", "MMG", "SIH")
  
  motivo_sul <- parto_sul %>%
    full_join(aborto_sul, by = "Tipo Unidade") %>%
    full_join(comp_gest_sul, by = "Tipo Unidade") %>%
    full_join(comp_puerp_sul, by = "Tipo Unidade") %>%
    full_join(outros_sul, by = "Tipo Unidade") %>%
    full_join(branco_sul, by = "Tipo Unidade")
  
  colnames(motivo_sul) <- c("Tipo Unidade", "MMG", "SIH", "MMG", "SIH", "MMG", "SIH", "MMG", "SIH",
                            "MMG", "SIH", "MMG", "SIH")
  
  
  
  ##### Região Sudeste
  
  # parto
  
  
  parto_sudeste_sih <- primeira_AIH %>%
    filter(REGIAO == "Região Sudeste" & MOTIVO_INTERNACAO == "Parto") %>%
    group_by(TIPO_UNIDADE) %>%
    tally %>%
    adorn_totals("row")
  
  
  parto_sudeste_mmg <- mmg %>%
    filter(REGIAO == "Região Sudeste" & MOTIVO_INTERNACAO == "Parto") %>%
    group_by(TIPO_UNIDADE) %>%
    tally %>%
    adorn_totals("row")
  
  parto_sudeste <- full_join(parto_sudeste_mmg, parto_sudeste_sih, by = "TIPO_UNIDADE")
  
  colnames(parto_sudeste) <- c("Tipo Unidade", "MMG", "SIH")
  
  
  # Aborto
  
  aborto_sudeste_sih <- primeira_AIH %>%
    filter(REGIAO == "Região Sudeste" & MOTIVO_INTERNACAO == "Aborto") %>%
    group_by(TIPO_UNIDADE) %>%
    tally %>%
    adorn_totals("row")
  
  
  aborto_sudeste_mmg <- mmg %>%
    filter(REGIAO == "Região Sudeste" & MOTIVO_INTERNACAO == "Aborto") %>%
    group_by(TIPO_UNIDADE) %>%
    tally %>%
    adorn_totals("row")
  
  aborto_sudeste <- full_join(aborto_sudeste_mmg, aborto_sudeste_sih, by = "TIPO_UNIDADE")
  
  aborto_sudeste[is.na(aborto_sudeste)] <- 0
  
  colnames(aborto_sudeste) <- c("Tipo Unidade", "MMG", "SIH")
  
  
  # Complicações na gestação
  
  comp_gest_sudeste_sih <- primeira_AIH %>%
    filter(REGIAO == "Região Sudeste" & MOTIVO_INTERNACAO == "Complicações na gestação") %>%
    group_by(TIPO_UNIDADE) %>%
    tally %>%
    adorn_totals("row")
  
  
  comp_gest_sudeste_mmg <- mmg %>%
    filter(REGIAO == "Região Sudeste" & MOTIVO_INTERNACAO == "Complicações na gestação") %>%
    group_by(TIPO_UNIDADE) %>%
    tally %>%
    adorn_totals("row")
  
  comp_gest_sudeste <- full_join(comp_gest_sudeste_mmg, comp_gest_sudeste_sih, by = "TIPO_UNIDADE")
  
  comp_gest_sudeste[is.na(comp_gest_sudeste)] <- 0
  
  colnames(comp_gest_sudeste) <- c("Tipo Unidade", "MMG", "SIH")
  
  
  # Complicações no puerpério
  
  comp_puerp_sudeste_sih <- primeira_AIH %>%
    filter(REGIAO == "Região Sudeste" & MOTIVO_INTERNACAO == "Complicações no puerpério") %>%
    group_by(TIPO_UNIDADE) %>%
    tally %>%
    adorn_totals("row")
  
  
  comp_puerp_sudeste_mmg <- mmg %>%
    filter(REGIAO == "Região Sudeste" & MOTIVO_INTERNACAO == "Complicações no puerpério") %>%
    group_by(TIPO_UNIDADE) %>%
    tally %>%
    adorn_totals("row")
  
  comp_puerp_sudeste <- full_join(comp_puerp_sudeste_mmg, comp_puerp_sudeste_sih, by = "TIPO_UNIDADE")
  
  comp_puerp_sudeste[is.na(comp_puerp_sudeste)] <- 0
  
  colnames(comp_puerp_sudeste) <- c("Tipo Unidade", "MMG", "SIH")
  
  
  # outros
  
  outros_sudeste_sih <- primeira_AIH %>%
    filter(REGIAO == "Região Sudeste" & MOTIVO_INTERNACAO == "Outros") %>%
    group_by(TIPO_UNIDADE) %>%
    tally %>%
    adorn_totals("row")
  
  
  outros_sudeste_mmg <- mmg %>%
    filter(REGIAO == "Região Sudeste" & MOTIVO_INTERNACAO == "Outros") %>%
    group_by(TIPO_UNIDADE) %>%
    tally %>%
    adorn_totals("row")
  
  outros_sudeste <- full_join(outros_sudeste_mmg, outros_sudeste_sih, by = "TIPO_UNIDADE")
  
  outros_sudeste[is.na(outros_sudeste)] <- 0
  
  colnames(outros_sudeste) <- c("Tipo Unidade", "MMG", "SIH")
  
  # Em branco
  
  branco_sudeste_sih <- primeira_AIH %>%
    filter(REGIAO == "Região Sudeste" & MOTIVO_INTERNACAO == "Em branco") %>%
    group_by(TIPO_UNIDADE) %>%
    tally %>%
    adorn_totals("row")
  
  
  branco_sudeste_mmg <- mmg %>%
    filter(REGIAO == "Região Sudeste" & MOTIVO_INTERNACAO == "Em branco") %>%
    group_by(TIPO_UNIDADE) %>%
    tally %>%
    adorn_totals("row")
  
  branco_sudeste <- full_join(branco_sudeste_mmg, branco_sudeste_sih, by = "TIPO_UNIDADE")
  
  branco_sudeste[is.na(branco_sudeste)] <- 0
  
  colnames(branco_sudeste) <- c("Tipo Unidade", "MMG", "SIH")
  
  motivo_sudeste <- parto_sudeste %>%
    full_join(aborto_sudeste, by = "Tipo Unidade") %>%
    full_join(comp_gest_sudeste, by = "Tipo Unidade") %>%
    full_join(comp_puerp_sudeste, by = "Tipo Unidade") %>%
    full_join(outros_sudeste, by = "Tipo Unidade") %>%
    full_join(branco_sudeste, by = "Tipo Unidade")
  
  colnames(motivo_sudeste) <- c("Tipo Unidade", "MMG", "SIH", "MMG", "SIH", "MMG", "SIH", "MMG", "SIH",
                                "MMG", "SIH", "MMG", "SIH")
  
  
  
  
  ##### Região Centro-Oeste
  
  # parto
  
  
  parto_centro_oeste_sih <- primeira_AIH %>%
    filter(REGIAO == "Região Centro-Oeste" & MOTIVO_INTERNACAO == "Parto") %>%
    group_by(TIPO_UNIDADE) %>%
    tally %>%
    adorn_totals("row")
  
  
  parto_centro_oeste_mmg <- mmg %>%
    filter(REGIAO == "Região Centro-Oeste" & MOTIVO_INTERNACAO == "Parto") %>%
    group_by(TIPO_UNIDADE) %>%
    tally %>%
    adorn_totals("row")
  
  parto_centro_oeste <- full_join(parto_centro_oeste_mmg, parto_centro_oeste_sih, by = "TIPO_UNIDADE")
  
  colnames(parto_centro_oeste) <- c("Tipo Unidade", "MMG", "SIH")
  

  # Aborto
  
  aborto_centro_oeste_sih <- primeira_AIH %>%
    filter(REGIAO == "Região Centro-Oeste" & MOTIVO_INTERNACAO == "Aborto") %>%
    group_by(TIPO_UNIDADE) %>%
    tally %>%
    adorn_totals("row")
  
  
  aborto_centro_oeste_mmg <- mmg %>%
    filter(REGIAO == "Região Centro-Oeste" & MOTIVO_INTERNACAO == "Aborto") %>%
    group_by(TIPO_UNIDADE) %>%
    tally %>%
    adorn_totals("row")
  
  aborto_centro_oeste <- full_join(aborto_centro_oeste_mmg, aborto_centro_oeste_sih, by = "TIPO_UNIDADE")
  
  aborto_centro_oeste[is.na(aborto_centro_oeste)] <- 0
  
  colnames(aborto_centro_oeste) <- c("Tipo Unidade", "MMG", "SIH")
  
   
  # Complicações na gestação
  
  comp_gest_centro_oeste_sih <- primeira_AIH %>%
    filter(REGIAO == "Região Centro-Oeste" & MOTIVO_INTERNACAO == "Complicações na gestação") %>%
    group_by(TIPO_UNIDADE) %>%
    tally %>%
    adorn_totals("row")
  
  
  comp_gest_centro_oeste_mmg <- mmg %>%
    filter(REGIAO == "Região Centro-Oeste" & MOTIVO_INTERNACAO == "Complicações na gestação") %>%
    group_by(TIPO_UNIDADE) %>%
    tally %>%
    adorn_totals("row")
  
  comp_gest_centro_oeste <- full_join(comp_gest_centro_oeste_mmg, comp_gest_centro_oeste_sih, by = "TIPO_UNIDADE")
  
  comp_gest_centro_oeste[is.na(comp_gest_centro_oeste)] <- 0
  
    colnames(comp_gest_centro_oeste) <- c("Tipo Unidade", "MMG", "SIH")
  
  
  # Complicações no puerpério
  
  comp_puerp_centro_oeste_sih <- primeira_AIH %>%
    filter(REGIAO == "Região Centro-Oeste" & MOTIVO_INTERNACAO == "Complicações no puerpério") %>%
    group_by(TIPO_UNIDADE) %>%
    tally %>%
    adorn_totals("row")
  
  
  comp_puerp_centro_oeste_mmg <- mmg %>%
    filter(REGIAO == "Região Centro-Oeste" & MOTIVO_INTERNACAO == "Complicações no puerpério") %>%
    group_by(TIPO_UNIDADE) %>%
    tally %>%
    adorn_totals("row")
  
  comp_puerp_centro_oeste <- full_join(comp_puerp_centro_oeste_mmg, comp_puerp_centro_oeste_sih, by = "TIPO_UNIDADE")
  
  comp_puerp_centro_oeste[is.na(comp_puerp_centro_oeste)] <- 0
  
    colnames(comp_puerp_centro_oeste) <- c("Tipo Unidade", "MMG", "SIH")
  
  
  # outros
  
  outros_centro_oeste_sih <- primeira_AIH %>%
    filter(REGIAO == "Região Centro-Oeste" & MOTIVO_INTERNACAO == "Outros") %>%
    group_by(TIPO_UNIDADE) %>%
    tally %>%
    adorn_totals("row")
  
  
  outros_centro_oeste_mmg <- mmg %>%
    filter(REGIAO == "Região Centro-Oeste" & MOTIVO_INTERNACAO == "Outros") %>%
    group_by(TIPO_UNIDADE) %>%
    tally %>%
    adorn_totals("row")
  
  outros_centro_oeste <- full_join(outros_centro_oeste_mmg, outros_centro_oeste_sih, by = "TIPO_UNIDADE")
  
    outros_centro_oeste[is.na(outros_centro_oeste)] <- 0
  
  colnames(outros_centro_oeste) <- c("Tipo Unidade", "MMG", "SIH")
  
  # Em branco
  
  branco_centro_oeste_sih <- primeira_AIH %>%
    filter(REGIAO == "Região Centro-Oeste" & MOTIVO_INTERNACAO == "Em branco") %>%
    group_by(TIPO_UNIDADE) %>%
    tally %>%
    adorn_totals("row")
  
  
  branco_centro_oeste_mmg <- mmg %>%
    filter(REGIAO == "Região Centro-Oeste" & MOTIVO_INTERNACAO == "Em branco") %>%
    group_by(TIPO_UNIDADE) %>%
    tally %>%
    adorn_totals("row")
  
  branco_centro_oeste <- full_join(branco_centro_oeste_mmg, branco_centro_oeste_sih, by = "TIPO_UNIDADE")
  
  branco_centro_oeste[is.na(branco_centro_oeste)] <- 0
  
   colnames(branco_centro_oeste) <- c("Tipo Unidade", "MMG", "SIH")
  
  
  motivo_centro_oeste <- parto_centro_oeste %>%
    full_join(aborto_centro_oeste, by = "Tipo Unidade") %>%
    full_join(comp_gest_centro_oeste, by = "Tipo Unidade") %>%
    full_join(comp_puerp_centro_oeste, by = "Tipo Unidade") %>%
    full_join(outros_centro_oeste, by = "Tipo Unidade") %>%
    full_join(branco_centro_oeste, by = "Tipo Unidade")
  
  colnames(motivo_centro_oeste) <- c("Tipo Unidade", "MMG", "SIH", "MMG", "SIH", "MMG", "SIH", "MMG", "SIH",
                                     "MMG", "SIH", "MMG", "SIH")
  
  
  motivo_centro_oeste[is.na(motivo_centro_oeste)] <- 0
  
  
  ##### Região Brasil
  
  # parto
  
  
  parto_br_sih <- primeira_AIH %>%
    filter( MOTIVO_INTERNACAO == "Parto") %>%
    group_by(TIPO_UNIDADE) %>%
    tally %>%
    adorn_totals("row")
  
  
  parto_br_mmg <- mmg %>%
    filter( MOTIVO_INTERNACAO == "Parto") %>%
    group_by(TIPO_UNIDADE) %>%
    tally %>%
    adorn_totals("row")
  
  parto_br <- full_join(parto_br_mmg, parto_br_sih, by = "TIPO_UNIDADE")
  
  colnames(parto_br) <- c("Tipo Unidade", "MMG", "SIH")
  
  
  # Aborto
  
  aborto_br_sih <- primeira_AIH %>%
    filter( MOTIVO_INTERNACAO == "Aborto") %>%
    group_by(TIPO_UNIDADE) %>%
    tally %>%
    adorn_totals("row")
  
  
  aborto_br_mmg <- mmg %>%
    filter( MOTIVO_INTERNACAO == "Aborto") %>%
    group_by(TIPO_UNIDADE) %>%
    tally %>%
    adorn_totals("row")
  
  aborto_br <- full_join(aborto_br_mmg, aborto_br_sih, by = "TIPO_UNIDADE")
  
  aborto_br[is.na(aborto_br)] <- 0
  
  colnames(aborto_br) <- c("Tipo Unidade", "MMG", "SIH")
  
  
  # Complicações na gestação
  
  comp_gest_br_sih <- primeira_AIH %>%
    filter( MOTIVO_INTERNACAO == "Complicações na gestação") %>%
    group_by(TIPO_UNIDADE) %>%
    tally %>%
    adorn_totals("row")
  
  
  comp_gest_br_mmg <- mmg %>%
    filter( MOTIVO_INTERNACAO == "Complicações na gestação") %>%
    group_by(TIPO_UNIDADE) %>%
    tally %>%
    adorn_totals("row")
  
  comp_gest_br <- full_join(comp_gest_br_mmg, comp_gest_br_sih, by = "TIPO_UNIDADE")
  
  comp_gest_br[is.na(comp_gest_br)] <- 0
  
  colnames(comp_gest_br) <- c("Tipo Unidade", "MMG", "SIH")
  
  
  # Complicações no puerpério
  
  comp_puerp_br_sih <- primeira_AIH %>%
    filter( MOTIVO_INTERNACAO == "Complicações no puerpério") %>%
    group_by(TIPO_UNIDADE) %>%
    tally %>%
    adorn_totals("row")
  
  
  comp_puerp_br_mmg <- mmg %>%
    filter( MOTIVO_INTERNACAO == "Complicações no puerpério") %>%
    group_by(TIPO_UNIDADE) %>%
    tally %>%
    adorn_totals("row")
  
  comp_puerp_br <- full_join(comp_puerp_br_mmg, comp_puerp_br_sih, by = "TIPO_UNIDADE")
  
  comp_puerp_br[is.na(comp_puerp_br)] <- 0
  
  colnames(comp_puerp_br) <- c("Tipo Unidade", "MMG", "SIH")
  
  
  # outros
  
  outros_br_sih <- primeira_AIH %>%
    filter( MOTIVO_INTERNACAO == "Outros") %>%
    group_by(TIPO_UNIDADE) %>%
    tally %>%
    adorn_totals("row")
  
  
  outros_br_mmg <- mmg %>%
    filter( MOTIVO_INTERNACAO == "Outros") %>%
    group_by(TIPO_UNIDADE) %>%
    tally %>%
    adorn_totals("row")
  
  outros_br <- full_join(outros_br_mmg, outros_br_sih, by = "TIPO_UNIDADE")
  
  outros_br[is.na(outros_br)] <- 0
  
  colnames(outros_br) <- c("Tipo Unidade", "MMG", "SIH")
  
  # Em branco
  
  branco_br_sih <- primeira_AIH %>%
    filter( MOTIVO_INTERNACAO == "Em branco") %>%
    group_by(TIPO_UNIDADE) %>%
    tally %>%
    adorn_totals("row")
  
  
  branco_br_mmg <- mmg %>%
    filter( MOTIVO_INTERNACAO == "Em branco") %>%
    group_by(TIPO_UNIDADE) %>%
    tally %>%
    adorn_totals("row")
  
  branco_br <- full_join(branco_br_mmg, branco_br_sih, by = "TIPO_UNIDADE")
  
  branco_br[is.na(branco_br)] <- 0
  
  colnames(branco_br) <- c("Tipo Unidade", "MMG", "SIH")
  
  motivo_br <- parto_br %>%
    full_join(aborto_br, by = "Tipo Unidade") %>%
    full_join(comp_gest_br, by = "Tipo Unidade") %>%
    full_join(comp_puerp_br, by = "Tipo Unidade") %>%
    full_join(outros_br, by = "Tipo Unidade") %>%
    full_join(branco_br, by = "Tipo Unidade")
  
  colnames(motivo_br) <- c("Tipo Unidade", "MMG", "SIH", "MMG", "SIH", "MMG", "SIH", "MMG", "SIH",
                           "MMG", "SIH", "MMG", "SIH")
  
  
  tabelao_motivo_inter <- rbind(motivo_norte, motivo_nordeste, motivo_sul,
                                motivo_sudeste, motivo_centro_oeste, motivo_br)
  
  tabela_motivo_inter <- kbl(tabelao_motivo_inter, align = "lcccccccccccc") %>%
    add_header_above(c(" " = 1, "Parto" = 2, "Aborto" = 2, "Inter. Gestação" = 2,
                       "Inter. Puerpério" = 2, "Outros" = 2, "Em Branco" = 2)) %>%
    kable_classic(full_width = F) %>%
    pack_rows("Região Norte", 1, 3) %>%
    pack_rows("Região Nordeste", 4, 6) %>%
    pack_rows("Região Sul", 7, 9) %>%
    pack_rows("Região Sudeste", 10, 12) %>%
    pack_rows("Região Centro-Oeste", 13, 15) %>%
    pack_rows("Brasil", 16, 18)
  
  return(tabela_motivo_inter)
}

motivo_internacao_tab(sih, mmg)


####### Tabela - Tipo de Alta

tipo_alta_tab <- function(sih, mmg){
  
  sih <- sih %>%
    group_by(grupoaih) %>%
    filter(DT_SAIDA == max(DT_SAIDA)) %>%
    ungroup()
  
  ##### Região Norte
  
  colnames(mmg)[12] <- "ALTA"
  
  # administrativa
  
  summary.factor(sih$ALTA)
  
  administrativa_norte_sih <- sih %>%
    filter(REGIAO == "Região Norte" & ALTA == "Administrativa") %>%
    group_by(TIPO_UNIDADE) %>%
    tally %>%
    adorn_totals("row")
  
  
  administrativa_norte_mmg <- mmg %>%
    filter(REGIAO == "Região Norte" & ALTA == "Administrativa") %>%
    group_by(TIPO_UNIDADE) %>%
    tally %>%
    adorn_totals("row")
  
  administrativa_norte <- full_join(administrativa_norte_mmg, administrativa_norte_sih, by = "TIPO_UNIDADE")
  
  colnames(administrativa_norte) <- c("TIPO_UNIDADE", "MMG", "SIH")
  
  data <- data.frame(TIPO_UNIDADE = c("Unidades Privadas"),
                     MMG = c(0),
                     SIH = c(0))
  
  
  administrativa_norte <- rbind(data, administrativa_norte)
  
  colnames(administrativa_norte) <- c("Tipo Unidade", "MMG", "SIH")
  
  administrativa_norte[is.na(administrativa_norte)] <- 0
  
  
  # Aborto
  
  alta_norte_sih <- sih %>%
    filter(REGIAO == "Região Norte" & ALTA == "Alta") %>%
    group_by(TIPO_UNIDADE) %>%
    tally %>%
    adorn_totals("row")
  
  
  alta_norte_mmg <- mmg %>%
    filter(REGIAO == "Região Norte" & ALTA == "Alta") %>%
    group_by(TIPO_UNIDADE) %>%
    tally %>%
    adorn_totals("row")
  
  alta_norte <- full_join(alta_norte_mmg, alta_norte_sih, by = "TIPO_UNIDADE")
  
  alta_norte[is.na(alta_norte)] <- 0
  
  colnames(alta_norte) <- c("Tipo Unidade", "MMG", "SIH")
  
  
  # Complicações na gestação
  
  morte_norte_sih <- sih %>%
    filter(REGIAO == "Região Norte" & ALTA == "Morte") %>%
    group_by(TIPO_UNIDADE) %>%
    tally %>%
    adorn_totals("row")
  
  
  morte_norte_mmg <- mmg %>%
    filter(REGIAO == "Região Norte" & ALTA == "Morte") %>%
    group_by(TIPO_UNIDADE) %>%
    tally %>%
    adorn_totals("row")
  
  morte_norte <- full_join(morte_norte_mmg, morte_norte_sih, by = "TIPO_UNIDADE")
  
  colnames(morte_norte) <- c("TIPO_UNIDADE", "MMG", "SIH")
  
  data <- data.frame(TIPO_UNIDADE = c("Unidades Privadas"),
                     MMG = c(0),
                     SIH = c(0))
  
  
  morte_norte <- rbind(data, morte_norte)
  
  morte_norte[is.na(morte_norte)] <- 0
  
  colnames(morte_norte) <- c("Tipo Unidade", "MMG", "SIH")
  
  
  # Complicações no puerpério
  
  permanencia_norte_sih <- sih %>%
    filter(REGIAO == "Região Norte" & ALTA == "Permanencia") %>%
    group_by(TIPO_UNIDADE) %>%
    tally %>%
    adorn_totals("row")
  
  
  permanencia_norte_mmg <- mmg %>%
    filter(REGIAO == "Região Norte" & ALTA == "Permanencia") %>%
    group_by(TIPO_UNIDADE) %>%
    tally %>%
    adorn_totals("row")
  
  permanencia_norte <- full_join(permanencia_norte_mmg, permanencia_norte_sih, by = "TIPO_UNIDADE")
  
  colnames(permanencia_norte) <- c("TIPO_UNIDADE", "MMG", "SIH")
  
  data <- data.frame(TIPO_UNIDADE = c("Unidades Privadas"),
                     MMG = c(0),
                     SIH = c(0))
  
  
  permanencia_norte <- rbind(data, permanencia_norte)
  
  permanencia_norte[is.na(permanencia_norte)] <- 0
  
  colnames(permanencia_norte) <- c("Tipo Unidade", "MMG", "SIH")
  
  
  # outros
  
  transferencia_norte_sih <- sih %>%
    filter(REGIAO == "Região Norte" & ALTA == "Transferencia") %>%
    group_by(TIPO_UNIDADE) %>%
    tally %>%
    adorn_totals("row")
  
  
  transferencia_norte_mmg <- mmg %>%
    filter(REGIAO == "Região Norte" & ALTA == "Transferencia") %>%
    group_by(TIPO_UNIDADE) %>%
    tally %>%
    adorn_totals("row")
  
  transferencia_norte <- full_join(transferencia_norte_mmg, transferencia_norte_sih, by = "TIPO_UNIDADE")
  
  transferencia_norte[is.na(transferencia_norte)] <- 0
  
  colnames(transferencia_norte) <- c("Tipo Unidade", "MMG", "SIH")
  
  
  motivo_alta_norte <- alta_norte %>%
    full_join(administrativa_norte, by = "Tipo Unidade") %>%
    full_join(permanencia_norte, by = "Tipo Unidade") %>%
    full_join(morte_norte, by = "Tipo Unidade") %>%
    full_join(transferencia_norte, by = "Tipo Unidade")
  
  colnames(motivo_alta_norte) <- c("Tipo Unidade", "MMG", "SIH", "MMG", "SIH", "MMG", "SIH", "MMG", "SIH",
                                   "MMG", "SIH")
  
  
  ##### Região Nordeste
  
  # administrativa
  
  
  administrativa_nordeste_sih <- sih %>%
    filter(REGIAO == "Região Nordeste" & ALTA == "Administrativa") %>%
    group_by(TIPO_UNIDADE) %>%
    tally %>%
    adorn_totals("row")
  
  
  administrativa_nordeste_mmg <- mmg %>%
    filter(REGIAO == "Região Nordeste" & ALTA == "Administrativa") %>%
    group_by(TIPO_UNIDADE) %>%
    tally %>%
    adorn_totals("row")
  
  administrativa_nordeste <- full_join(administrativa_nordeste_mmg, administrativa_nordeste_sih, by = "TIPO_UNIDADE")
  
  colnames(administrativa_nordeste) <- c("TIPO_UNIDADE", "MMG", "SIH")
  
  data <- data.frame(TIPO_UNIDADE = c("Unidades Privadas"),
                     MMG = c(0),
                     SIH = c(0))
  
  
  administrativa_nordeste <- rbind(data, administrativa_nordeste)
  
  colnames(administrativa_nordeste) <- c("Tipo Unidade", "MMG", "SIH")
  
  administrativa_nordeste[is.na(administrativa_nordeste)] <- 0
  
  
  # alta
  
  alta_nordeste_sih <- sih %>%
    filter(REGIAO == "Região Nordeste" & ALTA == "Alta") %>%
    group_by(TIPO_UNIDADE) %>%
    tally %>%
    adorn_totals("row")
  
  
  alta_nordeste_mmg <- mmg %>%
    filter(REGIAO == "Região Nordeste" & ALTA == "Alta") %>%
    group_by(TIPO_UNIDADE) %>%
    tally %>%
    adorn_totals("row")
  
  alta_nordeste <- full_join(alta_nordeste_mmg, alta_nordeste_sih, by = "TIPO_UNIDADE")
  
  alta_nordeste[is.na(alta_nordeste)] <- 0
  
  colnames(alta_nordeste) <- c("Tipo Unidade", "MMG", "SIH")
  
  
  # Complicações na gestação
  
  morte_nordeste_sih <- sih %>%
    filter(REGIAO == "Região Nordeste" & ALTA == "Morte") %>%
    group_by(TIPO_UNIDADE) %>%
    tally %>%
    adorn_totals("row")
  
  
  morte_nordeste_mmg <- mmg %>%
    filter(REGIAO == "Região Nordeste" & ALTA == "Morte") %>%
    group_by(TIPO_UNIDADE) %>%
    tally %>%
    adorn_totals("row")
  
  morte_nordeste <- full_join(morte_nordeste_mmg, morte_nordeste_sih, by = "TIPO_UNIDADE")
  
  morte_nordeste[is.na(morte_nordeste)] <- 0
  
  colnames(morte_nordeste) <- c("Tipo Unidade", "MMG", "SIH")
  
  
  # Complicações no puerpério
  
  permanencia_nordeste_sih <- sih %>%
    filter(REGIAO == "Região Nordeste" & ALTA == "Permanencia") %>%
    group_by(TIPO_UNIDADE) %>%
    tally %>%
    adorn_totals("row")
  
  
  permanencia_nordeste_mmg <- mmg %>%
    filter(REGIAO == "Região Nordeste" & ALTA == "Permanencia") %>%
    group_by(TIPO_UNIDADE) %>%
    tally %>%
    adorn_totals("row")
  
  permanencia_nordeste <- full_join(permanencia_nordeste_mmg, permanencia_nordeste_sih, by = "TIPO_UNIDADE")
  
  permanencia_nordeste[is.na(permanencia_nordeste)] <- 0
  
  colnames(permanencia_nordeste) <- c("Tipo Unidade", "MMG", "SIH")
  
  
  # transferencia
  
  transferencia_nordeste_sih <- sih %>%
    filter(REGIAO == "Região Nordeste" & ALTA == "Transferencia") %>%
    group_by(TIPO_UNIDADE) %>%
    tally %>%
    adorn_totals("row")
  
  
  transferencia_nordeste_mmg <- mmg %>%
    filter(REGIAO == "Região Nordeste" & ALTA == "Transferencia") %>%
    group_by(TIPO_UNIDADE) %>%
    tally %>%
    adorn_totals("row")
  
  transferencia_nordeste <- full_join(transferencia_nordeste_mmg, transferencia_nordeste_sih, by = "TIPO_UNIDADE")
  
  transferencia_nordeste[is.na(transferencia_nordeste)] <- 0
  
  colnames(transferencia_nordeste) <- c("Tipo Unidade", "MMG", "SIH")
  
  motivo_alta_nordeste <- alta_nordeste %>%
    full_join(administrativa_nordeste, by = "Tipo Unidade") %>%
    full_join(permanencia_nordeste, by = "Tipo Unidade") %>%
    full_join(morte_nordeste, by = "Tipo Unidade") %>%
    full_join(transferencia_nordeste, by = "Tipo Unidade")
  
  colnames(motivo_alta_nordeste) <- c("Tipo Unidade", "MMG", "SIH", "MMG", "SIH", "MMG", "SIH", "MMG", "SIH",
                                      "MMG", "SIH")
  
  ##### Região Sul
  
  # administrativa
  
  
  administrativa_sul_sih <- sih %>%
    filter(REGIAO == "Região Sul" & ALTA == "Administrativa") %>%
    group_by(TIPO_UNIDADE) %>%
    tally %>%
    adorn_totals("row")
  
  
  administrativa_sul_mmg <- mmg %>%
    filter(REGIAO == "Região Sul" & ALTA == "Administrativa") %>%
    group_by(TIPO_UNIDADE) %>%
    tally %>%
    adorn_totals("row")
  
  administrativa_sul <- full_join(administrativa_sul_mmg, administrativa_sul_sih, by = "TIPO_UNIDADE")
  
  colnames(administrativa_sul) <- c("TIPO_UNIDADE", "MMG", "SIH")
  
  data <- data.frame(TIPO_UNIDADE = c("Unidades Públicas"),
                     MMG = c(0),
                     SIH = c(0))
  
  
  administrativa_sul <- rbind(administrativa_sul, data)
  
  colnames(administrativa_sul) <- c("Tipo Unidade", "MMG", "SIH")
  
  administrativa_sul[is.na(administrativa_sul)] <- 0
  
  colnames(administrativa_sul) <- c("Tipo Unidade", "MMG", "SIH")
  
  
  # alta
  
  alta_sul_sih <- sih %>%
    filter(REGIAO == "Região Sul" & ALTA == "Alta") %>%
    group_by(TIPO_UNIDADE) %>%
    tally %>%
    adorn_totals("row")
  
  
  alta_sul_mmg <- mmg %>%
    filter(REGIAO == "Região Sul" & ALTA == "Alta") %>%
    group_by(TIPO_UNIDADE) %>%
    tally %>%
    adorn_totals("row")
  
  alta_sul <- full_join(alta_sul_mmg, alta_sul_sih, by = "TIPO_UNIDADE")
  
  alta_sul[is.na(alta_sul)] <- 0
  
  colnames(alta_sul) <- c("Tipo Unidade", "MMG", "SIH")
  
  
  # Complicações na gestação
  
  morte_sul_sih <- sih %>%
    filter(REGIAO == "Região Sul" & ALTA == "Morte") %>%
    group_by(TIPO_UNIDADE) %>%
    tally %>%
    adorn_totals("row")
  
  
  morte_sul_mmg <- mmg %>%
    filter(REGIAO == "Região Sul" & ALTA == "Morte") %>%
    group_by(TIPO_UNIDADE) %>%
    tally %>%
    adorn_totals("row")
  
  morte_sul <- full_join(morte_sul_mmg, morte_sul_sih, by = "TIPO_UNIDADE")
  
  morte_sul[is.na(morte_sul)] <- 0
  
  colnames(morte_sul) <- c("Tipo Unidade", "MMG", "SIH")
  
  
  # Complicações no puerpério
  
  permanencia_sul_sih <- sih %>%
    filter(REGIAO == "Região Sul" & ALTA == "Permanencia") %>%
    group_by(TIPO_UNIDADE) %>%
    tally %>%
    adorn_totals("row")
  
  
  permanencia_sul_mmg <- mmg %>%
    filter(REGIAO == "Região Sul" & ALTA == "Permanencia") %>%
    group_by(TIPO_UNIDADE) %>%
    tally %>%
    adorn_totals("row")
  
  permanencia_sul <- full_join(permanencia_sul_mmg, permanencia_sul_sih, by = "TIPO_UNIDADE")
  
  permanencia_sul[is.na(permanencia_sul)] <- 0
  
  colnames(permanencia_sul) <- c("Tipo Unidade", "MMG", "SIH")
  
  
  # transferencia
  
  transferencia_sul_sih <- sih %>%
    filter(REGIAO == "Região Sul" & ALTA == "Transferencia") %>%
    group_by(TIPO_UNIDADE) %>%
    tally %>%
    adorn_totals("row")
  
  
  transferencia_sul_mmg <- mmg %>%
    filter(REGIAO == "Região Sul" & ALTA == "Transferencia") %>%
    group_by(TIPO_UNIDADE) %>%
    tally %>%
    adorn_totals("row")
  
  transferencia_sul <- full_join(transferencia_sul_mmg, transferencia_sul_sih, by = "TIPO_UNIDADE")
  
  transferencia_sul[is.na(transferencia_sul)] <- 0
  
  colnames(transferencia_sul) <- c("Tipo Unidade", "MMG", "SIH")
  
  motivo_alta_sul <- alta_sul %>%
    full_join(administrativa_sul, by = "Tipo Unidade") %>%
    full_join(permanencia_sul, by = "Tipo Unidade") %>%
    full_join(morte_sul, by = "Tipo Unidade") %>%
    full_join(transferencia_sul, by = "Tipo Unidade")
  
  colnames(motivo_alta_sul) <- c("Tipo Unidade", "MMG", "SIH", "MMG", "SIH", "MMG", "SIH", "MMG", "SIH",
                                 "MMG", "SIH")
  
  ##### Região Sudeste
  
  # administrativa
  
  
  administrativa_sudeste_sih <- sih %>%
    filter(REGIAO == "Região Sudeste" & ALTA == "Administrativa") %>%
    group_by(TIPO_UNIDADE) %>%
    tally %>%
    adorn_totals("row")
  
  
  administrativa_sudeste_mmg <- mmg %>%
    filter(REGIAO == "Região Sudeste" & ALTA == "Administrativa") %>%
    group_by(TIPO_UNIDADE) %>%
    tally %>%
    adorn_totals("row")
  
  administrativa_sudeste <- full_join(administrativa_sudeste_mmg, administrativa_sudeste_sih, by = "TIPO_UNIDADE")
  
  colnames(administrativa_sudeste) <- c("Tipo Unidade", "MMG", "SIH")
  
  administrativa_sudeste[is.na(administrativa_sudeste)] <- 0
  
  # alta
  
  alta_sudeste_sih <- sih %>%
    filter(REGIAO == "Região Sudeste" & ALTA == "Alta") %>%
    group_by(TIPO_UNIDADE) %>%
    tally %>%
    adorn_totals("row")
  
  
  alta_sudeste_mmg <- mmg %>%
    filter(REGIAO == "Região Sudeste" & ALTA == "Alta") %>%
    group_by(TIPO_UNIDADE) %>%
    tally %>%
    adorn_totals("row")
  
  alta_sudeste <- full_join(alta_sudeste_mmg, alta_sudeste_sih, by = "TIPO_UNIDADE")
  
  alta_sudeste[is.na(alta_sudeste)] <- 0
  
  colnames(alta_sudeste) <- c("Tipo Unidade", "MMG", "SIH")
  
  
  # Complicações na gestação
  
  morte_sudeste_sih <- sih %>%
    filter(REGIAO == "Região Sudeste" & ALTA == "Morte") %>%
    group_by(TIPO_UNIDADE) %>%
    tally %>%
    adorn_totals("row")
  
  
  morte_sudeste_mmg <- mmg %>%
    filter(REGIAO == "Região Sudeste" & ALTA == "Morte") %>%
    group_by(TIPO_UNIDADE) %>%
    tally %>%
    adorn_totals("row")
  
  morte_sudeste <- full_join(morte_sudeste_mmg, morte_sudeste_sih, by = "TIPO_UNIDADE")
  
  morte_sudeste[is.na(morte_sudeste)] <- 0
  
  colnames(morte_sudeste) <- c("Tipo Unidade", "MMG", "SIH")
  
  
  # Complicações no puerpério
  
  permanencia_sudeste_sih <- sih %>%
    filter(REGIAO == "Região Sudeste" & ALTA == "Permanencia") %>%
    group_by(TIPO_UNIDADE) %>%
    tally %>%
    adorn_totals("row")
  
  
  permanencia_sudeste_mmg <- mmg %>%
    filter(REGIAO == "Região Sudeste" & ALTA == "Permanencia") %>%
    group_by(TIPO_UNIDADE) %>%
    tally %>%
    adorn_totals("row")
  
  permanencia_sudeste <- full_join(permanencia_sudeste_mmg, permanencia_sudeste_sih, by = "TIPO_UNIDADE")
  
  permanencia_sudeste[is.na(permanencia_sudeste)] <- 0
  
  colnames(permanencia_sudeste) <- c("Tipo Unidade", "MMG", "SIH")
  
  
  # transferencia
  
  transferencia_sudeste_sih <- sih %>%
    filter(REGIAO == "Região Sudeste" & ALTA == "Transferencia") %>%
    group_by(TIPO_UNIDADE) %>%
    tally %>%
    adorn_totals("row")
  
  
  transferencia_sudeste_mmg <- mmg %>%
    filter(REGIAO == "Região Sudeste" & ALTA == "Transferencia") %>%
    group_by(TIPO_UNIDADE) %>%
    tally %>%
    adorn_totals("row")
  
  transferencia_sudeste <- full_join(transferencia_sudeste_mmg, transferencia_sudeste_sih, by = "TIPO_UNIDADE")
  
  transferencia_sudeste[is.na(transferencia_sudeste)] <- 0
  
  colnames(transferencia_sudeste) <- c("Tipo Unidade", "MMG", "SIH")
  
  motivo_alta_sudeste <- alta_sudeste %>%
    full_join(administrativa_sudeste, by = "Tipo Unidade") %>%
    full_join(permanencia_sudeste, by = "Tipo Unidade") %>%
    full_join(morte_sudeste, by = "Tipo Unidade") %>%
    full_join(transferencia_sudeste, by = "Tipo Unidade")
  
  colnames(motivo_alta_sudeste) <- c("Tipo Unidade", "MMG", "SIH", "MMG", "SIH", "MMG", "SIH", "MMG", "SIH",
                                     "MMG", "SIH")
  
  ##### Região Centro-Oeste
  
  # administrativa
  
  
  administrativa_centro_oeste_sih <- sih %>%
    filter(REGIAO == "Região Centro-Oeste" & ALTA == "Administrativa") %>%
    group_by(TIPO_UNIDADE) %>%
    tally %>%
    adorn_totals("row")
  
  
  administrativa_centro_oeste_mmg <- mmg %>%
    filter(REGIAO == "Região Centro-Oeste" & ALTA == "Administrativa") %>%
    group_by(TIPO_UNIDADE) %>%
    tally %>%
    adorn_totals("row")
  
  administrativa_centro_oeste <- full_join(administrativa_centro_oeste_mmg, administrativa_centro_oeste_sih, by = "TIPO_UNIDADE")
  
  colnames(administrativa_centro_oeste) <- c("Tipo Unidade", "MMG", "SIH")
  
  administrativa_centro_oeste[is.na(administrativa_centro_oeste)] <- 0
  
  # alta
  
  alta_centro_oeste_sih <- sih %>%
    filter(REGIAO == "Região Centro-Oeste" & ALTA == "Alta") %>%
    group_by(TIPO_UNIDADE) %>%
    tally %>%
    adorn_totals("row")
  
  
  alta_centro_oeste_mmg <- mmg %>%
    filter(REGIAO == "Região Centro-Oeste" & ALTA == "Alta") %>%
    group_by(TIPO_UNIDADE) %>%
    tally %>%
    adorn_totals("row")
  
  alta_centro_oeste <- full_join(alta_centro_oeste_mmg, alta_centro_oeste_sih, by = "TIPO_UNIDADE")
  
  alta_centro_oeste[is.na(alta_centro_oeste)] <- 0
  
  colnames(alta_centro_oeste) <- c("Tipo Unidade", "MMG", "SIH")
  
  
  # Complicações na gestação
  
  morte_centro_oeste_sih <- sih %>%
    filter(REGIAO == "Região Centro-Oeste" & ALTA == "Morte") %>%
    group_by(TIPO_UNIDADE) %>%
    tally %>%
    adorn_totals("row")
  
  
  morte_centro_oeste_mmg <- mmg %>%
    filter(REGIAO == "Região Centro-Oeste" & ALTA == "Morte") %>%
    group_by(TIPO_UNIDADE) %>%
    tally %>%
    adorn_totals("row")
  
  morte_centro_oeste <- full_join(morte_centro_oeste_mmg, morte_centro_oeste_sih, by = "TIPO_UNIDADE")
  
  colnames(morte_centro_oeste) <- c("TIPO_UNIDADE", "MMG", "SIH")
  
  data <- data.frame(TIPO_UNIDADE = c("Unidades Públicas"),
                     MMG = c(0),
                     SIH = c(0))
  
  
  morte_centro_oeste <- rbind(morte_centro_oeste, data)
  
  colnames(morte_centro_oeste) <- c("Tipo Unidade", "MMG", "SIH")
  
  morte_centro_oeste[is.na(morte_centro_oeste)] <- 0
  
  colnames(morte_centro_oeste) <- c("Tipo Unidade", "MMG", "SIH")
  
  
  # Complicações no puerpério
  
  permanencia_centro_oeste_sih <- sih %>%
    filter(REGIAO == "Região Centro-Oeste" & ALTA == "Permanencia") %>%
    group_by(TIPO_UNIDADE) %>%
    tally %>%
    adorn_totals("row")
  
  
  permanencia_centro_oeste_mmg <- mmg %>%
    filter(REGIAO == "Região Centro-Oeste" & ALTA == "Permanencia") %>%
    group_by(TIPO_UNIDADE) %>%
    tally %>%
    adorn_totals("row")
  
  permanencia_centro_oeste <- full_join(permanencia_centro_oeste_mmg, permanencia_centro_oeste_sih, by = "TIPO_UNIDADE")
  
  permanencia_centro_oeste[is.na(permanencia_centro_oeste)] <- 0
  
  colnames(permanencia_centro_oeste) <- c("Tipo Unidade", "MMG", "SIH")
  
  
  # transferencia
  
  transferencia_centro_oeste_sih <- sih %>%
    filter(REGIAO == "Região Centro-Oeste" & ALTA == "Transferencia") %>%
    group_by(TIPO_UNIDADE) %>%
    tally %>%
    adorn_totals("row")
  
  
  transferencia_centro_oeste_mmg <- mmg %>%
    filter(REGIAO == "Região Centro-Oeste" & ALTA == "Transferencia") %>%
    group_by(TIPO_UNIDADE) %>%
    tally %>%
    adorn_totals("row")
  
  transferencia_centro_oeste <- full_join(transferencia_centro_oeste_mmg, transferencia_centro_oeste_sih, by = "TIPO_UNIDADE")
  
  transferencia_centro_oeste[is.na(transferencia_centro_oeste)] <- 0
  
  colnames(transferencia_centro_oeste) <- c("Tipo Unidade", "MMG", "SIH")
  
  motivo_alta_centro_oeste <- alta_centro_oeste %>%
    full_join(administrativa_centro_oeste, by = "Tipo Unidade") %>%
    full_join(permanencia_centro_oeste, by = "Tipo Unidade") %>%
    full_join(morte_centro_oeste, by = "Tipo Unidade") %>%
    full_join(transferencia_centro_oeste, by = "Tipo Unidade")
  
  colnames(motivo_alta_centro_oeste) <- c("Tipo Unidade", "MMG", "SIH", "MMG", "SIH", "MMG", "SIH", "MMG", "SIH",
                                          "MMG", "SIH")
  
  ##### Região Brasil
  
  # administrativa
  
  
  administrativa_br_sih <- sih %>%
    filter( ALTA == "Administrativa") %>%
    group_by(TIPO_UNIDADE) %>%
    tally %>%
    adorn_totals("row")
  
  
  administrativa_br_mmg <- mmg %>%
    filter( ALTA == "Administrativa") %>%
    group_by(TIPO_UNIDADE) %>%
    tally %>%
    adorn_totals("row")
  
  administrativa_br <- full_join(administrativa_br_mmg, administrativa_br_sih, by = "TIPO_UNIDADE")
  
  colnames(administrativa_br) <- c("Tipo Unidade", "MMG", "SIH")
  
  administrativa_br[is.na(administrativa_br)] <- 0
  
  
  # alta
  
  alta_br_sih <- sih %>%
    filter( ALTA == "Alta") %>%
    group_by(TIPO_UNIDADE) %>%
    tally %>%
    adorn_totals("row")
  
  
  alta_br_mmg <- mmg %>%
    filter( ALTA == "Alta") %>%
    group_by(TIPO_UNIDADE) %>%
    tally %>%
    adorn_totals("row")
  
  alta_br <- full_join(alta_br_mmg, alta_br_sih, by = "TIPO_UNIDADE")
  
  alta_br[is.na(alta_br)] <- 0
  
  colnames(alta_br) <- c("Tipo Unidade", "MMG", "SIH")
  
  
  # Complicações na gestação
  
  morte_br_sih <- sih %>%
    filter( ALTA == "Morte") %>%
    group_by(TIPO_UNIDADE) %>%
    tally %>%
    adorn_totals("row")
  
  
  morte_br_mmg <- mmg %>%
    filter( ALTA == "Morte") %>%
    group_by(TIPO_UNIDADE) %>%
    tally %>%
    adorn_totals("row")
  
  morte_br <- full_join(morte_br_mmg, morte_br_sih, by = "TIPO_UNIDADE")
  
  morte_br[is.na(morte_br)] <- 0
  
  colnames(morte_br) <- c("Tipo Unidade", "MMG", "SIH")
  
  
  # Complicações no puerpério
  
  permanencia_br_sih <- sih %>%
    filter( ALTA == "Permanencia") %>%
    group_by(TIPO_UNIDADE) %>%
    tally %>%
    adorn_totals("row")
  
  
  permanencia_br_mmg <- mmg %>%
    filter( ALTA == "Permanencia") %>%
    group_by(TIPO_UNIDADE) %>%
    tally %>%
    adorn_totals("row")
  
  permanencia_br <- full_join(permanencia_br_mmg, permanencia_br_sih, by = "TIPO_UNIDADE")
  
  permanencia_br[is.na(permanencia_br)] <- 0
  
  colnames(permanencia_br) <- c("Tipo Unidade", "MMG", "SIH")
  
  
  # transferencia
  
  transferencia_br_sih <- sih %>%
    filter( ALTA == "Transferencia") %>%
    group_by(TIPO_UNIDADE) %>%
    tally %>%
    adorn_totals("row")
  
  
  transferencia_br_mmg <- mmg %>%
    filter( ALTA == "Transferencia") %>%
    group_by(TIPO_UNIDADE) %>%
    tally %>%
    adorn_totals("row")
  
  transferencia_br <- full_join(transferencia_br_mmg, transferencia_br_sih, by = "TIPO_UNIDADE")
  
  transferencia_br[is.na(transferencia_br)] <- 0
  
  colnames(transferencia_br) <- c("Tipo Unidade", "MMG", "SIH")
  
  
  motivo_alta_br <- alta_br %>%
    full_join(administrativa_br, by = "Tipo Unidade") %>%
    full_join(permanencia_br, by = "Tipo Unidade") %>%
    full_join(morte_br, by = "Tipo Unidade") %>%
    full_join(transferencia_br, by = "Tipo Unidade")
  
  colnames(motivo_alta_br) <- c("Tipo Unidade", "MMG", "SIH", "MMG", "SIH", "MMG", "SIH", "MMG", "SIH",
                                "MMG", "SIH")
  
  tabelao_motivo_alta <- rbind(motivo_alta_norte, motivo_alta_nordeste, motivo_alta_sul,
                               motivo_alta_sudeste, motivo_alta_centro_oeste, motivo_alta_br)
  
  tabelao_motivo_alta[is.na(tabelao_motivo_alta)] <- 0
  
  tabela_motivo_alta <- kbl(tabelao_motivo_alta, align = "lcccccccccc") %>%
    add_header_above(c(" " = 1, "Alta" = 2, "Admnistrativa" = 2, "Permanência" = 2,
                       "Morte" = 2, "Transferência" = 2)) %>%
    kable_classic(full_width = F) %>%
    pack_rows("Região Norte", 1, 3) %>%
    pack_rows("Região Nordeste", 4, 6) %>%
    pack_rows("Região Sul", 7, 9) %>%
    pack_rows("Região Sudeste", 10, 12) %>%
    pack_rows("Região Centro-Oeste", 13, 15) %>%
    pack_rows("Brasil", 16, 18)
  
  return(tabela_motivo_alta)
}

tipo_alta_tab(sih, mmg)



##############################################################################################################

##### TOTAL INTERNAÇÕES SEM AS UNIDADES

total_internacoes_tab2 <- function(sih, mmg){
  
  sem_motivo <- sih %>%
    filter(!CNES %in% c("434", "2319454", "9717", "2311682", "2396866", "2361787",
                        "2710935", "2338564", "10472", "6518893", "7011857", "2711613", "7958838",
                        "9768"))
  
  mmg2 <- mmg %>%
    filter(!CNES %in% c("0000434", "2319454", "0009717",  "2311682", "2396866", "2361787",
                        "2710935", "2338564", "0010472", "6518893", "7011857", "2711613", "7958838",
                        "0009768"))
  
  primeira_AIH <- setDT(sem_motivo)[order(grupoaih, as.IDate(DT_INTER, "%m/%d/%Y"))][!duplicated(grupoaih)]
  
  
  # Região Norte
  
  norte_sih <- primeira_AIH %>%
    filter(REGIAO == "Região Norte") %>%
    group_by(TIPO_UNIDADE) %>%
    tally %>%
    adorn_totals("row")
  
  
  norte_mmg <- mmg2 %>%
    filter(REGIAO == "Região Norte") %>%
    group_by(TIPO_UNIDADE) %>%
    tally %>%
    adorn_totals("row")
  
  
  total_internacoes_norte <- full_join(norte_mmg, norte_sih, by = "TIPO_UNIDADE")
  
  colnames(total_internacoes_norte) <- c("Tipo Unidade", "MMG", "SIH")
  
  
  # Região Nordeste
  
  nordeste_sih <- primeira_AIH %>%
    filter(REGIAO == "Região Nordeste") %>%
    group_by(TIPO_UNIDADE) %>%
    tally %>%
    adorn_totals("row")
  
  
  nordeste_mmg <- mmg2 %>%
    filter(REGIAO == "Região Nordeste") %>%
    group_by(TIPO_UNIDADE) %>%
    tally %>%
    adorn_totals("row")
  
  
  total_internacoes_nordeste <- full_join(nordeste_mmg, nordeste_sih, by = "TIPO_UNIDADE")
  
  colnames(total_internacoes_nordeste) <- c("Tipo Unidade", "MMG", "SIH")
  
  
  # Região Sul
  
  sul_sih <- primeira_AIH %>%
    filter(REGIAO == "Região Sul") %>%
    group_by(TIPO_UNIDADE) %>%
    tally %>%
    adorn_totals("row")
  
  
  sul_mmg <- mmg2 %>%
    filter(REGIAO == "Região Sul") %>%
    group_by(TIPO_UNIDADE) %>%
    tally %>%
    adorn_totals("row")
  
  
  total_internacoes_sul <- full_join(sul_mmg, sul_sih, by = "TIPO_UNIDADE")
  
  colnames(total_internacoes_sul) <- c("Tipo Unidade", "MMG", "SIH")
  
  
  # Região Sudeste
  
  sudeste_sih <- primeira_AIH %>%
    filter(REGIAO == "Região Sudeste") %>%
    group_by(TIPO_UNIDADE) %>%
    tally %>%
    adorn_totals("row")
  
  
  sudeste_mmg <- mmg2 %>%
    filter(REGIAO == "Região Sudeste") %>%
    group_by(TIPO_UNIDADE) %>%
    tally %>%
    adorn_totals("row")
  
  
  total_internacoes_sudeste <- full_join(sudeste_mmg, sudeste_sih, by = "TIPO_UNIDADE")
  
  colnames(total_internacoes_sudeste) <- c("Tipo Unidade", "MMG", "SIH")
  
  
  # Região Centro-Oeste
  
  centro_oeste_sih <- primeira_AIH %>%
    filter(REGIAO == "Região Centro-Oeste") %>%
    group_by(TIPO_UNIDADE) %>%
    tally %>%
    adorn_totals("row")
  
  
  centro_oeste_mmg <- mmg2 %>%
    filter(REGIAO == "Região Centro-Oeste") %>%
    group_by(TIPO_UNIDADE) %>%
    tally %>%
    adorn_totals("row")
  
  
  total_internacoes_centro_oeste <- full_join(centro_oeste_mmg, centro_oeste_sih, by = "TIPO_UNIDADE")
  
  colnames(total_internacoes_centro_oeste) <- c("TIPO_UNIDADE", "MMG", "SIH")
  
  data <- data.frame(TIPO_UNIDADE = c("Unidades Privadas"),
                     MMG = c(0),
                     SIH = c(0))
  
  total_internacoes_centro_oeste <- rbind(data, total_internacoes_centro_oeste)
  
  colnames(total_internacoes_centro_oeste) <- c("Tipo Unidade", "MMG", "SIH")
  
  
  # Brasil
  
  br_sih <- primeira_AIH %>%
    group_by(TIPO_UNIDADE) %>%
    tally %>%
    adorn_totals("row")
  
  br_mmg <- mmg2 %>%
    group_by(TIPO_UNIDADE) %>%
    tally %>%
    adorn_totals("row")
  
  
  total_internacoes_br <- full_join(br_mmg, br_sih, by = "TIPO_UNIDADE")
  
  colnames(total_internacoes_br) <- c("Tipo Unidade", "MMG", "SIH")
  
  
  # Tabela 
  
  tabelao_internacoes <- rbind(total_internacoes_norte, total_internacoes_nordeste, total_internacoes_sul,
                               total_internacoes_sudeste, total_internacoes_centro_oeste, total_internacoes_br)
  
  tabela_internacoes <- kbl(tabelao_internacoes, align = "lcc") %>%
    kable_classic(full_width = F) %>%
    pack_rows("Região Norte", 1, 3) %>%
    pack_rows("Região Nordeste", 4, 6) %>%
    pack_rows("Região Sul", 7, 9) %>%
    pack_rows("Região Sudeste", 10, 12) %>%
    pack_rows("Região Centro-Oeste", 13, 15) %>%
    pack_rows("Brasil", 16, 18)
  
  return(tabela_internacoes)
}

total_internacoes_tab2(sih, mmg)


####### Tabela - Motivo de internação

motivo_internacao_tab <- function(sih, mmg){
  
  
  sem_motivo <- sih %>%
    filter(!CNES %in% c("434", "2319454", "9717", "2311682", "2396866", "2361787",
                        "2710935", "2338564", "10472", "6518893", "7011857", "2711613", "7958838",
                        "9768"))
  
  mmg2 <- mmg %>%
    filter(!CNES %in% c("0000434", "2319454", "0009717",  "2311682", "2396866", "2361787",
                        "2710935", "2338564", "0010472", "6518893", "7011857", "2711613", "7958838",
                        "0009768"))
  
  primeira_AIH <- setDT(sem_motivo)[order(grupoaih, as.IDate(DT_INTER, "%m/%d/%Y"))][!duplicated(grupoaih)]
  
##### Região Norte

# parto

summary.factor(mmg2$MOTIVO_INTERNACAO)

parto_norte_sih <- primeira_AIH %>%
  filter(REGIAO == "Região Norte" & MOTIVO_INTERNACAO == "Parto") %>%
  group_by(TIPO_UNIDADE) %>%
  tally %>%
  adorn_totals("row")


parto_norte_mmg <- mmg2 %>%
  filter(REGIAO == "Região Norte" & MOTIVO_INTERNACAO == "Parto") %>%
  group_by(TIPO_UNIDADE) %>%
  tally %>%
  adorn_totals("row")

parto_norte <- full_join(parto_norte_mmg, parto_norte_sih, by = "TIPO_UNIDADE")

colnames(parto_norte) <- c("Tipo Unidade", "MMG", "SIH")


# Aborto

aborto_norte_sih <- primeira_AIH %>%
  filter(REGIAO == "Região Norte" & MOTIVO_INTERNACAO == "Aborto") %>%
  group_by(TIPO_UNIDADE) %>%
  tally %>%
  adorn_totals("row")


aborto_norte_mmg <- mmg2 %>%
  filter(REGIAO == "Região Norte" & MOTIVO_INTERNACAO == "Aborto") %>%
  group_by(TIPO_UNIDADE) %>%
  tally %>%
  adorn_totals("row")

aborto_norte <- full_join(aborto_norte_mmg, aborto_norte_sih, by = "TIPO_UNIDADE")

aborto_norte[is.na(aborto_norte)] <- 0

colnames(aborto_norte) <- c("Tipo Unidade", "MMG", "SIH")


# Complicações na gestação

comp_gest_norte_sih <- primeira_AIH %>%
  filter(REGIAO == "Região Norte" & MOTIVO_INTERNACAO == "Complicações na gestação") %>%
  group_by(TIPO_UNIDADE) %>%
  tally %>%
  adorn_totals("row")


comp_gest_norte_mmg <- mmg2 %>%
  filter(REGIAO == "Região Norte" & MOTIVO_INTERNACAO == "Complicações na gestação") %>%
  group_by(TIPO_UNIDADE) %>%
  tally %>%
  adorn_totals("row")

comp_gest_norte <- full_join(comp_gest_norte_mmg, comp_gest_norte_sih, by = "TIPO_UNIDADE")

comp_gest_norte[is.na(comp_gest_norte)] <- 0

colnames(comp_gest_norte) <- c("Tipo Unidade", "MMG", "SIH")


# Complicações no puerpério

comp_puerp_norte_sih <- primeira_AIH %>%
  filter(REGIAO == "Região Norte" & MOTIVO_INTERNACAO == "Complicações no puerpério") %>%
  group_by(TIPO_UNIDADE) %>%
  tally %>%
  adorn_totals("row")


comp_puerp_norte_mmg <- mmg2 %>%
  filter(REGIAO == "Região Norte" & MOTIVO_INTERNACAO == "Complicações no puerpério") %>%
  group_by(TIPO_UNIDADE) %>%
  tally %>%
  adorn_totals("row")

comp_puerp_norte <- full_join(comp_puerp_norte_mmg, comp_puerp_norte_sih, by = "TIPO_UNIDADE")

colnames(comp_puerp_norte) <- c("TIPO_UNIDADE", "MMG", "SIH")

data <- data.frame(TIPO_UNIDADE = c("Unidades Privadas"),
                   MMG = c(0),
                   SIH = c(0))


comp_puerp_norte <- rbind(data, comp_puerp_norte)

comp_puerp_norte[is.na(comp_puerp_norte)] <- 0

colnames(comp_puerp_norte) <- c("Tipo Unidade", "MMG", "SIH")


# outros

outros_norte_sih <- primeira_AIH %>%
  filter(REGIAO == "Região Norte" & MOTIVO_INTERNACAO == "Outros") %>%
  group_by(TIPO_UNIDADE) %>%
  tally %>%
  adorn_totals("row")


outros_norte_mmg <- mmg2 %>%
  filter(REGIAO == "Região Norte" & MOTIVO_INTERNACAO == "Outros") %>%
  group_by(TIPO_UNIDADE) %>%
  tally %>%
  adorn_totals("row")

outros_norte <- full_join(outros_norte_mmg, outros_norte_sih, by = "TIPO_UNIDADE")

outros_norte[is.na(outros_norte)] <- 0

colnames(outros_norte) <- c("Tipo Unidade", "MMG", "SIH")

# Em branco

branco_norte_sih <- primeira_AIH %>%
  filter(REGIAO == "Região Norte" & MOTIVO_INTERNACAO == "Em branco") %>%
  group_by(TIPO_UNIDADE) %>%
  tally %>%
  adorn_totals("row")


branco_norte_mmg <- mmg2 %>%
  filter(REGIAO == "Região Norte" & MOTIVO_INTERNACAO == "Em branco") %>%
  group_by(TIPO_UNIDADE) %>%
  tally %>%
  adorn_totals("row")

branco_norte <- full_join(branco_norte_mmg, branco_norte_sih, by = "TIPO_UNIDADE")

branco_norte[is.na(branco_norte)] <- 0

colnames(branco_norte) <- c("Tipo Unidade", "MMG", "SIH")

motivo_norte <- parto_norte %>%
  full_join(aborto_norte, by = "Tipo Unidade") %>%
  full_join(comp_gest_norte, by = "Tipo Unidade") %>%
  full_join(comp_puerp_norte, by = "Tipo Unidade") %>%
  full_join(outros_norte, by = "Tipo Unidade") %>%
  full_join(branco_norte, by = "Tipo Unidade")

colnames(motivo_norte) <- c("Tipo Unidade", "MMG", "SIH", "MMG", "SIH", "MMG", "SIH", "MMG", "SIH",
                            "MMG", "SIH", "MMG", "SIH")


##### Região Nordeste

# parto


parto_nordeste_sih <- primeira_AIH %>%
  filter(REGIAO == "Região Nordeste" & MOTIVO_INTERNACAO == "Parto") %>%
  group_by(TIPO_UNIDADE) %>%
  tally %>%
  adorn_totals("row")


parto_nordeste_mmg <- mmg2 %>%
  filter(REGIAO == "Região Nordeste" & MOTIVO_INTERNACAO == "Parto") %>%
  group_by(TIPO_UNIDADE) %>%
  tally %>%
  adorn_totals("row")

parto_nordeste <- full_join(parto_nordeste_mmg, parto_nordeste_sih, by = "TIPO_UNIDADE")

colnames(parto_nordeste) <- c("Tipo Unidade", "MMG", "SIH")


# Aborto

aborto_nordeste_sih <- primeira_AIH %>%
  filter(REGIAO == "Região Nordeste" & MOTIVO_INTERNACAO == "Aborto") %>%
  group_by(TIPO_UNIDADE) %>%
  tally %>%
  adorn_totals("row")


aborto_nordeste_mmg <- mmg2 %>%
  filter(REGIAO == "Região Nordeste" & MOTIVO_INTERNACAO == "Aborto") %>%
  group_by(TIPO_UNIDADE) %>%
  tally %>%
  adorn_totals("row")

aborto_nordeste <- full_join(aborto_nordeste_mmg, aborto_nordeste_sih, by = "TIPO_UNIDADE")

aborto_nordeste[is.na(aborto_nordeste)] <- 0

colnames(aborto_nordeste) <- c("Tipo Unidade", "MMG", "SIH")


# Complicações na gestação

comp_gest_nordeste_sih <- primeira_AIH %>%
  filter(REGIAO == "Região Nordeste" & MOTIVO_INTERNACAO == "Complicações na gestação") %>%
  group_by(TIPO_UNIDADE) %>%
  tally %>%
  adorn_totals("row")


comp_gest_nordeste_mmg <- mmg2 %>%
  filter(REGIAO == "Região Nordeste" & MOTIVO_INTERNACAO == "Complicações na gestação") %>%
  group_by(TIPO_UNIDADE) %>%
  tally %>%
  adorn_totals("row")

comp_gest_nordeste <- full_join(comp_gest_nordeste_mmg, comp_gest_nordeste_sih, by = "TIPO_UNIDADE")

comp_gest_nordeste[is.na(comp_gest_nordeste)] <- 0

colnames(comp_gest_nordeste) <- c("Tipo Unidade", "MMG", "SIH")


# Complicações no puerpério

comp_puerp_nordeste_sih <- primeira_AIH %>%
  filter(REGIAO == "Região Nordeste" & MOTIVO_INTERNACAO == "Complicações no puerpério") %>%
  group_by(TIPO_UNIDADE) %>%
  tally %>%
  adorn_totals("row")


comp_puerp_nordeste_mmg <- mmg2 %>%
  filter(REGIAO == "Região Nordeste" & MOTIVO_INTERNACAO == "Complicações no puerpério") %>%
  group_by(TIPO_UNIDADE) %>%
  tally %>%
  adorn_totals("row")

comp_puerp_nordeste <- full_join(comp_puerp_nordeste_mmg, comp_puerp_nordeste_sih, by = "TIPO_UNIDADE")

comp_puerp_nordeste[is.na(comp_puerp_nordeste)] <- 0

colnames(comp_puerp_nordeste) <- c("Tipo Unidade", "MMG", "SIH")


# outros

outros_nordeste_sih <- primeira_AIH %>%
  filter(REGIAO == "Região Nordeste" & MOTIVO_INTERNACAO == "Outros") %>%
  group_by(TIPO_UNIDADE) %>%
  tally %>%
  adorn_totals("row")


outros_nordeste_mmg <- mmg2 %>%
  filter(REGIAO == "Região Nordeste" & MOTIVO_INTERNACAO == "Outros") %>%
  group_by(TIPO_UNIDADE) %>%
  tally %>%
  adorn_totals("row")

outros_nordeste <- full_join(outros_nordeste_mmg, outros_nordeste_sih, by = "TIPO_UNIDADE")

outros_nordeste[is.na(outros_nordeste)] <- 0

colnames(outros_nordeste) <- c("Tipo Unidade", "MMG", "SIH")

# Em branco

branco_nordeste_sih <- primeira_AIH %>%
  filter(REGIAO == "Região Nordeste" & MOTIVO_INTERNACAO == "Em branco") %>%
  group_by(TIPO_UNIDADE) %>%
  tally %>%
  adorn_totals("row")


branco_nordeste_mmg <- mmg2 %>%
  filter(REGIAO == "Região Nordeste" & MOTIVO_INTERNACAO == "Em branco") %>%
  group_by(TIPO_UNIDADE) %>%
  tally %>%
  adorn_totals("row")

branco_nordeste <- full_join(branco_nordeste_mmg, branco_nordeste_sih, by = "TIPO_UNIDADE")

branco_nordeste[is.na(branco_nordeste)] <- 0

colnames(branco_nordeste) <- c("Tipo Unidade", "MMG", "SIH")

motivo_nordeste <- parto_nordeste %>%
  full_join(aborto_nordeste, by = "Tipo Unidade") %>%
  full_join(comp_gest_nordeste, by = "Tipo Unidade") %>%
  full_join(comp_puerp_nordeste, by = "Tipo Unidade") %>%
  full_join(outros_nordeste, by = "Tipo Unidade") %>%
  full_join(branco_nordeste, by = "Tipo Unidade")

motivo_nordeste[is.na(motivo_nordeste)] <- 0

colnames(motivo_nordeste) <- c("Tipo Unidade", "MMG", "SIH", "MMG", "SIH", "MMG", "SIH", "MMG", "SIH",
                            "MMG", "SIH", "MMG", "SIH")


##### Região Sul

# parto


parto_sul_sih <- primeira_AIH %>%
  filter(REGIAO == "Região Sul" & MOTIVO_INTERNACAO == "Parto") %>%
  group_by(TIPO_UNIDADE) %>%
  tally %>%
  adorn_totals("row")


parto_sul_mmg <- mmg2 %>%
  filter(REGIAO == "Região Sul" & MOTIVO_INTERNACAO == "Parto") %>%
  group_by(TIPO_UNIDADE) %>%
  tally %>%
  adorn_totals("row")

parto_sul <- full_join(parto_sul_mmg, parto_sul_sih, by = "TIPO_UNIDADE")

colnames(parto_sul) <- c("Tipo Unidade", "MMG", "SIH")


# Aborto

aborto_sul_sih <- primeira_AIH %>%
  filter(REGIAO == "Região Sul" & MOTIVO_INTERNACAO == "Aborto") %>%
  group_by(TIPO_UNIDADE) %>%
  tally %>%
  adorn_totals("row")


aborto_sul_mmg <- mmg2 %>%
  filter(REGIAO == "Região Sul" & MOTIVO_INTERNACAO == "Aborto") %>%
  group_by(TIPO_UNIDADE) %>%
  tally %>%
  adorn_totals("row")

aborto_sul <- full_join(aborto_sul_mmg, aborto_sul_sih, by = "TIPO_UNIDADE")

aborto_sul[is.na(aborto_sul)] <- 0

colnames(aborto_sul) <- c("Tipo Unidade", "MMG", "SIH")


# Complicações na gestação

comp_gest_sul_sih <- primeira_AIH %>%
  filter(REGIAO == "Região Sul" & MOTIVO_INTERNACAO == "Complicações na gestação") %>%
  group_by(TIPO_UNIDADE) %>%
  tally %>%
  adorn_totals("row")


comp_gest_sul_mmg <- mmg2 %>%
  filter(REGIAO == "Região Sul" & MOTIVO_INTERNACAO == "Complicações na gestação") %>%
  group_by(TIPO_UNIDADE) %>%
  tally %>%
  adorn_totals("row")

comp_gest_sul <- full_join(comp_gest_sul_mmg, comp_gest_sul_sih, by = "TIPO_UNIDADE")

comp_gest_sul[is.na(comp_gest_sul)] <- 0

colnames(comp_gest_sul) <- c("Tipo Unidade", "MMG", "SIH")


# Complicações no puerpério

comp_puerp_sul_sih <- primeira_AIH %>%
  filter(REGIAO == "Região Sul" & MOTIVO_INTERNACAO == "Complicações no puerpério") %>%
  group_by(TIPO_UNIDADE) %>%
  tally %>%
  adorn_totals("row")


comp_puerp_sul_mmg <- mmg2 %>%
  filter(REGIAO == "Região Sul" & MOTIVO_INTERNACAO == "Complicações no puerpério") %>%
  group_by(TIPO_UNIDADE) %>%
  tally %>%
  adorn_totals("row")

comp_puerp_sul <- full_join(comp_puerp_sul_mmg, comp_puerp_sul_sih, by = "TIPO_UNIDADE")

comp_puerp_sul[is.na(comp_puerp_sul)] <- 0

colnames(comp_puerp_sul) <- c("Tipo Unidade", "MMG", "SIH")


# outros

outros_sul_sih <- primeira_AIH %>%
  filter(REGIAO == "Região Sul" & MOTIVO_INTERNACAO == "Outros") %>%
  group_by(TIPO_UNIDADE) %>%
  tally %>%
  adorn_totals("row")


outros_sul_mmg <- mmg2 %>%
  filter(REGIAO == "Região Sul" & MOTIVO_INTERNACAO == "Outros") %>%
  group_by(TIPO_UNIDADE) %>%
  tally %>%
  adorn_totals("row")

outros_sul <- full_join(outros_sul_mmg, outros_sul_sih, by = "TIPO_UNIDADE")

outros_sul[is.na(outros_sul)] <- 0

colnames(outros_sul) <- c("Tipo Unidade", "MMG", "SIH")

# Em branco

branco_sul_sih <- primeira_AIH %>%
  filter(REGIAO == "Região Sul" & MOTIVO_INTERNACAO == "Em branco") %>%
  group_by(TIPO_UNIDADE) %>%
  tally %>%
  adorn_totals("row")


branco_sul_mmg <- mmg2 %>%
  filter(REGIAO == "Região Sul" & MOTIVO_INTERNACAO == "Em branco") %>%
  group_by(TIPO_UNIDADE) %>%
  tally %>%
  adorn_totals("row")

branco_sul <- full_join(branco_sul_mmg, branco_sul_sih, by = "TIPO_UNIDADE")

colnames(branco_sul) <- c("TIPO_UNIDADE", "MMG", "SIH")

data <- data.frame(TIPO_UNIDADE = c("Unidades Públicas"),
                   MMG = c(0),
                   SIH = c(0))


branco_sul <- rbind(branco_sul, data)


branco_sul[is.na(branco_sul)] <- 0

colnames(branco_sul) <- c("Tipo Unidade", "MMG", "SIH")

motivo_sul <- parto_sul %>%
  full_join(aborto_sul, by = "Tipo Unidade") %>%
  full_join(comp_gest_sul, by = "Tipo Unidade") %>%
  full_join(comp_puerp_sul, by = "Tipo Unidade") %>%
  full_join(outros_sul, by = "Tipo Unidade") %>%
  full_join(branco_sul, by = "Tipo Unidade")

colnames(motivo_sul) <- c("Tipo Unidade", "MMG", "SIH", "MMG", "SIH", "MMG", "SIH", "MMG", "SIH",
                               "MMG", "SIH", "MMG", "SIH")



##### Região Sudeste

# parto


parto_sudeste_sih <- primeira_AIH %>%
  filter(REGIAO == "Região Sudeste" & MOTIVO_INTERNACAO == "Parto") %>%
  group_by(TIPO_UNIDADE) %>%
  tally %>%
  adorn_totals("row")


parto_sudeste_mmg <- mmg2 %>%
  filter(REGIAO == "Região Sudeste" & MOTIVO_INTERNACAO == "Parto") %>%
  group_by(TIPO_UNIDADE) %>%
  tally %>%
  adorn_totals("row")

parto_sudeste <- full_join(parto_sudeste_mmg, parto_sudeste_sih, by = "TIPO_UNIDADE")

colnames(parto_sudeste) <- c("Tipo Unidade", "MMG", "SIH")


# Aborto

aborto_sudeste_sih <- primeira_AIH %>%
  filter(REGIAO == "Região Sudeste" & MOTIVO_INTERNACAO == "Aborto") %>%
  group_by(TIPO_UNIDADE) %>%
  tally %>%
  adorn_totals("row")


aborto_sudeste_mmg <- mmg2 %>%
  filter(REGIAO == "Região Sudeste" & MOTIVO_INTERNACAO == "Aborto") %>%
  group_by(TIPO_UNIDADE) %>%
  tally %>%
  adorn_totals("row")

aborto_sudeste <- full_join(aborto_sudeste_mmg, aborto_sudeste_sih, by = "TIPO_UNIDADE")

aborto_sudeste[is.na(aborto_sudeste)] <- 0

colnames(aborto_sudeste) <- c("Tipo Unidade", "MMG", "SIH")


# Complicações na gestação

comp_gest_sudeste_sih <- primeira_AIH %>%
  filter(REGIAO == "Região Sudeste" & MOTIVO_INTERNACAO == "Complicações na gestação") %>%
  group_by(TIPO_UNIDADE) %>%
  tally %>%
  adorn_totals("row")


comp_gest_sudeste_mmg <- mmg2 %>%
  filter(REGIAO == "Região Sudeste" & MOTIVO_INTERNACAO == "Complicações na gestação") %>%
  group_by(TIPO_UNIDADE) %>%
  tally %>%
  adorn_totals("row")

comp_gest_sudeste <- full_join(comp_gest_sudeste_mmg, comp_gest_sudeste_sih, by = "TIPO_UNIDADE")

comp_gest_sudeste[is.na(comp_gest_sudeste)] <- 0

colnames(comp_gest_sudeste) <- c("Tipo Unidade", "MMG", "SIH")


# Complicações no puerpério

comp_puerp_sudeste_sih <- primeira_AIH %>%
  filter(REGIAO == "Região Sudeste" & MOTIVO_INTERNACAO == "Complicações no puerpério") %>%
  group_by(TIPO_UNIDADE) %>%
  tally %>%
  adorn_totals("row")


comp_puerp_sudeste_mmg <- mmg2 %>%
  filter(REGIAO == "Região Sudeste" & MOTIVO_INTERNACAO == "Complicações no puerpério") %>%
  group_by(TIPO_UNIDADE) %>%
  tally %>%
  adorn_totals("row")

comp_puerp_sudeste <- full_join(comp_puerp_sudeste_mmg, comp_puerp_sudeste_sih, by = "TIPO_UNIDADE")

comp_puerp_sudeste[is.na(comp_puerp_sudeste)] <- 0

colnames(comp_puerp_sudeste) <- c("Tipo Unidade", "MMG", "SIH")


# outros

outros_sudeste_sih <- primeira_AIH %>%
  filter(REGIAO == "Região Sudeste" & MOTIVO_INTERNACAO == "Outros") %>%
  group_by(TIPO_UNIDADE) %>%
  tally %>%
  adorn_totals("row")


outros_sudeste_mmg <- mmg2 %>%
  filter(REGIAO == "Região Sudeste" & MOTIVO_INTERNACAO == "Outros") %>%
  group_by(TIPO_UNIDADE) %>%
  tally %>%
  adorn_totals("row")

outros_sudeste <- full_join(outros_sudeste_mmg, outros_sudeste_sih, by = "TIPO_UNIDADE")

outros_sudeste[is.na(outros_sudeste)] <- 0

colnames(outros_sudeste) <- c("Tipo Unidade", "MMG", "SIH")

# Em branco

branco_sudeste_sih <- primeira_AIH %>%
  filter(REGIAO == "Região Sudeste" & MOTIVO_INTERNACAO == "Em branco") %>%
  group_by(TIPO_UNIDADE) %>%
  tally %>%
  adorn_totals("row")


branco_sudeste_mmg <- mmg2 %>%
  filter(REGIAO == "Região Sudeste" & MOTIVO_INTERNACAO == "Em branco") %>%
  group_by(TIPO_UNIDADE) %>%
  tally %>%
  adorn_totals("row")

branco_sudeste <- full_join(branco_sudeste_mmg, branco_sudeste_sih, by = "TIPO_UNIDADE")

branco_sudeste[is.na(branco_sudeste)] <- 0

colnames(branco_sudeste) <- c("Tipo Unidade", "MMG", "SIH")

motivo_sudeste <- parto_sudeste %>%
  full_join(aborto_sudeste, by = "Tipo Unidade") %>%
  full_join(comp_gest_sudeste, by = "Tipo Unidade") %>%
  full_join(comp_puerp_sudeste, by = "Tipo Unidade") %>%
  full_join(outros_sudeste, by = "Tipo Unidade") %>%
  full_join(branco_sudeste, by = "Tipo Unidade")

colnames(motivo_sudeste) <- c("Tipo Unidade", "MMG", "SIH", "MMG", "SIH", "MMG", "SIH", "MMG", "SIH",
                          "MMG", "SIH", "MMG", "SIH")




##### Região Centro-Oeste

# parto


parto_centro_oeste_sih <- primeira_AIH %>%
  filter(REGIAO == "Região Centro-Oeste" & MOTIVO_INTERNACAO == "Parto") %>%
  group_by(TIPO_UNIDADE) %>%
  tally %>%
  adorn_totals("row")


parto_centro_oeste_mmg <- mmg2 %>%
  filter(REGIAO == "Região Centro-Oeste" & MOTIVO_INTERNACAO == "Parto") %>%
  group_by(TIPO_UNIDADE) %>%
  tally %>%
  adorn_totals("row")

parto_centro_oeste <- full_join(parto_centro_oeste_mmg, parto_centro_oeste_sih, by = "TIPO_UNIDADE")

colnames(parto_centro_oeste) <- c("TIPO_UNIDADE", "MMG", "SIH")

data <- data.frame(TIPO_UNIDADE = c("Unidades Privadas"),
                   MMG = c(0),
                   SIH = c(0))


parto_centro_oeste <- rbind(data, parto_centro_oeste)

colnames(parto_centro_oeste) <- c("Tipo Unidade", "MMG", "SIH")

# Aborto

aborto_centro_oeste_sih <- primeira_AIH %>%
  filter(REGIAO == "Região Centro-Oeste" & MOTIVO_INTERNACAO == "Aborto") %>%
  group_by(TIPO_UNIDADE) %>%
  tally %>%
  adorn_totals("row")


aborto_centro_oeste_mmg <- mmg2 %>%
  filter(REGIAO == "Região Centro-Oeste" & MOTIVO_INTERNACAO == "Aborto") %>%
  group_by(TIPO_UNIDADE) %>%
  tally %>%
  adorn_totals("row")

aborto_centro_oeste <- full_join(aborto_centro_oeste_mmg, aborto_centro_oeste_sih, by = "TIPO_UNIDADE")

aborto_centro_oeste[is.na(aborto_centro_oeste)] <- 0

colnames(aborto_centro_oeste) <- c("TIPO_UNIDADE", "MMG", "SIH")

data <- data.frame(TIPO_UNIDADE = c("Unidades Privadas"),
                   MMG = c(0),
                   SIH = c(0))


aborto_centro_oeste <- rbind(data, aborto_centro_oeste)

colnames(aborto_centro_oeste) <- c("Tipo Unidade", "MMG", "SIH")


# Complicações na gestação

comp_gest_centro_oeste_sih <- primeira_AIH %>%
  filter(REGIAO == "Região Centro-Oeste" & MOTIVO_INTERNACAO == "Complicações na gestação") %>%
  group_by(TIPO_UNIDADE) %>%
  tally %>%
  adorn_totals("row")


comp_gest_centro_oeste_mmg <- mmg2 %>%
  filter(REGIAO == "Região Centro-Oeste" & MOTIVO_INTERNACAO == "Complicações na gestação") %>%
  group_by(TIPO_UNIDADE) %>%
  tally %>%
  adorn_totals("row")

comp_gest_centro_oeste <- full_join(comp_gest_centro_oeste_mmg, comp_gest_centro_oeste_sih, by = "TIPO_UNIDADE")

comp_gest_centro_oeste[is.na(comp_gest_centro_oeste)] <- 0

colnames(comp_gest_centro_oeste) <- c("TIPO_UNIDADE", "MMG", "SIH")

data <- data.frame(TIPO_UNIDADE = c("Unidades Privadas"),
                   MMG = c(0),
                   SIH = c(0))


comp_gest_centro_oeste <- rbind(data, comp_gest_centro_oeste)

colnames(comp_gest_centro_oeste) <- c("Tipo Unidade", "MMG", "SIH")


# Complicações no puerpério

comp_puerp_centro_oeste_sih <- primeira_AIH %>%
  filter(REGIAO == "Região Centro-Oeste" & MOTIVO_INTERNACAO == "Complicações no puerpério") %>%
  group_by(TIPO_UNIDADE) %>%
  tally %>%
  adorn_totals("row")


comp_puerp_centro_oeste_mmg <- mmg2 %>%
  filter(REGIAO == "Região Centro-Oeste" & MOTIVO_INTERNACAO == "Complicações no puerpério") %>%
  group_by(TIPO_UNIDADE) %>%
  tally %>%
  adorn_totals("row")

comp_puerp_centro_oeste <- full_join(comp_puerp_centro_oeste_mmg, comp_puerp_centro_oeste_sih, by = "TIPO_UNIDADE")

comp_puerp_centro_oeste[is.na(comp_puerp_centro_oeste)] <- 0

colnames(comp_puerp_centro_oeste) <- c("TIPO_UNIDADE", "MMG", "SIH")

data <- data.frame(TIPO_UNIDADE = c("Unidades Privadas"),
                   MMG = c(0),
                   SIH = c(0))


comp_puerp_centro_oeste <- rbind(data, comp_puerp_centro_oeste)

colnames(comp_puerp_centro_oeste) <- c("Tipo Unidade", "MMG", "SIH")


# outros

outros_centro_oeste_sih <- primeira_AIH %>%
  filter(REGIAO == "Região Centro-Oeste" & MOTIVO_INTERNACAO == "Outros") %>%
  group_by(TIPO_UNIDADE) %>%
  tally %>%
  adorn_totals("row")


outros_centro_oeste_mmg <- mmg2 %>%
  filter(REGIAO == "Região Centro-Oeste" & MOTIVO_INTERNACAO == "Outros") %>%
  group_by(TIPO_UNIDADE) %>%
  tally %>%
  adorn_totals("row")

outros_centro_oeste <- full_join(outros_centro_oeste_mmg, outros_centro_oeste_sih, by = "TIPO_UNIDADE")

colnames(outros_centro_oeste) <- c("TIPO_UNIDADE", "MMG", "SIH")

data <- data.frame(TIPO_UNIDADE = c("Unidades Privadas"),
                   MMG = c(0),
                   SIH = c(0))


outros_centro_oeste <- rbind(data, outros_centro_oeste)

outros_centro_oeste[is.na(outros_centro_oeste)] <- 0

colnames(outros_centro_oeste) <- c("Tipo Unidade", "MMG", "SIH")

# Em branco

branco_centro_oeste_sih <- primeira_AIH %>%
  filter(REGIAO == "Região Centro-Oeste" & MOTIVO_INTERNACAO == "Em branco") %>%
  group_by(TIPO_UNIDADE) %>%
  tally %>%
  adorn_totals("row")


branco_centro_oeste_mmg <- mmg2 %>%
  filter(REGIAO == "Região Centro-Oeste" & MOTIVO_INTERNACAO == "Em branco") %>%
  group_by(TIPO_UNIDADE) %>%
  tally %>%
  adorn_totals("row")

branco_centro_oeste <- full_join(branco_centro_oeste_mmg, branco_centro_oeste_sih, by = "TIPO_UNIDADE")

branco_centro_oeste[is.na(branco_centro_oeste)] <- 0

colnames(branco_centro_oeste) <- c("TIPO_UNIDADE", "MMG", "SIH")

data <- data.frame(TIPO_UNIDADE = c("Unidades Privadas"),
                   MMG = c(0),
                   SIH = c(0))


branco_centro_oeste<- rbind(data, branco_centro_oeste)

colnames(branco_centro_oeste) <- c("Tipo Unidade", "MMG", "SIH")


motivo_centro_oeste <- parto_centro_oeste %>%
  full_join(aborto_centro_oeste, by = "Tipo Unidade") %>%
  full_join(comp_gest_centro_oeste, by = "Tipo Unidade") %>%
  full_join(comp_puerp_centro_oeste, by = "Tipo Unidade") %>%
  full_join(outros_centro_oeste, by = "Tipo Unidade") %>%
  full_join(branco_centro_oeste, by = "Tipo Unidade")

colnames(motivo_centro_oeste) <- c("Tipo Unidade", "MMG", "SIH", "MMG", "SIH", "MMG", "SIH", "MMG", "SIH",
                              "MMG", "SIH", "MMG", "SIH")


motivo_centro_oeste[is.na(motivo_centro_oeste)] <- 0


##### Região Brasil

# parto


parto_br_sih <- primeira_AIH %>%
  filter( MOTIVO_INTERNACAO == "Parto") %>%
  group_by(TIPO_UNIDADE) %>%
  tally %>%
  adorn_totals("row")


parto_br_mmg <- mmg2 %>%
  filter( MOTIVO_INTERNACAO == "Parto") %>%
  group_by(TIPO_UNIDADE) %>%
  tally %>%
  adorn_totals("row")

parto_br <- full_join(parto_br_mmg, parto_br_sih, by = "TIPO_UNIDADE")

colnames(parto_br) <- c("Tipo Unidade", "MMG", "SIH")


# Aborto

aborto_br_sih <- primeira_AIH %>%
  filter( MOTIVO_INTERNACAO == "Aborto") %>%
  group_by(TIPO_UNIDADE) %>%
  tally %>%
  adorn_totals("row")


aborto_br_mmg <- mmg2 %>%
  filter( MOTIVO_INTERNACAO == "Aborto") %>%
  group_by(TIPO_UNIDADE) %>%
  tally %>%
  adorn_totals("row")

aborto_br <- full_join(aborto_br_mmg, aborto_br_sih, by = "TIPO_UNIDADE")

aborto_br[is.na(aborto_br)] <- 0

colnames(aborto_br) <- c("Tipo Unidade", "MMG", "SIH")


# Complicações na gestação

comp_gest_br_sih <- primeira_AIH %>%
  filter( MOTIVO_INTERNACAO == "Complicações na gestação") %>%
  group_by(TIPO_UNIDADE) %>%
  tally %>%
  adorn_totals("row")


comp_gest_br_mmg <- mmg2 %>%
  filter( MOTIVO_INTERNACAO == "Complicações na gestação") %>%
  group_by(TIPO_UNIDADE) %>%
  tally %>%
  adorn_totals("row")

comp_gest_br <- full_join(comp_gest_br_mmg, comp_gest_br_sih, by = "TIPO_UNIDADE")

comp_gest_br[is.na(comp_gest_br)] <- 0

colnames(comp_gest_br) <- c("Tipo Unidade", "MMG", "SIH")


# Complicações no puerpério

comp_puerp_br_sih <- primeira_AIH %>%
  filter( MOTIVO_INTERNACAO == "Complicações no puerpério") %>%
  group_by(TIPO_UNIDADE) %>%
  tally %>%
  adorn_totals("row")


comp_puerp_br_mmg <- mmg2 %>%
  filter( MOTIVO_INTERNACAO == "Complicações no puerpério") %>%
  group_by(TIPO_UNIDADE) %>%
  tally %>%
  adorn_totals("row")

comp_puerp_br <- full_join(comp_puerp_br_mmg, comp_puerp_br_sih, by = "TIPO_UNIDADE")

comp_puerp_br[is.na(comp_puerp_br)] <- 0

colnames(comp_puerp_br) <- c("Tipo Unidade", "MMG", "SIH")


# outros

outros_br_sih <- primeira_AIH %>%
  filter( MOTIVO_INTERNACAO == "Outros") %>%
  group_by(TIPO_UNIDADE) %>%
  tally %>%
  adorn_totals("row")


outros_br_mmg <- mmg2 %>%
  filter( MOTIVO_INTERNACAO == "Outros") %>%
  group_by(TIPO_UNIDADE) %>%
  tally %>%
  adorn_totals("row")

outros_br <- full_join(outros_br_mmg, outros_br_sih, by = "TIPO_UNIDADE")

outros_br[is.na(outros_br)] <- 0

colnames(outros_br) <- c("Tipo Unidade", "MMG", "SIH")

# Em branco

branco_br_sih <- primeira_AIH %>%
  filter( MOTIVO_INTERNACAO == "Em branco") %>%
  group_by(TIPO_UNIDADE) %>%
  tally %>%
  adorn_totals("row")


branco_br_mmg <- mmg2 %>%
  filter( MOTIVO_INTERNACAO == "Em branco") %>%
  group_by(TIPO_UNIDADE) %>%
  tally %>%
  adorn_totals("row")

branco_br <- full_join(branco_br_mmg, branco_br_sih, by = "TIPO_UNIDADE")

branco_br[is.na(branco_br)] <- 0

colnames(branco_br) <- c("Tipo Unidade", "MMG", "SIH")

motivo_br <- parto_br %>%
  full_join(aborto_br, by = "Tipo Unidade") %>%
  full_join(comp_gest_br, by = "Tipo Unidade") %>%
  full_join(comp_puerp_br, by = "Tipo Unidade") %>%
  full_join(outros_br, by = "Tipo Unidade") %>%
  full_join(branco_br, by = "Tipo Unidade")

colnames(motivo_br) <- c("Tipo Unidade", "MMG", "SIH", "MMG", "SIH", "MMG", "SIH", "MMG", "SIH",
                              "MMG", "SIH", "MMG", "SIH")


tabelao_motivo_inter <- rbind(motivo_norte, motivo_nordeste, motivo_sul,
                              motivo_sudeste, motivo_centro_oeste, motivo_br)

tabela_motivo_inter <- kbl(tabelao_motivo_inter, align = "lcccccccccccc") %>%
  add_header_above(c(" " = 1, "Parto" = 2, "Aborto" = 2, "Inter. Gestação" = 2,
                     "Inter. Puerpério" = 2, "Outros" = 2, "Em Branco" = 2)) %>%
  kable_classic(full_width = F) %>%
  pack_rows("Região Norte", 1, 3) %>%
  pack_rows("Região Nordeste", 4, 6) %>%
  pack_rows("Região Sul", 7, 9) %>%
  pack_rows("Região Sudeste", 10, 12) %>%
  pack_rows("Região Centro-Oeste", 13, 15) %>%
  pack_rows("Brasil", 16, 18)

return(tabela_motivo_inter)
}

motivo_internacao_tab(sih, mmg)


##############################################################################################################



