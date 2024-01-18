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
library(caret)


### abrindo a base linkada

found_all <- import("Linkage MMG x SIH - encontrados_21.04.23.csv")


############################################################################################################

#### Especificidade = possibilidade de dar negativo quando realmente é negativo
#### Sensibilidade = possibilidade de dar positivo quando realmente é positivo
#### VPP = é a probabilidade de um indivíduo avaliado e com resultado positivo ser realmente doente
#### VPN = é a probabilidade de um indivíduo avaliado e com resultado negativo ser realmente normal


### Transformando em factor

found_all$mmg_total[is.na(found_all$mmg_total)] <- 0

found_all$nmm_total <- as.factor(found_all$nmm_total)
found_all$mmg_total <- as.factor(found_all$mmg_total)

### Arrumando os levels

found_all$nmm_total <- factor(found_all$nmm_total, levels = c("1", "0"))
found_all$mmg_total <- factor(found_all$mmg_total, levels = c("1", "0"))


##### Ajeitando as variaveis do SIH

found_all$CPAV_SIH_BR_ORIGINAL <- factor(found_all$CPAV_SIH_BR_ORIGINAL, levels = c("1", "0"))
found_all$CPAV_SIH_BR_SP_ORIGINAL <- factor(found_all$CPAV_SIH_BR_SP_ORIGINAL, levels = c("1", "0"))
found_all$CPAV_SIH_TMP_BR <- factor(found_all$CPAV_SIH_TMP_BR, levels = c("1", "0"))
found_all$CPAV_SIH_TMP_BR_SP <- factor(found_all$CPAV_SIH_TMP_BR_SP, levels = c("1", "0"))
found_all$CPAV_SIH_TMP_IC_BR <- factor(found_all$CPAV_SIH_TMP_IC_BR, levels = c("1", "0"))
found_all$CPAV_SIH_TMP_IC_BR_SP <- factor(found_all$CPAV_SIH_TMP_IC_BR_SP, levels = c("1", "0"))
found_all$CPAV_SIH_TMP_IC_HAa_BR <- factor(found_all$CPAV_SIH_TMP_IC_HAa_BR, levels = c("1", "0"))
found_all$CPAV_SIH_TMP_IC_HAa_BR_SP <- factor(found_all$CPAV_SIH_TMP_IC_HAa_BR_SP, levels = c("1", "0"))
found_all$CPAV_SIH_TMP_IC_HAd_BR <- factor(found_all$CPAV_SIH_TMP_IC_HAd_BR, levels = c("1", "0"))
found_all$CPAV_SIH_TMP_IC_HAd_BR_SP <- factor(found_all$CPAV_SIH_TMP_IC_HAd_BR_SP, levels = c("1", "0"))
found_all$CPAV_SIH_TMP_IC_HAe_BR <- factor(found_all$CPAV_SIH_TMP_IC_HAe_BR, levels = c("1", "0"))
found_all$CPAV_SIH_TMP_IC_HAe_BR_SP <- factor(found_all$CPAV_SIH_TMP_IC_HAe_BR_SP, levels = c("1", "0"))


### MMG x INTUBACAO BR

found_all$CPAV_SIH_TMP_IC_HAd_ENTUB_BR[is.na(found_all$CPAV_SIH_TMP_IC_HAd_INTUB_BR)] <- 0
found_all$CPAV_SIH_TMP_IC_HAd_ENTUB_BR <- factor(found_all$CPAV_SIH_TMP_IC_HAd_INTUB_BR, levels = c("1", "0"))

act <- found_all$CPAV_SIH_TMP_IC_HAd_INTUB_BR
ref <- found_all$mmg_total #referencia

tab <- table(act,ref)

confusionMatrix(tab)


### MMG x INTUBACAO BR_SP

found_all$CPAV_SIH_TMP_IC_HAd_ENTUB_BR_SP[is.na(found_all$CPAV_SIH_TMP_IC_HAd_INTUB_BR_SP)] <- 0
found_all$CPAV_SIH_TMP_IC_HAd_ENTUB_BR_SP <- factor(found_all$CPAV_SIH_TMP_IC_HAd_INTUB_BR_SP, levels = c("1", "0"))

act <- found_all$CPAV_SIH_TMP_IC_HAd_INTUB_BR_SP
ref <- found_all$mmg_total #referencia

tab <- table(act,ref)

confusionMatrix(tab)


##### cpav intubação

summary.factor(found_all$CPAV_INTUB_BR)
summary.factor(found_all$mmg_intub)

found_all$mmg_intub[is.na(found_all$mmg_intub)] <- 0

found_all$mmg_intub <- factor(found_all$mmg_intub, levels = c("1", "0"))
found_all$CPAV_INTUB_BR <- factor(found_all$CPAV_INTUB_BR, levels = c("1", "0"))


act <- found_all$CPAV_INTUB_BR
ref <- found_all$mmg_intub #referencia

tab <- table(act,ref)

confusionMatrix(tab)



#########################################################################################################

######### critérios especificos

# Descolamento prematuro de placenta

found_all$mmg_dpp[is.na(found_all$mmg_dpp)] <- 0

found_all$mmg_dpp <- factor(found_all$mmg_dpp, levels = c("1", "0"))
found_all$CPAV_DESC_PLAC_SIH_BR <- factor(found_all$CPAV_DESC_PLAC_SIH_BR, levels = c("1", "0"))


act <- found_all$CPAV_DESC_PLAC_SIH_BR
ref <- found_all$mmg_dpp #referencia

tab <- table(act,ref)

confusionMatrix(tab)

summary.factor(found_all$CPAV_DESC_PLAC_SIH_BR)
summary.factor(found_all$mmg_dpp)
 

# Placenta acreta, increta ou percreta

found_all$mmg_aipcreta[is.na(found_all$mmg_aipcreta)] <- 0

summary.factor(found_all$mmg_aipcreta)


# Gravidez ectópica

found_all$mmg_ectopica[is.na(found_all$mmg_ectopica)] <- 0

found_all$mmg_ectopica <- factor(found_all$mmg_ectopica, levels = c("1", "0"))
found_all$CPAV_ECTOPICA_SIH_BR <- factor(found_all$CPAV_ECTOPICA_SIH_BR, levels = c("1", "0"))


act <- found_all$CPAV_ECTOPICA_SIH_BR
ref <- found_all$mmg_ectopica #referencia

tab <- table(act,ref)

confusionMatrix(tab)

summary.factor(found_all$CPAV_ECTOPICA_SIH_BR)
summary.factor(found_all$mmg_ectopica)


# Hemorragia pós-parto

found_all$mmg_hemorragia[is.na(found_all$mmg_hemorragia)] <- 0

found_all$mmg_hemorragia <- factor(found_all$mmg_hemorragia, levels = c("1", "0"))
found_all$CPAV_HEMORRAGIA_SIH_BR <- factor(found_all$CPAV_HEMORRAGIA_SIH_BR, levels = c("1", "0"))


act <- found_all$CPAV_HEMORRAGIA_SIH_BR
ref <- found_all$mmg_hemorragia #referencia

tab <- table(act,ref)

confusionMatrix(tab)

summary.factor(found_all$CPAV_HEMORRAGIA_SIH_BR)
summary.factor(found_all$mmg_hemorragia)


# Rotura uterina

found_all$mmg_ruterina[is.na(found_all$mmg_ruterina)] <- 0

found_all$mmg_ruterina <- factor(found_all$mmg_ruterina, levels = c("1", "0"))
found_all$CPAV_ROTURA_SIH_BR <- factor(found_all$CPAV_ROTURA_SIH_BR, levels = c("1", "0"))


act <- found_all$CPAV_ROTURA_SIH_BR
ref <- found_all$mmg_ruterina #referencia

tab <- table(act,ref)

confusionMatrix(tab)

summary.factor(found_all$CPAV_ROTURA_SIH_BR)
summary.factor(found_all$mmg_ruterina)


# Total hemorrágicos

# criando total de hemorrágicas para o mmg e sih

found_all$mmg_total_hemorragicas <- ifelse(found_all$mmg_dpp == "1" |
                                           found_all$mmg_aipcreta == "1" |
                                           found_all$mmg_ectopica == "1" |
                                           found_all$mmg_hemorragia == "1" |
                                           found_all$mmg_ruterina == "1", "1", "0")

found_all$mmg_total_hemorragicas <- factor(found_all$mmg_total_hemorragicas, levels = c("1", "0"))
found_all$SIH_HEMORRAGICAS_BR <- factor(found_all$SIH_HEMORRAGICAS_BR, levels = c("1", "0"))


act <- found_all$SIH_HEMORRAGICAS_BR
ref <- found_all$mmg_total_hemorragicas #referencia

tab <- table(act,ref)

confusionMatrix(tab)

summary.factor(found_all$SIH_HEMORRAGICAS_BR)
summary.factor(found_all$mmg_total_hemorragicas)


# Pré-eclâmpsia grave

found_all$mmg_pegrave[is.na(found_all$mmg_pegrave)] <- 0

found_all$mmg_pegrave <- factor(found_all$mmg_pegrave, levels = c("1", "0"))
found_all$CPAV_PRE_ECLAMP_SIH_BR <- factor(found_all$CPAV_PRE_ECLAMP_SIH_BR, levels = c("1", "0"))


act <- found_all$CPAV_PRE_ECLAMP_SIH_BR
ref <- found_all$mmg_pegrave #referencia

tab <- table(act,ref)

confusionMatrix(tab)

summary.factor(found_all$CPAV_PRE_ECLAMP_SIH_BR)
summary.factor(found_all$mmg_pegrave)



# Eclâmpsia

found_all$mmg_eclampsia[is.na(found_all$mmg_eclampsia)] <- 0

found_all$mmg_eclampsia <- factor(found_all$mmg_eclampsia, levels = c("1", "0"))
found_all$CPAV_ECLAMPSIA_SIH_BR <- factor(found_all$CPAV_ECLAMPSIA_SIH_BR, levels = c("1", "0"))


act <- found_all$CPAV_ECLAMPSIA_SIH_BR
ref <- found_all$mmg_eclampsia #referencia

tab <- table(act,ref)

confusionMatrix(tab)

summary.factor(found_all$CPAV_ECLAMPSIA_SIH_BR)
summary.factor(found_all$mmg_eclampsia)



# Hipertensão grave

found_all$mmg_hagrave[is.na(found_all$mmg_hagrave)] <- 0

found_all$mmg_hagrave <- factor(found_all$mmg_hagrave, levels = c("1", "0"))
found_all$CPAV_HIPER_GRAVE_SIH_BR <- factor(found_all$CPAV_HIPER_GRAVE_SIH_BR, levels = c("1", "0"))


act <- found_all$CPAV_HIPER_GRAVE_SIH_BR
ref <- found_all$mmg_hagrave #referencia

tab <- table(act,ref)

confusionMatrix(tab)

summary.factor(found_all$CPAV_HIPER_GRAVE_SIH_BR)
summary.factor(found_all$mmg_hagrave)


# Encefalopatia hipertensiva

found_all$mmg_enchipert[is.na(found_all$mmg_enchipert)] <- 0

found_all$mmg_enchipert <- factor(found_all$mmg_enchipert, levels = c("1", "0"))
found_all$CPAV_ENCEF_HIPER_SIH_BR <- factor(found_all$CPAV_ENCEF_HIPER_SIH_BR, levels = c("1", "0"))


act <- found_all$CPAV_ENCEF_HIPER_SIH_BR
ref <- found_all$mmg_enchipert #referencia

tab <- table(act,ref)

confusionMatrix(tab)

summary.factor(found_all$CPAV_ENCEF_HIPER_SIH_BR)
summary.factor(found_all$mmg_enchipert)


# Síndrome HELLP

found_all$mmg_hellp[is.na(found_all$mmg_hellp)] <- 0

found_all$mmg_hellp <- factor(found_all$mmg_hellp, levels = c("1", "0"))
found_all$CPAV_HELLP_SIH_BR <- factor(found_all$CPAV_HELLP_SIH_BR, levels = c("1", "0"))


act <- found_all$CPAV_HELLP_SIH_BR
ref <- found_all$mmg_hellp #referencia

tab <- table(act,ref)

confusionMatrix(tab)

summary.factor(found_all$CPAV_HELLP_SIH_BR)
summary.factor(found_all$mmg_hellp)


# total hipertensivas

# varivel total pro mmg

found_all$mmg_total_hipertensivas <- ifelse(found_all$mmg_pegrave == "1" |
                                             found_all$mmg_eclampsia == "1" |
                                             found_all$mmg_hagrave == "1" |
                                             found_all$mmg_enchipert == "1" |
                                             found_all$mmg_hellp == "1", "1", "0")


found_all$mmg_total_hipertensivas <- factor(found_all$mmg_total_hipertensivas, levels = c("1", "0"))
found_all$CPAV_HA_SIH_BR <- factor(found_all$CPAV_HA_SIH_BR , levels = c("1", "0"))

act <- found_all$CPAV_HA_SIH_BR 
ref <- found_all$mmg_total_hipertensivas #referencia

tab <- table(act,ref)

confusionMatrix(tab)

summary.factor(found_all$CPAV_HA_SIH_BR)
summary.factor(found_all$mmg_total_hipertensivas)


# Total hipertensivas(incluindo outras gestacionais)

found_all$mmg_total_hipertensivas <- factor(found_all$mmg_total_hipertensivas, levels = c("1", "0"))
found_all$CPAV_HA_SIHa <- factor(found_all$CPAV_HA_SIHa, levels = c("1", "0"))

act <- found_all$CPAV_HA_SIHa
ref <- found_all$mmg_total_hipertensivas #referencia

tab <- table(act,ref)

confusionMatrix(tab)

summary.factor(found_all$CPAV_HA_SIHa)
summary.factor(found_all$mmg_total_hipertensivas)


# Total hipertensivas(incluindo crônicas)

found_all$mmg_total_hipertensivas <- factor(found_all$mmg_total_hipertensivas, levels = c("1", "0"))
found_all$CPAV_HA_SIHb <- factor(found_all$CPAV_HA_SIHb, levels = c("1", "0"))

act <- found_all$CPAV_HA_SIHb
ref <- found_all$mmg_total_hipertensivas #referencia

tab <- table(act,ref)

confusionMatrix(tab)

summary.factor(found_all$CPAV_HA_SIHb)
summary.factor(found_all$mmg_total_hipertensivas)


# Total hipertensivas(incluindo cronicas sem essenciais)

found_all$mmg_total_hipertensivas <- factor(found_all$mmg_total_hipertensivas, levels = c("1", "0"))
found_all$CPAV_HA_SIHc <- factor(found_all$CPAV_HA_SIHc, levels = c("1", "0"))

act <- found_all$CPAV_HA_SIHc
ref <- found_all$mmg_total_hipertensivas #referencia

tab <- table(act,ref)

confusionMatrix(tab)

summary.factor(found_all$CPAV_HA_SIHc)
summary.factor(found_all$mmg_total_hipertensivas)


# Total hipertensivas(incluindo outras gestacionais e cronicas)

found_all$mmg_total_hipertensivas <- factor(found_all$mmg_total_hipertensivas, levels = c("1", "0"))
found_all$CPAV_HA_SIHd <- factor(found_all$CPAV_HA_SIHd, levels = c("1", "0"))

act <- found_all$CPAV_HA_SIHd
ref <- found_all$mmg_total_hipertensivas #referencia

tab <- table(act,ref)

confusionMatrix(tab)

summary.factor(found_all$CPAV_HA_SIHd)
summary.factor(found_all$mmg_total_hipertensivas)


# Total hipertensivas(incluindo outras gestacionais e cronicas, sem essenciais)

found_all$mmg_total_hipertensivas <- factor(found_all$mmg_total_hipertensivas, levels = c("1", "0"))
found_all$CPAV_HA_SIHe <- factor(found_all$CPAV_HA_SIHe, levels = c("1", "0"))

act <- found_all$CPAV_HA_SIHe
ref <- found_all$mmg_total_hipertensivas #referencia

tab <- table(act,ref)

confusionMatrix(tab)

summary.factor(found_all$CPAV_HA_SIHe)
summary.factor(found_all$mmg_total_hipertensivas)


# Endometrite

found_all$mmg_endometrite[is.na(found_all$mmg_endometrite)] <- 0

found_all$mmg_endometrite <- factor(found_all$mmg_endometrite, levels = c("1", "0"))
found_all$CPAV_ENDOMETRITE_SIH_BR <- factor(found_all$CPAV_ENDOMETRITE_SIH_BR , levels = c("1", "0"))


act <- found_all$CPAV_ENDOMETRITE_SIH_BR 
ref <- found_all$mmg_endometrite #referencia

tab <- table(act,ref)

confusionMatrix(tab)

summary.factor(found_all$CPAV_ENDOMETRITE_SIH_BR )
summary.factor(found_all$mmg_endometrite)


# Edema pulmonar

found_all$mmg_edemap[is.na(found_all$mmg_edemap)] <- 0

found_all$mmg_edemap <- factor(found_all$mmg_edemap, levels = c("1", "0"))
found_all$CPAV_EDEMA_PULM_SIH_BR <- factor(found_all$CPAV_EDEMA_PULM_SIH_BR , levels = c("1", "0"))


act <- found_all$CPAV_EDEMA_PULM_SIH_BR 
ref <- found_all$mmg_edemap #referencia

tab <- table(act,ref)

confusionMatrix(tab)

summary.factor(found_all$CPAV_EDEMA_PULM_SIH_BR )
summary.factor(found_all$mmg_edemap)


# Insuficiência respiratória

found_all$mmg_irpa[is.na(found_all$mmg_irpa)] <- 0

found_all$mmg_irpa <- factor(found_all$mmg_irpa, levels = c("1", "0"))
found_all$CPAV_INSU_RESP_SIH_BR <- factor(found_all$CPAV_INSU_RESP_SIH_BR , levels = c("1", "0"))


act <- found_all$CPAV_INSU_RESP_SIH_BR 
ref <- found_all$mmg_irpa #referencia

tab <- table(act,ref)

confusionMatrix(tab)

summary.factor(found_all$CPAV_INSU_RESP_SIH_BR )
summary.factor(found_all$mmg_irpa)


# Convulsões

found_all$mmg_convulsoes[is.na(found_all$mmg_convulsoes)] <- 0

found_all$mmg_convulsoes <- factor(found_all$mmg_convulsoes, levels = c("1", "0"))
found_all$CPAV_CONVULSOES_SIH_BR <- factor(found_all$CPAV_CONVULSOES_SIH_BR , levels = c("1", "0"))


act <- found_all$CPAV_CONVULSOES_SIH_BR 
ref <- found_all$mmg_convulsoes #referencia

tab <- table(act,ref)

confusionMatrix(tab)

summary.factor(found_all$CPAV_CONVULSOES_SIH_BR )
summary.factor(found_all$mmg_convulsoes)


# Sepse

found_all$mmg_sepse[is.na(found_all$mmg_sepse)] <- 0

found_all$mmg_sepse <- factor(found_all$mmg_sepse, levels = c("1", "0"))
found_all$CPAV_SEPSE_SIH_BR <- factor(found_all$CPAV_SEPSE_SIH_BR  , levels = c("1", "0"))


act <- found_all$CPAV_SEPSE_SIH_BR 
ref <- found_all$mmg_sepse #referencia

tab <- table(act,ref)

confusionMatrix(tab)

summary.factor(found_all$CPAV_SEPSE_SIH_BR  )
summary.factor(found_all$mmg_sepse)


# Choque

found_all$mmg_choque[is.na(found_all$mmg_choque)] <- 0

found_all$mmg_choque <- factor(found_all$mmg_choque, levels = c("1", "0"))
found_all$CPAV_CHOQUE_SIH_BR <- factor(found_all$CPAV_CHOQUE_SIH_BR   , levels = c("1", "0"))


act <- found_all$CPAV_CHOQUE_SIH_BR 
ref <- found_all$mmg_choque #referencia

tab <- table(act,ref)

confusionMatrix(tab)

summary.factor(found_all$CPAV_CHOQUE_SIH_BR)
summary.factor(found_all$mmg_choque)


# Trombocitopenia < 100.000

found_all$mmg_tcp[is.na(found_all$mmg_tcp)] <- 0

found_all$mmg_tcp <- factor(found_all$mmg_tcp, levels = c("1", "0"))

summary.factor(found_all$mmg_tcp)


# Crise tireotóxica

found_all$mmg_ct[is.na(found_all$mmg_ct)] <- 0

found_all$mmg_ct <- factor(found_all$mmg_ct, levels = c("1", "0"))
found_all$CPAV_CRISE_TIREO_SIH_BR <- factor(found_all$CPAV_CRISE_TIREO_SIH_BR, levels = c("1", "0"))


act <- found_all$CPAV_CRISE_TIREO_SIH_BR
ref <- found_all$mmg_ct #referencia

tab <- table(act,ref)

confusionMatrix(tab)

summary.factor(found_all$CPAV_CRISE_TIREO_SIH_BR)
summary.factor(found_all$mmg_ct)


# total outros

# criando total outros

found_all$mmg_total_outros <- ifelse(found_all$mmg_endometrite == "1" |
                                     found_all$mmg_edemap == "1" |
                                     found_all$mmg_irpa == "1" |
                                     found_all$mmg_convulsoes == "1" |
                                     found_all$mmg_sepse == "1" |
                                     found_all$mmg_choque == "1" |
                                     found_all$mmg_tcp == "1" |
                                     found_all$mmg_ct == "1", "1", "0")

found_all$mmg_total_outros <- factor(found_all$mmg_total_outros, levels = c("1", "0"))
found_all$SIH_OUTROS_BR <- factor(found_all$SIH_OUTROS_BR, levels = c("1", "0"))


act <- found_all$SIH_OUTROS_BR
ref <- found_all$mmg_total_outros #referencia

tab <- table(act,ref)

confusionMatrix(tab)

summary.factor(found_all$SIH_OUTROS_BR)
summary.factor(found_all$mmg_total_outros)


# Transfusão sanguínea

found_all$mmg_transfusao[is.na(found_all$mmg_transfusao)] <- 0

found_all$mmg_transfusao <- factor(found_all$mmg_transfusao, levels = c("1", "0"))
found_all$CPAV_TRANSFUSAO_SIH_BR <- factor(found_all$CPAV_TRANSFUSAO_SIH_BR, levels = c("1", "0"))


act <- found_all$CPAV_TRANSFUSAO_SIH_BR
ref <- found_all$mmg_transfusao #referencia

tab <- table(act,ref)

confusionMatrix(tab)

summary.factor(found_all$CPAV_TRANSFUSAO_SIH_BR)
summary.factor(found_all$mmg_transfusao)


# Acesso venoso central

found_all$mmg_acessovc[is.na(found_all$mmg_acessovc)] <- 0

found_all$mmg_acessovc <- factor(found_all$mmg_acessovc, levels = c("1", "0"))
found_all$CPAV_ACESSO_VENOSO_SIH_BR <- factor(found_all$CPAV_ACESSO_VENOSO_SIH_BR, levels = c("1", "0"))


act <- found_all$CPAV_ACESSO_VENOSO_SIH_BR
ref <- found_all$mmg_acessovc #referencia

tab <- table(act,ref)

confusionMatrix(tab)

summary.factor(found_all$CPAV_ACESSO_VENOSO_SIH_BR)
summary.factor(found_all$mmg_acessovc)


# Histerectomia

found_all$mmg_histerec[is.na(found_all$mmg_histerec)] <- 0

found_all$mmg_histerec <- factor(found_all$mmg_histerec, levels = c("1", "0"))
found_all$CPAV_HISTERECTOMIA_SIH_BR <- factor(found_all$CPAV_HISTERECTOMIA_SIH_BR, levels = c("1", "0"))


act <- found_all$CPAV_HISTERECTOMIA_SIH_BR
ref <- found_all$mmg_histerec #referencia

tab <- table(act,ref)

confusionMatrix(tab)

summary.factor(found_all$CPAV_HISTERECTOMIA_SIH_BR)
summary.factor(found_all$mmg_histerec)


# Admissão à unidade de tratamento intensivo

found_all$mmg_uti[is.na(found_all$mmg_uti)] <- 0

found_all$mmg_uti <- factor(found_all$mmg_uti, levels = c("1", "0"))
found_all$CPAV_UTI_SIH_BR <- factor(found_all$CPAV_UTI_SIH_BR, levels = c("1", "0"))


act <- found_all$CPAV_UTI_SIH_BR
ref <- found_all$mmg_uti #referencia

tab <- table(act,ref)

confusionMatrix(tab)

summary.factor(found_all$CPAV_UTI_SIH_BR)
summary.factor(found_all$mmg_uti)

# Internação hospitalar prolongada (considerando todas no SIH)

found_all$mmg_internprol[is.na(found_all$mmg_internprol)] <- 0

found_all$mmg_internprol <- factor(found_all$mmg_internprol, levels = c("1", "0"))
found_all$CPAV_INTER_7_DIAS_SIH_BR <- factor(found_all$CPAV_INTER_7_DIAS_SIH_BR, levels = c("1", "0"))


act <- found_all$CPAV_INTER_7_DIAS_SIH_BR
ref <- found_all$mmg_internprol #referencia

tab <- table(act,ref)

confusionMatrix(tab)


summary.factor(found_all$CPAV_INTER_7_DIAS_SIH_BR)
summary.factor(found_all$mmg_internprol)


# Internação hospitalar prolongada (apenas parto no SIH)

found_all$mmg_internprol[is.na(found_all$mmg_internprol)] <- 0

found_all$mmg_internprol <- factor(found_all$mmg_internprol, levels = c("1", "0"))
found_all$CPAV_INTER_7_DIAS_PARTO_SIH_BR <- factor(found_all$CPAV_INTER_7_DIAS_PARTO_SIH_BR, levels = c("1", "0"))


act <- found_all$CPAV_INTER_7_DIAS_PARTO_SIH_BR
ref <- found_all$mmg_internprol #referencia

tab <- table(act,ref)

confusionMatrix(tab)


summary.factor(found_all$CPAV_INTER_7_DIAS_PARTO_SIH_BR)
summary.factor(found_all$mmg_internprol)


# Intubação não anestésica

found_all$mmg_intub[is.na(found_all$mmg_intub)] <- 0

found_all$mmg_intub <- factor(found_all$mmg_intub, levels = c("1", "0"))

summary.factor(found_all$mmg_intub)


# Retorno à sala operatória (centro cirúrgico)

found_all$mmg_salacir[is.na(found_all$mmg_salacir)] <- 0

found_all$mmg_salacir <- factor(found_all$mmg_salacir, levels = c("1", "0"))

summary.factor(found_all$mmg_salacir)


# Retorno + intervenção (critério original)

found_all$mmg_internprol[is.na(found_all$mmg_internprol)] <- 0

found_all$mmg_internprol <- factor(found_all$mmg_internprol, levels = c("1", "0"))
found_all$CPAV_INTER_7_DIAS_PARTO_SIH_BR <- factor(found_all$CPAV_INTER_7_DIAS_PARTO_SIH_BR, levels = c("1", "0"))


act <- found_all$CPAV_INTER_7_DIAS_PARTO_SIH_BR
ref <- found_all$mmg_internprol #referencia

tab <- table(act,ref)

confusionMatrix(tab)


summary.factor(found_all$CPAV_INTER_7_DIAS_PARTO_SIH_BR)
summary.factor(found_all$mmg_internprol)


# Intervenção cirúrgica 

# no mmg juntar sala cirurgica + laparotomia

found_all$mmg_inter_cirurgica <- ifelse(found_all$mmg_salacir == "1" |
                                        found_all$mmg_laparot == "1", "1", "0")


found_all$mmg_inter_cirurgica[is.na(found_all$mmg_inter_cirurgica)] <- 0

found_all$mmg_inter_cirurgica <- factor(found_all$mmg_inter_cirurgica, levels = c("1", "0"))

summary.factor(found_all$mmg_inter_cirurgica)


# Retorno + intervenção (critério original)

found_all$mmg_inter_cirurgica[is.na(found_all$mmg_inter_cirurgica)] <- 0

found_all$mmg_inter_cirurgica <- factor(found_all$mmg_inter_cirurgica, levels = c("1", "0"))
found_all$CPAV_INTER_CIRURGICA_SIH_BR <- factor(found_all$CPAV_INTER_CIRURGICA_SIH_BR, levels = c("1", "0"))


act <- found_all$CPAV_INTER_CIRURGICA_SIH_BR
ref <- found_all$mmg_inter_cirurgica #referencia

tab <- table(act,ref)

confusionMatrix(tab)


summary.factor(found_all$CPAV_INTER_CIRURGICA_SIH_BR)
summary.factor(found_all$mmg_inter_cirurgica)


# Retorno + intervenção (critério ampliado)

found_all$mmg_inter_cirurgica[is.na(found_all$mmg_inter_cirurgica)] <- 0

found_all$mmg_inter_cirurgica <- factor(found_all$mmg_inter_cirurgica, levels = c("1", "0"))
found_all$CPAV_INTER_CIRURGICA_SIH2_BR <- factor(found_all$CPAV_INTER_CIRURGICA_SIH2_BR, levels = c("1", "0"))


act <- found_all$CPAV_INTER_CIRURGICA_SIH2_BR
ref <- found_all$mmg_inter_cirurgica #referencia

tab <- table(act,ref)

confusionMatrix(tab)


summary.factor(found_all$CPAV_INTER_CIRURGICA_SIH2_BR)
summary.factor(found_all$mmg_inter_cirurgica)


# Total manejo original

# criando grupo de manejo mmg

found_all$mmg_total_manejo <- ifelse(found_all$mmg_transfusao == "1" |
                                       found_all$mmg_acessovc == "1" |
                                       found_all$mmg_histerec == "1" |
                                       found_all$mmg_uti == "1" |
                                       found_all$mmg_internprol == "1" |
                                       found_all$mmg_intub == "1" |
                                       found_all$mmg_salacir == "1" |
                                       found_all$mmg_inter_cirurgica == "1", "1", "0")


found_all$mmg_total_manejo[is.na(found_all$mmg_total_manejo)] <- 0

found_all$mmg_total_manejo <- factor(found_all$mmg_total_manejo, levels = c("1", "0"))
found_all$SIH_MANEJO_ORIGINAL_BR <- factor(found_all$SIH_MANEJO_ORIGINAL_BR, levels = c("1", "0"))


act <- found_all$SIH_MANEJO_ORIGINAL_BR
ref <- found_all$mmg_total_manejo #referencia

tab <- table(act,ref)

confusionMatrix(tab)


summary.factor(found_all$SIH_MANEJO_ORIGINAL_BR)
summary.factor(found_all$mmg_total_manejo)


# Total manejo com TMP apenas pós-parto

found_all$mmg_total_manejo[is.na(found_all$mmg_total_manejo)] <- 0

found_all$mmg_total_manejo <- factor(found_all$mmg_total_manejo, levels = c("1", "0"))
found_all$SIH_MANEJO_TMP_BR <- factor(found_all$SIH_MANEJO_TMP_BR, levels = c("1", "0"))


act <- found_all$SIH_MANEJO_TMP_BR
ref <- found_all$mmg_total_manejo #referencia

tab <- table(act,ref)

confusionMatrix(tab)


summary.factor(found_all$SIH_MANEJO_TMP_BR)
summary.factor(found_all$mmg_total_manejo)


# Total manejo com intervenção cirúrgica ampliada

found_all$SIH_MANEJO_INT_CIR_BR <- ifelse(found_all$CPAV_TRANSFUSAO_SIH_BR == "1" |
                                                           found_all$CPAV_ACESSO_VENOSO_SIH_BR == "1" |
                                                           found_all$CPAV_HISTERECTOMIA_SIH_BR == "1" |
                                                           found_all$CPAV_UTI_SIH_BR == "1" |
                                                           found_all$CPAV_INTER_7_DIAS_SIH_BR == "1" |
                                                           found_all$CPAV_INTER_CIRURGICA_SIH2_BR == "1", "1", "0")

found_all$mmg_total_manejo[is.na(found_all$mmg_total_manejo)] <- 0

found_all$mmg_total_manejo <- factor(found_all$mmg_total_manejo, levels = c("1", "0"))
found_all$SIH_MANEJO_INT_CIR_BR <- factor(found_all$SIH_MANEJO_INT_CIR_BR, levels = c("1", "0"))


act <- found_all$SIH_MANEJO_INT_CIR_BR
ref <- found_all$mmg_total_manejo #referencia

tab <- table(act,ref)

confusionMatrix(tab)


summary.factor(found_all$SIH_MANEJO_INT_CIR_BR)
summary.factor(found_all$mmg_total_manejo)


# Total manejo com TMP apenas pós-parto e intervenção cirúrgica ampliada

found_all$SIH_MANEJO_TMP_IC_BR <- ifelse(found_all$CPAV_TRANSFUSAO_SIH_BR == "1" |
                                              found_all$CPAV_ACESSO_VENOSO_SIH_BR == "1" |
                                              found_all$CPAV_HISTERECTOMIA_SIH_BR == "1" |
                                              found_all$CPAV_UTI_SIH_BR_SP == "1" |
                                              found_all$CPAV_INTER_7_DIAS_PARTO_SIH_BR == "1" |
                                              found_all$CPAV_INTER_CIRURGICA_SIH2_BR == "1", "1", "0")


found_all$mmg_total_manejo[is.na(found_all$mmg_total_manejo)] <- 0

found_all$mmg_total_manejo <- factor(found_all$mmg_total_manejo, levels = c("1", "0"))
found_all$SIH_MANEJO_TMP_IC_BR <- factor(found_all$SIH_MANEJO_TMP_IC_BR, levels = c("1", "0"))


act <- found_all$SIH_MANEJO_TMP_IC_BR
ref <- found_all$mmg_total_manejo #referencia

tab <- table(act,ref)

confusionMatrix(tab)

summary.factor(found_all$SIH_MANEJO_TMP_IC_BR)
summary.factor(found_all$mmg_total_manejo)

###########################################################################################################

# TOTAL (critério original)

found_all$mmg_total[is.na(found_all$mmg_total)] <- 0

found_all$mmg_total <- factor(found_all$mmg_total, levels = c("1", "0"))
found_all$CPAV_SIH_BR_ORIGINAL <- factor(found_all$CPAV_SIH_BR_ORIGINAL , levels = c("1", "0"))


act <- found_all$CPAV_SIH_BR_ORIGINAL 
ref <- found_all$mmg_total #referencia

tab <- table(act,ref)

confusionMatrix(tab)

summary.factor(found_all$CPAV_SIH_BR_ORIGINAL )
summary.factor(found_all$mmg_total)

# TOTAL (critério original + TMP apenas pós-parto)

found_all$mmg_total[is.na(found_all$mmg_total)] <- 0

found_all$mmg_total <- factor(found_all$mmg_total, levels = c("1", "0"))
found_all$CPAV_SIH_TMP_BR <- factor(found_all$CPAV_SIH_TMP_BR , levels = c("1", "0"))


act <- found_all$CPAV_SIH_TMP_BR
ref <- found_all$mmg_total #referencia

tab <- table(act,ref)

confusionMatrix(tab)

summary.factor(found_all$CPAV_SIH_TMP_BR )
summary.factor(found_all$mmg_total)

# TOTAL (critério original + TMP pós-parto + interv cirurgica ampliada)

found_all$mmg_total[is.na(found_all$mmg_total)] <- 0

found_all$mmg_total <- factor(found_all$mmg_total, levels = c("1", "0"))
found_all$CPAV_SIH_TMP_IC_BR <- factor(found_all$CPAV_SIH_TMP_IC_BR , levels = c("1", "0"))


act <- found_all$CPAV_SIH_TMP_IC_BR
ref <- found_all$mmg_total #referencia

tab <- table(act,ref)

confusionMatrix(tab)

summary.factor(found_all$CPAV_SIH_TMP_IC_BR )
summary.factor(found_all$mmg_total)


# TOTAL (critério original + TMP pós parto+ IC + HA gestacional)

found_all$mmg_total[is.na(found_all$mmg_total)] <- 0

found_all$mmg_total <- factor(found_all$mmg_total, levels = c("1", "0"))
found_all$CPAV_SIH_TMP_IC_HAa_BR <- factor(found_all$CPAV_SIH_TMP_IC_HAa_BR , levels = c("1", "0"))


act <- found_all$CPAV_SIH_TMP_IC_HAa_BR
ref <- found_all$mmg_total #referencia

tab <- table(act,ref)

confusionMatrix(tab)

summary.factor(found_all$CPAV_SIH_TMP_IC_HAa_BR )
summary.factor(found_all$mmg_total)


# TOTAL (critério original + TMP pós parto+ IC + HA gestacional e cronicas)

found_all$mmg_total[is.na(found_all$mmg_total)] <- 0

found_all$mmg_total <- factor(found_all$mmg_total, levels = c("1", "0"))
found_all$CPAV_SIH_TMP_IC_HAd_BR <- factor(found_all$CPAV_SIH_TMP_IC_HAd_BR , levels = c("1", "0"))


act <- found_all$CPAV_SIH_TMP_IC_HAd_BR
ref <- found_all$mmg_total #referencia

tab <- table(act,ref)

confusionMatrix(tab)

summary.factor(found_all$CPAV_SIH_TMP_IC_HAd_BR )
summary.factor(found_all$mmg_total)


# TOTAL (critério original + TMP pós parto + IC + HA gestacional e cronicas sem essencial)

found_all$mmg_total[is.na(found_all$mmg_total)] <- 0

found_all$mmg_total <- factor(found_all$mmg_total, levels = c("1", "0"))
found_all$CPAV_SIH_TMP_IC_HAe_BR <- factor(found_all$CPAV_SIH_TMP_IC_HAe_BR , levels = c("1", "0"))


act <- found_all$CPAV_SIH_TMP_IC_HAe_BR
ref <- found_all$mmg_total #referencia

tab <- table(act,ref)

confusionMatrix(tab)

summary.factor(found_all$CPAV_SIH_TMP_IC_HAe_BR )
summary.factor(found_all$mmg_total)


#########################################################################################################

######### SERVIÇO PROFISSIONAL

######### critérios especificos

# Descolamento prematuro de placenta

found_all$mmg_dpp[is.na(found_all$mmg_dpp)] <- 0

found_all$mmg_dpp <- factor(found_all$mmg_dpp, levels = c("1", "0"))
found_all$CPAV_DESC_PLAC_SIH_BR <- factor(found_all$CPAV_DESC_PLAC_SIH_BR, levels = c("1", "0"))


act <- found_all$CPAV_DESC_PLAC_SIH_BR
ref <- found_all$mmg_dpp #referencia

tab <- table(act,ref)

confusionMatrix(tab)

summary.factor(found_all$CPAV_DESC_PLAC_SIH_BR)
summary.factor(found_all$mmg_dpp)


# Placenta acreta, increta ou percreta

found_all$mmg_aipcreta[is.na(found_all$mmg_aipcreta)] <- 0

summary.factor(found_all$mmg_aipcreta)


# Gravidez ectópica

found_all$mmg_ectopica[is.na(found_all$mmg_ectopica)] <- 0

found_all$mmg_ectopica <- factor(found_all$mmg_ectopica, levels = c("1", "0"))
found_all$CPAV_ECTOPICA_SIH_BR_SP <- factor(found_all$CPAV_ECTOPICA_SIH_BR_SP, levels = c("1", "0"))


act <- found_all$CPAV_ECTOPICA_SIH_BR_SP
ref <- found_all$mmg_ectopica #referencia

tab <- table(act,ref)

confusionMatrix(tab)

summary.factor(found_all$CPAV_ECTOPICA_SIH_BR_SP)
summary.factor(found_all$mmg_ectopica)


# Hemorragia pós-parto

found_all$mmg_hemorragia[is.na(found_all$mmg_hemorragia)] <- 0

found_all$mmg_hemorragia <- factor(found_all$mmg_hemorragia, levels = c("1", "0"))
found_all$CPAV_HEMORRAGIA_SIH_BR <- factor(found_all$CPAV_HEMORRAGIA_SIH_BR, levels = c("1", "0"))


act <- found_all$CPAV_HEMORRAGIA_SIH_BR
ref <- found_all$mmg_hemorragia #referencia

tab <- table(act,ref)

confusionMatrix(tab)

summary.factor(found_all$CPAV_HEMORRAGIA_SIH_BR)
summary.factor(found_all$mmg_hemorragia)


# Rotura uterina

found_all$mmg_ruterina[is.na(found_all$mmg_ruterina)] <- 0

found_all$mmg_ruterina <- factor(found_all$mmg_ruterina, levels = c("1", "0"))
found_all$CPAV_ROTURA_SIH_BR_SP <- factor(found_all$CPAV_ROTURA_SIH_BR_SP, levels = c("1", "0"))


act <- found_all$CPAV_ROTURA_SIH_BR_SP
ref <- found_all$mmg_ruterina #referencia

tab <- table(act,ref)

confusionMatrix(tab)

summary.factor(found_all$CPAV_ROTURA_SIH_BR_SP)
summary.factor(found_all$mmg_ruterina)


# Total hemorrágicos

# criando total de hemorrágicas para o mmg e sih

found_all$mmg_total_hemorragicas <- ifelse(found_all$mmg_dpp == "1" |
                                             found_all$mmg_aipcreta == "1" |
                                             found_all$mmg_ectopica == "1" |
                                             found_all$mmg_hemorragia == "1" |
                                             found_all$mmg_ruterina == "1", "1", "0")

found_all$mmg_total_hemorragicas <- factor(found_all$mmg_total_hemorragicas, levels = c("1", "0"))
found_all$SIH_HEMORRAGICAS_BR_SP <- factor(found_all$SIH_HEMORRAGICAS_BR_SP, levels = c("1", "0"))


act <- found_all$SIH_HEMORRAGICAS_BR_SP
ref <- found_all$mmg_total_hemorragicas #referencia

tab <- table(act,ref)

confusionMatrix(tab)

summary.factor(found_all$SIH_HEMORRAGICAS_BR_SP)
summary.factor(found_all$mmg_total_hemorragicas)


# Pré-eclâmpsia grave

found_all$mmg_pegrave[is.na(found_all$mmg_pegrave)] <- 0

found_all$mmg_pegrave <- factor(found_all$mmg_pegrave, levels = c("1", "0"))
found_all$CPAV_PRE_ECLAMP_SIH_BR <- factor(found_all$CPAV_PRE_ECLAMP_SIH_BR, levels = c("1", "0"))


act <- found_all$CPAV_PRE_ECLAMP_SIH_BR
ref <- found_all$mmg_pegrave #referencia

tab <- table(act,ref)

confusionMatrix(tab)

summary.factor(found_all$CPAV_PRE_ECLAMP_SIH_BR)
summary.factor(found_all$mmg_pegrave)



# Eclâmpsia

found_all$mmg_eclampsia[is.na(found_all$mmg_eclampsia)] <- 0

found_all$mmg_eclampsia <- factor(found_all$mmg_eclampsia, levels = c("1", "0"))
found_all$CPAV_ECLAMPSIA_SIH_BR_SP <- factor(found_all$CPAV_ECLAMPSIA_SIH_BR_SP, levels = c("1", "0"))


act <- found_all$CPAV_ECLAMPSIA_SIH_BR_SP
ref <- found_all$mmg_eclampsia #referencia

tab <- table(act,ref)

confusionMatrix(tab)

summary.factor(found_all$CPAV_ECLAMPSIA_SIH_BR_SP)
summary.factor(found_all$mmg_eclampsia)



# Hipertensão grave

found_all$mmg_hagrave[is.na(found_all$mmg_hagrave)] <- 0

found_all$mmg_hagrave <- factor(found_all$mmg_hagrave, levels = c("1", "0"))
found_all$CPAV_HIPER_GRAVE_SIH_BR_SP <- factor(found_all$CPAV_HIPER_GRAVE_SIH_BR_SP, levels = c("1", "0"))


act <- found_all$CPAV_HIPER_GRAVE_SIH_BR_SP
ref <- found_all$mmg_hagrave #referencia

tab <- table(act,ref)

confusionMatrix(tab)

summary.factor(found_all$CPAV_HIPER_GRAVE_SIH_BR_SP)
summary.factor(found_all$mmg_hagrave)


# Encefalopatia hipertensiva

found_all$mmg_enchipert[is.na(found_all$mmg_enchipert)] <- 0

found_all$mmg_enchipert <- factor(found_all$mmg_enchipert, levels = c("1", "0"))
found_all$CPAV_ENCEF_HIPER_SIH_BR_SP <- factor(found_all$CPAV_ENCEF_HIPER_SIH_BR_SP, levels = c("1", "0"))


act <- found_all$CPAV_ENCEF_HIPER_SIH_BR_SP
ref <- found_all$mmg_enchipert #referencia

tab <- table(act,ref)

confusionMatrix(tab)

summary.factor(found_all$CPAV_ENCEF_HIPER_SIH_BR_SP)
summary.factor(found_all$mmg_enchipert)


# Síndrome HELLP

found_all$mmg_hellp[is.na(found_all$mmg_hellp)] <- 0

found_all$mmg_hellp <- factor(found_all$mmg_hellp, levels = c("1", "0"))
found_all$CPAV_HELLP_SIH_BR <- factor(found_all$CPAV_HELLP_SIH_BR, levels = c("1", "0"))


act <- found_all$CPAV_HELLP_SIH_BR
ref <- found_all$mmg_hellp #referencia

tab <- table(act,ref)

confusionMatrix(tab)

summary.factor(found_all$CPAV_HELLP_SIH_BR)
summary.factor(found_all$mmg_hellp)


# total hipertensivas

# varivel total pro mmg

found_all$mmg_total_hipertensivas <- ifelse(found_all$mmg_pegrave == "1" |
                                              found_all$mmg_eclampsia == "1" |
                                              found_all$mmg_hagrave == "1" |
                                              found_all$mmg_enchipert == "1" |
                                              found_all$mmg_hellp == "1", "1", "0")


found_all$mmg_total_hipertensivas <- factor(found_all$mmg_total_hipertensivas, levels = c("1", "0"))
found_all$CPAV_HA_SIH_BR <- factor(found_all$CPAV_HA_SIH_BR , levels = c("1", "0"))

act <- found_all$CPAV_HA_SIH_BR 
ref <- found_all$mmg_total_hipertensivas #referencia

tab <- table(act,ref)

confusionMatrix(tab)

summary.factor(found_all$CPAV_HA_SIH_BR)
summary.factor(found_all$mmg_total_hipertensivas)


# Total hipertensivas(incluindo outras gestacionais)

found_all$mmg_total_hipertensivas <- factor(found_all$mmg_total_hipertensivas, levels = c("1", "0"))
found_all$CPAV_HA_SIHa <- factor(found_all$CPAV_HA_SIHa, levels = c("1", "0"))

act <- found_all$CPAV_HA_SIHa
ref <- found_all$mmg_total_hipertensivas #referencia

tab <- table(act,ref)

confusionMatrix(tab)

summary.factor(found_all$CPAV_HA_SIHa)
summary.factor(found_all$mmg_total_hipertensivas)


# Total hipertensivas(incluindo crônicas)

found_all$mmg_total_hipertensivas <- factor(found_all$mmg_total_hipertensivas, levels = c("1", "0"))
found_all$CPAV_HA_SIHb <- factor(found_all$CPAV_HA_SIHb, levels = c("1", "0"))

act <- found_all$CPAV_HA_SIHb
ref <- found_all$mmg_total_hipertensivas #referencia

tab <- table(act,ref)

confusionMatrix(tab)

summary.factor(found_all$CPAV_HA_SIHb)
summary.factor(found_all$mmg_total_hipertensivas)


# Total hipertensivas(incluindo cronicas sem essenciais)

found_all$mmg_total_hipertensivas <- factor(found_all$mmg_total_hipertensivas, levels = c("1", "0"))
found_all$CPAV_HA_SIHc <- factor(found_all$CPAV_HA_SIHc, levels = c("1", "0"))

act <- found_all$CPAV_HA_SIHc
ref <- found_all$mmg_total_hipertensivas #referencia

tab <- table(act,ref)

confusionMatrix(tab)

summary.factor(found_all$CPAV_HA_SIHc)
summary.factor(found_all$mmg_total_hipertensivas)


# Total hipertensivas(incluindo outras gestacionais e cronicas)

found_all$mmg_total_hipertensivas <- factor(found_all$mmg_total_hipertensivas, levels = c("1", "0"))
found_all$CPAV_HA_SIHd <- factor(found_all$CPAV_HA_SIHd, levels = c("1", "0"))

act <- found_all$CPAV_HA_SIHd
ref <- found_all$mmg_total_hipertensivas #referencia

tab <- table(act,ref)

confusionMatrix(tab)

summary.factor(found_all$CPAV_HA_SIHd)
summary.factor(found_all$mmg_total_hipertensivas)


# Total hipertensivas(incluindo outras gestacionais e cronicas, sem essenciais)

found_all$mmg_total_hipertensivas <- factor(found_all$mmg_total_hipertensivas, levels = c("1", "0"))
found_all$CPAV_HA_SIHe <- factor(found_all$CPAV_HA_SIHe, levels = c("1", "0"))

act <- found_all$CPAV_HA_SIHe
ref <- found_all$mmg_total_hipertensivas #referencia

tab <- table(act,ref)

confusionMatrix(tab)

summary.factor(found_all$CPAV_HA_SIHe)
summary.factor(found_all$mmg_total_hipertensivas)


# Endometrite

found_all$mmg_endometrite[is.na(found_all$mmg_endometrite)] <- 0

found_all$mmg_endometrite <- factor(found_all$mmg_endometrite, levels = c("1", "0"))
found_all$CPAV_ENDOMETRITE_SIH_BR <- factor(found_all$CPAV_ENDOMETRITE_SIH_BR , levels = c("1", "0"))


act <- found_all$CPAV_ENDOMETRITE_SIH_BR 
ref <- found_all$mmg_endometrite #referencia

tab <- table(act,ref)

confusionMatrix(tab)

summary.factor(found_all$CPAV_ENDOMETRITE_SIH_BR )
summary.factor(found_all$mmg_endometrite)


# Edema pulmonar

found_all$mmg_edemap[is.na(found_all$mmg_edemap)] <- 0

found_all$mmg_edemap <- factor(found_all$mmg_edemap, levels = c("1", "0"))
found_all$CPAV_EDEMA_PULM_SIH_BR_SP <- factor(found_all$CPAV_EDEMA_PULM_SIH_BR_SP , levels = c("1", "0"))


act <- found_all$CPAV_EDEMA_PULM_SIH_BR_SP 
ref <- found_all$mmg_edemap #referencia

tab <- table(act,ref)

confusionMatrix(tab)

summary.factor(found_all$CPAV_EDEMA_PULM_SIH_BR_SP )
summary.factor(found_all$mmg_edemap)


# Insuficiência respiratória

found_all$mmg_irpa[is.na(found_all$mmg_irpa)] <- 0

found_all$mmg_irpa <- factor(found_all$mmg_irpa, levels = c("1", "0"))
found_all$CPAV_INSU_RESP_SIH_BR_SP <- factor(found_all$CPAV_INSU_RESP_SIH_BR_SP , levels = c("1", "0"))


act <- found_all$CPAV_INSU_RESP_SIH_BR_SP 
ref <- found_all$mmg_irpa #referencia

tab <- table(act,ref)

confusionMatrix(tab)

summary.factor(found_all$CPAV_INSU_RESP_SIH_BR_SP )
summary.factor(found_all$mmg_irpa)


# Convulsões

found_all$mmg_convulsoes[is.na(found_all$mmg_convulsoes)] <- 0

found_all$mmg_convulsoes <- factor(found_all$mmg_convulsoes, levels = c("1", "0"))
found_all$CPAV_CONVULSOES_SIH_BR <- factor(found_all$CPAV_CONVULSOES_SIH_BR , levels = c("1", "0"))


act <- found_all$CPAV_CONVULSOES_SIH_BR 
ref <- found_all$mmg_convulsoes #referencia

tab <- table(act,ref)

confusionMatrix(tab)

summary.factor(found_all$CPAV_CONVULSOES_SIH_BR )
summary.factor(found_all$mmg_convulsoes)


# Sepse

found_all$mmg_sepse[is.na(found_all$mmg_sepse)] <- 0

found_all$mmg_sepse <- factor(found_all$mmg_sepse, levels = c("1", "0"))
found_all$CPAV_SEPSE_SIH_BR <- factor(found_all$CPAV_SEPSE_SIH_BR  , levels = c("1", "0"))


act <- found_all$CPAV_SEPSE_SIH_BR 
ref <- found_all$mmg_sepse #referencia

tab <- table(act,ref)

confusionMatrix(tab)

summary.factor(found_all$CPAV_SEPSE_SIH_BR  )
summary.factor(found_all$mmg_sepse)


# Choque

found_all$mmg_choque[is.na(found_all$mmg_choque)] <- 0

found_all$mmg_choque <- factor(found_all$mmg_choque, levels = c("1", "0"))
found_all$CPAV_CHOQUE_SIH_BR_SP <- factor(found_all$CPAV_CHOQUE_SIH_BR_SP   , levels = c("1", "0"))


act <- found_all$CPAV_CHOQUE_SIH_BR_SP
ref <- found_all$mmg_choque #referencia

tab <- table(act,ref)

confusionMatrix(tab)

summary.factor(found_all$CPAV_CHOQUE_SIH_BR_SP)
summary.factor(found_all$mmg_choque)


# Trombocitopenia < 100.000

found_all$mmg_tcp[is.na(found_all$mmg_tcp)] <- 0

found_all$mmg_tcp <- factor(found_all$mmg_tcp, levels = c("1", "0"))

summary.factor(found_all$mmg_tcp)


# Crise tireotóxica

found_all$mmg_ct[is.na(found_all$mmg_ct)] <- 0

found_all$mmg_ct <- factor(found_all$mmg_ct, levels = c("1", "0"))
found_all$CPAV_CRISE_TIREO_SIH_BR <- factor(found_all$CPAV_CRISE_TIREO_SIH_BR, levels = c("1", "0"))


act <- found_all$CPAV_CRISE_TIREO_SIH_BR
ref <- found_all$mmg_ct #referencia

tab <- table(act,ref)

confusionMatrix(tab)

summary.factor(found_all$CPAV_CRISE_TIREO_SIH_BR)
summary.factor(found_all$mmg_ct)


# total outros

# criando total outros

found_all$mmg_total_outros <- ifelse(found_all$mmg_endometrite == "1" |
                                       found_all$mmg_edemap == "1" |
                                       found_all$mmg_irpa == "1" |
                                       found_all$mmg_convulsoes == "1" |
                                       found_all$mmg_sepse == "1" |
                                       found_all$mmg_choque == "1" |
                                       found_all$mmg_tcp == "1" |
                                       found_all$mmg_ct == "1", "1", "0")

found_all$mmg_total_outros <- factor(found_all$mmg_total_outros, levels = c("1", "0"))
found_all$SIH_OUTROS_BR_SP <- factor(found_all$SIH_OUTROS_BR_SP, levels = c("1", "0"))


act <- found_all$SIH_OUTROS_BR_SP
ref <- found_all$mmg_total_outros #referencia

tab <- table(act,ref)

confusionMatrix(tab)

summary.factor(found_all$SIH_OUTROS_BR_SP)
summary.factor(found_all$mmg_total_outros)


# Transfusão sanguínea

found_all$mmg_transfusao[is.na(found_all$mmg_transfusao)] <- 0

found_all$mmg_transfusao <- factor(found_all$mmg_transfusao, levels = c("1", "0"))
found_all$CPAV_TRANSFUSAO_SIH_BR_SP <- factor(found_all$CPAV_TRANSFUSAO_SIH_BR_SP, levels = c("1", "0"))


act <- found_all$CPAV_TRANSFUSAO_SIH_BR_SP
ref <- found_all$mmg_transfusao #referencia

tab <- table(act,ref)

confusionMatrix(tab)

summary.factor(found_all$CPAV_TRANSFUSAO_SIH_BR_SP)
summary.factor(found_all$mmg_transfusao)


# Acesso venoso central

found_all$mmg_acessovc[is.na(found_all$mmg_acessovc)] <- 0

found_all$mmg_acessovc <- factor(found_all$mmg_acessovc, levels = c("1", "0"))
found_all$CPAV_ACESSO_VENOSO_SIH_BR_SP <- factor(found_all$CPAV_ACESSO_VENOSO_SIH_BR_SP, levels = c("1", "0"))


act <- found_all$CPAV_ACESSO_VENOSO_SIH_BR_SP
ref <- found_all$mmg_acessovc #referencia

tab <- table(act,ref)

confusionMatrix(tab)

summary.factor(found_all$CPAV_ACESSO_VENOSO_SIH_BR_SP)
summary.factor(found_all$mmg_acessovc)


# Histerectomia

found_all$mmg_histerec[is.na(found_all$mmg_histerec)] <- 0

found_all$mmg_histerec <- factor(found_all$mmg_histerec, levels = c("1", "0"))
found_all$CPAV_HISTERECTOMIA_SIH_BR_SP <- factor(found_all$CPAV_HISTERECTOMIA_SIH_BR_SP, levels = c("1", "0"))


act <- found_all$CPAV_HISTERECTOMIA_SIH_BR_SP
ref <- found_all$mmg_histerec #referencia

tab <- table(act,ref)

confusionMatrix(tab)

summary.factor(found_all$CPAV_HISTERECTOMIA_SIH_BR_SP)
summary.factor(found_all$mmg_histerec)


# Admissão à unidade de tratamento intensivo

found_all$mmg_uti[is.na(found_all$mmg_uti)] <- 0

found_all$mmg_uti <- factor(found_all$mmg_uti, levels = c("1", "0"))
found_all$CPAV_UTI_SIH_BR_SP <- factor(found_all$CPAV_UTI_SIH_BR_SP, levels = c("1", "0"))


act <- found_all$CPAV_UTI_SIH_BR_SP
ref <- found_all$mmg_uti #referencia

tab <- table(act,ref)

confusionMatrix(tab)

summary.factor(found_all$CPAV_UTI_SIH_BR_SP)
summary.factor(found_all$mmg_uti)

# Internação hospitalar prolongada (considerando todas no SIH)

found_all$mmg_internprol[is.na(found_all$mmg_internprol)] <- 0

found_all$mmg_internprol <- factor(found_all$mmg_internprol, levels = c("1", "0"))
found_all$CPAV_INTER_7_DIAS_SIH_BR <- factor(found_all$CPAV_INTER_7_DIAS_SIH_BR, levels = c("1", "0"))


act <- found_all$CPAV_INTER_7_DIAS_SIH_BR
ref <- found_all$mmg_internprol #referencia

tab <- table(act,ref)

confusionMatrix(tab)


summary.factor(found_all$CPAV_INTER_7_DIAS_SIH_BR)
summary.factor(found_all$mmg_internprol)


# Internação hospitalar prolongada (apenas parto no SIH)

found_all$mmg_internprol[is.na(found_all$mmg_internprol)] <- 0

found_all$mmg_internprol <- factor(found_all$mmg_internprol, levels = c("1", "0"))
found_all$CPAV_INTER_7_DIAS_PARTO_SIH_BR <- factor(found_all$CPAV_INTER_7_DIAS_PARTO_SIH_BR, levels = c("1", "0"))


act <- found_all$CPAV_INTER_7_DIAS_PARTO_SIH_BR
ref <- found_all$mmg_internprol #referencia

tab <- table(act,ref)

confusionMatrix(tab)


summary.factor(found_all$CPAV_INTER_7_DIAS_PARTO_SIH_BR)
summary.factor(found_all$mmg_internprol)


# Intubação não anestésica

found_all$mmg_intub[is.na(found_all$mmg_intub)] <- 0

found_all$mmg_intub <- factor(found_all$mmg_intub, levels = c("1", "0"))

summary.factor(found_all$mmg_intub)


# Retorno à sala operatória (centro cirúrgico)

found_all$mmg_salacir[is.na(found_all$mmg_salacir)] <- 0

found_all$mmg_salacir <- factor(found_all$mmg_salacir, levels = c("1", "0"))

summary.factor(found_all$mmg_salacir)


# Intervenção cirúrgica 

# no mmg juntar sala cirurgica + laparotomia

found_all$mmg_inter_cirurgica <- ifelse(found_all$mmg_salacir == "1" |
                                          found_all$mmg_laparot == "1", "1", "0")


found_all$mmg_inter_cirurgica[is.na(found_all$mmg_inter_cirurgica)] <- 0

found_all$mmg_inter_cirurgica <- factor(found_all$mmg_inter_cirurgica, levels = c("1", "0"))

summary.factor(found_all$mmg_inter_cirurgica)


# Retorno + intervenção (critério original)

found_all$mmg_inter_cirurgica[is.na(found_all$mmg_inter_cirurgica)] <- 0

found_all$mmg_inter_cirurgica <- factor(found_all$mmg_inter_cirurgica, levels = c("1", "0"))
found_all$CPAV_INTER_CIRURGICA_SIH_BR_SP <- factor(found_all$CPAV_INTER_CIRURGICA_SIH_BR_SP, levels = c("1", "0"))


act <- found_all$CPAV_INTER_CIRURGICA_SIH_BR_SP
ref <- found_all$mmg_inter_cirurgica #referencia

tab <- table(act,ref)

confusionMatrix(tab)


summary.factor(found_all$CPAV_INTER_CIRURGICA_SIH_BR_SP)
summary.factor(found_all$mmg_inter_cirurgica)


# Retorno + intervenção (critério ampliado)

found_all$mmg_inter_cirurgica[is.na(found_all$mmg_inter_cirurgica)] <- 0

found_all$mmg_inter_cirurgica <- factor(found_all$mmg_inter_cirurgica, levels = c("1", "0"))
found_all$CPAV_INTER_CIRURGICA_SIH2_BR_SP <- factor(found_all$CPAV_INTER_CIRURGICA_SIH2_BR_SP, levels = c("1", "0"))


act <- found_all$CPAV_INTER_CIRURGICA_SIH2_BR_SP
ref <- found_all$mmg_inter_cirurgica #referencia

tab <- table(act,ref)

confusionMatrix(tab)


summary.factor(found_all$CPAV_INTER_CIRURGICA_SIH2_BR_SP)
summary.factor(found_all$mmg_inter_cirurgica)


# Total manejo original

# criando grupo de manejo mmg

found_all$mmg_total_manejo <- ifelse(found_all$mmg_transfusao == "1" |
                                       found_all$mmg_acessovc == "1" |
                                       found_all$mmg_histerec == "1" |
                                       found_all$mmg_uti == "1" |
                                       found_all$mmg_internprol == "1" |
                                       found_all$mmg_intub == "1" |
                                       found_all$mmg_salacir == "1" |
                                       found_all$mmg_inter_cirurgica == "1", "1", "0")


found_all$mmg_total_manejo[is.na(found_all$mmg_total_manejo)] <- 0

found_all$mmg_total_manejo <- factor(found_all$mmg_total_manejo, levels = c("1", "0"))
found_all$SIH_MANEJO_ORIGINAL_BR_SP <- factor(found_all$SIH_MANEJO_ORIGINAL_BR_SP, levels = c("1", "0"))


act <- found_all$SIH_MANEJO_ORIGINAL_BR_SP
ref <- found_all$mmg_total_manejo #referencia

tab <- table(act,ref)

confusionMatrix(tab)


summary.factor(found_all$SIH_MANEJO_ORIGINAL_BR_SP)
summary.factor(found_all$mmg_total_manejo)


# Total manejo com TMP apenas pós-parto

found_all$SIH_MANEJO_TMP_PARTO_BR_SP <- ifelse(found_all$CPAV_TRANSFUSAO_SIH_BR_SP == "1" |
                                           found_all$CPAV_ACESSO_VENOSO_SIH_BR_SP == "1" |
                                           found_all$CPAV_HISTERECTOMIA_SIH_BR_SP == "1" |
                                           found_all$CPAV_UTI_SIH_BR_SP == "1" |
                                           found_all$CPAV_INTER_7_DIAS_PARTO_SIH_BR == "1", "1", "0")

found_all$mmg_total_manejo[is.na(found_all$mmg_total_manejo)] <- 0

found_all$mmg_total_manejo <- factor(found_all$mmg_total_manejo, levels = c("1", "0"))
found_all$SIH_MANEJO_TMP_PARTO_BR_SP <- factor(found_all$SIH_MANEJO_TMP_PARTO_BR_SP, levels = c("1", "0"))

act <- found_all$SIH_MANEJO_TMP_PARTO_BR_SP
ref <- found_all$mmg_total_manejo #referencia

tab <- table(act,ref)

confusionMatrix(tab)


summary.factor(found_all$SIH_MANEJO_TMP_PARTO_BR_SP)
summary.factor(found_all$mmg_total_manejo)


# Total manejo com intervenção cirúrgica ampliada


found_all$SIH_MANEJO_INT_CIR_BR_SP <- ifelse(found_all$CPAV_TRANSFUSAO_SIH_BR_SP == "1" |
                                                 found_all$CPAV_ACESSO_VENOSO_SIH_BR_SP == "1" |
                                                 found_all$CPAV_HISTERECTOMIA_SIH_BR_SP == "1" |
                                                 found_all$CPAV_UTI_SIH_BR_SP == "1" |
                                                 found_all$CPAV_INTER_7_DIAS_SIH_BR == "1" |
                                                 found_all$CPAV_INTER_CIRURGICA_SIH2_BR == "1", "1", "0")

found_all$mmg_total_manejo[is.na(found_all$mmg_total_manejo)] <- 0

found_all$mmg_total_manejo <- factor(found_all$mmg_total_manejo, levels = c("1", "0"))
found_all$SIH_MANEJO_INT_CIR_BR_SP <- factor(found_all$SIH_MANEJO_INT_CIR_BR_SP, levels = c("1", "0"))


act <- found_all$SIH_MANEJO_INT_CIR_BR_SP
ref <- found_all$mmg_total_manejo #referencia

tab <- table(act,ref)

confusionMatrix(tab)

summary.factor(found_all$SIH_MANEJO_INT_CIR_BR_SP)
summary.factor(found_all$mmg_total_manejo)


# Total manejo com TMP apenas pós-parto e intervenção cirúrgica ampliada

found_all$SIH_MANEJO_TMP_IC_BR_SP <- ifelse(found_all$CPAV_TRANSFUSAO_SIH_BR_SP == "1" |
                                                           found_all$CPAV_ACESSO_VENOSO_SIH_BR_SP == "1" |
                                                           found_all$CPAV_HISTERECTOMIA_SIH_BR_SP == "1" |
                                                           found_all$CPAV_UTI_SIH_BR_SP == "1" |
                                                           found_all$CPAV_INTER_7_DIAS_PARTO_SIH_BR == "1" |
                                                           found_all$CPAV_INTER_CIRURGICA_SIH2_BR_SP == "1", "1", "0")

found_all$mmg_total_manejo[is.na(found_all$mmg_total_manejo)] <- 0

found_all$mmg_total_manejo <- factor(found_all$mmg_total_manejo, levels = c("1", "0"))
found_all$SIH_MANEJO_TMP_IC_BR_SP <- factor(found_all$SIH_MANEJO_TMP_IC_BR_SP, levels = c("1", "0"))


act <- found_all$SIH_MANEJO_TMP_IC_BR_SP
ref <- found_all$mmg_total_manejo #referencia

tab <- table(act,ref)

confusionMatrix(tab)

summary.factor(found_all$SIH_MANEJO_TMP_IC_BR_SP)
summary.factor(found_all$mmg_total_manejo)

###########################################################################################################

# TOTAL (critério original)

found_all$mmg_total[is.na(found_all$mmg_total)] <- 0

found_all$mmg_total <- factor(found_all$mmg_total, levels = c("1", "0"))
found_all$CPAV_SIH_BR_SP_ORIGINAL <- factor(found_all$CPAV_SIH_BR_SP_ORIGINAL , levels = c("1", "0"))


act <- found_all$CPAV_SIH_BR_SP_ORIGINAL 
ref <- found_all$mmg_total #referencia

tab <- table(act,ref)

confusionMatrix(tab)

summary.factor(found_all$CPAV_SIH_BR_SP_ORIGINAL )
summary.factor(found_all$mmg_total)

# TOTAL (critério original + TMP apenas pós-parto)

found_all$mmg_total[is.na(found_all$mmg_total)] <- 0

found_all$mmg_total <- factor(found_all$mmg_total, levels = c("1", "0"))
found_all$CPAV_SIH_TMP_BR_SP <- factor(found_all$CPAV_SIH_TMP_BR_SP , levels = c("1", "0"))


act <- found_all$CPAV_SIH_TMP_BR_SP
ref <- found_all$mmg_total #referencia

tab <- table(act,ref)

confusionMatrix(tab)

summary.factor(found_all$CPAV_SIH_TMP_BR_SP )
summary.factor(found_all$mmg_total)

# TOTAL (critério original + TMP pós-parto + interv cirurgica ampliada)

found_all$mmg_total[is.na(found_all$mmg_total)] <- 0

found_all$mmg_total <- factor(found_all$mmg_total, levels = c("1", "0"))
found_all$CPAV_SIH_TMP_IC_BR_SP <- factor(found_all$CPAV_SIH_TMP_IC_BR_SP , levels = c("1", "0"))


act <- found_all$CPAV_SIH_TMP_IC_BR_SP
ref <- found_all$mmg_total #referencia

tab <- table(act,ref)

confusionMatrix(tab)

summary.factor(found_all$CPAV_SIH_TMP_IC_BR_SP )
summary.factor(found_all$mmg_total)


# TOTAL (critério original + TMP pós parto+ IC + HA gestacional)

found_all$mmg_total[is.na(found_all$mmg_total)] <- 0

found_all$mmg_total <- factor(found_all$mmg_total, levels = c("1", "0"))
found_all$CPAV_SIH_TMP_IC_HAa_BR_SP <- factor(found_all$CPAV_SIH_TMP_IC_HAa_BR_SP , levels = c("1", "0"))


act <- found_all$CPAV_SIH_TMP_IC_HAa_BR_SP
ref <- found_all$mmg_total #referencia

tab <- table(act,ref)

confusionMatrix(tab)

summary.factor(found_all$CPAV_SIH_TMP_IC_HAa_BR_SP )
summary.factor(found_all$mmg_total)


# TOTAL (critério original + TMP pós parto+ IC + HA gestacional e cronicas)

found_all$mmg_total[is.na(found_all$mmg_total)] <- 0

found_all$mmg_total <- factor(found_all$mmg_total, levels = c("1", "0"))
found_all$CPAV_SIH_TMP_IC_HAd_BR_SP <- factor(found_all$CPAV_SIH_TMP_IC_HAd_BR_SP , levels = c("1", "0"))


act <- found_all$CPAV_SIH_TMP_IC_HAd_BR_SP
ref <- found_all$mmg_total #referencia

tab <- table(act,ref)

confusionMatrix(tab)

summary.factor(found_all$CPAV_SIH_TMP_IC_HAd_BR_SP )
summary.factor(found_all$mmg_total)


# TOTAL (critério original + TMP pós parto + IC + HA gestacional e cronicas sem essencial)

found_all$mmg_total[is.na(found_all$mmg_total)] <- 0

found_all$mmg_total <- factor(found_all$mmg_total, levels = c("1", "0"))
found_all$CPAV_SIH_TMP_IC_HAe_BR_SP <- factor(found_all$CPAV_SIH_TMP_IC_HAe_BR_SP , levels = c("1", "0"))


act <- found_all$CPAV_SIH_TMP_IC_HAe_BR_SP
ref <- found_all$mmg_total #referencia

tab <- table(act,ref)

confusionMatrix(tab)

summary.factor(found_all$CPAV_SIH_TMP_IC_HAe_BR_SP)
summary.factor(found_all$mmg_total)

