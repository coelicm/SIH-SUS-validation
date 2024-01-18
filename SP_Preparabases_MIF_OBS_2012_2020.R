##############################################################################################################

### Arquivos MIF SP 2012 a 2020 - Mulheres de 10 a 49 anos, AIH do tipo 1 
### Prepara Arquivos MIF e OBS considerando episódio de cuidado com múltiplos registros  
### Usa aqruivo gerado pelo script de captação de bases e pelo algoritmo que identifica espisódio de cuidado com
### múltiplos eventos
## Data: 21/05/2022
## Atualizado: 23/06/2022
## Autora: Claudia Medina

###############################################################################################################

# Carregando pacotes
library(readr)
library(stringr)
library(dplyr)


# Lendo a base de permanencia e transferencia
permtransf <- read_csv("SP_Permtransf_2012_2020.csv", 
                       col_types = cols_only(AIH_REF = col_character(), 
                                       AIH = col_character(),FLAG = col_character()))
#lendo a base mif
SP_MIF_2012_2020<- read_delim("SP_MIF_Todos_Campos_Sem_Cluster_2012_2020.csv", 
                    delim = ";", escape_double = FALSE, col_types = cols_only(UF_ZI = col_character(), 
                                                                              ANO_CMPT = col_character(), 
                                                                              ESPEC = col_character(), N_AIH = col_character(), 
                                                                              IDENT = col_character(), CEP = col_character(), 
                                                                              MUNIC_RES = col_character(), NASC = col_character(),
                                                                              SEXO=col_character(),UTI_MES_TO=col_character(),
                                                                              UTI_INT_TO=col_character(), CAR_INT=col_character(),
                                                                              PROC_REA = col_character(),VAL_SH=col_character(),
                                                                              VAL_SP=col_character(),VAL_TOT=col_character(), 
                                                                              VAL_UTI=col_character(), VAL_UCI=col_character(), 
                                                                              DT_INTER = col_character(),
                                                                              DT_SAIDA=col_character(),DIAG_PRINC=col_character(),
                                                                              DIAG_SECUN=col_character(),COD_IDADE=col_character(),
                                                                              IDADE=col_character(),DIAS_PERM=col_character(),
                                                                              COBRANCA=col_character(),MUNIC_MOV=col_character(),
                                                                              CNES=col_character(), CAR_INT=col_character(),
                                                                              NACIONAL=col_character(), NUM_FILHOS=col_character(),
                                                                              INSTRU=col_character(),RACA_COR=col_character(), 
                                                                              ETNIA=col_character(), GESTRISCO=col_character(),
                                                                              DIAGSEC1=col_character(), DIAGSEC2=col_character(),
                                                                              DIAGSEC3=col_character(), DIAGSEC4=col_character(),
                                                                              DIAGSEC5=col_character(), DIAGSEC6=col_character(),
                                                                              DIAGSEC7=col_character(), DIAGSEC8=col_character(),
                                                                              DIAGSEC9=col_character(), TPDISEC1=col_character(),
                                                                              TPDISEC2=col_character(),TPDISEC3=col_character(),
                                                                              TPDISEC4=col_character(),TPDISEC5=col_character(),
                                                                              TPDISEC6=col_character(),TPDISEC7=col_character(),
                                                                              TPDISEC8=col_character(),TPDISEC9=col_character(),
                                                                              CID_MORTE=col_character(),CID_ASSO=col_character(),
                                                                              CID_NOTIF=col_character(),
                                                                              FLAG_PROC_REA=col_character(),FLAG_DIAGNOSTICO_REST=col_character(),
                                                                              FLAG_PARTO=col_character(),FLAG_DIAGNOSTICO=col_character()))


#criando a variavel alta para juntar tipo de saida
SP_MIF_2012_2020$ALTA<-SP_MIF_2012_2020$COBRANCA
SP_MIF_2012_2020$ALTA[SP_MIF_2012_2020$COBRANCA>="11"& SP_MIF_2012_2020$COBRANCA<="19"]<-"Alta"
SP_MIF_2012_2020$ALTA[SP_MIF_2012_2020$COBRANCA>="21"& SP_MIF_2012_2020$COBRANCA<="29"]<-"Permanencia"
SP_MIF_2012_2020$ALTA[SP_MIF_2012_2020$COBRANCA>="31"& SP_MIF_2012_2020$COBRANCA<="32"]<-"Transferencia"
SP_MIF_2012_2020$ALTA[SP_MIF_2012_2020$COBRANCA>="41"& SP_MIF_2012_2020$COBRANCA<="43"]<-"Morte"
SP_MIF_2012_2020$ALTA[SP_MIF_2012_2020$COBRANCA=="51"]<-"Administrativa" 
SP_MIF_2012_2020$ALTA[SP_MIF_2012_2020$COBRANCA>="61"& SP_MIF_2012_2020$COBRANCA<="64"]<-"Alta"
SP_MIF_2012_2020$ALTA[SP_MIF_2012_2020$COBRANCA>="65"& SP_MIF_2012_2020$COBRANCA<="67"]<-"Morte"


# Construir uma variavel que indica se a AIH atende aos critérios do projeto=1, caso contrario=0
SP_MIF_2012_2020<-SP_MIF_2012_2020 %>%
  mutate(criterio=if_else(FLAG_DIAGNOSTICO==1 | FLAG_PROC_REA==1,1,0))
SP_MIF_2012_2020$criterio[is.na(SP_MIF_2012_2020$criterio)]<-"0"

#gerar um subset das aihs que atendem ao criterio com somente as variaveis AIH e criterio, para servir como uma tabela
criterio<-SP_MIF_2012_2020 %>% 
  filter(criterio==1)%>%
  dplyr:: select(N_AIH, criterio)

  #Mudando o nome da variavel criterio para CRITERIO2 
criterio<-criterio %>% 
   rename(CRITERIO2=criterio)

# juntar a esquerda a base SP_MIF_2012_2020 com a base permtransf
SP_MIF_2012_2020<-SP_MIF_2012_2020 %>% 
  left_join(permtransf,by=c("N_AIH"= "AIH")) 

# criar uma variavel grupo aih na base SP_MIF_2012_2020 substituindo com N_AIH ou o aih_ref (essa ultima se aih_ref estiver preenchida)
SP_MIF_2012_2020<-SP_MIF_2012_2020 %>% 
  mutate(grupoaih=if_else(is.na(AIH_REF),N_AIH,AIH_REF))


# juntar a base SP_MIF_2012_2020 a esquerda com a base criterio usando como chave N_AIH e grupoaih
SP_MIF_2012_2020<-SP_MIF_2012_2020 %>% 
  left_join(criterio,by=c("grupoaih"= "N_AIH")) 

# susbstituir zero em CRITERIO2 quand forem NA
SP_MIF_2012_2020$CRITERIO2[is.na(SP_MIF_2012_2020$CRITERIO2)]<-0

# Criar a Variavel SELECAO que recebera o seguinte codigo (0- não atende, 1- atende criterio original, 2-atende relacionada a outra AIH
SP_MIF_2012_2020$SELECAO<-" "
SP_MIF_2012_2020$SELECAO[SP_MIF_2012_2020$criterio==0 & SP_MIF_2012_2020$CRITERIO2==0 ]<-"0"
SP_MIF_2012_2020$SELECAO[SP_MIF_2012_2020$criterio==1 &  SP_MIF_2012_2020$CRITERIO2==1 ]<-"1"
SP_MIF_2012_2020$SELECAO[SP_MIF_2012_2020$criterio==0 & SP_MIF_2012_2020$CRITERIO2==1 ]<-"2"
SP_MIF_2012_2020$SELECAO[SP_MIF_2012_2020$criterio==1 & SP_MIF_2012_2020$CRITERIO2==0 ]<-"1"

# Gravando a base MIF com alguns campos selecionados e incluindo novos campos 
write.csv2(SP_MIF_2012_2020, "SP_MIF_Alguns_Campos_Com_Cluster_2012_2020.csv", row.names = F) 

# gerar uma base MIF e obstetricia (OBS) que atende ao criterio original ou é relacionada a uma AIH que atende ao critério original
SP_OBS_2012_2020<-SP_MIF_2012_2020 %>% 
   filter(SELECAO=="1" | SELECAO=="2")

# Incluindo uma Variável que detalha a participação em um cluster     
    SP_OBS_2012_2020$CLUSTER<-" "
    SP_OBS_2012_2020$CLUSTER[is.na(SP_OBS_2012_2020$AIH_REF)]<-"10"
    SP_OBS_2012_2020$CLUSTER[!is.na(SP_OBS_2012_2020$AIH_REF) & SP_OBS_2012_2020$criterio==1 &  SP_OBS_2012_2020$CRITERIO2==1 &  SP_OBS_2012_2020$AIH_REF == SP_OBS_2012_2020$N_AIH ]<-"11"
    SP_OBS_2012_2020$CLUSTER[!is.na(SP_OBS_2012_2020$AIH_REF) & SP_OBS_2012_2020$criterio==1 &  SP_OBS_2012_2020$CRITERIO2==1 &  SP_OBS_2012_2020$AIH_REF != SP_OBS_2012_2020$N_AIH ]<-"12"
    SP_OBS_2012_2020$CLUSTER[!is.na(SP_OBS_2012_2020$AIH_REF) &SP_OBS_2012_2020$criterio==0 &  SP_OBS_2012_2020$CRITERIO2==1]<-"22"
    SP_OBS_2012_2020$CLUSTER[!is.na(SP_OBS_2012_2020$AIH_REF) &SP_OBS_2012_2020$criterio==1 &  SP_OBS_2012_2020$CRITERIO2==0]<-"13"
    
# Identificando na  base  MIF as AIHS que  antecedem as AIH com categoria de CLUSTER=13
    CLUSTER13<-SP_OBS_2012_2020 %>% 
      filter(CLUSTER=="13")  %>% 
      dplyr:: select(grupoaih, CLUSTER)
    
        SP_MIF_2012_2020<-SP_MIF_2012_2020 %>% 
      left_join(CLUSTER13, by=c("N_AIH"="grupoaih")) 
  
      ANTECEDE<-SP_MIF_2012_2020 %>% 
      filter(CLUSTER=="13")
      ANTECEDE$CLUSTER<-"33"  
      ANTECEDE<-ANTECEDE %>% 
           filter(ANTECEDE$CRITERIO2=="0")
      ANTECEDE<- distinct(ANTECEDE, N_AIH, .keep_all=TRUE)
   
  # Incluindo os registros da base antecede na base OBS
   SP_OBS_2012_2020<-rbind(SP_OBS_2012_2020, ANTECEDE)
 
    # Incluindo uma variavel chamada FREQ que conta os clusters de AIH na base OBS
   SP_OBS_2012_2020_cluster<-as.data.frame(table(SP_OBS_2012_2020$grupoaih))
   SP_OBS_2012_2020<-SP_OBS_2012_2020 %>% 
     left_join(SP_OBS_2012_2020_cluster,by=c("grupoaih"= "Var1")) 

  
# gravando os aqruivo SP_OBS_MIF_2012_2020.scv) 
    
    write.csv2(SP_OBS_2012_2020, "SP_OBS_Alguns_Campos_Com_Cluster_2012_2020.csv", row.names = F) 
  
# limpando o wokspace
    rm(list=ls())
###########################################################################################################
   

