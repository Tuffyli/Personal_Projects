# ---------------------------------------------------------------------------- #
# Data Organization
# DataBase adjustment
# Last edited by: Tuffy Licciardi Issa
# Date: 01/05/2025
# ---------------------------------------------------------------------------- #

# ---------------------------------------------------------------------------- #
# Libraries -----
# ---------------------------------------------------------------------------- #

library(dplyr)
library(fixest)
library(ggplot2)
library(did)
library(data.table)
library(zoo)
library(stargazer)


# ---------------------------------------------------------------------------- #
# 1. First data construction -----
# ---------------------------------------------------------------------------- #

# base <- read.csv("C:/Users/tuffy.issa/Documents/IC/Bases/panel_workers_2003to2019.csv")


base <- read.csv("C:/Users/tuffy/Documents/IC/Bases/panel_workers_2003to2019.csv")

base <- base %>%
  filter (ano < 2014)

## 1.1 Dummy for RAIS presence ####
base <- base %>%
  group_by(code_id) %>%
  mutate(
    rais_aux = ifelse(year_first_treated - ano == 1, #Dummy pre-treat
                      rais_, NA),
    max_aux = max(rais_aux, na.rm = TRUE),

    #Dummy for presence in RAIS before-treatment
    rais_aux2 = ifelse(any(rais_ == 1 & ano < year_first_treated), 1, 0),
    max_aux2 = max(rais_aux2, na.rm = TRUE)
  )


temp <- base

#Filtering
base <- base %>%
  filter( max_aux2 == 1)

#Sample
print(nrow(base)/nrow(temp))

##1.2 Fixed Effects and controls #####
base$ano_mun <- as.numeric(interaction(base$ano, base$cod_municipio))
                           
base <- base %>%
  select(
    -rais_aux,
    -max_aux
  )

##1.3 Arraning Control data ####
# Searching for control variables in the year before treat closest to treatment
base <- base %>%
  group_by(code_id) %>%
  mutate(
    raca_aux = ifelse(
      is.na(raca_initial),
      raca_[ano == max(ano[!is.na(raca_) & ano < year_first_treated], na.rm = TRUE)],
      raca_initial),

    sexo_aux = ifelse(
      is.na(sexo_initial),
      sexo_[ano == max(ano[!is.na(sexo_) & ano < year_first_treated], na.rm = TRUE)],
      sexo_initial),

    ensino_aux = ifelse(
      is.na(sexo_initial),
      escolaridade_[ano == max(ano[!is.na(escolaridade_) & ano < year_first_treated], na.rm = TRUE)],
      escolaridade_initial)
    )


#Criando as dummys
base <- base %>%
  mutate(
    branco_dummy = ifelse( #Dummy de raça/cor
      !is.na(raca_aux) & raca_aux == 2, 1, 0
    ),
    sexo_dummy = ifelse( #Dummy por sexo Masculino = 1
      !is.na(sexo_aux) & sexo_aux == 1, 1, 0
    ),

    ensino_dummy = ifelse(
      !is.na(ensino_aux) & ensino_aux >= 9, 1, 0
    )

  )
base$ano_sexo <- as.numeric(interaction(base$sexo_dummy, base$ano))
base$ano_branco <- as.numeric(interaction(base$branco_dummy, base$ano))
base$ano_ensino <- as.numeric(interaction(base$ensino_dummy, base$ano))


# ---------------------------------------------------------------------------- #
##1.4 CNAE CBO code stablishment ####
# ---------------------------------------------------------------------------- #

#Cnae numeric code
base$cnae_group <- as.numeric(factor(base$gcnae))
base$cnae_group <- ifelse(base$cnae_group == 1, 0, base$cnae_group)

#CBO numeric code
base$cbo_group <- as.numeric(cut( base$ocupacao_cbo2002_,
                                  breaks = c(-Inf,100000,200000, 300000, 400000, 500000, 600000, 700000,800000, 900000, Inf)))
str(base)

summary(base$cbo_group)


#Tratando do dos NA para os grupos CBO
base$cbo_group <- ifelse(is.na(base$cbo_group), 0, base$cbo_group)

## 1.5 Salvando a Base de dados editada#################################
write.csv(base, "C:/Users/tuffy/Documents/IC/Bases/panel_workers_2003to2013_nova.csv")



# ---------------------------------------------------------------------------- #
#2.  Rais presence ----
##2.1 More adjustments ----
# ---------------------------------------------------------------------------- #
base <- read.csv("C:/Users/tuffy/Documents/IC/Bases/panel_workers_2003to2013_nova.csv")

#Estabelecendo o grupo de indivíduos que se encontrará na Rais em todos os períodos.
base <- base %>%   
  group_by(code_id) %>% 
  mutate( all_in_rais = min(rais_)
    )

#Cortando algumas variáveis
base <- base %>% 
  select(
    -raca_aux,
    -sexo_aux,
    -ensino_aux,
    -sexo_initial,
    -escolaridade_initial,
    -raca_initial,
    -sexo_,
    -escolaridade_,
    -raca_,
    -rais_aux2,
    -max_aux2,
  )


#Estabelecendo se os trabalhadores são White collars
strings <- unique(base$ocupacao_cbo2002_)

#Extraindo os valores em string do grupo cbo
classification_collar <- data.frame(strings) %>%
  mutate(
    first_two_digits = substr(strings, 1, 2),
    first_digit = substr(strings, 1, 1),
    white_collar = case_when(
      first_two_digits %in% c("62", "63", "64", "34", "37", "51", "61" ) |
        first_digit %in% c("7", "8", "9") ~ 0, #Blue Collar
      
      
      first_two_digits %in% c("30", "31", "32", "33", "35", "39", "52") |
        first_digit %in% c("1", "2", "4") ~ 1, #White collar
      TRUE ~ NA_real_
    )
  ) %>% 
  select(strings, white_collar)
base <- merge(base, classification_collar,
              by.x = "ocupacao_cbo2002_", by.y = "strings", all.x = TRUE)



#Criação de variáveis de auxílio e remuneração
base <- base %>% 
  mutate(time_to_treat = (ano - year_first_treated),
         treat = 1,
         previous_year = year_first_treated - 1,
         #Criando a variável em log para o salário
         nvl_rem = log(remuneracao_media_sm_ + 1)
         ) %>%
  #Removendo os valores das categorias CBO, não mais necessários
  select(-ocupacao_cbo2002_)



# Extraindo os valores pré-tratamento
pre_treatment <- base %>%
  filter(ano == previous_year) %>%
  rename(
    cbo_pre_treat = cbo_group,
    rem_pre_treat = nvl_rem,
    mun_pre_treat = cod_mun_rais_,
    cnae_pre_treat = cnae_group
         ) %>% 
  select(
    code_id,
    previous_year,
    cbo_pre_treat,
    rem_pre_treat,
    mun_pre_treat,
    cnae_pre_treat
  )

# Mesclando com o dataframe original
base <- base %>%
  left_join(pre_treatment, by = c("code_id", "previous_year")) %>% 
  arrange(code_id)




####CNAE###
summary(base$cnae_group)

#Passos para  a criação da dummy
treat <- base %>%
  arrange(code_id, ano) %>% 
  select(
    code_id,
    ano,
    cnae_group,
    cnae_pre_treat,
    year_first_treated,
    rais_
    ) %>%
  group_by(code_id) %>% 
  
  mutate(
    #Dummy auxiliar - livre mudança após o tratamento
    dummy_cnae_aux =        
      case_when(
        #Aqui listamos os possíveis casos
          ano < year_first_treated & cnae_group == 0 ~ 1, 
          
          ano >= year_first_treated & cnae_group == 0 ~ 1,
          
          ano >= year_first_treated & cnae_group != 0 & !(cnae_group %in% cnae_group[ano < year_first_treated]) ~ 1,
          
          TRUE ~ 0
        ),
    #Dummy final
    first_one = which(dummy_cnae_aux == 1 & ano >= year_first_treated)[1], #Primeira linha post-treat
    
    dummy_cnae = ifelse(
      row_number() >= first_one & !is.na(first_one), 1, dummy_cnae_aux
    ),

    dummy_cnae = ifelse(
      is.na(first_one) & ano >= year_first_treated, 0 , dummy_cnae #Essa condição lida com os casos em que não há first_one
    )
  )

treat <- treat %>% 
  select(
    code_id, ano, dummy_cnae #selecionando apenas as colunas suficientes para realizar o merge
  )

#Unindo as os dataframes
base <- base %>% 
  left_join(treat, by= c("code_id", "ano"))


###CB0###
 
#Informações que importam para o CBO
treat <- base %>%
  select(
    code_id,
    ano,
    cbo_group,
    cbo_pre_treat,
    year_first_treated,
    rais_
  ) 


treat <- treat %>% 
  arrange(code_id, ano) %>% 
  group_by(code_id) %>% 
  mutate(
    dummy_cbo_aux =
      case_when(
        ano < year_first_treated & cbo_group == 0 ~ 1, 
        
        ano >= year_first_treated & cbo_group == 0 ~ 1,
        
        ano >= year_first_treated & cbo_group != 0 & !(cbo_group %in% cbo_group[ano < year_first_treated]) ~ 1,

        TRUE ~ 0
      )
  )

#A última dummy
treat <- treat %>% 
  group_by(code_id) %>% 
  mutate(
    first_one = which(dummy_cbo_aux == 1 & ano >= year_first_treated)[1], #Primeira linha com mudança post-treat
    
    #DUMMY FINAL
    dummy_cbo = case_when(
      ano < year_first_treated ~ dummy_cbo_aux,
      ano >= year_first_treated & row_number() >= first_one & !is.na(first_one) ~ 1,
      ano >= year_first_treated & is.na(first_one) ~ 0,
      TRUE ~ 0
    )
    
  )


treat <- treat %>% 
  select(
    code_id, ano, dummy_cbo
  )


base <- base %>%
  left_join(treat, by = c("code_id","ano")
            ) %>% 
  arrange(code_id, ano) 


###Collars###
#Separando os indivíduos entre os collars:
base <- base %>% 
  group_by(code_id) %>% 
  mutate(
    white_dummy =
      case_when( 
        ano < year_first_treated & any(white_collar == 1) ~ 1,
        ano < year_first_treated & any(white_collar == 0) ~ 0,
        TRUE ~ NA_real_
      )
  )

#Extraindo o maior valor
base <- base %>% 
  group_by(code_id) %>% 
  mutate(
    white_dummy = ifelse(any(!is.na(white_dummy)),
                        max(white_dummy, na.rm = T),
                        NA)
  )


#Para a descrição das variáveis da base, o Stargazer necessita dos valores numérios
# Convert the variables to numeric if necessary (if they are factors)
base$sexo_dummy <- as.numeric(base$sexo_dummy)
base$branco_dummy <- as.numeric(base$branco_dummy)
base$ensino_dummy <- as.numeric(base$ensino_dummy)
base$white_dummy <- as.numeric(base$white_dummy)


## 2.2 Descriptive Statistics ----
stargazer(base[, c("sexo_dummy", "branco_dummy", "ensino_dummy", "white_dummy")], 
          type = "text",          
          title = "Descrição das variáveis",
          summary.stat = c("mean", "sd", "min", "max", "median"), 
          digits = 3,
          covariate.labels = c(
            "Sexo masculino",
            "Cor/raça branca",
            "Ensino Superior",
            "White Collar"
          ))



stargazer::stargazer(base,
                     type = "text",
                     digits = 3)


summary(base)

#Saving final
write.csv(base, "C:/Users/tuffy/Documents/IC/Bases/base_atual_dum_v3.csv")

