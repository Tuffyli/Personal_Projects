# ---------------------------------------------------------------------------- #
# Combined DrDiD Estimators
# Last edited by: Tuffy Licciardi Issa
# Date: 25/10/2025
# ---------------------------------------------------------------------------- #

library(dplyr);
library(here);
library(tidyverse);
library(stargazer);
library(MatchIt);
library(broom);
library(kableExtra);
library(cobalt);
library(cowplot);
library(fixest)
# ---------------------------------------------------------------------------- #
#1. Data Open ----
# ---------------------------------------------------------------------------- #

data <- data %>%
  group_by(ID_DOMICILIO) %>%
  
  mutate(
    num_moradores = n(),
    
    renda_dom_pc = case_when(
      # Condition 1: If household income (v4614) is NA or an "ignored" code,
      # set per-capita income to NA.
      is.na(first(v4614)) | first(v4614) == 999999999999 ~ NA,
      
      # Condition 2: If the number of residents is greater than 0 (valid case),
      # compute per-capita income.
      num_moradores > 0 ~ first(v4614) / num_moradores,
      
      # Condition 3: Fallback — set to 0 to avoid errors in edge cases.
      TRUE ~ 0
    )
  ) %>%
    ungroup()

# ---------------------- #
##1.1 Filter -----
# ---------------------- #



temp <- data %>% 
  #filter(!is.na(treatment)) %>% 
  select(
    # --- Identificação ---
    v0101,            # Ano de referência
    uf,               # Unidade da Federação
    v0102,            # Número de controle do domicílio
    v0103,            # Número de série do domicílio
    v0301,            # Número de ordem do morador
    v0302,            # Sexo
    ID_DOMICILIO,     # Identificador único do domicílio
    
    # --- Demográficas ---
    v8005,            # Idade
    v0401,            # Condição na unidade domiciliar
    v0404,            # Cor/Raça
    
    # --- Educação ---
    v0607,            # Curso mais elevado que frequentou
    v0610,            # Última série concluída
    v0602,            # Frequenta escola ou creche
    v0603,            # Curso que frequenta
    v4703,            # Anos de estudo
    
    # --- Trabalho e Rendimento ---
    v9001,            # Trabalhou na semana de referência
    v0713,            # Horas trabalhadas normalmente por semana (para crianças 5-9 anos)
    v7122,            # Rendimento mensal em dinheiro do trabalho
    v7125,            # Rendimento mensal em produtos/mercadorias do trabalho
    v7127,            # Código de rendimento em benefícios
    v7128,            # Indicador de trabalho não remunerado
    v1254,            # Código de rendimento de pensão
    v4721,            # Valor do rendimento mensal domiciliar
    v4614,            # Rendimento mensal domiciliar (do arquivo de domicílio)
    renda_dom_pc,     # Sua variável já criada de renda domiciliar per capita
    
    # --- Ocupação ---
    v9906,            # Código da ocupação
    v9907,            # Código da atividade principal
    v0701,            # Trabalhou no último ano
    
    # --- Fecundidade (Nº de filhos da mulher) ---
    v1141,            # Nº de filhos homens no domicílio
    v1142,            # Nº de filhos mulheres no domicílio
    v1151,            # Nº de filhos homens em outro local
    v1152,            # Nº de filhos mulheres em outro local
    
    # --- Família e Migração ---
    v4723,            # Tipo de família
    v0501,            # Nasceu neste município
    
    # --- Variáveis de controle/peso ---
    v4729,            # Peso da pessoa
    treatment,
    
    # --- Union and Household ---
    v1001,
    v1002,
    v0402
    
  ) %>% 
  rename(
    # --- Identificação ---
    ano = v0101,
    uf = uf,
    id_domicilio = ID_DOMICILIO,
    num_controle_dom = v0102,
    num_serie_dom = v0103,
    num_ordem_morador = v0301,
    
    # --- Demográficas ---
    age = v8005,
    sex = v0302,
    condicao_no_dom = v0401,  # Condição na unidade domiciliar 
    cor = v0404,
    
    # --- Educação ---
    curso_mais_elevado = v0607, # Curso mais elevado que frequentou 
    ultima_serie_concluida = v0610, # Última série concluída neste curso que frequentou 
    frequenta_escola = v0602, # Frequenta escola ou creche 
    tipo_curso_frequenta = v0603, # Qual o curso que frequenta 
    anos_estudo = v4703, # Anos de estudo 
    
    # --- Trabalho e Rendimento ---
    trabalhou_semana_ref = v9001, # Trabalhou na semana de 24 a 30/09/95 
    horas_trabalhadas = v0713, # Quantas horas trabalhava normalmente na semana 
    renda_trab_dinheiro = v7122, # Valor de rendimento mensal em dinheiro (no trabalho da semana) 
    renda_trab_produto = v7125, # Valor de rendimento mensal em produtos ou mercadorias 
    cod_renda_beneficio = v7127, # Código_6 de rendimento mensal em beneficios 
    indicador_nao_remunerado = v7128, # Código 8 de não remunerado 
    pensao = v1254, # Código 2 de rendimento de pensão 
    renda_dom_total = v4721, # Valor do rendimento mensal domiciliar 
    renda_dom_total_v2 = v4614, # Rendimento mensal domiciliar' 
    renda_dom_per_capita = renda_dom_pc,
    
    # --- Ocupação ---
    trabalhou_ultimo_ano = v0701,
    cod_ocupacao = v9906, # Código da ocupação que exercia no trabalho 
    cod_atividade = v9907, # Código da atividade principal do Emprendimento 
    
    # --- Fecundidade ---
    filhos_homens_dom = v1141, # Número de filhos - Homens que moram neste domicilio 
    filhos_mulheres_dom = v1142, # Número de filhos - Mulheres que moram neste domicilio 
    filhos_homens_outrolocal = v1151, # Número de filhos Homens que moram em outro local 
    filhos_mulheres_outrolocal = v1152, # Número de filhos - Mulheres que moram em outro local 
    
    # --- Família e Migração ---
    tipo_familia = v4723, # Tipo de familia 
    nasceu_no_municipio = v0501, # Nasceu neste municipio 
    house_status = v0402,
    union_cond = v1002,
    union_status = v1001,
    
    # --- Variáveis de controle/peso ---
    peso_pessoa = v4729 # Peso da pessoa 
  )


saveRDS(temp %>% select(-treatment), "C:/Users/tuffy/Documents/Trabalhos/Ava_Pol/Bases/Pnadpnad_filtered_9295.rds")


# ---------------------------------------------------------------------------- #
# 2. Data Adjustment ----
# ---------------------------------------------------------------------------- #

data <- readRDS("C:/Users/tuffy/Documents/Trabalhos/Ava_Pol/Bases/Pnadpnad_filtered_9295.rds")

## 3.1 Treatment Def ----
#To define treatment status

data <- data %>% 
  mutate(
    treatment = case_when(
      # Treatment (= 1)
      sex == 4 &                               # Female
        (age >= 15 & age <= 24) &                # Age between 15 e 24
        union_status == 1 &                      # In Union
        (union_cond == 8 | union_cond == 6) &    # Consesual (8) OR Religious (6)
        (house_status == 1 | house_status == 2)  # Family Household status Head (1) ou Wife (2)
      ~ 1,
      
      # Control Group (= 0)
      sex == 4 &                               # Female
        (age >= 15 & age <= 24) &                # Age between 15 e 24
        union_status == 1 &                      # In Union
        (union_cond == 2 | union_cond == 4)  &   # Rligious and Civil (2) OR Civil (4)
        (house_status == 1 | house_status == 2)  # Family Household status Head (1) ou Wife (2)
      ~ 0,
      #Other cases
      TRUE ~ NA_integer_
    ),
    
    #Other controls
    cor = case_when(       #Race/Color
      cor %in% c(2,6) ~ 1, #Asian and White 
      cor == 9 ~ NA, .default = 0),
    
    grupo_cbo = case_when( #Occupation classification
      !is.na(cod_ocupacao) & cod_ocupacao < 900  ~ cod_ocupacao %/% 100, #Large Groups
      TRUE ~ NA_real_),
    
    ano = ano + 1900, #year
    trabalhou_ultimo_ano = ifelse(trabalhou_ultimo_ano == 1, 1 , 0), #Worked last year status
    fem = ifelse(sex == 4, 1, 0), #Is female (=1)
    
    pensao_dummy = ifelse(!is.na(pensao), 1, 0), #Recieves alimony
    #Total sons calculation
    dummy_filhos_homens_dom = ifelse(!filhos_homens_dom %in% c(-1,99), filhos_homens_dom, 0),
    dummy_filhos_mulheres_dom = ifelse(!filhos_mulheres_dom %in% c(-1,99), filhos_mulheres_dom, 0),
    dummy_filhos_homens_outrolocal = ifelse(!filhos_homens_outrolocal %in% c(-1,99), filhos_homens_outrolocal, 0),
    dummy_filhos_mulheres_outrolocal = ifelse(!filhos_mulheres_outrolocal %in% c(-1,99), filhos_mulheres_outrolocal, 0),
    #Total kids per woman
    total_filhos = dummy_filhos_homens_dom + dummy_filhos_mulheres_dom + dummy_filhos_homens_outrolocal + dummy_filhos_mulheres_outrolocal
  ) %>%
  filter(!is.na(treatment)) #removing other groups


# ---------------------------------------------------------------------------- #
# 3. Data Summary ----
# ---------------------------------------------------------------------------- #

#3.1 Data extraction
summary_df <- data %>%
  select(
    treatment,                    # Treatment indicator (0/1)
    fem,                          # Female (=1)
    cor,                          # Race/Color (per your coding)
    age,                          # Age
    anos_estudo,                  # Years of education
    ultima_serie_concluida,       # Last grade completed
    tipo_curso_frequenta,         # Course enrollment/type
    renda_dom_per_capita,         # Household per-capita income
    pensao_dummy,                 # Alimony/Pension (=1)
    grupo_cbo,                    # CBO/occupation group
    #trabalhou_ultimo_ano,        # Worked last year (=1) [optional]
    dummy_filhos_homens_dom,      # Male children in household
    dummy_filhos_mulheres_dom,    # Female children in household
    total_filhos,                 # Total children
    peso_pessoa                   # Sampling weight
  ) %>%
  mutate(across(everything(), ~ as.numeric(as.character(.))))  # Coerce to numeric for summary stats


weighted_stats <- function(x, w) {
  w <- w[!is.na(x)]                       # Keep weights where x is not NA
  x <- x[!is.na(x)]                       # Drop NA values in x
  w_mean <- weighted.mean(x, w)           # Weighted mean
  w_var <- sum(w * (x - w_mean)^2) / sum(w)  # Population-weighted variance
  w_sd <- sqrt(w_var)                     # Weighted SD
  list(
    mean = w_mean,
    sd = w_sd,
    min = min(x),                         # Minimum (unweighted)
    max = max(x)                          # Maximum (unweighted)
  )
}

vars <- c("treatment","fem", "cor", "curso_mais_elevado", "age", "anos_estudo", "ultima_serie_concluida",
          "tipo_curso_frequenta", "renda_dom_per_capita", "pensao_dummy", "grupo_cbo",
          "dummy_filhos_homens_dom", "dummy_filhos_mulheres_dom", "total_filhos")

# Appling the function
summary_table <- lapply(vars, function(v) {
  res <- weighted_stats(data[[v]], data$peso_pessoa)
  data.frame(
    Variable = v,
    Mean = res[["mean"]],
    SD   = res[["sd"]],
    Min  = res[["min"]],
    Max  = res[["max"]])
}) %>% bind_rows() %>%  #Stacks the results into a single data frame  
  select(-Variable) #Drops the variable

#Column and row Labels
col <- c( "Mean", "SD", "Min", "Max")
row <- c("Treatment","Female = 1",
         "Race (White or Asian = 1)",
         "Highest Education",
         "Age",
         "Years of education",
         "Last grade concluded",
         "Course enrollment",
         "Household per capita wage",
         "Pension (yes = 1)",
         "CBO Group",
         "Male child (house)",
         "Female child (house)",
         "Total childs")
colnames(summary_table) <- col
rownames(summary_table) <- row


print(summary_table <- summary_table %>% mutate(across(where(is.numeric), ~ round(.x, 2)))) 

saveRDS(summary_table, "C:/Users/tuffy/Documents/Trabalhos/Ava_Pol/Bases/summary.rds")
saveRDS(data,"C:/Users/tuffy/Documents/Trabalhos/Ava_Pol/Bases/final_filtered_9295.rds")
