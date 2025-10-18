# ---------------------------------------------------------------------------- #
# Overall Working Paper Code
# Compilation of all module codes
# Last edited by: Tuffy Licciardi Issa
# Date: 13/10/2025
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
library(gridExtra)
library(knitr)
library(grid)

# ---------------------------------------------------------------------------- #
#DATA ----
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

rm(list = ls())



# ---------------------------------------------------------------------------- #
# <<<<<<<<<<< ----
#REGRESSION -----
# ---------------------------------------------------------------------------- #

data <- read.csv("C:/Users/tuffy/Documents/IC/Bases/base_atual_dum_v3.csv")


# 3. Main Graphs ----
## 3.1 Function ----
plot <- function(df,
                 plot_title,
                 var_y,
                 controles
) {
  
  
  ini <- Sys.time()
  
  print(paste0("Calculando para:", var_y," :)"))
  
  var_y <- as.character(substitute(var_y))
  
  # Equações com contorles
  sunab_formula <- as.formula(
    paste(
      var_y, "~  sunab(year_first_treated,time_to_treat,ref.p = -1,ref.c = 2013) | code_id + ano_sexo + ano_branco + ano_ensino"
    )
  )
  
  calsan_formula <- as.formula(
    "~ ano_sexo + ano_branco + ano_ensino + code_id "
  )
  
  
  ##Estimações##
  #Sun & Abraham
  est_sunab <- feols(sunab_formula, data = df, cluster = ~ code_id)
  # Callaway & Sant'anna
  calsan_did <- did::att_gt(
    yname = var_y,
    gname = "year_first_treated",
    idname = "code_id",
    tname = "ano",
    xformla = calsan_formula,
    data = df,
    control_group = "notyettreated",
    base_period = "universal",
    clustervars = "code_id"
  )
  est_calsan <- aggte( MP = calsan_did, type = "dynamic", na.rm = TRUE)
  print(est_calsan)
  
  ##Extraindo os gráficos das duas estimações##
  plot_sunab <- iplot(est_sunab, ref.line = -1,
                      xlab = 'Time to treatment',
                      main = 'Cal_San ES: IGNORAR')  
  
  plot_calsan <- ggdid(est_calsan) +
    ggtitle("Event Study: Callaway & Sant'anna, IGNORAR ") +
    labs("Tempo até o tratamento") +
    theme_minimal()
  
  ##Extraindo os coeficientes##
  data_calsan <- ggplot_build(plot_calsan)$data[[1]]
  data_calsan <- as.data.frame(data_calsan)
  
  data_sunab <- plot_sunab[[1]] 
  data_sunab <- data_sunab %>% 
    mutate(colour = ifelse(id == 1, '#f7200a')) %>% 
    rename(
      ymin = ci_low,
      ymax = ci_high,  
      group = id ) %>% 
    select(colour, x, y, ymin, ymax, group,-estimate, -estimate_names, -estimate_names_raw,-is_ref)
  
  data_calsan <- data_calsan %>% 
    mutate(colour = '#145ede',
           group = 2,
           y = ifelse(x == -1, 0, y),
           ymin = ifelse(x == -1, 0, ymin),
           ymax = ifelse(x == -1, 0, ymax)) %>% 
    select(-PANEL, -shape, -size, -fill, -alpha, -stroke)
  
  
  #Unindo os coeficientes das duas estimações
  df_completo <- rbind(data_sunab,data_calsan)
  
  
  
  
  
  #Resultados das estimações
  print(summary(est_sunab))
  print(summary(est_calsan))
  
  df_completo$x <- case_when(df_completo$group == 1 ~ df_completo$x,
                             df_completo$group == 2 & df_completo$x != -1 ~ df_completo$x + 0.2,
                             TRUE ~ NA)
  
  return(df_completo)
  
  
  delta_t <- Sys.time() - ini
  print(delta_t)
  rm(delta_t, ini)
  
}


# ---------------------------------------------------------------------------- #
## 3.2 RAIS ----

estimacoes_rais <- plot(data,
                        plot_title = '',
                        var_y = "rais_")

if (!dir.exists("C:/Users/tuffy/Documents/IC/Graphs")) {
  dir.create("C:/Users/tuffy/Documents/IC/Graphs")
}



#Para retir



p <- ggplot(estimacoes_rais, aes(x = x, y = y, color = colour, group = group)) +
  geom_point(size = 3) +
  geom_errorbar(aes(ymin = ymin, ymax = ymax), width = 0.5) +
  geom_hline(yintercept = 0, color = "#D62728") +
  geom_vline(xintercept = -1, color = "#BEBEBE", linetype = "dashed") +
  scale_color_manual(values=c('black','red'),labels=c("Callaway & Sant'anna","Sun & Abraham")) +
  labs(x = "Years to treatment", y = '', colour = '') +
  theme_minimal(base_size = 14) +
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.line.x = element_line(),
    axis.line.y = element_line(),
    legend.position = "bottom",
    panel.grid.minor.x = element_blank()
  ) +
  scale_x_continuous(limits = c(-9.2, 5.2),
                     breaks = c(-9,-8,-7,-6,-5,-4,-3,-2,-1,0,1,2,3,4,5),
                     labels = c('-9','-8','-7','-6','-5','-4','-3','-2','-1','0','+1','+2','+3','+4','+5')) +
  scale_y_continuous(limits = c(-0.95, 0.155),
                     breaks = c(-0.90,-0.75,-0.60,-0.45,-0.30,-0.15,0,0.15),
                     labels = c('-0.90','-0.75','-0.60','-0.45','-0.30','-0.15','0','0.15'))



ggsave("C:/Users/tuffy/Documents/IC/Graphs/plot_rais.jpeg", plot = p, device = "jpeg", width = 10, height = 6, dpi = 600)







# ---------------------------------------------------------------------------- #
## Retirando A SUNAB

est_rais2 <- calsun %>% 
  filter(group == 2) %>% 
  mutate(x = x - 0.2)

p <- ggplot(est_rais2, aes(x = x, y = y, color = colour, group = group)) +
  geom_point(size = 3) +
  geom_errorbar(aes(ymin = ymin, ymax = ymax), width = 0.5) +
  geom_hline(yintercept = 0, color = "#D62728") +
  geom_vline(xintercept = -1, color = "#BEBEBE", linetype = "dashed") +
  scale_color_manual(values='black',labels="Callaway & Sant'anna") +
  labs(x = "Years to treatment", y = '', colour = '') +
  theme_classic(base_size = 18) +   
  theme(
    axis.line = element_line(),
    axis.ticks.length = unit(5, "pt"),
    axis.ticks = element_line(colour = "black"),  
    axis.ticks.x = element_line(colour = "black"),
    axis.ticks.y = element_line(colour = "black"),
    axis.text.x = element_text(margin = margin(t = 5), size = 18),
    legend.position = "none",
    
    axis.text.y = element_text(size = 18)
  ) +
  scale_x_continuous(
    limits = c(-9.2, 4.5),
    breaks = c(-9,-8,-7,-6,-5,-4,-3,-2,-1,0,1,2,3,4),
    labels = c('-9','-8','-7','-6','-5','-4','-3','-2','-1','0','+1','+2','+3','+4'),
    minor_breaks = c(-9,-8,-7,-6,-5,-4,-3,-2,-1,0,1,2,3,4)  
  ) +
  scale_y_continuous(
    limits = c(-0.95, 0.155),
    breaks = c(-0.90,-0.75,-0.60,-0.45,-0.30,-0.15,0,0.15),
    labels = c('-0.90','-0.75','-0.60','-0.45','-0.30','-0.15','0','0.15')
  )



p



ggsave("C:/Users/tuffy/Documents/IC/Graphs//united/plot_rais2_v3.jpeg", plot = p, device = "jpeg", width = 10, height = 6, dpi = 600)
ggsave("C:/Users/tuffy/Documents/IC/Graphs//united/plot_rais2_v3.pdf", plot = p, device = "pdf", width = 10, height = 6, dpi = 300)


rm(p)


# ---------------------------------------------------------------------------- #
## 3.3 CBO ----

estimacoes_cbo <- plot(data,
                       plot_title = '',
                       var_y = dummy_cbo)


p <- ggplot(estimacoes_cbo, aes(x = x, y = y, color = colour, group = group)) +
  geom_point(size = 3) +
  geom_errorbar(aes(ymin = ymin, ymax = ymax), width = 0.5) +
  geom_hline(yintercept = 0, color = "#D62728") +
  geom_vline(xintercept = -1, color = "#BEBEBE", linetype = "dashed") +
  scale_color_manual(values=c('black','red'),labels=c("Callaway & Sant'anna","Sun & Abraham")) +
  labs(x = "Years to treatment", y = '', colour = '') +
  theme_minimal(base_size = 14) +
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.line.x = element_line(),
    axis.line.y = element_line(),
    legend.position = "bottom"
  ) +
  scale_x_continuous(limits = c(-9.2, 5.2),
                     breaks = c(-9,-8,-7,-6,-5,-4,-3,-2,-1,0,1,2,3,4,5),
                     labels = c('-9','-8','-7','-6','-5','-4','-3','-2','-1','0','+1','+2','+3','+4','+5')) +
  scale_y_continuous(limits = c( -0.155, 0.95),
                     breaks = c(-0.15,0,0.15,0.30,0.45,0.60,0.75,0.90),
                     labels = c('-0.15','0','0.15','0.30','0.45','0.60','0.75','0.90'))


ggsave("Graphs/plot_cbo.jpeg", plot = p, device = "jpeg", width = 10, height = 6, dpi = 600)


# ---------------------------------------------------------------------------- #
## 3.4 CNAE ----

estimacoes_cnae <- plot(data,
                        plot_title = '',
                        var_y = dummy_cnae)


p <- ggplot(estimacoes_cnae, aes(x = x, y = y, color = colour, group = group)) +
  geom_point(size = 3) +
  geom_errorbar(aes(ymin = ymin, ymax = ymax), width = 0.5) +
  geom_hline(yintercept = 0, color = "#D62728") +
  geom_vline(xintercept = -1, color = "#BEBEBE", linetype = "dashed") +
  scale_color_manual(values=c('black','red'),labels=c("Callaway & Sant'anna","Sun & Abraham")) +
  labs(x = "Years to treatment", y = '', colour = '') +
  theme_minimal(base_size = 14) +
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.line.x = element_line(),
    axis.line.y = element_line(),
    legend.position = "bottom"
  ) +
  scale_x_continuous(limits = c(-9.2, 5.2),
                     breaks = c(-9,-8,-7,-6,-5,-4,-3,-2,-1,0,1,2,3,4,5),
                     labels = c('-9','-8','-7','-6','-5','-4','-3','-2','-1','0','+1','+2','+3','+4','+5')) +
  scale_y_continuous(limits = c( -0.155, 1.10),
                     breaks = c(-0.15,0,0.15,0.30,0.45,0.60,0.75,0.90,1.05),
                     labels = c('-0.15','0','0.15','0.30','0.45','0.60','0.75','0.90','1.05'))



ggsave("Graphs/plot_cnae.jpeg", plot = p, device = "jpeg", width = 10, height = 6, dpi = 600)

gc()

# ---------------------------------------------------------------------------- #
# 4. White vs. Blue ----
# ---------------------------------------------------------------------------- #
## 4.1 Data Frames ----


blue_data <- data %>% 
  filter(white_dummy == 0)

white_data <- data %>% 
  filter(white_dummy == 1)

# ---------------------------------------------------------------------------- #
## 4.2 Blue Collar ----
### 4.2.1 RAIS ----
estimacoes_brais <- plot(blue_data,
                         plot_title = '',
                         var_y = "rais_")


p <- ggplot(estimacoes_brais, aes(x = x, y = y, color = colour, group = group)) +
  geom_point(size = 3) +
  geom_errorbar(aes(ymin = ymin, ymax = ymax), width = 0.2) +
  geom_hline(yintercept = 0, color = "#D62728") +
  geom_vline(xintercept = -1, color = "#BEBEBE", linetype = "dashed") +
  scale_color_manual(values=c('black','red'),labels=c("Callaway & Sant'anna","Sun & Abraham")) +
  labs(x = "Years to treatment", y = '', colour = '') +
  theme_minimal(base_size = 14) +
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.line.x = element_line(),
    axis.line.y = element_line(),
    legend.position = "bottom"
  ) +
  scale_x_continuous(limits = c(-9.2, 5.2),
                     breaks = c(-9,-8,-7,-6,-5,-4,-3,-2,-1,0,1,2,3,4,5),
                     labels = c('-9','-8','-7','-6','-5','-4','-3','-2','-1','0','+1','+2','+3','+4','+5')) +
  scale_y_continuous(limits = c(-0.95, 0.155),
                     breaks = c(-0.90,-0.75,-0.60,-0.45,-0.30,-0.15,0,0.15),
                     labels = c('-0.90','-0.75','-0.60','-0.45','-0.30','-0.15','0','0.15'))



p

ggsave("Graphs/plot_rais_bluecol.jpeg", plot = p, device = "jpeg", width = 10, height = 6, dpi = 600)



# ---------------------------------------------------------------------------- #
### 4.2.2 CBO ----
estimacoes_bcbo <- plot(blue_data,
                        plot_title = '',
                        var_y = "dummy_cbo")


p <- ggplot(estimacoes_bcbo, aes(x = x, y = y, color = colour, group = group)) +
  geom_point(size = 3) +
  geom_errorbar(aes(ymin = ymin, ymax = ymax), width = 0.2) +
  geom_hline(yintercept = 0, color = "#D62728") +
  geom_vline(xintercept = -1, color = "#BEBEBE", linetype = "dashed") +
  scale_color_manual(values=c('black','red'),labels=c("Callaway & Sant'anna","Sun & Abraham")) +
  labs(x = "Years to treatment", y = '', colour = '') +
  theme_minimal(base_size = 14) +
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.line.x = element_line(),
    axis.line.y = element_line(),
    legend.position = "bottom"
  ) +
  scale_x_continuous(limits = c(-9.2, 5.2),
                     breaks = c(-9,-8,-7,-6,-5,-4,-3,-2,-1,0,1,2,3,4,5),
                     labels = c('-9','-8','-7','-6','-5','-4','-3','-2','-1','0','+1','+2','+3','+4','+5')) +
  scale_y_continuous(limits = c( -0.155, 0.95),
                     breaks = c(-0.15,0,0.15,0.30,0.45,0.60,0.75,0.90),
                     labels = c('-0.15','0','0.15','0.30','0.45','0.60','0.75','0.90'))



ggsave("Graphs/plot_cbo_bluecol.jpeg", plot = p, device = "jpeg", width = 10, height = 6, dpi = 600)

# ----------------------------------------------------------------------------- #
### 4.2.3 CNAE ----
estimacoes_bcnae <- plot(blue_data,
                         plot_title = '',
                         var_y = "dummy_cnae")


p <- ggplot(estimacoes_bcnae, aes(x = x, y = y, color = colour, group = group)) +
  geom_point(size = 3) +
  geom_errorbar(aes(ymin = ymin, ymax = ymax), width = 0.2) +
  geom_hline(yintercept = 0, color = "#D62728") +
  geom_vline(xintercept = -1, color = "#BEBEBE", linetype = "dashed") +
  scale_color_manual(values=c('black','red'),labels=c("Callaway & Sant'anna","Sun & Abraham")) +
  labs(x = "Years to treatment", y = '', colour = '') +
  theme_minimal(base_size = 14) +
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.line.x = element_line(),
    axis.line.y = element_line(),
    legend.position = "bottom"
  ) +
  scale_x_continuous(limits = c(-9.2, 5.2),
                     breaks = c(-9,-8,-7,-6,-5,-4,-3,-2,-1,0,1,2,3,4,5),
                     labels = c('-9','-8','-7','-6','-5','-4','-3','-2','-1','0','+1','+2','+3','+4','+5')) +
  scale_y_continuous(limits = c( -0.155, 1.10),
                     breaks = c(-0.15,0,0.15,0.30,0.45,0.60,0.75,0.90, 1.05),
                     labels = c('-0.15','0','0.15','0.30','0.45','0.60','0.75','0.90', '1.05'))



ggsave("Graphs/plot_cnae_bluecol.jpeg", plot = p, device = "jpeg", width = 10, height = 6, dpi = 600)


# ---------------------------------------------------------------------------- #
## 4.3 White Collar ----
# ---------------------------------------------------------------------------- #
### 4.3.1 RAIS --------------
estimacoes_wrais <- plot(white_data,
                         plot_title = '',
                         var_y = "rais_")


p <- ggplot(estimacoes_wrais, aes(x = x, y = y, color = colour, group = group)) +
  geom_point(size = 3) +
  geom_errorbar(aes(ymin = ymin, ymax = ymax), width = 0.2) +
  geom_hline(yintercept = 0, color = "#D62728") +
  geom_vline(xintercept = -1, color = "#BEBEBE", linetype = "dashed") +
  scale_color_manual(values=c('black','red'),labels=c("Callaway & Sant'anna","Sun & Abraham")) +
  labs(x = "Years to treatment", y = '', colour = '') +
  theme_minimal(base_size = 14) +
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.line.x = element_line(),
    axis.line.y = element_line(),
    legend.position = "bottom"
  ) +
  scale_x_continuous(limits = c(-9.2, 5.2),
                     breaks = c(-9,-8,-7,-6,-5,-4,-3,-2,-1,0,1,2,3,4,5),
                     labels = c('-9','-8','-7','-6','-5','-4','-3','-2','-1','0','+1','+2','+3','+4','+5')) +
  scale_y_continuous(limits = c(-0.95, 0.155),
                     breaks = c(-0.90,-0.75,-0.60,-0.45,-0.30,-0.15,0,0.15),
                     labels = c('-0.90','-0.75','-0.60','-0.45','-0.30','-0.15','0','0.15'))



ggsave("Graphs/plot_rais_whitecol.jpeg", plot = p, device = "jpeg", width = 10, height = 6, dpi = 600)




# ---------------------------------------------------------------------------- #
### 4.3.2 CBO --------------
estimacoes_wcbo <- plot(white_data,
                        plot_title = '',
                        var_y = "dummy_cbo")


p <- ggplot(estimacoes_wcbo, aes(x = x, y = y, color = colour, group = group)) +
  geom_point(size = 3) +
  geom_errorbar(aes(ymin = ymin, ymax = ymax), width = 0.2) +
  geom_hline(yintercept = 0, color = "#D62728") +
  geom_vline(xintercept = -1, color = "#BEBEBE", linetype = "dashed") +
  scale_color_manual(values=c('black','red'),labels=c("Callaway & Sant'anna","Sun & Abraham")) +
  labs(x = "Years to treatment", y = '', colour = '') +
  theme_minimal(base_size = 14) +
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.line.x = element_line(),
    axis.line.y = element_line(),
    legend.position = "bottom"
  ) +
  scale_x_continuous(limits = c(-9.2, 5.2),
                     breaks = c(-9,-8,-7,-6,-5,-4,-3,-2,-1,0,1,2,3,4,5),
                     labels = c('-9','-8','-7','-6','-5','-4','-3','-2','-1','0','+1','+2','+3','+4','+5')) +
  scale_y_continuous(limits = c( -0.155, 1.10),
                     breaks = c(-0.15,0,0.15,0.30,0.45,0.60,0.75,0.90, 1.05),
                     labels = c('-0.15','0','0.15','0.30','0.45','0.60','0.75','0.90', '1.05'))



ggsave("Graphs/plot_cbo_whitecol.jpeg", plot = p, device = "jpeg", width = 10, height = 6, dpi = 600)


# ---------------------------------------------------------------------------- #
### 4.3.3 CNAE --------------
estimacoes_wcnae <- plot(white_data,
                         plot_title = '',
                         var_y = "dummy_cnae"
)


p <- ggplot(estimacoes_wcnae, aes(x = x, y = y, color = colour, group = group)) +
  geom_point(size = 3) +
  geom_errorbar(aes(ymin = ymin, ymax = ymax), width = 0.2) +
  geom_hline(yintercept = 0, color = "#D62728") +
  geom_vline(xintercept = -1, color = "#BEBEBE", linetype = "dashed") +
  scale_color_manual(values=c('black','red'),labels=c("Callaway & Sant'anna","Sun & Abraham")) +
  labs(x = "Years to treatment", y = '', colour = '') +
  theme_minimal(base_size = 14) +
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.line.x = element_line(),
    axis.line.y = element_line(),
    legend.position = "bottom"
  ) +
  scale_x_continuous(limits = c(-9.2, 5.2),
                     breaks = c(-9,-8,-7,-6,-5,-4,-3,-2,-1,0,1,2,3,4,5),
                     labels = c('-9','-8','-7','-6','-5','-4','-3','-2','-1','0','+1','+2','+3','+4','+5')) +
  scale_y_continuous(limits = c( -0.155, 1.10),
                     breaks = c(-0.15,0,0.15,0.30,0.45,0.60,0.75,0.90, 1.05),
                     labels = c('-0.15','0','0.15','0.30','0.45','0.60','0.75','0.90', '1.05'))



ggsave("Graphs/plot_cnae_whitecol.jpeg", plot = p, device = "jpeg", width = 10, height = 6, dpi = 600)

# ---------------------------------------------------------------------------- #
# 5. New Spec (WxB)----
# ---------------------------------------------------------------------------- #
## 5.1 Data ----


### 5.1.1 RAIS ----

estimacoes_brais$collar = 0#Blue
estimacoes_wrais$collar = 1
#União das Bases
both_rais <- rbind(estimacoes_brais, estimacoes_wrais)

#Escolhendo os estimadores CALSAN
both_rais <- both_rais %>% 
  filter(group == 2) %>% 
  mutate(
    group = ifelse(collar == 1, 1, group),
    x = ifelse(collar == 1, x-0.2, x),
    colour = ifelse(group == 2, "#f7200a", colour)
  ) %>% 
  select(
    -collar
  )


### 5.1.2 CBO ----
estimacoes_bcbo$collar = 0#Blue
estimacoes_wcbo$collar = 1
#União das Bases
both_cbo <- rbind(estimacoes_bcbo, estimacoes_wcbo)

#Escolhendo os estimadores CALSAN
both_cbo <- both_cbo %>% 
  filter(group == 2) %>% 
  mutate(
    group = ifelse(collar == 1, 1, group),
    
    x = ifelse(collar == 1, x-0.2, x),
    
    colour = ifelse(group == 2, "#f7200a", colour)
  ) %>% 
  select(-collar)


###5.1.3 CNAE ----
estimacoes_bcnae$collar = 0#Blue
estimacoes_wcnae$collar = 1
#União das Bases
both_cnae <- rbind(estimacoes_bcnae, estimacoes_wcnae)

#Escolhendo os estimadores CALSAN
both_cnae <- both_cnae %>% 
  filter(group == 2) %>% 
  mutate(
    group = ifelse(collar == 1, 1, group),
    
    x = ifelse(collar == 1, x-0.2, x),
    
    colour = ifelse(group == 2, "#f7200a", colour)
  ) %>% 
  select(-collar)

### 5.1.4 Saving ----
# saveRDS(estimacoes_rais, "C:/Users/tuffy/Documents/IC/Bases/results_est/rais_total.RDS")
# saveRDS(both_rais, "C:/Users/tuffy/Documents/IC/Bases/results_est/rais_both_wc.RDS")
# saveRDS(both_cbo, "C:/Users/tuffy/Documents/IC/Bases/results_est/cbo_both_wc.RDS")
# saveRDS(both_cnae, "C:/Users/tuffy/Documents/IC/Bases/results_est/cnae_both_wc.RDS")




# ---------------------------------------------------------------------------- #
## 5.2 Estimation ----
### 5.2.1 RAIS ----
#both_rais <- readRDS("C:/Users/tuffy/Documents/IC/Bases/results_est/rais_both_wc.RDS")


p <- ggplot(both_rais, aes(x = x, y = y, color = colour, group = group)) +
  geom_point(size = 3) +
  geom_errorbar(aes(ymin = ymin, ymax = ymax), width = 0.5) +
  geom_hline(yintercept = 0, color = "#D62728") +
  geom_vline(xintercept = -1, color = "#BEBEBE", linetype = "dashed") +
  scale_color_manual(values=c('black','red'),labels=c("White-collar","Blue-collar")) +
  labs(x = "Years to treatment", y = '', colour = '') +
  theme_classic(base_size = 18) +   
  theme(
    axis.line = element_line(),
    axis.ticks.length = unit(5, "pt"),
    axis.ticks = element_line(colour = "black"),  
    axis.ticks.x = element_line(colour = "black"),
    axis.ticks.y = element_line(colour = "black"),
    axis.text.x = element_text(margin = margin(t = 5), size = 18),
    axis.text.y = element_text(size = 18
    ),
    
    legend.text = element_text(size = 18),
    legend.position = c(0.05, 0.05),         
    legend.justification = c(0, 0)           
  ) +
  scale_x_continuous(limits = c(-9.2, 4.5),
                     breaks = c(-9,-8,-7,-6,-5,-4,-3,-2,-1,0,1,2,3,4),
                     labels = c('-9','-8','-7','-6','-5','-4','-3','-2','-1','0','+1','+2','+3','+4')) +
  scale_y_continuous(limits = c(-0.95, 0.155),
                     breaks = c(-0.90,-0.75,-0.60,-0.45,-0.30,-0.15,0,0.15),
                     labels = c('-0.90','-0.75','-0.60','-0.45','-0.30','-0.15','0','0.15'))




ggsave("C:/Users/tuffy/Documents/IC/Graphs/united/plot_rais_wb_col.jpeg", plot = p, device = "jpeg", width = 10, height = 6, dpi = 600)
ggsave("C:/Users/tuffy/Documents/IC/Graphs/united/plot_rais_wb_col.pdf", plot = p, device = "pdf", width = 10, height = 6, dpi = 300)

#  --------------------------------------------------------------------------- #
### 5.2.2 CBO ----



#both_cbo <- readRDS("C:/Users/tuffy/Documents/IC/Bases/results_est/cbo_both_wc.RDS")

p <- ggplot(both_cbo, aes(x = x, y = y, color = colour, group = group)) +
  geom_point(size = 3) +
  geom_errorbar(aes(ymin = ymin, ymax = ymax), width = 0.5) +
  geom_hline(yintercept = 0, color = "#D62728") +
  geom_vline(xintercept = -1, color = "#BEBEBE", linetype = "dashed") +
  scale_color_manual(values=c('black','red'),labels=c("White-collar","Blue-collar")) +
  labs(x = "Years to treatment", y = '', colour = '') +
  theme_classic(base_size = 18) +   
  theme(
    axis.line = element_line(),
    axis.ticks.length = unit(5, "pt"),
    axis.ticks = element_line(colour = "black"),  
    axis.ticks.x = element_line(colour = "black"),
    axis.ticks.y = element_line(colour = "black"),
    axis.text.x = element_text(margin = margin(t = 5), size = 18),
    axis.text.y = element_text(size = 18),
    
    legend.text = element_text(size = 18),
    legend.position = c(0.25, 1.05),         
    legend.justification = c(1, 1)           
  ) +
  scale_x_continuous(limits = c(-9.2, 4.5),
                     breaks = c(-9,-8,-7,-6,-5,-4,-3,-2,-1,0,1,2,3,4),
                     labels = c('-9','-8','-7','-6','-5','-4','-3','-2','-1','0','+1','+2','+3','+4')) +
  scale_y_continuous(limits = c( -0.155, 1.10),
                     breaks = c(-0.15,0,0.15,0.30,0.45,0.60,0.75,0.90, 1.05),
                     labels = c('-0.15','0','0.15','0.30','0.45','0.60','0.75','0.90', '1.05'))



p

ggsave("C:/Users/tuffy/Documents/IC/Graphs/united/plot_cbo_wb_col.jpeg", plot = p, device = "jpeg", width = 10, height = 6, dpi = 600)
ggsave("C:/Users/tuffy/Documents/IC/Graphs/united/plot_cbo_wb_col.pdf", plot = p, device = "pdf", width = 10, height = 6, dpi = 300)

# ---------------------------------------------------------------------------- #
###5.2.3 CNAE ----


#both_cnae <- readRDS( "C:/Users/tuffy/Documents/IC/Bases/results_est/cnae_both_wc.RDS")


p <- ggplot(both_cnae, aes(x = x, y = y, color = colour, group = group)) +
  geom_point(size = 3) +
  geom_errorbar(aes(ymin = ymin, ymax = ymax), width = 0.5) +
  geom_hline(yintercept = 0, color = "#D62728") +
  geom_vline(xintercept = -1, color = "#BEBEBE", linetype = "dashed") +
  scale_color_manual(values=c('black','red'),labels=c("White-collar","Blue-collar")) +
  labs(x = "Years to treatment", y = '', colour = '') +
  theme_classic(base_size = 18) +   
  theme(
    axis.line = element_line(),
    axis.ticks.length = unit(5, "pt"),
    axis.ticks = element_line(colour = "black"),  
    axis.ticks.x = element_line(colour = "black"),
    axis.ticks.y = element_line(colour = "black"),
    axis.text.x = element_text(margin = margin(t = 5), size = 18),
    axis.text.y = element_text(size = 18),
    
    legend.text = element_text(size = 18),
    legend.position = c(0.25, 1.05),         
    legend.justification = c(1, 1)           
  ) +
  scale_x_continuous(limits = c(-9.2, 4.5),
                     breaks = c(-9,-8,-7,-6,-5,-4,-3,-2,-1,0,1,2,3,4),
                     labels = c('-9','-8','-7','-6','-5','-4','-3','-2','-1','0','+1','+2','+3','+4')) +
  scale_y_continuous(limits = c( -0.155, 1.10),
                     breaks = c(-0.15,0,0.15,0.30,0.45,0.60,0.75,0.90, 1.05),
                     labels = c('-0.15','0','0.15','0.30','0.45','0.60','0.75','0.90', '1.05'))

p

ggsave("C:/Users/tuffy/Documents/IC/Graphs/united/plot_cnae_wb_col.jpeg", plot = p, device = "jpeg", width = 10, height = 6, dpi = 600)
ggsave("C:/Users/tuffy/Documents/IC/Graphs/united/plot_cnae_wb_col.pdf", plot = p, device = "pdf", width = 10, height = 6, dpi = 300)





# ---------------------------------------------------------------------------- #
# 6. More analysis ----
# ---------------------------------------------------------------------------- #

stargazer(data,
          summary = T,
          summary.stat = c("n", "mean", "sd", "median", "min", "max"),
          type = "latex",
          label = "tab:desc_all",
          file = "C:/Users/tuffy/Documents/IC/Tables/total_desc.tex")

#Repetir para BC e WC

stargazer(blue_data,
          summary = T,
          summary.stat = c("n", "mean", "sd", "median", "min", "max"),
          type = "latex",
          label = "tab:desc_blue",
          file = "C:/Users/tuffy/Documents/IC/Tables/blue_desc.tex")

stargazer(white_data,
          summary = T,
          summary.stat = c("n", "mean", "sd", "median", "min", "max"),
          type = "latex",
          label = "tab:desc_white",
          file = "C:/Users/tuffy/Documents/IC/Tables/white_desc.tex")

# ---------------------------------------------------------------------------- #
# 7. ATT Values ----
# ---------------------------------------------------------------------------- #
## 7.1 RAIS ----

no_control <- did::att_gt(
  yname = "rais_",
  gname = "year_first_treated",
  idname = "code_id",
  tname = "ano",
  data = data,
  control_group = "notyettreated",
  base_period = "universal",
  clustervars = "code_id"
)



est_calsan_sc1 <- aggte( MP = no_control, type = "dynamic", na.rm = TRUE)
est_calsan_scs <- aggte( MP = no_control, type = "simple", na.rm = T)
print(est_calsan_sc1)
print(est_calsan_scs)

# ---------------------------------------------------------------------------- #
## 7.2 CBO ----

no_control <- did::att_gt(
  yname = "dummy_cbo",
  gname = "year_first_treated",
  idname = "code_id",
  tname = "ano",
  data =data,
  control_group = "notyettreated",
  base_period = "universal",
  clustervars = "code_id"
)



est_calsan_sc1 <- aggte( MP = no_control, type = "dynamic", na.rm = TRUE)
est_calsan_scs <- aggte( MP = no_control, type = "simple", na.rm = T)
print(est_calsan_sc1)
print(est_calsan_scs)


# ---------------------------------------------------------------------------- #
## 7.3 CNAE ----

no_control <- did::att_gt(
  yname = "dummy_cnae",
  gname = "year_first_treated",
  idname = "code_id",
  tname = "ano",
  data = data,
  control_group = "notyettreated",
  base_period = "universal",
  clustervars = "code_id"
)



est_calsan_sc1 <- aggte( MP = no_control, type = "dynamic", na.rm = TRUE)
est_calsan_scs <- aggte( MP = no_control, type = "simple", na.rm = T)
print(est_calsan_sc1)
print(est_calsan_scs)

# ---------------------------------------------------------------------------- #
## 7.4 Blue Collar ----
### 7.4.1 RAIS BC ----

no_control <- did::att_gt(
  yname = "rais_",
  gname = "year_first_treated",
  idname = "code_id",
  tname = "ano",
  data = blue_data,
  control_group = "notyettreated",
  base_period = "universal",
  clustervars = "code_id"
)



est_calsan_sc1 <- aggte( MP = no_control, type = "dynamic", na.rm = TRUE)
est_calsan_scs <- aggte( MP = no_control, type = "simple", na.rm = T)
print(est_calsan_sc1)
print(est_calsan_scs)

# ---------------------------------------------------------------------------- #
### 7.4.2 CBO BC ----

no_control <- did::att_gt(
  yname = "dummy_cbo",
  gname = "year_first_treated",
  idname = "code_id",
  tname = "ano",
  data = blue_data,
  control_group = "notyettreated",
  base_period = "universal",
  clustervars = "code_id"
)



est_calsan_sc1 <- aggte( MP = no_control, type = "dynamic", na.rm = TRUE)
est_calsan_scs <- aggte( MP = no_control, type = "simple", na.rm = T)
print(est_calsan_sc1)
print(est_calsan_scs)

# ---------------------------------------------------------------------------- #
### 7.4.3 CNAE BC ----

no_control <- did::att_gt(
  yname = "dummy_cnae",
  gname = "year_first_treated",
  idname = "code_id",
  tname = "ano",
  data = blue_data,
  control_group = "notyettreated",
  base_period = "universal",
  clustervars = "code_id"
)



est_calsan_sc1 <- aggte( MP = no_control, type = "dynamic", na.rm = TRUE)
est_calsan_scs <- aggte( MP = no_control, type = "simple", na.rm = T)
print(est_calsan_sc1)
print(est_calsan_scs)


# ---------------------------------------------------------------------------- #
## 7.5 White Collar ----
### 7.5.1 RAIS WC ----

no_control <- did::att_gt(
  yname = "rais_",
  gname = "year_first_treated",
  idname = "code_id",
  tname = "ano",
  data = white_data,
  control_group = "notyettreated",
  base_period = "universal",
  clustervars = "code_id"
)



est_calsan_sc1 <- aggte( MP = no_control, type = "dynamic", na.rm = TRUE)
est_calsan_scs <- aggte( MP = no_control, type = "simple", na.rm = T)
print(est_calsan_sc1)
print(est_calsan_scs)


# ---------------------------------------------------------------------------- #
### 7.5.2 CBO WC ----

no_control <- did::att_gt(
  yname = "dummy_cbo",
  gname = "year_first_treated",
  idname = "code_id",
  tname = "ano",
  data = white_data,
  control_group = "notyettreated",
  base_period = "universal",
  clustervars = "code_id"
)



est_calsan_sc1 <- aggte( MP = no_control, type = "dynamic", na.rm = TRUE)
est_calsan_scs <- aggte( MP = no_control, type = "simple", na.rm = T)
print(est_calsan_sc1)
print(est_calsan_scs)

# ---------------------------------------------------------------------------- #
### 7.5.3 CNAE WC ----

no_control <- did::att_gt(
  yname = "dummy_cnae",
  gname = "year_first_treated",
  idname = "code_id",
  tname = "ano",
  data = white_data,
  control_group = "notyettreated",
  base_period = "universal",
  clustervars = "code_id"
)



est_calsan_sc1 <- aggte( MP = no_control, type = "dynamic", na.rm = TRUE)
est_calsan_scs <- aggte( MP = no_control, type = "simple", na.rm = T)
print(est_calsan_sc1)
print(est_calsan_scs)

# ---------------------------------------------------------------------------- #
# 8. Pre-Avg ----
# ---------------------------------------------------------------------------- #
## 8.1 RAIS ----
### 8.1.1 With Controls -----

#RAIS - Controles
pre_avg <- did::att_gt(
  yname = "rais_",
  gname = "year_first_treated",
  idname = "code_id",
  tname = "ano",
  xformla = ~ ano_sexo + ano_branco + ano_ensino + code_id,
  data = data,
  control_group = "notyettreated",
  base_period = "universal",
  clustervars = "code_id"
)

pre_dyn <- aggte(pre_avg, type = "dynamic", na.rm = T)

# Calculate pre-avg ATT and SE
# Identificar os períodos pré-tratamento
pre_egt <- pre_dyn$egt < 0

# Calcular a média dos efeitos placebo
pre_av <- mean(pre_dyn$att.egt[pre_egt], na.rm = TRUE)



## Extraindo a V ----#

es_inf_func <- pre_dyn$inf.function$dynamic.inf.func.e
n <- nrow(es_inf_func)
V <- t(es_inf_func) %*% es_inf_func / n / n




## 1) take the 8x8 block from V (rows/cols 1..8 as shown)
V8 <- V[1:8, 1:8]

## 2) equal weights
w <- rep(1/8, 8)

## 3) scalar variance of the equal-weighted average: w' V w
var_equal <- as.numeric(crossprod(w, V8 %*% w))

## 4) corresponding SE (if you need it)
se_equal  <- sqrt(var_equal)

## Outputs:
var_equal  # "remove sqrt" -> this is what you want if you only want the value without sqrt
se_equal   # optional: the average SE (with sqrt)


# Create LaTeX row string

print(pre_av)
print(se_equal)


# ---------------------------------------------------------------------------- #
### 8.1.2 No Controls ----


#RAIS - Sem Controles
pre_avg <- did::att_gt(
  yname = "rais_",
  gname = "year_first_treated",
  idname = "code_id",
  tname = "ano",
  data = data,
  control_group = "notyettreated",
  base_period = "universal",
  clustervars = "code_id"
)

pre_dyn <- aggte(pre_avg, type = "dynamic", na.rm = T)

# Calculate pre-avg ATT and SE
# Identificar os períodos pré-tratamento
pre_egt <- pre_dyn$egt < 0

# Calcular a média dos efeitos placebo
pre_av <- mean(pre_dyn$att.egt[pre_egt], na.rm = TRUE)


#SE
es_inf_func <- pre_dyn$inf.function$dynamic.inf.func.e
n <- nrow(es_inf_func)
V <- t(es_inf_func) %*% es_inf_func / n / n




## 1) take the 8x8 block from V (rows/cols 1..8 as shown)
V8 <- V[1:8, 1:8]

## 2) equal weights
w <- rep(1/8, 8)

## 3) scalar variance of the equal-weighted average: w' V w
var_equal <- as.numeric(crossprod(w, V8 %*% w))

## 4) corresponding SE (if you need it)
se_equal  <- sqrt(var_equal)

## Outputs:
var_equal  # "remove sqrt" -> this is what you want if you only want the value without sqrt
se_equal   # optional: the average SE (with sqrt)



# Create LaTeX row string

print(pre_av)
print(se_equal)

# ---------------------------------------------------------------------------- #
## 8.2 CBO ----
### 8.2.1 With Controls -----

#CBO - Controles
pre_avg <- did::att_gt(
  yname = "dummy_cbo",
  gname = "year_first_treated",
  idname = "code_id",
  tname = "ano",
  xformla = ~ ano_sexo + ano_branco + ano_ensino + code_id,
  data = data,
  control_group = "notyettreated",
  base_period = "universal",
  clustervars = "code_id"
)

pre_dyn <- aggte(pre_avg, type = "dynamic", na.rm = T)

# Calculate pre-avg ATT and SE
# Identificar os períodos pré-tratamento
pre_egt <- pre_dyn$egt < 0

# Calcular a média dos efeitos placebo
pre_av <- mean(pre_dyn$att.egt[pre_egt], na.rm = TRUE)

#SE
es_inf_func <- pre_dyn$inf.function$dynamic.inf.func.e
n <- nrow(es_inf_func)
V <- t(es_inf_func) %*% es_inf_func / n / n




## 1) take the 8x8 block from V (rows/cols 1..8 as shown)
V8 <- V[1:8, 1:8]

## 2) equal weights
w <- rep(1/8, 8)

## 3) scalar variance of the equal-weighted average: w' V w
var_equal <- as.numeric(crossprod(w, V8 %*% w))

## 4) corresponding SE (if you need it)
se_equal  <- sqrt(var_equal)

## Outputs:
var_equal  # "remove sqrt" -> this is what you want if you only want the value without sqrt
se_equal   # optional: the average SE (with sqrt)



# Create LaTeX row string

print(pre_av)
print(se_equal)


# ---------------------------------------------------------------------------- #
### 8.2.2 No Controls -----

#CBO - Sem Controles
pre_avg <- did::att_gt(
  yname = "dummy_cbo",
  gname = "year_first_treated",
  idname = "code_id",
  tname = "ano",
  data = data,
  control_group = "notyettreated",
  base_period = "universal",
  clustervars = "code_id"
)

pre_dyn <- aggte(pre_avg, type = "dynamic", na.rm = T)

# Calculate pre-avg ATT and SE
# Identificar os períodos pré-tratamento
pre_egt <- pre_dyn$egt < 0

# Calcular a média dos efeitos placebo
pre_av <- mean(pre_dyn$att.egt[pre_egt], na.rm = TRUE)

#SE
es_inf_func <- pre_dyn$inf.function$dynamic.inf.func.e
n <- nrow(es_inf_func)
V <- t(es_inf_func) %*% es_inf_func / n / n




## 1) take the 8x8 block from V (rows/cols 1..8 as shown)
V8 <- V[1:8, 1:8]

## 2) equal weights
w <- rep(1/8, 8)

## 3) scalar variance of the equal-weighted average: w' V w
var_equal <- as.numeric(crossprod(w, V8 %*% w))

## 4) corresponding SE (if you need it)
se_equal  <- sqrt(var_equal)

## Outputs:
var_equal  # "remove sqrt" -> this is what you want if you only want the value without sqrt
se_equal   # optional: the average SE (with sqrt)



# Create LaTeX row string

print(pre_av)
print(se_equal)


# ---------------------------------------------------------------------------- #
## 8.3 CNAE ----
### 8.3.1 With Controls -----


#CNAE - Controles
pre_avg <- did::att_gt(
  yname = "dummy_cnae",
  gname = "year_first_treated",
  idname = "code_id",
  tname = "ano",
  xformla = ~ ano_sexo + ano_branco + ano_ensino + code_id,
  data = data,
  control_group = "notyettreated",
  base_period = "universal",
  clustervars = "code_id"
)

pre_dyn <- aggte(pre_avg, type = "dynamic", na.rm = T)

# Calculate pre-avg ATT and SE
# Identificar os períodos pré-tratamento
pre_egt <- pre_dyn$egt < 0

# Calcular a média dos efeitos placebo
pre_av <- mean(pre_dyn$att.egt[pre_egt], na.rm = TRUE)

#SE
es_inf_func <- pre_dyn$inf.function$dynamic.inf.func.e
n <- nrow(es_inf_func)
V <- t(es_inf_func) %*% es_inf_func / n / n




## 1) take the 8x8 block from V (rows/cols 1..8 as shown)
V8 <- V[1:8, 1:8]

## 2) equal weights
w <- rep(1/8, 8)

## 3) scalar variance of the equal-weighted average: w' V w
var_equal <- as.numeric(crossprod(w, V8 %*% w))

## 4) corresponding SE (if you need it)
se_equal  <- sqrt(var_equal)

## Outputs:
var_equal  # "remove sqrt" -> this is what you want if you only want the value without sqrt
se_equal   # optional: the average SE (with sqrt)



# Create LaTeX row string

print(pre_av)
print(se_equal)


# ---------------------------------------------------------------------------- #
### 8.3.2 No Controls ----

#CNAE - Sem Controles
pre_avg <- did::att_gt(
  yname = "dummy_cnae",
  gname = "year_first_treated",
  idname = "code_id",
  tname = "ano",
  data = data,
  control_group = "notyettreated",
  base_period = "universal",
  clustervars = "code_id"
)

pre_dyn <- aggte(pre_avg, type = "dynamic", na.rm = T)

# Calculate pre-avg ATT and SE
# Identificar os períodos pré-tratamento
pre_egt <- pre_dyn$egt < 0

# Calcular a média dos efeitos placebo
pre_av <- mean(pre_dyn$att.egt[pre_egt], na.rm = TRUE)

#SE
es_inf_func <- pre_dyn$inf.function$dynamic.inf.func.e
n <- nrow(es_inf_func)
V <- t(es_inf_func) %*% es_inf_func / n / n




## 1) take the 8x8 block from V (rows/cols 1..8 as shown)
V8 <- V[1:8, 1:8]

## 2) equal weights
w <- rep(1/8, 8)

## 3) scalar variance of the equal-weighted average: w' V w
var_equal <- as.numeric(crossprod(w, V8 %*% w))

## 4) corresponding SE (if you need it)
se_equal  <- sqrt(var_equal)

## Outputs:
var_equal  # "remove sqrt" -> this is what you want if you only want the value without sqrt
se_equal   # optional: the average SE (with sqrt)



# Create LaTeX row string

print(pre_av)
print(se_equal)

# ---------------------------------------------------------------------------- #
## 8.4 Blue Collar ----
### 8.4.1 RAIS ----

#RAIS - Controles
pre_avg <- did::att_gt(
  yname = "rais_",
  gname = "year_first_treated",
  idname = "code_id",
  tname = "ano",
  xformla = ~ ano_sexo + ano_branco + ano_ensino + code_id,
  data = blue_data,
  control_group = "notyettreated",
  base_period = "universal",
  clustervars = "code_id"
)

pre_dyn <- aggte(pre_avg, type = "dynamic", na.rm = T)

# Calculate pre-avg ATT and SE
# Identificar os períodos pré-tratamento
pre_egt <- pre_dyn$egt < 0

# Calcular a média dos efeitos placebo
pre_av <- mean(pre_dyn$att.egt[pre_egt], na.rm = TRUE)

#SE
es_inf_func <- pre_dyn$inf.function$dynamic.inf.func.e
n <- nrow(es_inf_func)
V <- t(es_inf_func) %*% es_inf_func / n / n




## 1) take the 8x8 block from V (rows/cols 1..8 as shown)
V8 <- V[1:8, 1:8]

## 2) equal weights
w <- rep(1/8, 8)

## 3) scalar variance of the equal-weighted average: w' V w
var_equal <- as.numeric(crossprod(w, V8 %*% w))

## 4) corresponding SE (if you need it)
se_equal  <- sqrt(var_equal)

## Outputs:
var_equal  # "remove sqrt" -> this is what you want if you only want the value without sqrt
se_equal   # optional: the average SE (with sqrt)



# Create LaTeX row string

print(pre_av)
print(se_equal)

# ---------------------------------------------------------------------------- #
### 8.4.2 CBO ----

#CBO - Controles
pre_avg <- did::att_gt(
  yname = "dummy_cbo",
  gname = "year_first_treated",
  idname = "code_id",
  tname = "ano",
  xformla = ~ ano_sexo + ano_branco + ano_ensino + code_id,
  data = blue_data,
  control_group = "notyettreated",
  base_period = "universal",
  clustervars = "code_id"
)

pre_dyn <- aggte(pre_avg, type = "dynamic", na.rm = T)

# Calculate pre-avg ATT and SE
# Identificar os períodos pré-tratamento
pre_egt <- pre_dyn$egt < 0

# Calcular a média dos efeitos placebo
pre_av <- mean(pre_dyn$att.egt[pre_egt], na.rm = TRUE)

#SE
es_inf_func <- pre_dyn$inf.function$dynamic.inf.func.e
n <- nrow(es_inf_func)
V <- t(es_inf_func) %*% es_inf_func / n / n




## 1) take the 8x8 block from V (rows/cols 1..8 as shown)
V8 <- V[1:8, 1:8]

## 2) equal weights
w <- rep(1/8, 8)

## 3) scalar variance of the equal-weighted average: w' V w
var_equal <- as.numeric(crossprod(w, V8 %*% w))

## 4) corresponding SE (if you need it)
se_equal  <- sqrt(var_equal)

## Outputs:
var_equal  # "remove sqrt" -> this is what you want if you only want the value without sqrt
se_equal   # optional: the average SE (with sqrt)



# Create LaTeX row string

print(pre_av)
print(se_equal)

# ---------------------------------------------------------------------------- #
### 8.4.3 CNAE ----

#CNAE - Controles
pre_avg <- did::att_gt(
  yname = "dummy_cnae",
  gname = "year_first_treated",
  idname = "code_id",
  tname = "ano",
  xformla = ~ ano_sexo + ano_branco + ano_ensino + code_id,
  data = blue_data,
  control_group = "notyettreated",
  base_period = "universal",
  clustervars = "code_id"
)

pre_dyn <- aggte(pre_avg, type = "dynamic", na.rm = T)

# Calculate pre-avg ATT and SE
# Identificar os períodos pré-tratamento
pre_egt <- pre_dyn$egt < 0

# Calcular a média dos efeitos placebo
pre_av <- mean(pre_dyn$att.egt[pre_egt], na.rm = TRUE)
#SE
es_inf_func <- pre_dyn$inf.function$dynamic.inf.func.e
n <- nrow(es_inf_func)
V <- t(es_inf_func) %*% es_inf_func / n / n




## 1) take the 8x8 block from V (rows/cols 1..8 as shown)
V8 <- V[1:8, 1:8]

## 2) equal weights
w <- rep(1/8, 8)

## 3) scalar variance of the equal-weighted average: w' V w
var_equal <- as.numeric(crossprod(w, V8 %*% w))

## 4) corresponding SE (if you need it)
se_equal  <- sqrt(var_equal)

## Outputs:
var_equal  # "remove sqrt" -> this is what you want if you only want the value without sqrt
se_equal   # optional: the average SE (with sqrt)



# Create LaTeX row string

print(pre_av)
print(se_equal)


# ---------------------------------------------------------------------------- #
## 8.5 White Collar ----
