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
# >>>>>>>>>>> ----
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
# ---------------------------------------------------------------------------- #


### 8.5.1 RAIS ----
pre_avg <- did::att_gt(
  yname = "rais_",
  gname = "year_first_treated",
  idname = "code_id",
  tname = "ano",
  xformla = ~ ano_sexo + ano_branco + ano_ensino + code_id,
  data = white_data,
  control_group = "notyettreated",
  base_period = "universal",
  clustervars = "code_id"
)

pre_dyn <- aggte(pre_avg, type = "dynamic", na.rm = T)

# Calculate pre-avg ATT and SE
# Selecting pre-treatment periods
pre_egt <- pre_dyn$egt < 0

#Calculating the mean effect
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



### 8.5.2 CBO -----
pre_avg <- did::att_gt(
  yname = "dummy_cbo",
  gname = "year_first_treated",
  idname = "code_id",
  tname = "ano",
  xformla = ~ ano_sexo + ano_branco + ano_ensino + code_id,
  data = white_data,
  control_group = "notyettreated",
  base_period = "universal",
  clustervars = "code_id"
)

pre_dyn <- aggte(pre_avg, type = "dynamic", na.rm = T)


# Calculate pre-avg ATT and SE
# Selecting pre-treatment periods
pre_egt <- pre_dyn$egt < 0

#Calculating the mean effect
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



### 8.5.3 CNAE ----
pre_avg <- did::att_gt(
  yname = "dummy_cnae",
  gname = "year_first_treated",
  idname = "code_id",
  tname = "ano",
  xformla = ~ ano_sexo + ano_branco + ano_ensino + code_id,
  data = white_data,
  control_group = "notyettreated",
  base_period = "universal",
  clustervars = "code_id"
)

pre_dyn <- aggte(pre_avg, type = "dynamic", na.rm = T)

# Calculate pre-avg ATT and SE
# Selecting pre-treatment periods
pre_egt <- pre_dyn$egt < 0

#Calculating the mean effect
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
# <<<<<<<<<<< ----
#ROBUSTNESS TESTS -----
# >>>>>>>>>>> ----
# ---------------------------------------------------------------------------- #

# --------------------------------------------------------------------------- #
#Data Base ----
# --------------------------------------------------------------------------- #

base <- read.csv("C:/Users/tuffy/Documents/IC/Bases/panel_workers_2003to2013_nova.csv")

#Marking the individuals who appear in RAIS in all periods
base <- base %>%   
  group_by(code_id) %>% 
  mutate( all_in_rais = min(rais_)
  ) %>% 
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
  ) %>% 
  filter(
    all_in_rais == 1
  )

# ---------------------------------------------------------------------------- #
#9. Salary -------
# ---------------------------------------------------------------------------- #
# ------------- #
## 9.1 Data ----
# ------------- #
summary(base$remuneracao_media_sm_)

base <- base %>%
  group_by(code_id) %>% 
  mutate(
    sal_var = log(1+remuneracao_media_sm_),
    
    
    sal_var_t0_aux = ifelse(ano == year_first_treated, sal_var, NA),
    sal_var_t0_aux2 = max(sal_var_t0_aux, na.rm = T),
    sal_var_t0 = sal_var - sal_var_t0_aux2,
    
    sal_var_t2_aux = ifelse(ano == year_first_treated - 2, sal_var, NA),
    sal_var_t2_aux2 = max(sal_var_t2_aux, na.rm = T),
    sal_var_t2 = sal_var - sal_var_t2_aux2,
    
    
    sal_var_tmean_aux = ifelse( ano %in% c(year_first_treated - 2, year_first_treated -1, year_first_treated),
                                sal_var, NA),
    sal_var_tmean_aux2 = mean(sal_var_tmean_aux, na.rm = T),
    sal_var_tmean = sal_var - sal_var_tmean_aux2
  ) %>%
  ungroup() %>% 
  select(
    -c(sal_var_t0_aux, sal_var_t0_aux2,
       sal_var_t2_aux, sal_var_t2_aux2,
       sal_var_tmean_aux, sal_var_tmean_aux2)
  )

base$cbo_ano <- as.numeric(interaction(base$cbo_group, base$ano))
base$cnae_ano <- as.numeric(interaction(base$cnae_group, base$ano))

base$minus_1 <- base$year_first_treated - 1
base$minus_2 <- base$year_first_treated - 2

# ------------- #
## 9.2 Estimation ----
# ------------- #

# ------------- #
###9.2.1 No FE ----
# ------------- #
####9.2.1.1 T0 -----
#Here I utilized the real treatment timming
#Estimating
calsan_did <- did::att_gt(
  yname = "sal_var",
  gname = "year_first_treated",
  idname = "code_id",
  tname = "ano",
  xformla = ~ ano_sexo + ano_branco + ano_ensino + code_id,
  data = base,
  control_group = "notyettreated",
  base_period = "universal",
  clustervars = "code_id"
)

est_calsan_t0 <- aggte( MP = calsan_did, type = "dynamic", na.rm = TRUE)
#Result Table
print(est_calsan_t0)

plot_calsan_t0 <- ggdid(est_calsan_t0) +
  ggtitle("Event Study: Callaway & Sant'anna, ref T0") +
  labs("Time to treat") +
  theme_minimal()

plot_calsan_t0

#### 9.2.1.2 T2 -----
#'The results in T0 promped me to test for a tratment timming 2 periods before the real treatment
#'this is a direct resul from the labor lawsuits strutures in Brazil, where the
#'plaintiff workers has a 2 year period to start the lawsuit after the termination

#Estimating
calsan_did <- did::att_gt(
  yname = "sal_var_t2",
  gname = "year_first_treated",
  idname = "code_id",
  tname = "ano",
  xformla = ~ ano_sexo + ano_branco + ano_ensino + code_id,
  data = base,
  control_group = "notyettreated",
  base_period = "universal",
  clustervars = "code_id"
)

est_calsan_t2 <- aggte( MP = calsan_did, type = "dynamic", na.rm = TRUE)
#Results table
print(est_calsan_t2)

plot_calsan_t2 <- ggdid(est_calsan_t2) +
  ggtitle("Event Study: Callaway & Sant'anna, ref T2 ") +
  labs("Time to treat") +
  theme_minimal()

plot_calsan_t2


#### 9.2.1.3 T MEAN -----

#Estimating
calsan_did <- did::att_gt(
  yname = "sal_var_tmean",
  gname = "year_first_treated",
  idname = "code_id",
  tname = "ano",
  xformla = ~ ano_sexo + ano_branco + ano_ensino + code_id,
  data = base,
  control_group = "notyettreated",
  base_period = "universal",
  clustervars = "code_id"
)

est_calsan_tmean <- aggte( MP = calsan_did, type = "dynamic", na.rm = TRUE)
#Result Table
print(est_calsan_tmean)

plot_calsan_tmean <- ggdid(est_calsan_tmean) +
  ggtitle("Event Study: Callaway & Sant'anna, T Mean ") +
  labs("Time to treat") +
  theme_minimal()

plot_calsan_tmean


###9.2.2 New Spec ----

#' The new specification removed all observations where the observed salary was
#' equal to 0.

base2 <- base %>%
  group_by(code_id) %>%
  filter(all(remuneracao_media_sm_ != 0)) %>%
  mutate(
    sal_var = log(remuneracao_media_sm_)
  )


#### 9.2.2.1 T0 ----

#Estimation
calsan_did <- did::att_gt(
  yname = "sal_var",
  gname = "year_first_treated",
  idname = "code_id",
  tname = "ano",
  xformla = ~ ano_sexo + ano_branco + ano_ensino + code_id,
  data = base2,
  control_group = "notyettreated",
  base_period = "universal",
  clustervars = "code_id"
)

est_calsan_t0 <- aggte( MP = calsan_did, type = "dynamic", na.rm = TRUE)
#Result table
print(est_calsan_t0)

plot_calsan_t0 <- ggdid(est_calsan_t0) +
  ggtitle("Event Study: Callaway & Sant'anna, ref T0") +
  labs("Time to treat") +
  theme_minimal()

plot_calsan_t0


#### 9.2.2.2 T2 ----

#Estimation
calsan_did <- did::att_gt(
  yname = "sal_var_t2",
  gname = "year_first_treated",
  idname = "code_id",
  tname = "ano",
  xformla = ~ ano_sexo + ano_branco + ano_ensino + code_id,
  data = base,
  control_group = "notyettreated",
  base_period = "universal",
  clustervars = "code_id"
)

est_calsan_t2 <- aggte( MP = calsan_did, type = "dynamic", na.rm = TRUE)
#Result table
print(est_calsan_t2)

plot_calsan_t2 <- ggdid(est_calsan_t2) +
  ggtitle("Event Study: Callaway & Sant'anna, ref T2 ") +
  labs("Time to treat") +
  theme_minimal()

plot_calsan_t2


#### 9.2.2.3 T MEAN ----

#Estimating
calsan_did <- did::att_gt(
  yname = "sal_var_tmean",
  gname = "year_first_treated",
  idname = "code_id",
  tname = "ano",
  xformla = ~ ano_sexo + ano_branco + ano_ensino + code_id,
  data = base,
  control_group = "notyettreated",
  base_period = "universal",
  clustervars = "code_id"
)

est_calsan_tmean <- aggte( MP = calsan_did, type = "dynamic", na.rm = TRUE)
#Result Table
print(est_calsan_tmean)

plot_calsan_tmean <- ggdid(est_calsan_tmean) +
  ggtitle("Event Study: Callaway & Sant'anna, T Mean ") +
  labs("Time to treat") +
  theme_minimal()

plot_calsan_tmean




##9.3 With FE ----

#Estimating
calsan_did <- did::att_gt(
  yname = "sal_var",
  gname = "year_first_treated",
  idname = "code_id",
  tname = "ano",
  xformla = ~ ano_sexo + ano_branco + ano_ensino + code_id + cbo_ano + cnae_ano,
  data = base,
  control_group = "notyettreated",
  base_period = "universal",
  clustervars = "code_id"
)

est_calsan <- aggte( MP = calsan_did, type = "dynamic", na.rm = TRUE)
#Result Table
print(est_calsan)

plot_calsan <- ggdid(est_calsan) +
  ggtitle("Event Study: Callaway & Sant'anna, CNAE + CBO ") +
  labs("Time to treat") +
  theme_minimal()

plot_calsan




###9.3.1 -1 period ----

#Estimation
calsan_did <- did::att_gt(
  yname = "sal_var",
  gname = "minus_1",
  idname = "code_id",
  tname = "ano",
  xformla = ~ ano_sexo + ano_branco + ano_ensino + code_id + cbo_ano + cnae_ano,
  data = base,
  control_group = "notyettreated",
  base_period = "universal",
  clustervars = "code_id"
)

est_calsan <- aggte( MP = calsan_did, type = "dynamic", na.rm = TRUE)
#Result table
print(est_calsan)

plot_calsan <- ggdid(est_calsan) +
  ggtitle("Event Study: Callaway & Sant'anna, CNAE + CBO (-1) ") +
  labs("Time to treat") +
  theme_minimal()

plot_calsan


###9.3.2 -2 period ----

#Estimation
calsan_did <- did::att_gt(
  yname = "sal_var",
  gname = "minus_2",
  idname = "code_id",
  tname = "ano",
  xformla = ~ ano_sexo + ano_branco + ano_ensino + code_id + cbo_ano + cnae_ano,
  data = base,
  control_group = "notyettreated",
  base_period = "universal",
  clustervars = "code_id"
)

est_calsan <- aggte( MP = calsan_did, type = "dynamic", na.rm = TRUE)
#Result table
print(est_calsan)

plot_calsan <- ggdid(est_calsan) +
  ggtitle("Event Study: Callaway & Sant'anna, CNAE + CBO (-2) ") +
  labs("Time to treat") +
  theme_minimal()

plot_calsan



# 
# ##Extraindo os coeficientes##
# data_calsan <- ggplot_build(plot_calsan)$data[[1]]
# data_calsan <- as.data.frame(data_calsan)
# 
# 
# data_calsan <- data_calsan %>% 
#   mutate(colour = '#145ede',
#          group = 1,
#          y = ifelse(x == -1, 0, y),
#          ymin = ifelse(x == -1, 0, ymin),
#          ymax = ifelse(x == -1, 0, ymax)) %>% 
#   select(-PANEL, -shape, -size, -fill, -alpha, -stroke)

rm(base)

# -----------------------------------------------------------------------------#
# 10. Sex ----
# -----------------------------------------------------------------------------#

# ----------- #
## 10.0 Data -----
# ----------- #

data <- read.csv("C:/Users/tuffy/Documents/IC/Bases/base_atual_dum_v3.csv")

data$cbo_ano <- as.numeric(interaction(data$cbo_group, data$ano))
data$cnae_ano <- as.numeric(interaction(data$cnae_group, data$ano))

#Filtering the database according to each individual sex
base_mas <- data %>% 
  filter(sexo_dummy == 1) #male

base_fem <- data %>% 
  filter(sexo_dummy == 0 ) #female

#depois subdividir em white e blue collar

## 10.1 Female ----
#Estimating
calsan_did <- did::att_gt(
  yname = "rais_",
  gname = "year_first_treated",
  idname = "code_id",
  tname = "ano",
  xformla = ~ ano_branco + ano_ensino + code_id #+ all_in_rais + white_dummy
  ,
  data = base_fem,
  control_group = "notyettreated",
  base_period = "universal",
  clustervars = "code_id"
)

est_calsan <- aggte( MP = calsan_did, type = "dynamic", na.rm = TRUE)
#Result table
print(est_calsan)

plot_calsan <- ggdid(est_calsan) +
  ggtitle("Event Study: Callaway & Sant'anna, CNAE + CBO ") +
  labs("Time to treat") +
  theme_minimal()

plot_calsan


#Extracting
data_calsan <- ggplot_build(plot_calsan)$data[[1]]
data_calsan <- as.data.frame(data_calsan)

## 10.2 Male ----
#Estimating
calsan_did <- did::att_gt(
  yname = "rais_",
  gname = "year_first_treated",
  idname = "code_id",
  tname = "ano",
  xformla = ~ ano_branco + ano_ensino + code_id #+ all_in_rais + white_dummy
  ,
  data = base_mas,
  control_group = "notyettreated",
  base_period = "universal",
  clustervars = "code_id"
)

est_calsan2 <- aggte( MP = calsan_did, type = "dynamic", na.rm = TRUE)
#Result table
print(est_calsan2)

plot_calsan2 <- ggdid(est_calsan2) +
  ggtitle("Event Study: Callaway & Sant'anna, CNAE + CBO ") +
  labs("Time to treat") +
  theme_minimal()

plot_calsan2

#Extracting the results for the female workers
data_calsan2 <- ggplot_build(plot_calsan2)$data[[1]]
data_calsan2 <- as.data.frame(data_calsan2)


##10.3 Combined ----
#The combined results database

data_calsan <- data_calsan %>% 
  mutate(
    grupo = 1,
    colour= "red"
  )

data_calsan2 <- data_calsan2 %>% 
  mutate(
    grupo = 2,
    x = x + 0.2,
    colour = "black"
  )

data_final <- rbind(data_calsan,
                    data_calsan2)


#Graph
p <- ggplot(data_final, aes(x = x, y = y, colour = colour, group = grupo)) +
  geom_point(size = 3) +
  geom_errorbar(aes(ymin = ymin, ymax = ymax), width = 0.5) +
  geom_hline(yintercept = 0, color = "#D62728") +
  geom_vline(xintercept = -1, color = "#BEBEBE", linetype = "dashed") +
  scale_color_manual(values=c('black','red'),labels=c("Male","Female")) +
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




p

ggsave("C:/Users/tuffy/Documents/IC/Graphs/united/plot_sexo.jpeg", plot = p, device = "jpeg", width = 10, height = 6, dpi = 600)
ggsave("C:/Users/tuffy/Documents/IC/Graphs/united/plot_sexo.pdf", plot = p, device = "pdf", width = 10, height = 6, dpi = 300)

rm(p, base_fem, base_mas, data_calsan, data_calsan2, data_final, est_calsan, est_calsan2, plot_calsan, plot_calsan2)



##10.4 White-collar ----

### 10.4.1 Female ----

base_mas <- data %>% 
  filter(sexo_dummy == 1,
         white_dummy == 1) 


base_fem <- data %>% 
  filter(sexo_dummy == 0,
         white_dummy == 1)

#Estimation
calsan_did <- did::att_gt(
  yname = "rais_",
  gname = "year_first_treated",
  idname = "code_id",
  tname = "ano",
  xformla = ~ ano_branco + ano_ensino + code_id #+ all_in_rais + white_dummy
  ,
  data = base_fem,
  control_group = "notyettreated",
  base_period = "universal",
  clustervars = "code_id"
)

est_calsan <- aggte( MP = calsan_did, type = "dynamic", na.rm = TRUE)
#Result table
print(est_calsan)

plot_calsan <- ggdid(est_calsan) +
  ggtitle("Event Study: Callaway & Sant'anna, CNAE + CBO ") +
  labs("Time to treat") +
  theme_minimal()

plot_calsan

#Extracting the results for the female workers
data_calsan <- ggplot_build(plot_calsan)$data[[1]]
data_calsan <- as.data.frame(data_calsan)

### 10.4.2 Male ----
#Estimation
calsan_did <- did::att_gt(
  yname = "rais_",
  gname = "year_first_treated",
  idname = "code_id",
  tname = "ano",
  xformla = ~ ano_branco + ano_ensino + code_id #+ all_in_rais + white_dummy
  ,
  data = base_mas,
  control_group = "notyettreated",
  base_period = "universal",
  clustervars = "code_id"
)

est_calsan2 <- aggte( MP = calsan_did, type = "dynamic", na.rm = TRUE)
#Result table
print(est_calsan2)

plot_calsan2 <- ggdid(est_calsan2) +
  ggtitle("Event Study: Callaway & Sant'anna, CNAE + CBO ") +
  labs("Time to treat") +
  theme_minimal()

plot_calsan2

#Final results for the male workers
data_calsan2 <- ggplot_build(plot_calsan2)$data[[1]]
data_calsan2 <- as.data.frame(data_calsan2)


### 10.4.3 Combined  ----
# Combining the results databases
data_calsan <- data_calsan %>% 
  mutate(
    grupo = 1,
    colour= "red"
  )

data_calsan2 <- data_calsan2 %>% 
  mutate(
    grupo = 2,
    x = x + 0.2,
    colour = "black"
  )

data_final <- rbind(data_calsan,
                    data_calsan2)


#Final Graph
p <- ggplot(data_final, aes(x = x, y = y, colour = colour, group = grupo)) +
  geom_point(size = 3) +
  geom_errorbar(aes(ymin = ymin, ymax = ymax), width = 0.5) +
  geom_hline(yintercept = 0, color = "#D62728") +
  geom_vline(xintercept = -1, color = "#BEBEBE", linetype = "dashed") +
  scale_color_manual(values=c('black','red'),labels=c("Male","Female")) +
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




p

ggsave("C:/Users/tuffy/Documents/IC/Graphs/united/plot_sexo_wc.jpeg", plot = p, device = "jpeg", width = 10, height = 6, dpi = 600)
ggsave("C:/Users/tuffy/Documents/IC/Graphs/united/plot_sexo_wc.pdf", plot = p, device = "pdf", width = 10, height = 6, dpi = 300)

rm(p, base_fem, base_mas, data_calsan, data_calsan2, data_final, est_calsan, est_calsan2, plot_calsan, plot_calsan2)



##10.5 Blue-collar ----

### 10.5.1 Female ----

base_mas <- data %>% 
  filter(sexo_dummy == 1,
         white_dummy == 0) 


base_fem <- data %>% 
  filter(sexo_dummy == 0,
         white_dummy == 0)

#Estimation
calsan_did <- did::att_gt(
  yname = "rais_",
  gname = "year_first_treated",
  idname = "code_id",
  tname = "ano",
  xformla = ~ ano_branco + ano_ensino + code_id #+ all_in_rais + white_dummy
  ,
  data = base_fem,
  control_group = "notyettreated",
  base_period = "universal",
  clustervars = "code_id"
)

est_calsan <- aggte( MP = calsan_did, type = "dynamic", na.rm = TRUE)
#Result table
print(est_calsan)

plot_calsan <- ggdid(est_calsan) +
  ggtitle("Event Study: Callaway & Sant'anna, CNAE + CBO ") +
  labs("Time to treat") +
  theme_minimal()

plot_calsan


#Extracting the results for the female workers
data_calsan <- ggplot_build(plot_calsan)$data[[1]]
data_calsan <- as.data.frame(data_calsan)

### 10.5.2 Male ----
#Estimation
calsan_did <- did::att_gt(
  yname = "rais_",
  gname = "year_first_treated",
  idname = "code_id",
  tname = "ano",
  xformla = ~ ano_branco + ano_ensino + code_id #+ all_in_rais + white_dummy
  ,
  data = base_mas,
  control_group = "notyettreated",
  base_period = "universal",
  clustervars = "code_id"
)

est_calsan2 <- aggte( MP = calsan_did, type = "dynamic", na.rm = TRUE)
#Result table
print(est_calsan2)

plot_calsan2 <- ggdid(est_calsan2) +
  ggtitle("Event Study: Callaway & Sant'anna, CNAE + CBO ") +
  labs("Time to treat") +
  theme_minimal()

plot_calsan2

#Extracting the results for the male workers
data_calsan2 <- ggplot_build(plot_calsan2)$data[[1]]
data_calsan2 <- as.data.frame(data_calsan2)


### 10.5.3 Combined  ----
#The combined results database
data_calsan <- data_calsan %>% 
  mutate(
    grupo = 1,
    colour= "red"
  )

data_calsan2 <- data_calsan2 %>% 
  mutate(
    grupo = 2,
    x = x + 0.2,
    colour = "black"
  )

data_final <- rbind(data_calsan,
                    data_calsan2)


#Graph
p <- ggplot(data_final, aes(x = x, y = y, colour = colour, group = grupo)) +
  geom_point(size = 3) +
  geom_errorbar(aes(ymin = ymin, ymax = ymax), width = 0.5) +
  geom_hline(yintercept = 0, color = "#D62728") +
  geom_vline(xintercept = -1, color = "#BEBEBE", linetype = "dashed") +
  scale_color_manual(values=c('black','red'),labels=c("Male","Female")) +
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




p

ggsave("C:/Users/tuffy/Documents/IC/Graphs/united/plot_sexo_bc.jpeg", plot = p, device = "jpeg", width = 10, height = 6, dpi = 600)
ggsave("C:/Users/tuffy/Documents/IC/Graphs/united/plot_sexo_bc.pdf", plot = p, device = "pdf", width = 10, height = 6, dpi = 300)

rm(p, base_fem, base_mas, data_calsan, data_calsan2, data_final, est_calsan, est_calsan2, plot_calsan, plot_calsan2)



# -----------------------------------------------------------------------------#
# 11. Race/Color ----
# -----------------------------------------------------------------------------#

# ----------- #
## 11.0 Data -----
# ----------- #

#Filtering the database for individuals race/color
base_bra <- data %>% 
  filter(branco_dummy == 1) #white

base_nbr <- data %>% 
  filter(branco_dummy == 0 )#non-white

#separating into further white and blue collars

## 11.1 White----
#Estimation
calsan_did <- did::att_gt(
  yname = "rais_",
  gname = "year_first_treated",
  idname = "code_id",
  tname = "ano",
  xformla = ~ ano_sexo + ano_ensino + code_id #+ all_in_rais + white_dummy
  ,
  data = base_bra,
  control_group = "notyettreated",
  base_period = "universal",
  clustervars = "code_id"
)

est_calsan <- aggte( MP = calsan_did, type = "dynamic", na.rm = TRUE)
#Result table
print(est_calsan)

plot_calsan <- ggdid(est_calsan) +
  ggtitle("Event Study: Callaway & Sant'anna, CNAE + CBO ") +
  labs("Time to treat") +
  theme_minimal()

plot_calsan


#Final result dataframe for Whites
data_calsan <- ggplot_build(plot_calsan)$data[[1]]
data_calsan <- as.data.frame(data_calsan)

## 11.2 Non-White ----
#Estimation
calsan_did <- did::att_gt(
  yname = "rais_",
  gname = "year_first_treated",
  idname = "code_id",
  tname = "ano",
  xformla = ~ ano_sexo + ano_ensino + code_id #+ all_in_rais + white_dummy
  ,
  data = base_nbr,
  control_group = "notyettreated",
  base_period = "universal",
  clustervars = "code_id"
)

est_calsan2 <- aggte( MP = calsan_did, type = "dynamic", na.rm = TRUE)
#Result table
print(est_calsan2)

plot_calsan2 <- ggdid(est_calsan2) +
  ggtitle("Event Study: Callaway & Sant'anna, CNAE + CBO ") +
  labs("Time to treat") +
  theme_minimal()

plot_calsan2

#Extracting the final results for Non-Whites
data_calsan2 <- ggplot_build(plot_calsan2)$data[[1]]
data_calsan2 <- as.data.frame(data_calsan2)


##11.3 Combined  ----

data_calsan <- data_calsan %>% 
  mutate(
    grupo = 1,
    colour= "red"
  )

data_calsan2 <- data_calsan2 %>% 
  mutate(
    grupo = 2,
    x = x + 0.2,
    colour = "black"
  )

data_final <- rbind(data_calsan,
                    data_calsan2)


### Gráfico
p <- ggplot(data_final, aes(x = x, y = y, colour = colour, group = grupo)) +
  geom_point(size = 3) +
  geom_errorbar(aes(ymin = ymin, ymax = ymax), width = 0.5) +
  geom_hline(yintercept = 0, color = "#D62728") +
  geom_vline(xintercept = -1, color = "#BEBEBE", linetype = "dashed") +
  scale_color_manual(values=c('black','red'),labels=c("Non-White","White")) +
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




p

ggsave("C:/Users/tuffy/Documents/IC/Graphs/united/plot_raca.jpeg", plot = p, device = "jpeg", width = 10, height = 6, dpi = 600)
ggsave("C:/Users/tuffy/Documents/IC/Graphs/united/plot_raca.pdf", plot = p, device = "pdf", width = 10, height = 6, dpi = 300)

rm(p, base_fem, base_mas, data_calsan, data_calsan2, data_final, est_calsan, est_calsan2, plot_calsan, plot_calsan2)



##11.4 White-collar ----

### 11.4.1 White ----
#Creating the segmentated auxiliar databases
base_bra <- data %>% 
  filter(branco_dummy == 1, #white
         white_dummy == 1)  #white-collar


base_nbr <- data %>% 
  filter(branco_dummy == 0, #non-white
         white_dummy == 1) #white-collar

#Estimation
calsan_did <- did::att_gt(
  yname = "rais_",
  gname = "year_first_treated",
  idname = "code_id",
  tname = "ano",
  xformla = ~ ano_sexo + ano_ensino + code_id #+ all_in_rais + white_dummy
  ,
  data = base_bra,
  control_group = "notyettreated",
  base_period = "universal",
  clustervars = "code_id"
)

est_calsan <- aggte( MP = calsan_did, type = "dynamic", na.rm = TRUE)
#Result table
print(est_calsan)

plot_calsan <- ggdid(est_calsan) +
  ggtitle("Event Study: Callaway & Sant'anna, CNAE + CBO ") +
  labs("Time to treat") +
  theme_minimal()

plot_calsan


#Final results dataframe
data_calsan <- ggplot_build(plot_calsan)$data[[1]]
data_calsan <- as.data.frame(data_calsan)

### 11.4.2 Non-White ----
#Estimation
calsan_did <- did::att_gt(
  yname = "rais_",
  gname = "year_first_treated",
  idname = "code_id",
  tname = "ano",
  xformla = ~ ano_sexo + ano_ensino + code_id #+ all_in_rais + white_dummy
  ,
  data = base_nbr,
  control_group = "notyettreated",
  base_period = "universal",
  clustervars = "code_id"
)

est_calsan2 <- aggte( MP = calsan_did, type = "dynamic", na.rm = TRUE)
#Result table
print(est_calsan2)

plot_calsan2 <- ggdid(est_calsan2) +
  ggtitle("Event Study: Callaway & Sant'anna, CNAE + CBO ") +
  labs("Time to treat") +
  theme_minimal()

plot_calsan2


#Final result dataframe
data_calsan2 <- ggplot_build(plot_calsan2)$data[[1]]
data_calsan2 <- as.data.frame(data_calsan2)


### 11.4.3 Combined  ----

data_calsan <- data_calsan %>% 
  mutate(
    grupo = 1,
    colour= "red"
  )

data_calsan2 <- data_calsan2 %>% 
  mutate(
    grupo = 2,
    x = x + 0.2,
    colour = "black"
  )

data_final <- rbind(data_calsan,
                    data_calsan2)


### Gráfico
p <- ggplot(data_final, aes(x = x, y = y, colour = colour, group = grupo)) +
  geom_point(size = 3) +
  geom_errorbar(aes(ymin = ymin, ymax = ymax), width = 0.5) +
  geom_hline(yintercept = 0, color = "#D62728") +
  geom_vline(xintercept = -1, color = "#BEBEBE", linetype = "dashed") +
  scale_color_manual(values=c('black','red'),labels=c("Non-White","White")) +
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




p

ggsave("C:/Users/tuffy/Documents/IC/Graphs/united/plot_raca_wc.jpeg", plot = p, device = "jpeg", width = 10, height = 6, dpi = 600)
ggsave("C:/Users/tuffy/Documents/IC/Graphs/united/plot_raca_wc.pdf", plot = p, device = "pdf", width = 10, height = 6, dpi = 300)

rm(p, base_fem, base_mas, data_calsan, data_calsan2, data_final, est_calsan, est_calsan2, plot_calsan, plot_calsan2)



##11.5 Blue-collar ----

### 11.5.1 White ----

#Creating the databases
base_bra <- data %>% 
  filter(branco_dummy == 1, #White
         white_dummy == 0)  #Blue-collar


base_nbr <- data %>% 
  filter(branco_dummy == 0, #Non-white
         white_dummy == 0) #Blue-collar

#Estimation
calsan_did <- did::att_gt(
  yname = "rais_",
  gname = "year_first_treated",
  idname = "code_id",
  tname = "ano",
  xformla = ~ ano_sexo + ano_ensino + code_id #+ all_in_rais + white_dummy
  ,
  data = base_bra,
  control_group = "notyettreated",
  base_period = "universal",
  clustervars = "code_id"
)

est_calsan <- aggte( MP = calsan_did, type = "dynamic", na.rm = TRUE)
#Result table
print(est_calsan)

plot_calsan <- ggdid(est_calsan) +
  ggtitle("Event Study: Callaway & Sant'anna, CNAE + CBO ") +
  labs("Time to treat") +
  theme_minimal()

plot_calsan

#Creating the final results dataframe
data_calsan <- ggplot_build(plot_calsan)$data[[1]]
data_calsan <- as.data.frame(data_calsan)

### 11.5.2 Non-White ----
#Estimation
calsan_did <- did::att_gt(
  yname = "rais_",
  gname = "year_first_treated",
  idname = "code_id",
  tname = "ano",
  xformla = ~ ano_sexo + ano_ensino + code_id #+ all_in_rais + white_dummy
  ,
  data = base_nbr,
  control_group = "notyettreated",
  base_period = "universal",
  clustervars = "code_id"
)

est_calsan2 <- aggte( MP = calsan_did, type = "dynamic", na.rm = TRUE)
#Result table
print(est_calsan2)

plot_calsan2 <- ggdid(est_calsan2) +
  ggtitle("Event Study: Callaway & Sant'anna, CNAE + CBO ") +
  labs("Time to treat") +
  theme_minimal()

plot_calsan2

#Creating the final results dataframe
data_calsan2 <- ggplot_build(plot_calsan2)$data[[1]]
data_calsan2 <- as.data.frame(data_calsan2)


### 11.5.3 Combined ----

data_calsan <- data_calsan %>% 
  mutate(
    grupo = 1,
    colour= "red"
  )

data_calsan2 <- data_calsan2 %>% 
  mutate(
    grupo = 2,
    x = x + 0.2,
    colour = "black"
  )

data_final <- rbind(data_calsan,
                    data_calsan2)


### Gráfico
p <- ggplot(data_final, aes(x = x, y = y, colour = colour, group = grupo)) +
  geom_point(size = 3) +
  geom_errorbar(aes(ymin = ymin, ymax = ymax), width = 0.5) +
  geom_hline(yintercept = 0, color = "#D62728") +
  geom_vline(xintercept = -1, color = "#BEBEBE", linetype = "dashed") +
  scale_color_manual(values=c('black','red'),labels=c("Non-White","White")) +
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




p

ggsave("C:/Users/tuffy/Documents/IC/Graphs/united/plot_raca_bc.jpeg", plot = p, device = "jpeg", width = 10, height = 6, dpi = 600)
ggsave("C:/Users/tuffy/Documents/IC/Graphs/united/plot_raca_bc.pdf", plot = p, device = "pdf", width = 10, height = 6, dpi = 300)

rm(p, base_fem, base_mas, data_calsan, data_calsan2, data_final, est_calsan, est_calsan2, plot_calsan, plot_calsan2)





# -----------------------------------------------------------------------------#
# 12. Estimators ----
# -----------------------------------------------------------------------------#

rm(base)

# install.packages("didimputation")
# install.packages("DIDmultiplegt")




library(didimputation)
library(DIDmultiplegt)



data <- read.csv("C:/Users/tuffy/Documents/IC/Bases/base_atual_dum_v3.csv")


## ------------------------- #
## 12.1 CalSan & SunAb ----
##-------------------------- #
##' Here I estimate the results for the main estimators I employed, being the ones
##' developed by Callaway and Sant'anna, and Sun and Abraham.
#
# plot <- function(df,
#                  plot_title,
#                  var_y,
#                  controles
# ) {
#   
#   
#   ini <- Sys.time()
#   
#   print(paste0("Calculando para:", var_y," :)"))
#   
#   var_y <- as.character(substitute(var_y))
#   
#   # Equações com contorles
#   sunab_formula <- as.formula(
#     paste(
#       var_y, "~  sunab(year_first_treated,time_to_treat,ref.p = -1,ref.c = 2013) | code_id + ano_sexo + ano_branco + ano_ensino"
#     )
#   )
#   
#   calsan_formula <- as.formula(
#     "~ ano_sexo + ano_branco + ano_ensino + code_id "
#   )
#   
#   
#   ##Estimações##
#   #Sun & Abraham
#   est_sunab <- feols(sunab_formula, data = df, cluster = ~ code_id)
#   # Callaway & Sant'anna
#   calsan_did <- did::att_gt(
#     yname = var_y,
#     gname = "year_first_treated",
#     idname = "code_id",
#     tname = "ano",
#     xformla = calsan_formula,
#     data = df,
#     control_group = "notyettreated",
#     base_period = "universal",
#     clustervars = "code_id"
#   )
#   est_calsan <- aggte( MP = calsan_did, type = "dynamic", na.rm = TRUE)
#   print(est_calsan)
#   
#   ##Extraindo os gráficos das duas estimações##
#   plot_sunab <- iplot(est_sunab, ref.line = -1,
#                       xlab = 'Time to treatment',
#                       main = 'Cal_San ES: IGNORAR')  
#   
#   plot_calsan <- ggdid(est_calsan) +
#     ggtitle("Event Study: Callaway & Sant'anna, IGNORAR ") +
#     labs("Time to treat") +
#     theme_minimal()
#   
#   ##Extraindo os coeficientes##
#   data_calsan <- ggplot_build(plot_calsan)$data[[1]]
#   data_calsan <- as.data.frame(data_calsan)
#   
#   data_sunab <- plot_sunab[[1]] 
#   data_sunab <- data_sunab %>% 
#     mutate(colour = ifelse(id == 1, '#f7200a')) %>% 
#     rename(
#       ymin = ci_low,
#       ymax = ci_high,  
#       group = id ) %>% 
#     select(colour, x, y, ymin, ymax, group,-estimate, -estimate_names, -estimate_names_raw,-is_ref)
#   
#   data_calsan <- data_calsan %>% 
#     mutate(colour = '#145ede',
#            group = 2,
#            y = ifelse(x == -1, 0, y),
#            ymin = ifelse(x == -1, 0, ymin),
#            ymax = ifelse(x == -1, 0, ymax)) %>% 
#     select(-PANEL, -shape, -size, -fill, -alpha, -stroke)
#   
#   
#   #Unindo os coeficientes das duas estimações
#   df_completo <- rbind(data_sunab,data_calsan)
#   
#   
#   
#   
#   
#   #Resultados das estimações
#   print(summary(est_sunab))
#   print(summary(est_calsan))
#   
#   df_completo$x <- case_when(df_completo$group == 1 ~ df_completo$x,
#                              df_completo$group == 2 & df_completo$x != -1 ~ df_completo$x + 0.2,
#                              TRUE ~ NA)
#   
#   return(df_completo)
#   
#   
#   delta_t <- Sys.time() - ini
#   print(delta_t)
#   rm(delta_t, ini)
#   
# }
# 
# estimacoes_rais <- plot(data,
#                         plot_title = '',
#                         var_y = "rais_")


calsun <- readRDS("C:/Users/tuffy/Documents/IC/Bases/results_est/rais_total.RDS")

calsun <- calsun %>% 
  mutate(
    colour = ifelse( colour == "#145ede","black", "red"),
    x = ifelse(group == 2, x - 0.2, x - 0.2),
    group = ifelse(group == 2, 1, 2)
  )

# ------------------- #
## 12.2 Borusyak ----
# ------------------- #

res_bjs <- did_imputation(
  data = data,
  yname = "rais_",
  idname = "code_id",
  tname = "ano",
  gname = "year_first_treated",
  first_stage = ~ 1 | code_id + ano_sexo + ano_branco + ano_ensino,
  cluster_var = "code_id",
  horizon = T,
  pretrends = T
)


res_temp <- res_bjs %>% 
  mutate(
    colour = "blue",
    group = 3,
    term = as.numeric(term) + 0.2
  ) %>% 
  rename(
    x = term,
    y = estimate,
    ymin = conf.low,
    ymax = conf.high
  ) %>% 
  select(-std.error, -lhs) #%>% 

res_temp <- res_temp %>% 
  arrange(colour, x, y, ymin, ymax, group)



temp <- rbind(calsun, res_temp)


rm(res_bjs, res_temp)

# -------------------------- #
## 12.3 De ChaiseMartin ----
# -------------------------- #


data$treat_dcm <- ifelse(data$ano < data$year_first_treated, 0, 1)

# data$code_id <- as.numeric(data$code_id)
# 
# multiplegt_res <- did_multiplegt_old(
#   df = data, 
#   Y = "rais_",
#   G = "year_first_treated",
#   T = "ano",
#   D = "treat_dcm",
#   #i = "code_id",
#   controls = c("code_id","ano_sexo", "ano_branco", "ano_ensino"),
#   dynamic = 5,
#   placebo = 5,
#   brep = 50,
#   #parallel = T,
#   #cluster = "code_id"
#   #,
#   # homogeneous_att = FALSE,
#   # mode = "old"
# )

## --------------------------- #
### DISCLAIMER
## --------------------------- #
##' An older version of the data.table package was needed to correctly apply this
##' estimator. The following steps are directed towards this adaptation.
##----------- #
#
# install.packages("pkgbuild")
# pkgbuild::has_rtools()
# 
# install.packages("remotes")
# remotes::install_version("data.table", version = "1.16.4", repos = "http://cran.us.r-project.org")
#
## Estimated @FEA-USP
# multiplegt_res <- did_multiplegt_dyn(
#   df = data,
#   outcome = "rais_",
#   group = "year_first_treated",
#   time = "ano",
#   treatment = "treat_dcm",
#   controls = c("ano_sexo", "ano_ensino", "ano_branco", "code_id"),
#   effects = 5,
#   placebo = 9,
#   cluster = "code_id"
# )
# 
# 
# 
# multiplegt_res$plot
# 
# final <- Sys.time()
# 
# print(final - ini)
# 
# saveRDS(multiplegt_res, "C:/Users/tuffy/Documents/IC/Bases/results_est/dechaisemartin.RDS")

chaise <- readRDS("C:/Users/tuffy/Documents/IC/Bases/results_est/dechaisemartin.RDS")


# Joining into the final results dataframe
estimates_df <- chaise$plot$data

estimates_df <- estimates_df %>% 
  mutate(
    colour = "green",
    group = 4,
    Time = as.numeric(Time) - 1.4 #t = 0 Último período em que não é tratado.
  )  %>% 
  rename(
    x = Time,
    y = Estimate,
    ymin = LB.CI,
    ymax = UB.CI
  )

temp <- rbind(temp, estimates_df)


temp <- temp %>%
  arrange(group)



# ---------------------------------------------------------------------------- #
## 12.4 Graph  ----
# ---------------------------------------------------------------------------- #

p <- ggplot(temp, aes(x = x, y = y, color = colour, shape = colour, group = group)) +
  geom_point( size = 3) +
  geom_errorbar(aes(ymin = ymin, ymax = ymax), width = 0.5) +
  geom_hline(yintercept = 0, color = "#D62728") +
  geom_vline(xintercept = -1, color = "#BEBEBE", linetype = "dashed") +
  scale_color_manual(
    values = c('black', 'blue', '#009E60'	, 'red'),
    labels = c("Callaway & Sant'anna",
               "Borusyak et al.",
               "De Chaisemartin & d’Haultfoeuille",
               "Sun & Abraham")
  ) +
  scale_shape_manual(
    values = c(16, 15, 17, 18), # Circle, square, triangle, diamond
    labels = c("Callaway & Sant'anna",
               "Borusyak et al.",
               "De Chaisemartin & d’Haultfoeuille",
               "Sun & Abraham")
  ) +  
  labs(x = "Years to treatment", y = '', colour = '', shape = '') +
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
  scale_x_continuous(limits = c(-9.2, 5.2),
                     breaks = c(-9,-8,-7,-6,-5,-4,-3,-2,-1,0,1,2,3,4,5),
                     labels = c('-9','-8','-7','-6','-5','-4','-3','-2','-1','0','+1','+2','+3','+4','+5')) +
  scale_y_continuous(limits = c(-0.95, 0.155),
                     breaks = c(-0.90,-0.75,-0.60,-0.45,-0.30,-0.15,0,0.15),
                     labels = c('-0.90','-0.75','-0.60','-0.45','-0.30','-0.15','0','0.15'))
p

ggsave("C:/Users/tuffy/Documents/IC/Graphs/united/estimators.jpeg", plot = p, device = "jpeg", width = 10, height = 6, dpi = 600)
ggsave("C:/Users/tuffy/Documents/IC/Graphs/united/estimators.pdf", plot = p, device = "pdf", width = 10, height = 6, dpi = 300)



# ---------------------------------------------------------------------------- #
# 13. New variable tests ----
# ---------------------------------------------------------------------------- #

data <- read.csv("C:/Users/tuffy/Documents/IC/Bases/base_atual_dum_v3.csv")

data <- data %>% 
  filter(all_in_rais == 1) #presence in all periods

data <- data %>% 
  group_by(code_id) %>% 
  mutate(
    first_cnae = cnae_group[ano == 2003]
  ) %>% 
  arrange( code_id, ano) %>% 
  select(-X) %>% 
  ungroup()

# --------------------- #
## 13.1 CNAE ------ 
# --------------------- #


#' The CNAE variable can more easily capture movement between firms, since their
#' are naturally assigned into different codes. Thus, this is estimations tried 
#' to capture the true firm change movement. Differently from previous estimations
#' this section is directed to capturing the CNAE code when compared to the first
#' CNAE observation of each individual in the dataset.

summary(data$cnae_group)

#Steps for dummy criation
treat <- data %>%
  arrange(code_id, ano) %>% 
  select(
    code_id,
    ano,
    cnae_group,
    cnae_pre_treat,
    year_first_treated,
    rais_,
    first_cnae
  ) %>%
  group_by(code_id) %>% 
  
  mutate(
    #Auxiliatory Dummy - free movement after treatment
    dummy_cnae_aux =        
      case_when(
        #Possible cases
        ano < year_first_treated & cnae_group != first_cnae ~ 1, 
        
        #ano >= year_first_treated & cnae_group == 0 ~ 1,
        
        ano >= year_first_treated & cnae_group != 0 & !(cnae_group %in% cnae_group[ano < year_first_treated]) ~ 1,
        
        TRUE ~ 0
      ),
    #Final dummy
    first_one = which(dummy_cnae_aux == 1 & ano >= year_first_treated)[1], #First pre-treatment row
    
    dummy_cnae_all = ifelse(
      row_number() >= first_one & !is.na(first_one), 1, dummy_cnae_aux
    ),
    
    dummy_cnae_all = ifelse(
      is.na(first_one) & ano >= year_first_treated, 0 , dummy_cnae_all #Condition to deal with the cases where there is no first one
    ),
    
    new_dummy_cnae = ifelse(
      cnae_group != first_cnae, 1, 0
    )
  )

treat <- treat %>% 
  select(
    code_id, ano, dummy_cnae_all, new_dummy_cnae #Selecting only the merge necessary collumns
  )

#Joinning both dataframes
data <- data %>% 
  left_join(treat, by= c("code_id", "ano")) %>% 
  select(-X.1)

# ----------------- #
## 13.2 Estimation ----
# ------------------ #

calsan_did <- did::att_gt(
  yname = "new_dummy_cnae",
  gname = "year_first_treated",
  idname = "code_id",
  tname = "ano",
  xformla = ~ ano_sexo + ano_ensino + code_id + ano_branco #+ all_in_rais + white_dummy
  ,
  data = data,
  control_group = "notyettreated",
  base_period = "universal",
  clustervars = "code_id"
)

est_calsan <- aggte( MP = calsan_did, type = "dynamic", na.rm = TRUE)
#Result table
print(est_calsan)

plot_calsan <- ggdid(est_calsan) +
  ggtitle("Event Study: CNAE - all_in_rais ") +
  labs("Time to treat") +
  theme_minimal()

plot_calsan


ggsave("C:/Users/tuffy/Documents/IC/Graphs/plot_test_cnae.jpeg", plot = plot_calsan, device = "jpeg", width = 10, height = 6, dpi = 600)

# --------------------------------------------------------------------------- #


# ---------------------------------------------------------------------------- #
# 14. Newest CNAE variable ----
# ---------------------------------------------------------------------------- #
#dataset
data <- read.csv("C:/Users/tuffy/Documents/IC/Bases/base_atual_dum_v3.csv")


data <- data %>% 
  group_by(code_id) %>% 
  mutate(
    aux1 = which(cnae_group == 0 & ano < year_first_treated)[1],
    aux2 = which(cnae_group != 0 & ano < year_first_treated)[1]
    
  ) %>% 
  arrange( code_id, ano) %>% 
  select(-X) %>%
  ungroup()



treat <- data %>%
  arrange(code_id, ano) %>% 
  select(
    code_id,
    ano,
    cnae_group,
    cnae_pre_treat,
    year_first_treated,
    rais_,
    aux2,
    aux1
  ) %>%
  group_by(code_id) %>% 
  
  mutate(
    cnae_aux = cnae_group[row_number() == aux2],
    cnae_dummy_v2 = ifelse(cnae_group != cnae_aux & cnae_group != 0, 1, 0),
    
    
    first_one = which(cnae_dummy_v2 == 1 & ano >= year_first_treated)[1], #First row pre-treatment
    
    dummy_cnae_all_v2 = ifelse(
      row_number() >= first_one & !is.na(first_one), 1, cnae_dummy_v2
    ),
    
    
    #Accountig for INF values
    cnae_dummy_v3 = ifelse(cnae_group != cnae_aux, 1, 0),
    
    cnae_dummy_v4 = ifelse(cnae_group != cnae_aux, 1, 0), #direct dummy
    
    
    
    first_one2 = which(cnae_dummy_v3 == 1 & ano >= year_first_treated)[1], #First row pre-treat
    
    dummy_cnae_all_v3 = ifelse(
      row_number() >= first_one2 & !is.na(first_one2), 1, cnae_dummy_v3 #propagating the result through periods
    ),
  )

treat <- treat %>% 
  select(
    code_id, ano, dummy_cnae_all_v2, dummy_cnae_all_v3, cnae_dummy_v4 #Only necessary collumns
  )

#Combining the dataframes
data <- data %>% 
  left_join(treat, by= c("code_id", "ano")) %>% 
  select(-X.1)


#'Next we will comapre the results found between the two main dummy specifications

# ------------------------- #
## 14.2 D2 Estimation ----
# ------------------------- #

calsan_did <- did::att_gt(
  yname = "dummy_cnae_all_v2",
  gname = "year_first_treated",
  idname = "code_id",
  tname = "ano",
  xformla = ~ ano_sexo + ano_ensino + code_id #+ all_in_rais + white_dummy
  ,
  data = data,
  control_group = "notyettreated",
  base_period = "universal",
  clustervars = "code_id"
)

est_calsan <- aggte( MP = calsan_did, type = "dynamic", na.rm = TRUE)
#Result table
print(est_calsan)

plot_calsan <- ggdid(est_calsan) +
  ggtitle("Event Study: CNAE - Mudança CNAE ") +
  labs("Time to treat") +
  theme_minimal()

plot_calsan
ggsave("C:/Users/tuffy/Documents/IC/Graphs/plot_test2_cnae.jpeg", plot = plot_calsan, device = "jpeg", width = 10, height = 6, dpi = 600)

# -------------------------- #
## 14.2 D4*** Estimation ----
# -------------------------- #
#' This estimation utilized the 4 dummy spec. The stars refered to the prefered 
#' specification for the code.

#Aqui só utilizamos a variável de CNAE da primeira observação.

calsan_did <- did::att_gt(
  yname = "cnae_dummy_v4",
  gname = "year_first_treated",
  idname = "code_id",
  tname = "ano",
  xformla = ~ ano_sexo + ano_ensino + code_id + ano_branco #+ all_in_rais + white_dummy
  ,
  data = data,
  control_group = "notyettreated",
  base_period = "universal",
  clustervars = "code_id"
)

est_calsan <- aggte( MP = calsan_did, type = "dynamic", na.rm = TRUE)
#Result table
print(est_calsan)

plot_calsan <- ggdid(est_calsan) +
  ggtitle("Event Study: CNAE - Mudança  v2 ") +
  labs("Time to treat") +
  theme_minimal()

plot_calsan


ggsave("C:/Users/tuffy/Documents/IC/Graphs/plot_test5_cnae.jpeg", plot = plot_calsan, device = "jpeg", width = 10, height = 6, dpi = 600)

rm(treat)


#------------------------------------------------------------------------------#
# 15. Honest DID ----
# -----------------------------------------------------------------------------#

# # Install remotes package if not installed
# install.packages("remotes")
# 
# # Turn off warning-error-conversion, because the tiniest warning stops installation
# Sys.setenv("R_REMOTES_NO_ERRORS_FROM_WARNINGS" = "true")
# 
# # install from github
# remotes::install_github("asheshrambachan/HonestDiD")

library(here)
library(dplyr)
library(did)
library(haven)
library(ggplot2)
library(fixest)
library(HonestDiD)

# ---------------- #
## 15.1 Function ----


#' @title honest_did
#'
#' @description a function to compute a sensitivity analysis
#'  using the approach of Rambachan and Roth (2021)
#'
#' @param ... Parameters to pass to the relevant method.
honest_did <- function(...) UseMethod("honest_did")

#' @title honest_did.AGGTEobj
#'
#' @description a function to compute a sensitivity analysis
#'  using the approach of Rambachan and Roth (2021) when
#'  the event study is estimating using the `did` package
#'
#' @param es Result from aggte (object of class AGGTEobj).
#' @param e event time to compute the sensitivity analysis for.
#'  The default value is `e=0` corresponding to the "on impact"
#'  effect of participating in the treatment.
#' @param type Options are "smoothness" (which conducts a
#'  sensitivity analysis allowing for violations of linear trends
#'  in pre-treatment periods) or "relative_magnitude" (which
#'  conducts a sensitivity analysis based on the relative magnitudes
#'  of deviations from parallel trends in pre-treatment periods).
#' @param gridPoints Number of grid points used for the underlying test
#'  inversion. Default equals 100. User may wish to change the number of grid
#'  points for computational reasons.
#' @param ... Parameters to pass to `createSensitivityResults` or
#'  `createSensitivityResults_relativeMagnitudes`.
honest_did.AGGTEobj <- function(es,
                                e          = 0,
                                type       = c("smoothness", "relative_magnitude"),
                                gridPoints = 100,
                                ...) {
  
  type <- match.arg(type)
  
  # Make sure that user is passing in an event study
  if (es$type != "dynamic") {
    stop("need to pass in an event study")
  }
  
  # Check if used universal base period and warn otherwise
  if (es$DIDparams$base_period != "universal") {
    stop("Use a universal base period for honest_did")
  }
  
  # Recover influence function for event study estimates
  es_inf_func <- es$inf.function$dynamic.inf.func.e
  
  
  # Recover variance-covariance matrix ----
  n <- nrow(es_inf_func)
  V <- t(es_inf_func) %*% es_inf_func / n / n
  
  # Check time vector is consecutive with referencePeriod = -1
  referencePeriod <- -1
  consecutivePre  <- !all(diff(es$egt[es$egt <= referencePeriod]) == 1)
  consecutivePost <- !all(diff(es$egt[es$egt >= referencePeriod]) == 1)
  if ( consecutivePre | consecutivePost ) {
    msg <- "honest_did expects a time vector with consecutive time periods;"
    msg <- paste(msg, "please re-code your event study and interpret the results accordingly.", sep="\n")
    stop(msg)
  }
  
  # Remove the coefficient normalized to zero
  hasReference <- any(es$egt == referencePeriod)
  if ( hasReference ) {
    referencePeriodIndex <- which(es$egt == referencePeriod)
    V    <- V[-referencePeriodIndex,-referencePeriodIndex]
    beta <- es$att.egt[-referencePeriodIndex]
  } else {
    beta <- es$att.egt
  }
  
  nperiods <- nrow(V)
  npre     <- sum(1*(es$egt < referencePeriod))
  npost    <- nperiods - npre
  if ( !hasReference & (min(c(npost, npre)) <= 0) ) {
    if ( npost <= 0 ) {
      msg <- "not enough post-periods"
    } else {
      msg <- "not enough pre-periods"
    }
    msg <- paste0(msg, " (check your time vector; note honest_did takes -1 as the reference period)")
    stop(msg)
  }
  
  baseVec1 <- basisVector(index=(e+1),size=npost)
  orig_ci  <- constructOriginalCS(betahat        = beta,
                                  sigma          = V,
                                  numPrePeriods  = npre,
                                  numPostPeriods = npost,
                                  l_vec          = baseVec1)
  
  if (type=="relative_magnitude") {
    robust_ci <- createSensitivityResults_relativeMagnitudes(betahat        = beta,
                                                             sigma          = V,
                                                             numPrePeriods  = npre,
                                                             numPostPeriods = npost,
                                                             l_vec          = baseVec1,
                                                             gridPoints     = gridPoints,
                                                             ...)
    
  } else if (type == "smoothness") {
    robust_ci <- createSensitivityResults(betahat        = beta,
                                          sigma          = V,
                                          numPrePeriods  = npre,
                                          numPostPeriods = npost,
                                          l_vec          = baseVec1,
                                          ...)
  }
  
  return(list(robust_ci=robust_ci, orig_ci=orig_ci, type=type))
}

# ------------------ #
## 15.2 Graph -----
# ------------------ #

calsan_did <- did::att_gt(
  yname = "cnae_dummy_v4",
  gname = "year_first_treated",
  idname = "code_id",
  tname = "ano",
  xformla = ~ ano_sexo + ano_ensino + code_id #+ all_in_rais + white_dummy
  ,
  data = data,
  control_group = "notyettreated",
  base_period = "universal",
  clustervars = "code_id"
)

est_calsan <- aggte( MP = calsan_did, type = "dynamic", na.rm = TRUE)


es <- est_calsan


#Run sensitivity analysis for relative magnitudes
sensitivity_results <-
  honest_did(es,
             e=0,
             type="relative_magnitude",
             Mbarvec=seq(from = 0.5, to = 2, by = 0.5))

HonestDiD::createSensitivityPlot_relativeMagnitudes(sensitivity_results$robust_ci,
                                                    sensitivity_results$orig_ci)


sensitivity_results <-
  honest_did(es,
             e=0,
             type="relative_magnitude",
             Mbarvec=seq(from = 0.05, to = 1, by = 0.1))

HonestDiD::createSensitivityPlot_relativeMagnitudes(sensitivity_results$robust_ci,
                                                    sensitivity_results$orig_ci)


# --------------- #
## 15.3 Rais----
# --------------- #

calsan_did <- did::att_gt(
  yname = "rais_",
  gname = "year_first_treated",
  idname = "code_id",
  tname = "ano",
  xformla = ~ ano_sexo + ano_ensino + code_id + ano_branco #+ all_in_rais + white_dummy
  ,
  data = data,
  control_group = "notyettreated",
  base_period = "universal",
  clustervars = "code_id"
)

est_calsan <- aggte( MP = calsan_did, type = "dynamic", na.rm = TRUE)


es <- est_calsan

sensitivity_results <-
  honest_did(es,
             e=0,
             type="relative_magnitude",
             Mbarvec=seq(from = 0.5, to = 2, by = 0.5))

HonestDiD::createSensitivityPlot_relativeMagnitudes(sensitivity_results$robust_ci,
                                                    sensitivity_results$orig_ci)



es_inf_func <- es$inf.function$dynamic.inf.func.e

n <- nrow(es_inf_func)
V <- t(es_inf_func) %*% es_inf_func / n / n


pre_treatV <- V[1:8, 1:8, drop = FALSE]

# Assuming pre_treatV is your 8×8 matrix
M_row <- matrix(1/8, nrow = 1, ncol = 8)  # 1×8
M_col <- matrix(1/8, nrow = 8, ncol = 1)  # 8×1

# Multiply: (1×8) %*% (8×8) → 1×8, then %*% (8×1) → 1×1
result_1x1 <- M_row %*% pre_treatV %*% M_col


# ---------------------------------------------------------------------------- #
# 16. RAIS TWFE ----
# ---------------------------------------------------------------------------- #

#' Here we will test the TFWE in the main specification
colnames(data)


twfe <- feols(cnae_dummy_v4 ~ i(time_to_treat, ref = -1) | code_id + ano_sexo + ano_branco + ano_ensino,
              data = data,
              cluster = ~ code_id)

iplot(twfe)

#----------------------------------------------------------------------------- #
# <<< CBO >>> ----
# ---------------------------------------------------------------------------- #

#' In this section we will test how the relative probability of RAIS presence is
#' observed for different CBO groups.

# 17. CBO----
# Looking at RAIS within each CBO group
data <- read.csv("C:/Users/tuffy/Documents/IC/Bases/base_atual_dum_v3.csv")


data <- data %>% 
  group_by(code_id) %>% 
  mutate(
    aux1 = which(cbo_group == 0 & ano < year_first_treated)[1],
    aux2 = which(cbo_group != 0 & ano < year_first_treated)[1]
    
  ) %>% 
  arrange( code_id, ano) %>% 
  select(-X) %>%
  ungroup()



data <- data %>%
  group_by(code_id) %>% 
  
  mutate(
    cbo_first = cbo_group[row_number() == aux2]
  )

## 17.1 Data Frames ----

#Note that WC = White-Collar and BC = Blue-Collar

### 17.1.1 WC ----
data_wc <- data %>% 
  filter(white_dummy == 1 &
           cbo_first %in% c(1:5)) #Removing the individuals who don't start as WC

summary(data_wc$cbo_group)
summary(data_wc$cbo_first)

### 17.1.2 BC ------

data_bc <- data %>% 
  filter(white_dummy == 0 &
           cbo_first %in% c(6:10)) #Removing the individuals who don't start as BC

summary(data_bc$cbo_group)
summary(data_bc$cbo_first)


## 17.2 Estimation ----
### 17.2.1 WC ----

data_final <- data.frame()

#WC
for ( wc in c(1:5)) {
  
  
  temp <- data_wc %>% 
    filter( cbo_first == wc)
  
  
  #Estimation
  calsan_did <- did::att_gt(
    yname = "rais_",
    gname = "year_first_treated",
    idname = "code_id",
    tname = "ano",
    xformla = ~ code_id + ano_sexo + ano_branco + ano_ensino
    ,
    data = temp,
    control_group = "notyettreated",
    base_period = "universal",
    clustervars = "code_id"
  )
  
  est_calsan <- aggte( MP = calsan_did, type = "dynamic", na.rm = TRUE)
  #Result table
  print(est_calsan)
  # 
  plot_calsan <- ggdid(est_calsan) 
  
  # 
  plot_calsan
  
  #Result dataframe
  data_calsan <- ggplot_build(plot_calsan)$data[[1]]
  data_calsan$cbo <- wc
  data_final <- rbind(data_final,as.data.frame(data_calsan))
  
  rm(temp, calsan_did)
  
}

### 17.2.2. Graph ----


data_final <- data_final %>% 
  mutate(
    x = as.numeric(x),
    x = case_when(
      cbo == 1 ~ x - 0.25,
      cbo == 2 ~ x - 0.15,
      cbo == 4 ~ x + 0.15,
      cbo == 5 ~ x + 0.25,
      TRUE ~ x
    )
  ) %>%
  filter(cbo != 1)

data_final$cbo <- as.factor(data_final$cbo)

p <- ggplot(data_final, aes(x = x, y = y, color = cbo, shape = cbo, group = cbo)) +
  geom_point( size = 3) +
  geom_errorbar(aes(ymin = ymin, ymax = ymax), width = 0.3) +
  geom_hline(yintercept = 0, color = "#D62728") +
  geom_vline(xintercept = -1, color = "#BEBEBE", linetype = "dashed") +
  scale_color_manual(
    values = c(
      #'black',
      '#0072B2', '#009E73'	, '#E69F00', "#CC79A7"),
    labels = c(
      #"Group 0",
      "Group 1",
      "Group 2",
      "Group 3",
      "Group 4"
    )
  ) +
  scale_shape_manual(
    values = c(
      #16,
      15, 17, 18, 16), # Circle, Square, Triangle, Diamond
    labels = c(
      #"Group 0",
      "Group 1",
      "Group 2",
      "Group 3",
      "Group 4"
    )
  ) +  
  labs(x = "Years to treatment", y = '', colour = '', shape = '') +
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
  scale_x_continuous(limits = c(-9.5, 4.5),
                     breaks = c(-9,-8,-7,-6,-5,-4,-3,-2,-1,0,1,2,3,4),
                     labels = c('-9','-8','-7','-6','-5','-4','-3','-2','-1','0','+1','+2','+3','+4')) +
  scale_y_continuous(limits = c(-0.95, 0.155),
                     breaks = c(-0.90,-0.75,-0.60,-0.45,-0.30,-0.15,0,0.15),
                     labels = c('-0.90','-0.75','-0.60','-0.45','-0.30','-0.15','0','0.15'))
p


ggsave("C:/Users/tuffy/Documents/IC/Graphs/united/WC_groups.jpeg", plot = p, device = "jpeg", width = 10, height = 6, dpi = 600)
ggsave("C:/Users/tuffy/Documents/IC/Graphs/united/WC_groups.pdf", plot = p, device = "pdf", width = 10, height = 6, dpi = 300)

### 17.2.1 BC ----

data_final <- data.frame()

#BC
for ( wc in c(6,8,9,10)) {
  
  
  temp <- data_bc %>% 
    filter( cbo_first == wc)
  
  
  #Estimation
  calsan_did <- did::att_gt(
    yname = "rais_",
    gname = "year_first_treated",
    idname = "code_id",
    tname = "ano",
    xformla = ~ code_id + ano_sexo + ano_branco + ano_ensino
    ,
    data = temp,
    control_group = "notyettreated",
    base_period = "universal",
    clustervars = "code_id"
  )
  
  est_calsan <- aggte( MP = calsan_did, type = "dynamic", na.rm = TRUE)
  #Result table
  print(est_calsan)
  # 
  plot_calsan <- ggdid(est_calsan) 
  
  # 
  plot_calsan
  # 
  #   #Result dataframe
  # 
  data_calsan <- ggplot_build(plot_calsan)$data[[1]]
  data_calsan$cbo <- wc
  data_final <- rbind(data_final,as.data.frame(data_calsan))
  
  rm(temp, calsan_did)
  
}

### 17.2.2. Graph ----


data_final <- data_final %>% 
  mutate(
    x = as.numeric(x),
    x = case_when(
      cbo == 6 ~ x - 0.25,
      cbo == 7 ~ x - 0.15,
      cbo == 9 ~ x + 0.15,
      cbo == 10 ~ x + 0.25,
      TRUE ~ x
    )
  )

data_final$cbo <- as.factor(data_final$cbo)

p <- ggplot(data_final, aes(x = x, y = y, color = cbo, shape = cbo, group = cbo)) +
  geom_point( size = 3) +
  geom_errorbar(aes(ymin = ymin, ymax = ymax), width = 0.3) +
  geom_hline(yintercept = 0, color = "#D62728") +
  geom_vline(xintercept = -1, color = "#BEBEBE", linetype = "dashed") +
  scale_color_manual(
    values = c('black', '#0072B2', '#009E73'	, '#E69F00'
               #, "#CC79A7"
    ),
    labels = c("Group 5",
               #"Group 7",
               "Group 7",
               "Group 8",
               "Group 9"
    )
  ) +
  scale_shape_manual(
    values = c(16, 15, 17, 18
               #, 16
    ), # Circle, square, triangle, diamond
    labels = c("Group 5",
               #"Group 7",
               "Group 7",
               "Group 8",
               "Group 9"
    )
  ) +  
  labs(x = "Years to treatment", y = '', colour = '', shape = '') +
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
  scale_x_continuous(limits = c(-9.5, 4.5),
                     breaks = c(-9,-8,-7,-6,-5,-4,-3,-2,-1,0,1,2,3,4),
                     labels = c('-9','-8','-7','-6','-5','-4','-3','-2','-1','0','+1','+2','+3','+4')) +
  scale_y_continuous(limits = c(-0.95, 0.155),
                     breaks = c(-0.90,-0.75,-0.60,-0.45,-0.30,-0.15,0,0.15),
                     labels = c('-0.90','-0.75','-0.60','-0.45','-0.30','-0.15','0','0.15'))
p


ggsave("C:/Users/tuffy/Documents/IC/Graphs/united/BC_groups.jpeg", plot = p, device = "jpeg", width = 10, height = 6, dpi = 600)
ggsave("C:/Users/tuffy/Documents/IC/Graphs/united/BC_groups.pdf", plot = p, device = "pdf", width = 10, height = 6, dpi = 300)


# ---------------------------------------------------------------------------- #
# <<< CNAE >>> ----
# ---------------------------------------------------------------------------- #

#'The same methodology is applied to this section. We seek to observe how each
#'CNAE group differs from the overall observed result in RAIS.

# 18. CNAE----
# Database
data <- read.csv("C:/Users/tuffy/Documents/IC/Bases/base_atual_dum_v3.csv")


data <- data %>% 
  group_by(code_id) %>% 
  mutate(
    aux1 = which(cnae_group == 0 & ano < year_first_treated)[1], 
    aux2 = which(cnae_group != 0 & ano < year_first_treated)[1] # Removing NA values
    
  ) %>% 
  arrange( code_id, ano) %>% 
  select(-X) %>%
  ungroup() %>% 
  mutate(
    cnae_group = ifelse(cnae_group > 0, cnae_group - 1, cnae_group)
  )



data <- data %>%
  group_by(code_id) %>% 
  
  mutate(
    cnae_first = cnae_group[row_number() == aux2]
  )

## 18.1 Bases ----

### 18.1.1 WC ----
data_wc <- data %>% 
  filter(white_dummy == 1) #Removing the individuals who don't start as WC

summary(data_wc$cnae_group)
summary(data_wc$cnae_first)

### 18.1.2 BC ------

data_bc <- data %>% 
  filter(white_dummy == 0) #Removing the individuals who don't start as BC

summary(data_bc$cnae_group)
summary(data_bc$cnae_first)

# ---------------------------- #
## 18.2 WC ----
# ---------------------------- #
### 18.2.1 Estimation----

data_final <- data.frame()

#WC
for ( wc in c(1:18)) {
  
  
  temp <- data_wc %>% 
    filter( cnae_first == wc)
  
  
  #Estimation
  calsan_did <- did::att_gt(
    yname = "rais_",
    gname = "year_first_treated",
    idname = "code_id",
    tname = "ano",
    xformla = ~ code_id + ano_sexo + ano_branco + ano_ensino
    ,
    data = temp,
    control_group = "notyettreated",
    base_period = "universal",
    clustervars = "code_id"
  )
  
  est_calsan <- aggte( MP = calsan_did, type = "dynamic", na.rm = TRUE)
  #Result table
  print(est_calsan)
  # 
  plot_calsan <- ggdid(est_calsan) 
  
  # 
  plot_calsan
  # 
  # #Extraindo os resultados para o sexo feminino
  # 
  data_calsan <- ggplot_build(plot_calsan)$data[[1]]
  data_calsan$cnae <- wc
  data_final <- rbind(data_final,as.data.frame(data_calsan))
  
  rm(temp, calsan_did)
  
}

### 18.2.2. Graph ----


data_final <- data_final %>% 
  mutate(
    x = as.numeric(x),
    x = case_when(
      cnae == 1 ~ x - 0.30,
      cnae == 2 ~ x - 0.25,
      cnae == 3 ~ x - 0.20,
      cnae == 4 ~ x - 0.15,
      cnae == 5 ~ x - 0.10,
      cnae == 6 ~ x - 0.05,
      cnae == 7 ~ x - 0.03,
      cnae == 9 ~ x + 0.03,
      cnae == 10 ~ x + 0.05,
      cnae == 11 ~ x + 0.10,
      cnae == 12 ~ x + 0.15,
      cnae == 13 ~ x + 0.20,
      cnae == 14 ~ x + 0.25,
      cnae == 15 ~ x + 0.30,
      TRUE ~ x
    )
  )

data_final$cnae <- as.factor(data_final$cnae)


shapes18 <- c(0:17)   # squares, circles, triangles, etc.
colors18 <- RColorBrewer::brewer.pal(8, "Set2") |> 
  c(RColorBrewer::brewer.pal(12, "Paired")) |> 
  head(18)


p <- ggplot(data_final, aes(x = x, y = y, color = cnae, shape = cnae, group = cnae)) +
  geom_point(size = 3) +
  geom_errorbar(aes(ymin = ymin, ymax = ymax), width = 0.3) +
  geom_hline(yintercept = 0, color = "#D62728") +
  geom_vline(xintercept = -1, color = "#BEBEBE", linetype = "dashed") +
  labs(x = "Years to treatment", y = '', colour = '', shape = '') +
  theme_classic(base_size = 18) +
  theme(
    axis.line = element_line(),
    axis.ticks.length = unit(5, "pt"),
    axis.ticks = element_line(colour = "black"),  
    axis.ticks.x = element_line(colour = "black"),
    axis.ticks.y = element_line(colour = "black"),
    axis.text.x = element_text(margin = margin(t = 5), size = 18),
    axis.text.y = element_text(size = 18),
    legend.text = element_text(size = 16),
    legend.position = "right",  # move legend outside
    legend.box.margin = margin(0, 20, 0, 0) # add spacing from plot
  ) +
  scale_color_manual(values = colors18) +
  scale_shape_manual(values = shapes18) +
  scale_x_continuous(
    limits = c(-9.5, 4.5),
    breaks = -9:4,
    labels = c(-9:-1, 0, "+1","+2","+3","+4")
  ) +
  scale_y_continuous(
    limits = c(-0.95, 0.1555),
    breaks = seq(-0.90, 0.15, 0.15),
    labels = c('-0.90','-0.75','-0.60','-0.45','-0.30','-0.15','0','0.15')
  )
p


ggsave("C:/Users/tuffy/Documents/IC/Graphs/united/Cnae_WC_groups.jpeg", plot = p, device = "jpeg", width = 10, height = 6, dpi = 600)
ggsave("C:/Users/tuffy/Documents/IC/Graphs/united/Cnae_WC_groups.pdf", plot = p, device = "pdf", width = 10, height = 6, dpi = 300)

# ---------------------------- #
## 18.3.1 BC ----
# ---------------------------- #

### 18.3.1 REG ----


#BC
for ( wc in c(1:7,9:16,18)) {
  message("Rodando para: ", wc," ...")
  
  if( wc == 1) {
    data_final <- data.frame()
  }
  
  temp <- data_bc %>% 
    filter( cnae_first == wc)
  
  
  #Estimation
  calsan_did <- did::att_gt(
    yname = "rais_",
    gname = "year_first_treated",
    idname = "code_id",
    tname = "ano",
    xformla = ~ code_id + ano_sexo + ano_branco + ano_ensino
    ,
    data = temp,
    control_group = "notyettreated",
    base_period = "universal",
    clustervars = "code_id"
  )
  
  est_calsan <- aggte( MP = calsan_did, type = "dynamic", na.rm = TRUE)
  #Result table
  print(est_calsan)
  # 
  plot_calsan <- ggdid(est_calsan) 
  
  # 
  plot_calsan
  # 
  # #Extraindo os resultados para o sexo feminino
  # 
  data_calsan <- ggplot_build(plot_calsan)$data[[1]]
  data_calsan$cnae <- wc
  data_final <- rbind(data_final,as.data.frame(data_calsan))
  
  rm(temp, calsan_did)
  
}

### 18.3.2. Graph ----


data_final <- data_final %>% 
  mutate(
    x = as.numeric(x),
    x = case_when(
      cnae == 6 ~ x - 0.25,
      cnae == 7 ~ x - 0.15,
      cnae == 9 ~ x + 0.15,
      cnae == 10 ~ x + 0.25,
      TRUE ~ x
    )
  )

data_final$cnae <- as.factor(data_final$cnae)

shapes18 <- c(0:17)   # squares, circles, triangles, etc.
colors18 <- RColorBrewer::brewer.pal(8, "Set2") |> 
  c(RColorBrewer::brewer.pal(12, "Paired")) |> 
  head(18)


p <- ggplot(data_final, aes(x = x, y = y, color = cnae, shape = cnae, group = cnae)) +
  geom_point(size = 3) +
  geom_errorbar(aes(ymin = ymin, ymax = ymax), width = 0.3) +
  geom_hline(yintercept = 0, color = "#D62728") +
  geom_vline(xintercept = -1, color = "#BEBEBE", linetype = "dashed") +
  labs(x = "Years to treatment", y = '', colour = '', shape = '') +
  theme_classic(base_size = 18) +
  theme(
    axis.line = element_line(),
    axis.ticks.length = unit(5, "pt"),
    axis.ticks = element_line(colour = "black"),  
    axis.ticks.x = element_line(colour = "black"),
    axis.ticks.y = element_line(colour = "black"),
    axis.text.x = element_text(margin = margin(t = 5), size = 18),
    axis.text.y = element_text(size = 18),
    legend.text = element_text(size = 16),
    legend.position = "right",  # move legend outside
    legend.box.margin = margin(0, 20, 0, 0) # add spacing from plot
  ) +
  scale_color_manual(values = colors18) +
  scale_shape_manual(values = shapes18) +
  scale_x_continuous(
    limits = c(-9.5, 4.5),
    breaks = -9:4,
    labels = c(-9:-1, 0, "+1","+2","+3","+4")
  ) +
  scale_y_continuous(
    limits = c(-0.95, 0.1555),
    breaks = seq(-0.90, 0.15, 0.15),
    labels = c('-0.90','-0.75','-0.60','-0.45','-0.30','-0.15','0','0.15')
  )
p



ggsave("C:/Users/tuffy/Documents/IC/Graphs/united/Cnae_BC_groups.jpeg", plot = p, device = "jpeg", width = 10, height = 6, dpi = 600)
ggsave("C:/Users/tuffy/Documents/IC/Graphs/united/Cnae_BC_groups.pdf", plot = p, device = "pdf", width = 10, height = 6, dpi = 300)

# ---------------------------------------------------------------------------- #
# <<< SAL >>> ----
# ---------------------------------------------------------------------------- #

#' This is a WORK IN PROGRESS. I am tryng to better understang how each salary
#' quartile individual reacts with the labor lawsuit initiaon.
#' 
#' I must add that the current code dividing the groups in each years is badly
#' written. I have, still, to correct the issue regarding the quartile division 
#' for each year in the dataset.

## 19.1 Data ----
data <- read.csv("C:/Users/tuffy/Documents/IC/Bases/base_atual_dum_v3.csv")


data <- data %>% 
  group_by(code_id) %>% 
  mutate(
    aux1 = which(!is.na(remuneracao_media_sm_) &
                   ano < year_first_treated)[1],
    aux2 = which(remuneracao_media_sm_ != 0 & !is.na(remuneracao_media_sm_) &
                   ano < year_first_treated)[1]
    
  ) %>% 
  arrange( code_id, ano) %>% 
  select(-X) %>%
  ungroup() 

data <- data %>%
  group_by(code_id) %>% 
  
  mutate(
    sm_first = remuneracao_media_sm_[row_number() == aux1],
    
    #Para cada ano
    sm_03 = remuneracao_media_sm_[ano == 2003],
    sm_04 = remuneracao_media_sm_[ano == 2004],
    sm_05 = remuneracao_media_sm_[ano == 2005],
    sm_06 = remuneracao_media_sm_[ano == 2006],
    sm_07 = remuneracao_media_sm_[ano == 2007],
    sm_08 = remuneracao_media_sm_[ano == 2008],
    sm_09 = remuneracao_media_sm_[ano == 2009],
    sm_10 = remuneracao_media_sm_[ano == 2010],
    sm_11 = remuneracao_media_sm_[ano == 2011],
    sm_12 = remuneracao_media_sm_[ano == 2012],
    sm_13 = remuneracao_media_sm_[ano == 2013]
  )

##19.2 Variable ----
summary(data %>% filter(ano == 2008) %>% select(sm_first))
#' 1 - 1.520
#' 2 - 2.040
#' 3 - 3.160

ini <- Sys.time()

data <- data %>% 
  #select(-c(aux1, aux2)) %>%
  group_by(code_id) %>% 
  mutate(
    # quartile =  case_when(
    #   sm_first < 1.520 ~ 1,
    #   sm_first >= 1.520 & sm_first < 2.040 ~ 2,
    #   sm_first >= 2.040 & sm_first < 3.160 ~ 3,
    #   sm_first > 3.160 ~ 4,
    #   TRUE ~ NA
    # ),
    quartile_first = cut(sm_first,
                         breaks = unique(quantile(data$sm_first, #Definindo os quartis
                                                  probs = c(0, .25, .5, .75, 1),
                                                  na.rm = TRUE)
                         ),
                         include.lowest = TRUE, labels = FALSE),
    
    #Para os anos...
    quartile_03 = cut(sm_03,
                      breaks = unique(quantile(data$sm_03, probs = c(0, .25, .5, .75, 1),na.rm = TRUE)),
                      include.lowest = TRUE, labels = FALSE)
    
  )

fim <- Sys.time()
delta <- difftime(fim, ini, units = "mins")
message(delta)


data <- data %>% 
  group_by(code_id) %>%
  mutate(
    quartile_04 = cut(sm_04,
                      breaks = unique(quantile(data$sm_04, probs = c(0, .25, .5, .75, 1),na.rm = TRUE)),
                      include.lowest = TRUE, labels = FALSE),
    quartile_05 = cut(sm_05,
                      breaks = unique(quantile(data$sm_05, probs = c(0, .25, .5, .75, 1),na.rm = TRUE)),
                      include.lowest = TRUE, labels = FALSE)
  )

fim <- Sys.time()
delta <- difftime(fim, ini, units = "mins")
message(delta)


data <- data %>% 
  group_by(code_id) %>%
  mutate(
    quartile_06 = cut(sm_06,
                      breaks = unique(quantile(data$sm_06, probs = c(0, .25, .5, .75, 1),na.rm = TRUE)),
                      include.lowest = TRUE, labels = FALSE),
    quartile_07 = cut(sm_07,
                      breaks = unique(quantile(data$sm_07, probs = c(0, .25, .5, .75, 1),na.rm = TRUE)),
                      include.lowest = TRUE, labels = FALSE),
    quartile_08 = cut(sm_08,
                      breaks = unique(quantile(data$sm_08, probs = c(0, .25, .5, .75, 1),na.rm = TRUE)),
                      include.lowest = TRUE, labels = FALSE)
  )
fim <- Sys.time()
delta <- difftime(fim, ini, units = "mins")
message(delta)


data <- data %>% 
  group_by(code_id) %>%
  mutate(
    quartile_09 = cut(sm_09,
                      breaks = unique(quantile(data$sm_09, probs = c(0, .25, .5, .75, 1),na.rm = TRUE)),
                      include.lowest = TRUE, labels = FALSE),
    quartile_10 = cut(sm_10,
                      breaks = unique(quantile(data$sm_10, probs = c(0, .25, .5, .75, 1),na.rm = TRUE)),
                      include.lowest = TRUE, labels = FALSE)
  )

fim <- Sys.time()
delta <- difftime(fim, ini, units = "mins")
message(delta)


data <- data %>% 
  group_by(code_id) %>%
  mutate(
    quartile_11 = cut(sm_11,
                      breaks = unique(quantile(data$sm_11, probs = c(0, .25, .5, .75, 1),na.rm = TRUE)),
                      include.lowest = TRUE, labels = FALSE),
    quartile_12 = cut(sm_12,
                      breaks = unique(quantile(data$sm_12, probs = c(0, .25, .5, .75, 1),na.rm = TRUE)),
                      include.lowest = TRUE, labels = FALSE),
    quartile_13 = cut(sm_13,
                      breaks = unique(quantile(data$sm_13, probs = c(0, .25, .5, .75, 1),na.rm = TRUE)),
                      include.lowest = TRUE, labels = FALSE)
  ) %>% 
  ungroup()




nrow(data %>% filter(ano == 2008, quartile == 1)) #190495
nrow(data %>% filter(ano == 2008, quartile == 2)) #191730
nrow(data %>% filter(ano == 2008, quartile == 3)) #192892
nrow(data %>% filter(ano == 2008, quartile == 4)) #191344



##19.3 Estimation ----

for( qrt in c(1,2,3,4)) {
  
  if(qrt == 1) {
    data_final <- data.frame()
  }
  
  
  temp <- data %>% 
    filter( quartile_first == qrt)
  
  
  #Estimation
  calsan_did <- did::att_gt(
    yname = "rais_",
    gname = "year_first_treated",
    idname = "code_id",
    tname = "ano",
    xformla = ~ code_id + ano_sexo + ano_branco + ano_ensino
    ,
    data = temp,
    control_group = "notyettreated",
    base_period = "universal",
    clustervars = "code_id"
  )
  
  est_calsan <- aggte( MP = calsan_did, type = "dynamic", na.rm = TRUE)
  #Result table
  print(est_calsan)
  # 
  plot_calsan <- ggdid(est_calsan) 
  
  # 
  plot_calsan
  # 
  #
  # Results dataframe 
  data_calsan <- ggplot_build(plot_calsan)$data[[1]]
  data_calsan$quart <- qrt
  data_final <- rbind(data_final,as.data.frame(data_calsan))
  
  rm(temp, calsan_did)
  
  
}


### 19.3.1. Graph ----


data_final <- data_final %>% 
  mutate(
    x = as.numeric(x),
    x = case_when(
      quart == 1 ~ x - 0.25,
      quart == 2 ~ x - 0.15,
      quart == 4 ~ x + 0.15,
      quart == 5 ~ x + 0.25,
      TRUE ~ x
    )
  ) 

data_final$quart <- as.factor(data_final$quart)

p <- ggplot(data_final, aes(x = x, y = y, color = quart, shape = quart, group = quart)) +
  geom_point( size = 3) +
  geom_errorbar(aes(ymin = ymin, ymax = ymax), width = 0.3) +
  geom_hline(yintercept = 0, color = "#D62728") +
  geom_vline(xintercept = -1, color = "#BEBEBE", linetype = "dashed") +
  scale_color_manual(
    values = c(
      #'black',
      '#0072B2', '#009E73'	, '#E69F00', "#CC79A7"),
    labels = c(
      #"Group 0",
      "1st Quartile",
      "2nd Quartile",
      "3rd Quartile",
      "4th Quartile"
    )
  ) +
  scale_shape_manual(
    values = c(
      #16,
      15, 17, 18, 16), # Circle, square, triangle, diamond
    labels = c(
      #"Group 0",
      "1st Quartile",
      "2nd Quartile",
      "3rd Quartile",
      "4th Quartile"
    )
  ) +  
  labs(x = "Years to treatment", y = '', colour = '', shape = '') +
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
  scale_x_continuous(limits = c(-9.5, 4.5),
                     breaks = c(-9,-8,-7,-6,-5,-4,-3,-2,-1,0,1,2,3,4),
                     labels = c('-9','-8','-7','-6','-5','-4','-3','-2','-1','0','+1','+2','+3','+4')) +
  scale_y_continuous(limits = c(-0.95, 0.155),
                     breaks = c(-0.90,-0.75,-0.60,-0.45,-0.30,-0.15,0,0.15),
                     labels = c('-0.90','-0.75','-0.60','-0.45','-0.30','-0.15','0','0.15'))
p

ggsave("C:/Users/tuffy/Documents/IC/Graphs/united/SM_quart.jpeg", plot = p, device = "jpeg", width = 10, height = 6, dpi = 600)
ggsave("C:/Users/tuffy/Documents/IC/Graphs/united/Sm_quart.pdf", plot = p, device = "pdf", width = 10, height = 6, dpi = 300)


