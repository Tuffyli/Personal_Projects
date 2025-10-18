# Teste nova Variável CBO -------------------------------------------------#

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


# ----------------------------------------------------------------------------#
# Filtrando trabalhadores que aparecem pelo menos uma vez após o tratamento na RAIS


    #Dummy para qualquer periodos pós-tratamento
base <- base %>% 
  group_by(code_id) %>%
  mutate(
    rais_aux = ifelse(any(rais_ == 1 & ano >= year_first_treated), 1, 0),
    max_aux = max(rais_aux, na.rm = TRUE)
  )


#Filtro para Presença 1 vez pós
base <- base %>% 
  filter(
    max_aux == 1
  ) %>% 
  select(
    -max_aux, -rais_aux
  )


# Código igual ao anterior ---------------------------------------------------#

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
         nvl_rem = log(remuneracao_media_sm_+1))
# %>% 
# #Removendo os valores das categorias CBO, não mais necessários
# select(-ocupacao_cbo2002_)



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
  #Aqui faremos com que os valores 0 não ativem a dummy
  
  mutate(
    #Dummy auxiliar - livre mudança após o tratamento
    dummy_cnae_aux =        
      case_when(
        #Aqui listamos os possíveis casos
        ano < year_first_treated & cnae_group == 0 ~ 1, 
        
        ano >= year_first_treated & cnae_group == 0 ~ 0,
        
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
  
  #Aqui faremos com que os valores 0 não ativem a dummy
  mutate(
    dummy_cbo_aux =
      case_when(
        
        ano < year_first_treated & cbo_group == 0 ~ 1, 
        
        ano >= year_first_treated & cbo_group == 0 ~ 0,
      
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


summary(base$dummy_cbo)

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


# -----------------------------------------------------------------------------#
# Regressões com as Novas Dummies
# -----------------------------------------------------------------------------#

#Estabelecendo a função

plot <- function(df,
                 plot_title,
                 var_y,
                 controles) {
  
  
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



# ---------------- #
# RAIS
# ---------------- #

if (!dir.exists("C:/Users/tuffy/Documents/IC/Graphs/Teste_new")) {
  dir.create("C:/Users/tuffy/Documents/IC/Graphs/Teste_new")
}

estimacoes_rais <- plot(data,
                        plot_title = '',
                        var_y = "rais_")



rais_p <- ggplot(estimacoes_rais, aes(x = x, y = y, color = colour, group = group)) +
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
  scale_y_continuous(limits = c(-0.95, 0.155),
                     breaks = c(-0.90,-0.75,-0.60,-0.45,-0.30,-0.15,0,0.15),
                     labels = c('-0.90','-0.75','-0.60','-0.45','-0.30','-0.15','0','0.15'))



ggsave("C:/Users/tuffy/Documents/IC/Graphs/Teste_new/teste2_plot_rais.jpeg", plot = rais_p, device = "jpeg", width = 10, height = 6, dpi = 600)






#------------------------#
# Para o CBO
#------------------------#
estimacoes_cbo <- plot(base,
                       plot_title = '',
                       var_y = "dummy_cbo")


cbo_p <- ggplot(estimacoes_cbo, aes(x = x, y = y, color = colour, group = group)) +
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
# Resultado
cbo_p

ggsave("C:/Users/tuffy/Documents/IC/Graphs/Teste_new/teste2_plot_cbo.jpeg", plot = cbo_p, device = "jpeg", width = 10, height = 6, dpi = 600)


# ---------------- #
# Cnae
# ---------------- #
estimacoes_cnae <- plot(base,
                        plot_title = '',
                        var_y = "dummy_cnae")


cnae_p <- ggplot(estimacoes_cnae, aes(x = x, y = y, color = colour, group = group)) +
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

cnae_p

ggsave("C:/Users/tuffy/Documents/IC/Graphs/Teste_new/teste2_plot_cnae.jpeg", plot = cnae_p, device = "jpeg", width = 10, height = 6, dpi = 600)




#------------------------------------------------------------------------------#
# Heterogeneidade: Blue vs. White
#------------------------------------------------------------------------------#


# Blue-collar -----------------------------------------------------------------#

blue_data <- data %>% 
  filter(
    white_dummy == 0
  )


# CBO -----#

# CNAE ----#





# White-collar ----------------------------------------------------------------#

white_data <- data %>% 
  filter(
    white_dummy == 1
  )



