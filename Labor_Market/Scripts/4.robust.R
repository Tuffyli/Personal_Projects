# --------------------------------------------------------------------------- #
#Criando a var de salario
# --------------------------------------------------------------------------- #
# Library ----
library(dplyr)
library(fixest)
library(ggplot2)
library(did)
library(stargazer)
library(gridExtra)
library(knitr)
library(grid)

# --------------------------------------------------------------------------- #
#Abertura da Base ----
# --------------------------------------------------------------------------- #


base <- read.csv("C:/Users/tuffy/Documents/IC/Bases/panel_workers_2003to2013_nova.csv")

#Estabelecendo o grupo de indivíduos que se encontrará na Rais em todos os períodos.
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
#1. Salario -------
# ---------------------------------------------------------------------------- #

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

# ---------------------------------------------------------------------------- #
#2. EST ----
# ---------------------------------------------------------------------------- #

##2.1 No FE ----
#### T0

#Estimação
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
#Tabela de resultados
print(est_calsan_t0)

plot_calsan_t0 <- ggdid(est_calsan_t0) +
  ggtitle("Event Study: Callaway & Sant'anna, ref T0") +
  labs("Tempo até o tratamento") +
  theme_minimal()

plot_calsan_t0


#### T2

#Estimação
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
#Tabela de resultados
print(est_calsan_t2)

plot_calsan_t2 <- ggdid(est_calsan_t2) +
  ggtitle("Event Study: Callaway & Sant'anna, ref T2 ") +
  labs("Tempo até o tratamento") +
  theme_minimal()

plot_calsan_t2


#### T MEAN

#Estimação
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
#Tabela de resultados
print(est_calsan_tmean)

plot_calsan_tmean <- ggdid(est_calsan_tmean) +
  ggtitle("Event Study: Callaway & Sant'anna, T Mean ") +
  labs("Tempo até o tratamento") +
  theme_minimal()

plot_calsan_tmean


#### New Spec ----


base2 <- base %>%
  group_by(code_id) %>%
  filter(all(remuneracao_media_sm_ != 0)) %>%
  mutate(
    sal_var = log(remuneracao_media_sm_)
  )


#### T0

#Estimação
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
#Tabela de resultados
print(est_calsan_t0)

plot_calsan_t0 <- ggdid(est_calsan_t0) +
  ggtitle("Event Study: Callaway & Sant'anna, ref T0") +
  labs("Tempo até o tratamento") +
  theme_minimal()

plot_calsan_t0


#### T2

#Estimação
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
#Tabela de resultados
print(est_calsan_t2)

plot_calsan_t2 <- ggdid(est_calsan_t2) +
  ggtitle("Event Study: Callaway & Sant'anna, ref T2 ") +
  labs("Tempo até o tratamento") +
  theme_minimal()

plot_calsan_t2


#### T MEAN

#Estimação
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
#Tabela de resultados
print(est_calsan_tmean)

plot_calsan_tmean <- ggdid(est_calsan_tmean) +
  ggtitle("Event Study: Callaway & Sant'anna, T Mean ") +
  labs("Tempo até o tratamento") +
  theme_minimal()

plot_calsan_tmean




##2.2 COM FE ----

#Estimação
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
#Tabela de resultados
print(est_calsan)

plot_calsan <- ggdid(est_calsan) +
  ggtitle("Event Study: Callaway & Sant'anna, CNAE + CBO ") +
  labs("Tempo até o tratamento") +
  theme_minimal()

plot_calsan




##2.3 -1 periodo ----

#Estimação
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
#Tabela de resultados
print(est_calsan)

plot_calsan <- ggdid(est_calsan) +
  ggtitle("Event Study: Callaway & Sant'anna, CNAE + CBO (-1) ") +
  labs("Tempo até o tratamento") +
  theme_minimal()

plot_calsan


##2.3 -2 periodo ----

#Estimação
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
#Tabela de resultados
print(est_calsan)

plot_calsan <- ggdid(est_calsan) +
  ggtitle("Event Study: Callaway & Sant'anna, CNAE + CBO (-2) ") +
  labs("Tempo até o tratamento") +
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
# 3. Sexo ----
# -----------------------------------------------------------------------------#


data <- read.csv("C:/Users/tuffy/Documents/IC/Bases/base_atual_dum_v3.csv")

data$cbo_ano <- as.numeric(interaction(data$cbo_group, data$ano))
data$cnae_ano <- as.numeric(interaction(data$cnae_group, data$ano))

#Filtrando a base pela dummy de sexo
base_mas <- data %>% 
  filter(sexo_dummy == 1)

base_fem <- data %>% 
  filter(sexo_dummy == 0 )

#depois subdividir em white e blue collar

## 3.1 Fem ----
#Estimação
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
#Tabela de resultados
print(est_calsan)

plot_calsan <- ggdid(est_calsan) +
  ggtitle("Event Study: Callaway & Sant'anna, CNAE + CBO ") +
  labs("Tempo até o tratamento") +
  theme_minimal()

plot_calsan

#Extraindo os resultados para o sexo feminino

data_calsan <- ggplot_build(plot_calsan)$data[[1]]
data_calsan <- as.data.frame(data_calsan)

## 3.2 Masc ----
#Estimação
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
#Tabela de resultados
print(est_calsan2)

plot_calsan2 <- ggdid(est_calsan2) +
  ggtitle("Event Study: Callaway & Sant'anna, CNAE + CBO ") +
  labs("Tempo até o tratamento") +
  theme_minimal()

plot_calsan2

#Extrauindo os resultados para o sexo feminino

data_calsan2 <- ggplot_build(plot_calsan2)$data[[1]]
data_calsan2 <- as.data.frame(data_calsan2)


##3.3 Unindo as bases  ----

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



##3.4 White colar ----

### 3.4.1 Fem ----

base_mas <- data %>% 
  filter(sexo_dummy == 1,
         white_dummy == 1) 
  

base_fem <- data %>% 
  filter(sexo_dummy == 0,
         white_dummy == 1)

#Estimação
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
#Tabela de resultados
print(est_calsan)

plot_calsan <- ggdid(est_calsan) +
  ggtitle("Event Study: Callaway & Sant'anna, CNAE + CBO ") +
  labs("Tempo até o tratamento") +
  theme_minimal()

plot_calsan

#Extraindo os resultados para o sexo feminino

data_calsan <- ggplot_build(plot_calsan)$data[[1]]
data_calsan <- as.data.frame(data_calsan)

### 3.4.2 Masc ----
#Estimação
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
#Tabela de resultados
print(est_calsan2)

plot_calsan2 <- ggdid(est_calsan2) +
  ggtitle("Event Study: Callaway & Sant'anna, CNAE + CBO ") +
  labs("Tempo até o tratamento") +
  theme_minimal()

plot_calsan2

#Extrauindo os resultados para o sexo feminino

data_calsan2 <- ggplot_build(plot_calsan2)$data[[1]]
data_calsan2 <- as.data.frame(data_calsan2)


### 3.4.3 Unindo as bases  ----

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



##3.5 Blue colar ----

### 3.5.1 Fem ----

base_mas <- data %>% 
  filter(sexo_dummy == 1,
         white_dummy == 0) 


base_fem <- data %>% 
  filter(sexo_dummy == 0,
         white_dummy == 0)

#Estimação
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
#Tabela de resultados
print(est_calsan)

plot_calsan <- ggdid(est_calsan) +
  ggtitle("Event Study: Callaway & Sant'anna, CNAE + CBO ") +
  labs("Tempo até o tratamento") +
  theme_minimal()

plot_calsan

#Extraindo os resultados para o sexo feminino

data_calsan <- ggplot_build(plot_calsan)$data[[1]]
data_calsan <- as.data.frame(data_calsan)

### 3.5.2 Masc ----
#Estimação
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
#Tabela de resultados
print(est_calsan2)

plot_calsan2 <- ggdid(est_calsan2) +
  ggtitle("Event Study: Callaway & Sant'anna, CNAE + CBO ") +
  labs("Tempo até o tratamento") +
  theme_minimal()

plot_calsan2

#Extrauindo os resultados para o sexo feminino

data_calsan2 <- ggplot_build(plot_calsan2)$data[[1]]
data_calsan2 <- as.data.frame(data_calsan2)


### 3.5.3 Unindo as bases  ----

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
# 4. Cor/Raca ----
# -----------------------------------------------------------------------------#

#Filtrando a base pela dummy de sexo
base_bra <- data %>% 
  filter(branco_dummy == 1)

base_nbr <- data %>% 
  filter(branco_dummy == 0 )

#depois subdividir em white e blue collar

## 4.1 Brancos----
#Estimação
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
#Tabela de resultados
print(est_calsan)

plot_calsan <- ggdid(est_calsan) +
  ggtitle("Event Study: Callaway & Sant'anna, CNAE + CBO ") +
  labs("Tempo até o tratamento") +
  theme_minimal()

plot_calsan

#Extraindo os resultados para o sexo feminino

data_calsan <- ggplot_build(plot_calsan)$data[[1]]
data_calsan <- as.data.frame(data_calsan)

## 4.2 Non-Branco ----
#Estimação
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
#Tabela de resultados
print(est_calsan2)

plot_calsan2 <- ggdid(est_calsan2) +
  ggtitle("Event Study: Callaway & Sant'anna, CNAE + CBO ") +
  labs("Tempo até o tratamento") +
  theme_minimal()

plot_calsan2

#Extrauindo os resultados para o sexo feminino

data_calsan2 <- ggplot_build(plot_calsan2)$data[[1]]
data_calsan2 <- as.data.frame(data_calsan2)


##4.3 Unindo as bases  ----

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



##4.4 White colar ----

### 4.4.1 Branco ----

base_bra <- data %>% 
  filter(branco_dummy == 1,
         white_dummy == 1) 


base_nbr <- data %>% 
  filter(branco_dummy == 0,
         white_dummy == 1)

#Estimação
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
#Tabela de resultados
print(est_calsan)

plot_calsan <- ggdid(est_calsan) +
  ggtitle("Event Study: Callaway & Sant'anna, CNAE + CBO ") +
  labs("Tempo até o tratamento") +
  theme_minimal()

plot_calsan

#Extraindo os resultados para o sexo feminino

data_calsan <- ggplot_build(plot_calsan)$data[[1]]
data_calsan <- as.data.frame(data_calsan)

### 4.4.2 Non-Branco ----
#Estimação
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
#Tabela de resultados
print(est_calsan2)

plot_calsan2 <- ggdid(est_calsan2) +
  ggtitle("Event Study: Callaway & Sant'anna, CNAE + CBO ") +
  labs("Tempo até o tratamento") +
  theme_minimal()

plot_calsan2

#Extrauindo os resultados para o sexo feminino

data_calsan2 <- ggplot_build(plot_calsan2)$data[[1]]
data_calsan2 <- as.data.frame(data_calsan2)


### 4.4.3 Unindo as bases  ----

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



##4.5 Blue colar ----

### 4.5.1 Branco ----

base_bra <- data %>% 
  filter(branco_dummy == 1,
         white_dummy == 0) 


base_nbr <- data %>% 
  filter(branco_dummy == 0,
         white_dummy == 0)

#Estimação
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
#Tabela de resultados
print(est_calsan)

plot_calsan <- ggdid(est_calsan) +
  ggtitle("Event Study: Callaway & Sant'anna, CNAE + CBO ") +
  labs("Tempo até o tratamento") +
  theme_minimal()

plot_calsan

#Extraindo os resultados para o sexo feminino

data_calsan <- ggplot_build(plot_calsan)$data[[1]]
data_calsan <- as.data.frame(data_calsan)

### 4.5.2 Non ----
#Estimação
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
#Tabela de resultados
print(est_calsan2)

plot_calsan2 <- ggdid(est_calsan2) +
  ggtitle("Event Study: Callaway & Sant'anna, CNAE + CBO ") +
  labs("Tempo até o tratamento") +
  theme_minimal()

plot_calsan2

#Extrauindo os resultados para o sexo feminino

data_calsan2 <- ggplot_build(plot_calsan2)$data[[1]]
data_calsan2 <- as.data.frame(data_calsan2)


### 4.5.3 Unindo as bases  ----

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
# 5. Estimadores ----
# -----------------------------------------------------------------------------#

rm(base)

# install.packages("didimputation")
# install.packages("DIDmultiplegt")




library(didimputation)
library(DIDmultiplegt)



data <- read.csv("C:/Users/tuffy/Documents/IC/Bases/base_atual_dum_v3.csv")



## 5.1 CalSan & SunAb ----

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
#     labs("Tempo até o tratamento") +
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

## 5.2 Borusyak ----

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

## 5.3 De ChaiseMartin ----

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


### VERSÃO ANTIGA DO DATA.TABLE
# install.packages("pkgbuild")
# pkgbuild::has_rtools()
# 
# install.packages("remotes")
# remotes::install_version("data.table", version = "1.16.4", repos = "http://cran.us.r-project.org")

#ESTIMEI NA FEA-USP (03/////)
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
#6. Gráfico ----
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
    values = c(16, 15, 17, 18), # Círculo, quadrado, triângulo, diamante
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
# 7. Nova var de CBO ----
# ---------------------------------------------------------------------------- #

data <- read.csv("C:/Users/tuffy/Documents/IC/Bases/base_atual_dum_v3.csv")

data <- data %>% 
  filter(all_in_rais == 1)

data <- data %>% 
  group_by(code_id) %>% 
  mutate(
    first_cnae = cnae_group[ano == 2003]
  ) %>% 
  arrange( code_id, ano) %>% 
  select(-X) %>% 
  ungroup()

####7.1 CNAE ------ 
summary(data$cnae_group)

#Passos para  a criação da dummy
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
    #Dummy auxiliar - livre mudança após o tratamento
    dummy_cnae_aux =        
      case_when(
        #Aqui listamos os possíveis casos
        ano < year_first_treated & cnae_group != first_cnae ~ 1, 
        
        #ano >= year_first_treated & cnae_group == 0 ~ 1,
        
        ano >= year_first_treated & cnae_group != 0 & !(cnae_group %in% cnae_group[ano < year_first_treated]) ~ 1,
        
        TRUE ~ 0
      ),
    #Dummy final
    first_one = which(dummy_cnae_aux == 1 & ano >= year_first_treated)[1], #Primeira linha post-treat
    
    dummy_cnae_all = ifelse(
      row_number() >= first_one & !is.na(first_one), 1, dummy_cnae_aux
    ),
    
    dummy_cnae_all = ifelse(
      is.na(first_one) & ano >= year_first_treated, 0 , dummy_cnae_all #Essa condição lida com os casos em que não há first_one
    ),
    
    new_dummy_cnae = ifelse(
      cnae_group != first_cnae, 1, 0
    )
  )

treat <- treat %>% 
  select(
    code_id, ano, dummy_cnae_all, new_dummy_cnae #selecionando apenas as colunas suficientes para realizar o merge
  )

#Unindo as os dataframes
data <- data %>% 
  left_join(treat, by= c("code_id", "ano")) %>% 
  select(-X.1)

#### 7.2 Est 2 ----


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
#Tabela de resultados
print(est_calsan)

plot_calsan <- ggdid(est_calsan) +
  ggtitle("Event Study: CNAE - all_in_rais ") +
  labs("Tempo até o tratamento") +
  theme_minimal()

plot_calsan


ggsave("C:/Users/tuffy/Documents/IC/Graphs/plot_test_cnae.jpeg", plot = plot_calsan, device = "jpeg", width = 10, height = 6, dpi = 600)

# --------------------------------------------------------------------------- #


# ---------------------------------------------------------------------------- #
# 8. Novíssima var CNAE ----
# ---------------------------------------------------------------------------- #

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
    
    
    first_one = which(cnae_dummy_v2 == 1 & ano >= year_first_treated)[1], #Primeira linha post-treat
    
    dummy_cnae_all_v2 = ifelse(
      row_number() >= first_one & !is.na(first_one), 1, cnae_dummy_v2
    ),
    
    
    #Levando em conta inf
    cnae_dummy_v3 = ifelse(cnae_group != cnae_aux, 1, 0),
    
    cnae_dummy_v4 = ifelse(cnae_group != cnae_aux, 1, 0),
    
    
    
    first_one2 = which(cnae_dummy_v3 == 1 & ano >= year_first_treated)[1], #Primeira linha post-treat
    
    dummy_cnae_all_v3 = ifelse(
      row_number() >= first_one2 & !is.na(first_one2), 1, cnae_dummy_v3
    ),
  )

treat <- treat %>% 
  select(
    code_id, ano, dummy_cnae_all_v2, dummy_cnae_all_v3, cnae_dummy_v4 #selecionando apenas as colunas suficientes para realizar o merge
  )

#Unindo as os dataframes
data <- data %>% 
  left_join(treat, by= c("code_id", "ano")) %>% 
  select(-X.1)

#### 8.2 Est 2 ----


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
#Tabela de resultados
print(est_calsan)

plot_calsan <- ggdid(est_calsan) +
  ggtitle("Event Study: CNAE - Mudança CNAE ") +
  labs("Tempo até o tratamento") +
  theme_minimal()

plot_calsan
ggsave("C:/Users/tuffy/Documents/IC/Graphs/plot_test2_cnae.jpeg", plot = plot_calsan, device = "jpeg", width = 10, height = 6, dpi = 600)


#### 8.2 Est 4*** ----

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
#Tabela de resultados
print(est_calsan)

plot_calsan <- ggdid(est_calsan) +
  ggtitle("Event Study: CNAE - Mudança  v2 ") +
  labs("Tempo até o tratamento") +
  theme_minimal()

plot_calsan


ggsave("C:/Users/tuffy/Documents/IC/Graphs/plot_test5_cnae.jpeg", plot = plot_calsan, device = "jpeg", width = 10, height = 6, dpi = 600)

rm(treat)


#------------------------------------------------------------------------------#
##8.3 Honest DID ----
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

### 8.3.1 Fnc ----


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


### 8.3.2 Honest did -----



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



#####Rais----
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
# 9. RAIS TWFE ----
# ---------------------------------------------------------------------------- #

colnames(data)


twfe <- feols(cnae_dummy_v4 ~ i(time_to_treat, ref = -1) | code_id + ano_sexo + ano_branco + ano_ensino,
              data = data,
              cluster = ~ code_id)

iplot(twfe)


# **** CBO **** ----


# 10. CBO----
# Olhando na RAIS dentro dos grupos CBO
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

## 10.1 Bases ----

### 10.1.1 WC ----
data_wc <- data %>% 
  filter(white_dummy == 1 &
           cbo_first %in% c(1:5)) #Nesta condição removemos os que não começam WC

summary(data_wc$cbo_group)
summary(data_wc$cbo_first)

### 10.1.2 BC ------

data_bc <- data %>% 
  filter(white_dummy == 0 &
           cbo_first %in% c(6:10)) #Nesta condição removemos os que não começam WC

summary(data_bc$cbo_group)
summary(data_bc$cbo_first)


## 10.2 Reg ----
### 10.2.1 WC ----

data_final <- data.frame()

#WC
for ( wc in c(1:5)) {
  
  
  temp <- data_wc %>% 
    filter( cbo_first == wc)

  
  #Estimação
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
  #Tabela de resultados
   print(est_calsan)
  # 
   plot_calsan <- ggdid(est_calsan) 

  # 
   plot_calsan
  # 
  # #Extraindo os resultados para o sexo feminino
  # 
   data_calsan <- ggplot_build(plot_calsan)$data[[1]]
   data_calsan$cbo <- wc
   data_final <- rbind(data_final,as.data.frame(data_calsan))
  
  rm(temp, calsan_did)
  
}

### 10.2.2. Graph ----


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
      15, 17, 18, 16), # Círculo, quadrado, triângulo, diamante
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

### 10.2.1 BC ----

data_final <- data.frame()

#BC
for ( wc in c(6,8,9,10)) {
  
  
  temp <- data_bc %>% 
    filter( cbo_first == wc)
  
  
  #Estimação
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
  #Tabela de resultados
  print(est_calsan)
  # 
  plot_calsan <- ggdid(est_calsan) 
  
  # 
  plot_calsan
  # 
  # #Extraindo os resultados para o sexo feminino
  # 
  data_calsan <- ggplot_build(plot_calsan)$data[[1]]
  data_calsan$cbo <- wc
  data_final <- rbind(data_final,as.data.frame(data_calsan))
  
  rm(temp, calsan_did)
  
}

### 10.2.2. Graph ----


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
               ), # Círculo, quadrado, triângulo, diamante
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
 

# **** CNAE **** ----


# 11. CNAE----
# Olhando na RAIS dentro dos grupos CNAE
data <- read.csv("C:/Users/tuffy/Documents/IC/Bases/base_atual_dum_v3.csv")


data <- data %>% 
  group_by(code_id) %>% 
  mutate(
    aux1 = which(cnae_group == 0 & ano < year_first_treated)[1],
    aux2 = which(cnae_group != 0 & ano < year_first_treated)[1]
    
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

## 11.1 Bases ----

### 11.1.1 WC ----
data_wc <- data %>% 
  filter(white_dummy == 1) #Nesta condição removemos os que não começam WC

summary(data_wc$cnae_group)
summary(data_wc$cnae_first)

### 11.1.2 BC ------

data_bc <- data %>% 
  filter(white_dummy == 0) #Nesta condição removemos os que não começam WC

summary(data_bc$cnae_group)
summary(data_bc$cnae_first)


## 11.2 WC ----
### 11.2.1 REG----

data_final <- data.frame()

#WC
for ( wc in c(1:18)) {
  
  
  temp <- data_wc %>% 
    filter( cnae_first == wc)
  
  
  #Estimação
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
  #Tabela de resultados
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

### 11.2.2. Graph ----


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

## 11.3.1 BC ----

### 11.3.1 REG ----


#BC
for ( wc in c(1:7,9:16,18)) {
  message("Rodando para: ", wc," ...")
  
  if( wc == 1) {
    data_final <- data.frame()
  }
  
  temp <- data_bc %>% 
    filter( cnae_first == wc)
  
  
  #Estimação
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
  #Tabela de resultados
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

### 11.3.2. Graph ----


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

# **** SAL **** ----
## 12.1 Dados ----
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

##12.2 Variable ----
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



# REG ----

for( qrt in c(1,2,3,4)) {
  
  if(qrt == 1) {
    data_final <- data.frame()
  }
  
  
  temp <- data %>% 
    filter( quartile_first == qrt)
  
  
  #Estimação
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
  #Tabela de resultados
  print(est_calsan)
  # 
  plot_calsan <- ggdid(est_calsan) 
  
  # 
  plot_calsan
  # 
  # #Extraindo os resultados para o sexo feminino
  # 
  data_calsan <- ggplot_build(plot_calsan)$data[[1]]
  data_calsan$quart <- qrt
  data_final <- rbind(data_final,as.data.frame(data_calsan))
  
  rm(temp, calsan_did)
  
  
}


### 10.2.2. Graph ----


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
      15, 17, 18, 16), # Círculo, quadrado, triângulo, diamante
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

