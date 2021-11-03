library(ggplot2)
library(tidyverse)


setwd("C:\\Users\\luana\\OneDrive\\Documentos\\Monografia")


########## afazeres por sexo

# 2019
afazeres_homens_2019 = readRDS(file = "tmp/horas_afazeres_homens_2019.rds") %>% 
  mutate(id = "Homens", ano = 2019, tipo = "afazeres")
  
afazeres_mulheres_2019 = readRDS(file = "tmp/horas_afazeres_mulheres_2019.rds") %>% 
  mutate(id = "Mulheres", ano = 2019, tipo = "afazeres")

# 2018
afazeres_homens_2018 = readRDS(file = "tmp/horas_afazeres_homens_2018.rds") %>% 
  mutate(id = "Homens", ano = 2018, tipo = "afazeres")

afazeres_mulheres_2018 = readRDS(file = "tmp/horas_afazeres_mulheres_2018.rds") %>% 
  mutate(id = "Mulheres", ano = 2018, tipo = "afazeres")

# 2017
afazeres_homens_2017 = readRDS(file = "tmp/horas_afazeres_homens_2017.rds") %>% 
  mutate(id = "Homens", ano = 2017, tipo = "afazeres")

afazeres_mulheres_2017 = readRDS(file = "tmp/horas_afazeres_mulheres_2017.rds") %>% 
  mutate(id = "Mulheres", ano = 2017, tipo = "afazeres")

# 2016
afazeres_homens_2016 = readRDS(file = "tmp/horas_afazeres_homens_2016.rds") %>% 
  mutate(id = "Homens", ano = 2016, tipo = "afazeres")

afazeres_mulheres_2016 = readRDS(file = "tmp/horas_afazeres_mulheres_2016.rds") %>% 
  mutate(id = "Mulheres", ano = 2016, tipo = "afazeres")


########## trabalho por sexo

# 2019
trabalho_homens_2019 = readRDS(file = "tmp/horas_trabalho_homens_2019.rds") %>% 
  mutate(id = "Homens", ano = 2019, tipo = "trabalho")

trabalho_mulheres_2019 = readRDS(file = "tmp/horas_trabalho_mulheres_2019.rds") %>% 
  mutate(id = "Mulheres", ano = 2019, tipo = "trabalho")

# 2018
trabalho_homens_2018 = readRDS(file = "tmp/horas_trabalho_homens_2018.rds") %>% 
  mutate(id = "Homens", ano = 2018, tipo = "trabalho")

trabalho_mulheres_2018 = readRDS(file = "tmp/horas_trabalho_mulheres_2018.rds") %>% 
  mutate(id = "Mulheres", ano = 2018, tipo = "trabalho")

# 2017
trabalho_homens_2017 = readRDS(file = "tmp/horas_trabalho_homens_2017.rds") %>% 
  mutate(id = "Homens", ano = 2017, tipo = "trabalho")

trabalho_mulheres_2017 = readRDS(file = "tmp/horas_trabalho_mulheres_2017.rds") %>% 
  mutate(id = "Mulheres", ano = 2017, tipo = "trabalho")

# 2016
trabalho_homens_2016 = readRDS(file = "tmp/horas_trabalho_homens_2016.rds") %>% 
  mutate(id = "Homens", ano = 2016, tipo = "trabalho")

trabalho_mulheres_2016 = readRDS(file = "tmp/horas_trabalho_mulheres_2016.rds") %>% 
  mutate(id = "Mulheres", ano = 2016, tipo = "trabalho")


################################## grÃ¡fico de alocaÃ§Ã£o de horas, comparando homens e mulheres


base1 = bind_rows(afazeres_homens_2019, afazeres_mulheres_2019,
             afazeres_homens_2018, afazeres_mulheres_2018,
             afazeres_homens_2017, afazeres_mulheres_2017,
             afazeres_homens_2016, afazeres_mulheres_2016) %>%
  group_by(id, ano, tipo) %>% 
  summarise(xxx = weighted.mean(media_horas_afazeres, pessoas_afazeres))


base2 = bind_rows(trabalho_homens_2019, trabalho_mulheres_2019,
                 trabalho_homens_2018, trabalho_mulheres_2018,
                 trabalho_homens_2017, trabalho_mulheres_2017,
                 trabalho_homens_2016, trabalho_mulheres_2016) %>% 
  group_by(id, ano, tipo) %>% 
  summarise(xxx = weighted.mean(media_horas_trabalho, pessoas_ocupadas))


base = rbind(base1, base2)

  ggplot(base, aes(x = id, y = xxx, fill = tipo)) +
    geom_bar(stat = 'identity', position = 'stack', width = 0.8, ) +
    labs(title = "Média da alocação de horas semanais", 
         subtitle = "Entre homens e mulheres", x = " ", y = "Horas", fill = " ") +
    facet_wrap(~ano, ncol = 4, strip.position = 'bottom') +
    theme(strip.background = element_blank(),
          strip.placement = "outside",
          panel.background = element_rect(fill = "white"),
          axis.line = element_line(colour = "black"),
          panel.grid = element_line(colour = "grey90"),
          legend.key = element_blank(),
          plot.subtitle = element_text(size = 13),
          plot.title = element_text(size = 16),
          text = element_text(size = 16),
          axis.text.x = element_text(size = 10)) +
    scale_fill_manual(values = c("grey85","grey50"),
                      labels = c("afazeres" = "Cuidados e \n afazeres domésticos",
                                 "trabalho" = "Trabalho \n remunerado"))
  
  
  
########## todos os anos


  base_anos = base %>% group_by(tipo, id) %>% summarise(xxx = mean(xxx))
  
  ggplot(base_anos, aes(x = id, y = xxx, fill = tipo)) +
    geom_bar(stat = 'identity', position = 'stack', width = 0.6, ) +
    labs(title = "Média da alocação de horas semanais", 
         subtitle = "Entre homens e mulheres de 2016 a 2019" ,x = " ", y = "Horas", fill = " ") +
    theme(strip.background = element_blank(),
          strip.placement = "outside",
          panel.background = element_rect(fill = "white"),
          axis.line = element_line(colour = "black"),
          panel.grid = element_line(colour = "grey90"),
          legend.key = element_blank(),
          plot.subtitle = element_text(size = 13),
          plot.title = element_text(size = 16),
          text = element_text(size = 16),
          axis.text.x = element_text(size = 10)) +
    scale_fill_manual(values = c("grey85","grey50"),
                      labels = c("afazeres" = "Cuidados e \n afazeres domésticos",
                                 "trabalho" = "Trabalho \n remunerado"))
  
  
  

  

  
  
#################################################################
########################## adicionando trabalho formal e informal
#################################################################  

  
    
############ afazeres em trabalho informal por sexo
  
  
  # 2019
  afazeres_informal_homens_2019 = readRDS(file = "tmp/horas_afazeres_homens_informal_2019.rds") %>% 
    mutate(id = "Homens", ano = 2019, tipo = "afazeres", informalidade = "informal")
  
  afazeres_informal_mulheres_2019 = readRDS(file = "tmp/horas_afazeres_mulheres_informal_2019.rds") %>% 
    mutate(id = "Mulheres", ano = 2019, tipo = "afazeres", informalidade = "informal")
  
  # 2018
  afazeres_informal_homens_2018 = readRDS(file = "tmp/horas_afazeres_homens_informal_2018.rds") %>% 
    mutate(id = "Homens", ano = 2018, tipo = "afazeres", informalidade = "informal")
  
  afazeres_informal_mulheres_2018 = readRDS(file = "tmp/horas_afazeres_mulheres_informal_2018.rds") %>% 
    mutate(id = "Mulheres", ano = 2018, tipo = "afazeres", informalidade = "informal")
  
  # 2017
  afazeres_informal_homens_2017 = readRDS(file = "tmp/horas_afazeres_homens_informal_2017.rds") %>% 
    mutate(id = "Homens", ano = 2017, tipo = "afazeres", informalidade = "informal")
  
  afazeres_informal_mulheres_2017 = readRDS(file = "tmp/horas_afazeres_mulheres_informal_2017.rds") %>% 
    mutate(id = "Mulheres", ano = 2017, tipo = "afazeres", informalidade = "informal")
  
  # 2016
  afazeres_informal_homens_2016 = readRDS(file = "tmp/horas_afazeres_homens_informal_2016.rds") %>% 
    mutate(id = "Homens", ano = 2016, tipo = "afazeres", informalidade = "informal")
  
  afazeres_informal_mulheres_2016 = readRDS(file = "tmp/horas_afazeres_mulheres_informal_2016.rds") %>% 
    mutate(id = "Mulheres", ano = 2016, tipo = "afazeres", informalidade = "informal")
  
  
############ afazeres em trabalho formal por sexo

  
  # 2019
  afazeres_formal_homens_2019 = readRDS(file = "tmp/horas_afazeres_homens_formal_2019.rds") %>% 
    mutate(id = "Homens", ano = 2019, tipo = "afazeres", informalidade = "formal")
  
  afazeres_formal_mulheres_2019 = readRDS(file = "tmp/horas_afazeres_mulheres_formal_2019.rds") %>% 
    mutate(id = "Mulheres", ano = 2019, tipo = "afazeres", informalidade = "formal")
  
  # 2018
  afazeres_formal_homens_2018 = readRDS(file = "tmp/horas_afazeres_homens_formal_2018.rds") %>% 
    mutate(id = "Homens", ano = 2018, tipo = "afazeres", informalidade = "formal")
  
  afazeres_formal_mulheres_2018 = readRDS(file = "tmp/horas_afazeres_mulheres_formal_2018.rds") %>% 
    mutate(id = "Mulheres", ano = 2018, tipo = "afazeres", informalidade = "formal")
  
  # 2017
  afazeres_formal_homens_2017 = readRDS(file = "tmp/horas_afazeres_homens_formal_2017.rds") %>% 
    mutate(id = "Homens", ano = 2017, tipo = "afazeres", informalidade = "formal")
  
  afazeres_formal_mulheres_2017 = readRDS(file = "tmp/horas_afazeres_mulheres_formal_2017.rds") %>% 
    mutate(id = "Mulheres", ano = 2017, tipo = "afazeres", informalidade = "formal")
  
  # 2016
  afazeres_formal_homens_2016 = readRDS(file = "tmp/horas_afazeres_homens_formal_2016.rds") %>% 
    mutate(id = "Homens", ano = 2016, tipo = "afazeres", informalidade = "formal")
  
  afazeres_formal_mulheres_2016 = readRDS(file = "tmp/horas_afazeres_mulheres_formal_2016.rds") %>% 
    mutate(id = "Mulheres", ano = 2016, tipo = "afazeres", informalidade = "formal")
  
  
############ trabalho em trabalho formal por sexo

  # 2019
  trabalho_informal_homens_2019 = readRDS(file = "tmp/horas_trabalho_homens_informal_2019.rds") %>% 
    mutate(id = "Homens", ano = 2019, tipo = "trabalho", informalidade = "informal")
  
  trabalho_informal_mulheres_2019 = readRDS(file = "tmp/horas_trabalho_mulheres_informal_2019.rds") %>% 
    mutate(id = "Mulheres", ano = 2019, tipo = "trabalho", informalidade = "informal")
  
  # 2018
  trabalho_informal_homens_2018 = readRDS(file = "tmp/horas_trabalho_homens_informal_2018.rds") %>% 
    mutate(id = "Homens", ano = 2018, tipo = "trabalho", informalidade = "informal")
  
  trabalho_informal_mulheres_2018 = readRDS(file = "tmp/horas_trabalho_mulheres_informal_2018.rds") %>% 
    mutate(id = "Mulheres", ano = 2018, tipo = "trabalho", informalidade = "informal")
  
  # 2017
  trabalho_informal_homens_2017 = readRDS(file = "tmp/horas_trabalho_homens_informal_2017.rds") %>% 
    mutate(id = "Homens", ano = 2017, tipo = "trabalho", informalidade = "informal")
  
  trabalho_informal_mulheres_2017 = readRDS(file = "tmp/horas_trabalho_mulheres_informal_2017.rds") %>% 
    mutate(id = "Mulheres", ano = 2017, tipo = "trabalho", informalidade = "informal")
  
  # 2016
  trabalho_informal_homens_2016 = readRDS(file = "tmp/horas_trabalho_homens_informal_2016.rds") %>% 
    mutate(id = "Homens", ano = 2016, tipo = "trabalho", informalidade = "informal")
  
  trabalho_informal_mulheres_2016 = readRDS(file = "tmp/horas_trabalho_mulheres_informal_2016.rds") %>% 
    mutate(id = "Mulheres", ano = 2016, tipo = "trabalho", informalidade = "informal")
  
  
  
############ trabalho em trabalho formal por sexo
  
  
  # 2019
  trabalho_formal_homens_2019 = readRDS(file = "tmp/horas_trabalho_homens_formal_2019.rds") %>% 
    mutate(id = "Homens", ano = 2019, tipo = "trabalho", informalidade = "formal")
  
  trabalho_formal_mulheres_2019 = readRDS(file = "tmp/horas_trabalho_mulheres_formal_2019.rds") %>% 
    mutate(id = "Mulheres", ano = 2019, tipo = "trabalho", informalidade = "formal")
  
  # 2018
  trabalho_formal_homens_2018 = readRDS(file = "tmp/horas_trabalho_homens_formal_2018.rds") %>% 
    mutate(id = "Homens", ano = 2018, tipo = "trabalho", informalidade = "formal")
  
  trabalho_formal_mulheres_2018 = readRDS(file = "tmp/horas_trabalho_mulheres_formal_2018.rds") %>% 
    mutate(id = "Mulheres", ano = 2018, tipo = "trabalho", informalidade = "formal")
  
  # 2017
  trabalho_formal_homens_2017= readRDS(file = "tmp/horas_trabalho_homens_formal_2017.rds") %>% 
    mutate(id = "Homens", ano = 2017, tipo = "trabalho", informalidade = "formal")
  
  trabalho_formal_mulheres_2017 = readRDS(file = "tmp/horas_trabalho_mulheres_formal_2017.rds") %>% 
    mutate(id = "Mulheres", ano = 2017, tipo = "trabalho", informalidade = "formal")
  
  # 2016
  trabalho_formal_homens_2016 = readRDS(file = "tmp/horas_trabalho_homens_formal_2016.rds") %>% 
    mutate(id = "Homens", ano = 2016, tipo = "trabalho", informalidade = "formal")
  
  trabalho_formal_mulheres_2016 = readRDS(file = "tmp/horas_trabalho_mulheres_formal_2016.rds") %>% 
    mutate(id = "Mulheres", ano = 2016, tipo = "trabalho", informalidade = "formal")

  
  
####################################################################################################
#################################### gráfico de alocação de horas, comparando formais e informais 
####################################################################################################
  
  
  base1 = bind_rows(afazeres_informal_homens_2019, afazeres_informal_mulheres_2019,
                    afazeres_informal_homens_2018, afazeres_informal_mulheres_2018,
                    afazeres_informal_homens_2017, afazeres_informal_mulheres_2017,
                    afazeres_informal_homens_2016, afazeres_informal_mulheres_2016,
                    afazeres_formal_homens_2019, afazeres_formal_mulheres_2019,
                    afazeres_formal_homens_2018, afazeres_formal_mulheres_2018,
                    afazeres_formal_homens_2017, afazeres_formal_mulheres_2017,
                    afazeres_formal_homens_2016, afazeres_formal_mulheres_2016) %>%
    group_by(id, ano, tipo, informalidade) %>% 
    summarise(xxx = weighted.mean(media_horas_afazeres, pessoas_afazeres))
  
  
  base2 = bind_rows(trabalho_informal_homens_2019, trabalho_informal_mulheres_2019,
                    trabalho_informal_homens_2018, trabalho_informal_mulheres_2018,
                    trabalho_informal_homens_2017, trabalho_informal_mulheres_2017,
                    trabalho_informal_homens_2016, trabalho_informal_mulheres_2016,
                    trabalho_formal_homens_2019, trabalho_formal_mulheres_2019,
                    trabalho_formal_homens_2018, trabalho_formal_mulheres_2018,
                    trabalho_formal_homens_2017, trabalho_formal_mulheres_2017,
                    trabalho_formal_homens_2016, trabalho_formal_mulheres_2016) %>% 
    group_by(id, ano, tipo, informalidade) %>% 
    summarise(xxx = weighted.mean(media_horas_trabalho, pessoas_ocupadas))
  
  
  base = rbind(base1, base2)
  
  ggplot(base, aes(x = informalidade, y = xxx, fill = tipo)) +
    geom_bar(stat = 'identity', position = 'stack', width = 0.8, ) +
    labs(title = "Média da alocação de horas semanais", 
         subtitle = "Entre mercado formal e informal",x = " ", y = "Horas", fill = " ") +
    facet_wrap(~ano, ncol = 4, strip.position = 'bottom') +
    theme(strip.background = element_blank(),
          strip.placement = "outside",
          panel.background = element_rect(fill = "white"),
          axis.line = element_line(colour = "black"),
          panel.grid = element_line(colour = "grey90"),
          legend.key = element_blank(),
          plot.subtitle = element_text(size = 13),
          plot.title = element_text(size = 16),
          text = element_text(size = 16),
          axis.text.x = element_text(size = 10)) +
    scale_fill_manual(values = c("grey85","grey50"),
                      labels = c("afazeres" = "Cuidados e \n afazeres domésticos",
                                 "trabalho" = "Trabalho \n remunerado"))
  
  
####################################################################################################
#################################### gráfico de alocação de horas, comparando formais e informais ##
#################################################### homens#########################################
####################################################################################################

  
  base_homens = base %>% filter(id == "Homens")   
  
  ggplot(base_homens, aes(x = informalidade, y = xxx, fill = tipo)) +
    geom_bar(stat = 'identity', position = 'stack', width = 0.8, ) +
    labs(title = "Média da alocação de horas semanais por mercado formal e informal", 
         subtitle = "Homens", x = " ", y = "Horas", fill = " ") +
    facet_wrap(~ano, ncol = 4, strip.position = 'bottom') +
    theme(strip.background = element_blank(),
          strip.placement = "outside",
          panel.background = element_rect(fill = "white"),
          axis.line = element_line(colour = "black"),
          panel.grid = element_line(colour = "grey90"),
          legend.key = element_blank(),
          plot.subtitle = element_text(size = 13),
          plot.title = element_text(size = 16),
          text = element_text(size = 16),
          axis.text.x = element_text(size = 10)) +
    scale_fill_manual(values = c("grey85","grey50"),
                      labels = c("afazeres" = "Cuidados e \n afazeres domésticos",
                                 "trabalho" = "Trabalho \n remunerado")) 
####### mulher
  
  base_mulheres = base %>% filter(id == "Mulheres")   
  
  ggplot(base_mulheres, aes(x = informalidade, y = xxx, fill = tipo)) +
    geom_bar(stat = 'identity', position = 'stack', width = 0.8, ) +
    labs(title = "Média da alocação de horas semanais por mercado formal e informal", 
         subtitle = "Mulheres", x = " ", y = "Horas", fill = " ") +
    facet_wrap(~ano, ncol = 4, strip.position = 'bottom') +
    theme(strip.background = element_blank(),
          strip.placement = "outside",
          panel.background = element_rect(fill = "white"),
          axis.line = element_line(colour = "black"),
          panel.grid = element_line(colour = "grey90"),
          legend.key = element_blank(),
          plot.subtitle = element_text(size = 13),
          plot.title = element_text(size = 16),
          text = element_text(size = 16),
          axis.text.x = element_text(size = 10)) +
    scale_fill_manual(values = c("grey85","grey50"),
                      labels = c("afazeres" = "Cuidados e \n afazeres domésticos",
                                 "trabalho" = "Trabalho \n remunerado")) 
  
  
############ 2016 a 2019
  
  base_anos_formalidade = base %>% group_by(tipo, id, informalidade) %>% summarise(xxx = mean(xxx))
  
  ggplot(base_anos_formalidade, aes(x = informalidade, y = xxx, fill = tipo)) +
    geom_bar(stat = 'identity', position = 'stack', width = 0.6, ) +
    labs(title = "Média da alocação de horas semanais", 
         subtitle = "Entre mercado formal e informal de 2016 a 2019" ,x = " ", y = "Horas", fill = " ") +
    theme(strip.background = element_blank(),
          strip.placement = "outside",
          panel.background = element_rect(fill = "white"),
          axis.line = element_line(colour = "black"),
          panel.grid = element_line(colour = "grey90"),
          legend.key = element_blank(),
          plot.subtitle = element_text(size = 13),
          plot.title = element_text(size = 16),
          text = element_text(size = 16),
          axis.text.x = element_text(size = 10)) +
    scale_fill_manual(values = c("grey85","grey50"),
                      labels = c("afazeres" = "Cuidados e \n afazeres domésticos",
                                 "trabalho" = "Trabalho \n remunerado"))
  
  
  
  base_anos_formalidade_m = base_anos_formalidade %>% filter(id == "Mulheres")
  
  ggplot(base_anos_formalidade_m, aes(x = informalidade, y = xxx, fill = tipo)) +
    geom_bar(stat = 'identity', position = 'stack', width = 0.6, ) +
    labs(title = "Média da alocação de horas semanais", 
         subtitle = "Mulheres entre mercado formal e informal de 2016 a 2019", x = " ", y = "Horas", fill = " ") +
    theme(strip.background = element_blank(),
          strip.placement = "outside",
          panel.background = element_rect(fill = "white"),
          axis.line = element_line(colour = "black"),
          panel.grid = element_line(colour = "grey90"),
          legend.key = element_blank(),
          plot.subtitle = element_text(size = 13),
          plot.title = element_text(size = 16),
          text = element_text(size = 16),
          axis.text.x = element_text(size = 10)) +
    scale_fill_manual(values = c("grey85","grey50"),
                      labels = c("afazeres" = "Cuidados e \n afazeres domésticos",
                                 "trabalho" = "Trabalho \n remunerado")) 
  
  base_anos_formalidade_h = base_anos_formalidade %>% filter(id == "Homens")
  
  ggplot(base_anos_formalidade_h, aes(x = informalidade, y = xxx, fill = tipo)) +
    geom_bar(stat = 'identity', position = 'stack', width = 0.6, ) +
    labs(title = "Média da alocação de horas semanais", 
         subtitle = "Homens entre mercado formal e informal de 2016 a 2019", x = " ", y = "Horas", fill = " ") +
    theme(strip.background = element_blank(),
          strip.placement = "outside",
          panel.background = element_rect(fill = "white"),
          axis.line = element_line(colour = "black"),
          panel.grid = element_line(colour = "grey90"),
          legend.key = element_blank(),
          plot.subtitle = element_text(size = 13),
          plot.title = element_text(size = 16),
          text = element_text(size = 16),
          axis.text.x = element_text(size = 10)) +
    scale_fill_manual(values = c("grey85","grey50"),
                      labels = c("afazeres" = "Cuidados e \n afazeres domésticos",
                                 "trabalho" = "Trabalho \n remunerado")) 

  
  
  
  
    