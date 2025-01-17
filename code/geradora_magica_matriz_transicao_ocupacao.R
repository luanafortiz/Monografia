library(tidyverse)
library(haven)
library(dplyr)
 

# LEMBRAR DE COLOCAR VARIAVEL EM TODAS

geradora_magica_matriz_transicao_ocupacao = function(ano, tri0, tri1, painel) {
  
  trimestres = painel %>% dplyr::filter(Ano == ano & (Trimestre == tri0 | Trimestre == tri1)) 
  
  # filtros de caracteristicas que queremos (mulheres, homens, raça e educação)
  # trimestres = trimestres %>% filter(V2007 == 1)
  
  # ocupada x ocupada 
  uu = trimestres %>% group_by(idind) %>% arrange(idind, Ano, Trimestre) %>% 
    mutate(xxx = ifelse(VD4002 == 1 & Trimestre == tri0, 1, 0),                # se estiver ocupada no 1 tri
           xxx2 = ifelse(VD4002 == 1 & Trimestre == tri1, 1, 0),
           zzz = ifelse(VD4002 == 1 & Trimestre == tri1 & lag(xxx) == 1, 1, 0),    # se ela continuou ocupada
           kkk = lag(V1028)) %>%                                           # pega o peso dessa pessoa no periodo 1
    ungroup() %>% 
    summarise(uu = ((sum(zzz * kkk, na.rm = T))/(sum(xxx * V1028, na.rm = T)))) 
  
  # desocupada x desocupada 
  dd = trimestres %>% group_by(idind) %>% arrange(idind, Ano, Trimestre) %>% 
    mutate(xxx = ifelse(VD4002 == 2 & Trimestre == tri0, 1, 0),                # se estiver desocupada no 1 tri
           xxx2 = ifelse(VD4002 == 2 & Trimestre == tri1, 1, 0),
           zzz = ifelse(VD4002 == 2 & Trimestre == tri1 & lag(xxx) == 1, 1, 0),    # se ela continuou desocupada
           kkk = lag(V1028)) %>%                                           # pega o peso dessa pessoa no periodo 1
    ungroup() %>% 
    summarise(dd = ((sum(zzz * kkk, na.rm = T))/(sum(xxx * V1028, na.rm = T)))) 
  
  # fora da força de trabalho x fora da força de trabalho
  ff = trimestres %>% group_by(idind) %>% arrange(idind, Ano, Trimestre) %>% 
    mutate(xxx = ifelse(VD4001 == 2 & Trimestre == 1, 1, 0),                # se estiver desocupada no 1 tri
           xxx2 = ifelse(VD4001 == 2 & Trimestre == 2, 1, 0),
           zzz = ifelse(VD4001 == 2 & Trimestre == 2 & lag(xxx) == 1, 1, 0),    # se ela continuou desocupada
           kkk = lag(V1028)) %>%                                           # pega o peso dessa pessoa no periodo 1
    ungroup() %>% 
    summarise(ff = ((sum(zzz * kkk, na.rm = TRUE))/(sum(xxx * V1028, na.rm = TRUE)))) 
  
  # ocupada x desocupada 
  ud = trimestres %>% group_by(idind) %>% arrange(idind, Ano, Trimestre) %>% 
    mutate(xxx = ifelse(VD4002 == 1 & Trimestre == tri0, 1, 0),                # 
           xxx2 = ifelse(VD4002 == 2 & Trimestre == tri1, 1, 0),
           zzz = ifelse(VD4002 == 2 & Trimestre == tri1 & lag(xxx) == 1, 1, 0),    # 
           kkk = lag(V1028)) %>%                                           # pega o peso dessa pessoa no periodo 1
    ungroup() %>% 
    summarise(ud = ((sum(zzz * kkk, na.rm = T))/(sum(xxx * V1028, na.rm = T)))) 
  
  
  # ocupado x fora da força de trabalho 
  uf = trimestres %>% group_by(idind) %>% arrange(idind, Ano, Trimestre) %>% 
    mutate(xxx = ifelse(VD4002 == 1 & Trimestre == tri0, 1, 0),                # 
           xxx2 = ifelse(VD4001 == 2 & Trimestre == tri1, 1, 0),
           zzz = ifelse(VD4001 == 2 & Trimestre == tri1 & lag(xxx) == 1, 1, 0),    # 
           kkk = lag(V1028)) %>%                                           # pega o peso dessa pessoa no periodo 1
    ungroup() %>% 
    summarise(uf = ((sum(zzz * kkk, na.rm = T))/(sum(xxx * V1028, na.rm = T)))) 
  
  
  # desocupada x ocupada 
  du = trimestres %>% group_by(idind) %>% arrange(idind, Ano, Trimestre) %>% 
    mutate(xxx = ifelse(VD4002 == 2 & Trimestre == tri0, 1, 0),                # 
           xxx2 = ifelse(VD4002 == 1 & Trimestre == tri1, 1, 0),
           zzz = ifelse(VD4002 == 1 & Trimestre == tri1 & lag(xxx) == 1, 1, 0),    # 
           kkk = lag(V1028)) %>%                                           # pega o peso dessa pessoa no periodo 1
    ungroup() %>% 
    summarise(du = ((sum(zzz * kkk, na.rm = T))/(sum(xxx * V1028, na.rm = T)))) 
  
  # desocupada x fora da força de trabalho
  df = trimestres %>% group_by(idind) %>% arrange(idind, Ano, Trimestre) %>% 
    mutate(xxx = ifelse(VD4002 == 2 & Trimestre == tri0, 1, 0),                # 
           xxx2 = ifelse(VD4001 == 2 & Trimestre == tri1, 1, 0),
           zzz = ifelse(VD4001 == 2 & Trimestre == tri1 & lag(xxx) == 1, 1, 0),    # 
           kkk = lag(V1028)) %>%                                           # pega o peso dessa pessoa no periodo 1
    ungroup() %>% 
    summarise(df = ((sum(zzz * kkk, na.rm = T))/(sum(xxx * V1028, na.rm = T)))) 
  
  # fora da força de trabalho x ocupada 
  fu = trimestres %>% group_by(idind) %>% arrange(idind, Ano, Trimestre) %>% 
    mutate(xxx = ifelse(VD4001 == 2 & Trimestre == tri0, 1, 0),                # 
           xxx2 = ifelse(VD4002 == 1 & Trimestre == tri1, 1, 0),
           zzz = ifelse(VD4002 == 1 & Trimestre == tri1 & lag(xxx) == 1, 1, 0),    # 
           kkk = lag(V1028)) %>%                                           # pega o peso dessa pessoa no periodo 1
    ungroup() %>% 
    summarise(fu = ((sum(zzz * kkk, na.rm = T))/(sum(xxx * V1028, na.rm = T)))) 
  
  # fora da força de trabalho x desocupada 
  fd = trimestres %>% group_by(idind) %>% arrange(idind, Ano, Trimestre) %>% 
    mutate(xxx = ifelse(VD4001 == 2 & Trimestre == tri0, 1, 0),                # 
           xxx2 = ifelse(VD4002 == 2 & Trimestre == tri1, 1, 0),
           zzz = ifelse(VD4002 == 2 & Trimestre == tri1 & lag(xxx) == 1, 1, 0),    # 
           kkk = lag(V1028)) %>%                                           # pega o peso dessa pessoa no periodo 1
    ungroup() %>% 
    summarise(fd = ((sum(zzz * kkk, na.rm = T))/(sum(xxx * V1028, na.rm = T)))) 
  
  
  resultados = bind_rows(uu, ud, uf, du, dd, df, fu, fd, ff) %>% summarize(uu = mean(uu, na.rm = T), ud = mean(ud, na.rm = T), uf = mean(uf, na.rm = T), du = mean(du, na.rm = T), dd = mean(dd, na.rm = T), df = mean(df, na.rm = T), fu = mean(fu, na.rm = T), fd = mean(fd, na.rm = T), ff = mean(ff, na.rm = T)) %>% 
    mutate(quarter = tri1, year = ano) 
  
  write.csv(resultados, file = paste0("todos/transicao_", ano, "_", tri1, ".csv"))
}

