library(tidyverse)
library(haven)
library(dplyr)


geradora_magica_matriz_transicao_informalidade = function(ano, tri0, tri1, painel) {
  
  trimestres = painel78 %>% dplyr::filter(Ano == ano & (Trimestre == tri0 | Trimestre == tri1)) 
  
  # filtros de caracteristicas que queremos (mulheres, homens, raça e educação)
  trimestres = trimestres %>% filter(V2007 == 1) # 2= mulheres, 1= homens
  
  #criar coluna de contribuinte nos casos de ocupação com 2 possibilidades
  trimestres = trimestres %>%  mutate(contribuinte = ifelse((VD4009 == 8 | VD4009 == 9) & VD4012 == 1, 1, (ifelse((VD4009 == 8 | VD4009 == 9) & VD4012 == 2, 2, 0))))
  
  # ocupada formal x ocupada formal 
  ufuf = trimestres %>% group_by(idind) %>% arrange(idind, Ano, Trimestre) %>% 
    mutate(xxx = ifelse(VD4002 == 1 & (VD4009 == 1 | VD4009 == 3 | VD4009 == 5 | VD4009 == 7 | contribuinte == 1) & Trimestre == tri0, 1, 0),                # se estiver ocupada no 1 tri
           xxx2 = ifelse(VD4002 == 1 & (VD4009 == 1 | VD4009 == 3 | VD4009 == 5 | VD4009 == 7 | contribuinte == 1) & Trimestre == tri1, 1, 0),
           zzz = ifelse(VD4002 == 1 & (VD4009 == 1 | VD4009 == 3 | VD4009 == 5 | VD4009 == 7 | contribuinte == 1) & Trimestre == tri1 & lag(xxx) == 1, 1, 0),    # se ela continuou ocupada
           kkk = lag(V1028)) %>%                                           # pega o peso dessa pessoa no periodo 1
    ungroup() %>% 
    summarise(ufuf = ((sum(zzz * kkk, na.rm = T))/(sum(xxx * V1028, na.rm = T)))) 
  
  #ocupada informal x ocupada informal
  uiui = trimestres %>% group_by(idind) %>% arrange(idind, Ano, Trimestre) %>% 
    mutate(xxx = ifelse(VD4002 == 1 & (VD4009 == 2 | VD4009 == 4 | VD4009 == 6 | VD4009 == 10 | contribuinte == 2) & Trimestre == tri0, 1, 0),                # se estiver ocupada no 1 tri
           xxx2 = ifelse(VD4002 == 1 & (VD4009 == 2 | VD4009 == 4 | VD4009 == 6 | VD4009 == 10 | contribuinte == 2) & Trimestre == tri1, 1, 0),
           zzz = ifelse(VD4002 == 1 & (VD4009 == 2 | VD4009 == 4 | VD4009 == 6 | VD4009 == 10 | contribuinte == 2) & Trimestre == tri1 & lag(xxx) == 1, 1, 0),    # se ela continuou ocupada
           kkk = lag(V1028)) %>%                                           # pega o peso dessa pessoa no periodo 1
    ungroup() %>% 
    summarise(uiui = ((sum(zzz * kkk, na.rm = T))/(sum(xxx * V1028, na.rm = T))))   
  
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
    mutate(xxx = ifelse(VD4001 == 2 & Trimestre == tri0, 1, 0),                # se estiver desocupada no 1 tri
           xxx2 = ifelse(VD4001 == 2 & Trimestre == tri1, 1, 0),
           zzz = ifelse(VD4001 == 2 & Trimestre == tri1 & lag(xxx) == 1, 1, 0),    # se ela continuou desocupada
           kkk = lag(V1028)) %>%                                           # pega o peso dessa pessoa no periodo 1
    ungroup() %>% 
    summarise(ff = ((sum(zzz * kkk, na.rm = TRUE))/(sum(xxx * V1028, na.rm = TRUE)))) 
  
  # ocupada formal x ocupada informal
  ufui = trimestres %>% group_by(idind) %>% arrange(idind, Ano, Trimestre) %>% 
    mutate(xxx = ifelse(VD4002 == 1 & (VD4009 == 1 | VD4009 == 3 | VD4009 == 5 | VD4009 == 7 | contribuinte == 1) & Trimestre == tri0, 1, 0),                # se estiver ocupada no 1 tri
           xxx2 = ifelse(VD4002 == 1 & (VD4009 == 2 | VD4009 == 4 | VD4009 == 6 | VD4009 == 10 | contribuinte == 2) & Trimestre == tri1, 1, 0),
           zzz = ifelse(VD4002 == 1 & (VD4009 == 2 | VD4009 == 4 | VD4009 == 6 | VD4009 == 10 | contribuinte == 2) & Trimestre == tri1 & lag(xxx) == 1, 1, 0),    # se ela continuou ocupada
           kkk = lag(V1028)) %>%                                           # pega o peso dessa pessoa no periodo 1
    ungroup() %>% 
    summarise(ufui = ((sum(zzz * kkk, na.rm = T))/(sum(xxx * V1028, na.rm = T)))) 
  
  # ocupada informal x ocupada formal
  uiuf = trimestres %>% group_by(idind) %>% arrange(idind, Ano, Trimestre) %>% 
    mutate(xxx = ifelse(VD4002 == 1 & (VD4009 == 2 | VD4009 == 4 | VD4009 == 6 | VD4009 == 10 | contribuinte == 2) & Trimestre == tri0, 1, 0),                # se estiver ocupada no 1 tri
           xxx2 = ifelse(VD4002 == 1 & (VD4009 == 1 | VD4009 == 3 | VD4009 == 5 | VD4009 == 7 | contribuinte == 1) & Trimestre == tri1, 1, 0),
           zzz = ifelse(VD4002 == 1 & (VD4009 == 1 | VD4009 == 3 | VD4009 == 5 | VD4009 == 7 | contribuinte == 1) & Trimestre == tri1 & lag(xxx) == 1, 1, 0),    # se ela continuou ocupada
           kkk = lag(V1028)) %>%                                           # pega o peso dessa pessoa no periodo 1
    ungroup() %>% 
    summarise(uiuf = ((sum(zzz * kkk, na.rm = T))/(sum(xxx * V1028, na.rm = T)))) 
  
  
  # ocupada formal x desocupada 
  ufd = trimestres %>% group_by(idind) %>% arrange(idind, Ano, Trimestre) %>% 
    mutate(xxx = ifelse(VD4002 == 1 & (VD4009 == 1 | VD4009 == 3 | VD4009 == 5 | VD4009 == 7 | contribuinte == 1) & Trimestre == tri0, 1, 0),                # 
           xxx2 = ifelse(VD4002 == 2 & Trimestre == tri1, 1, 0),
           zzz = ifelse(VD4002 == 2 & Trimestre == tri1 & lag(xxx) == 1, 1, 0),    # 
           kkk = lag(V1028)) %>%                                           # pega o peso dessa pessoa no periodo 1
    ungroup() %>% 
    summarise(ufd = ((sum(zzz * kkk, na.rm = T))/(sum(xxx * V1028, na.rm = T)))) 
  
  # ocupada informal x desocupada 
  uid = trimestres %>% group_by(idind) %>% arrange(idind, Ano, Trimestre) %>% 
    mutate(xxx = ifelse(VD4002 == 1 & (VD4009 == 2 | VD4009 == 4 | VD4009 == 6 | VD4009 == 10 | contribuinte == 2) & Trimestre == tri0, 1, 0),                # 
           xxx2 = ifelse(VD4002 == 2 & Trimestre == tri1, 1, 0),
           zzz = ifelse(VD4002 == 2 & Trimestre == tri1 & lag(xxx) == 1, 1, 0),    # 
           kkk = lag(V1028)) %>%                                           # pega o peso dessa pessoa no periodo 1
    ungroup() %>% 
    summarise(uid = ((sum(zzz * kkk, na.rm = T))/(sum(xxx * V1028, na.rm = T)))) 
  
  
  # ocupado formal x fora da força de trabalho 
  uff = trimestres %>% group_by(idind) %>% arrange(idind, Ano, Trimestre) %>% 
    mutate(xxx = ifelse(VD4002 == 1 & (VD4009 == 1 | VD4009 == 3 | VD4009 == 5 | VD4009 == 7 | contribuinte == 1) & Trimestre == tri0, 1, 0),                # 
           xxx2 = ifelse(VD4001 == 2 & Trimestre == tri1, 1, 0),
           zzz = ifelse(VD4001 == 2 & Trimestre == tri1 & lag(xxx) == 1, 1, 0),    # 
           kkk = lag(V1028)) %>%                                           # pega o peso dessa pessoa no periodo 1
    ungroup() %>% 
    summarise(uff = ((sum(zzz * kkk, na.rm = T))/(sum(xxx * V1028, na.rm = T)))) 
  
  # ocupado informal x fora da força de trabalho 
  uif = trimestres %>% group_by(idind) %>% arrange(idind, Ano, Trimestre) %>% 
    mutate(xxx = ifelse(VD4002 == 1 & (VD4009 == 2 | VD4009 == 4 | VD4009 == 6 | VD4009 == 10 | contribuinte == 2) & Trimestre == tri0, 1, 0),                # 
           xxx2 = ifelse(VD4001 == 2 & Trimestre == tri1, 1, 0),
           zzz = ifelse(VD4001 == 2 & Trimestre == tri1 & lag(xxx) == 1, 1, 0),    # 
           kkk = lag(V1028)) %>%                                           # pega o peso dessa pessoa no periodo 1
    ungroup() %>% 
    summarise(uif = ((sum(zzz * kkk, na.rm = T))/(sum(xxx * V1028, na.rm = T)))) 
  
  # desocupada x ocupada formal
  duf = trimestres %>% group_by(idind) %>% arrange(idind, Ano, Trimestre) %>% 
    mutate(xxx = ifelse(VD4002 == 2 & Trimestre == tri0, 1, 0),                # 
           xxx2 = ifelse(VD4002 == 1 & (VD4009 == 1 | VD4009 == 3 | VD4009 == 5 | VD4009 == 7 | contribuinte == 1) & Trimestre == tri1, 1, 0),
           zzz = ifelse(VD4002 == 1 & (VD4009 == 1 | VD4009 == 3 | VD4009 == 5 | VD4009 == 7 | contribuinte == 1) & Trimestre == tri1 & lag(xxx) == 1, 1, 0),    # 
           kkk = lag(V1028)) %>%                                           # pega o peso dessa pessoa no periodo 1
    ungroup() %>% 
    summarise(duf = ((sum(zzz * kkk, na.rm = T))/(sum(xxx * V1028, na.rm = T)))) 
  
  # desocupada x ocupada informal
  dui = trimestres %>% group_by(idind) %>% arrange(idind, Ano, Trimestre) %>% 
    mutate(xxx = ifelse(VD4002 == 2 & Trimestre == tri0, 1, 0),                # 
           xxx2 = ifelse(VD4002 == 1 & (VD4009 == 2 | VD4009 == 4 | VD4009 == 6 | VD4009 == 10 | contribuinte == 2) & Trimestre == tri1, 1, 0),
           zzz = ifelse(VD4002 == 1 & (VD4009 == 2 | VD4009 == 4 | VD4009 == 6 | VD4009 == 10 | contribuinte == 2) & Trimestre == tri1 & lag(xxx) == 1, 1, 0),    # 
           kkk = lag(V1028)) %>%                                           # pega o peso dessa pessoa no periodo 1
    ungroup() %>% 
    summarise(dui = ((sum(zzz * kkk, na.rm = T))/(sum(xxx * V1028, na.rm = T)))) 
  
  
  # desocupada x fora da força de trabalho
  df = trimestres %>% group_by(idind) %>% arrange(idind, Ano, Trimestre) %>% 
    mutate(xxx = ifelse(VD4002 == 2 & Trimestre == tri0, 1, 0),                # 
           xxx2 = ifelse(VD4001 == 2 & Trimestre == tri1, 1, 0),
           zzz = ifelse(VD4001 == 2 & Trimestre == tri1 & lag(xxx) == 1, 1, 0),    # 
           kkk = lag(V1028)) %>%                                           # pega o peso dessa pessoa no periodo 1
    ungroup() %>% 
    summarise(df = ((sum(zzz * kkk, na.rm = T))/(sum(xxx * V1028, na.rm = T)))) 
  
  # fora da força de trabalho x ocupada formal
  fuf = trimestres %>% group_by(idind) %>% arrange(idind, Ano, Trimestre) %>% 
    mutate(xxx = ifelse(VD4001 == 2 & Trimestre == tri0, 1, 0),                # 
           xxx2 = ifelse(VD4002 == 1 & (VD4009 == 1 | VD4009 == 3 | VD4009 == 5 | VD4009 == 7 | contribuinte == 1) & Trimestre == tri1, 1, 0),
           zzz = ifelse(VD4002 == 1 & (VD4009 == 1 | VD4009 == 3 | VD4009 == 5 | VD4009 == 7 | contribuinte == 1) & Trimestre == tri1 & lag(xxx) == 1, 1, 0),    # 
           kkk = lag(V1028)) %>%                                           # pega o peso dessa pessoa no periodo 1
    ungroup() %>% 
    summarise(fuf = ((sum(zzz * kkk, na.rm = T))/(sum(xxx * V1028, na.rm = T)))) 
  
  # fora da força de trabalho x ocupada informal
  fui = trimestres %>% group_by(idind) %>% arrange(idind, Ano, Trimestre) %>% 
    mutate(xxx = ifelse(VD4001 == 2 & Trimestre == tri0, 1, 0),                # 
           xxx2 = ifelse(VD4002 == 1 & (VD4009 == 2 | VD4009 == 4 | VD4009 == 6 | VD4009 == 10 | contribuinte == 2) & Trimestre == tri1, 1, 0),
           zzz = ifelse(VD4002 == 1 & (VD4009 == 2 | VD4009 == 4 | VD4009 == 6 | VD4009 == 10 | contribuinte == 2) & Trimestre == tri1 & lag(xxx) == 1, 1, 0),    # 
           kkk = lag(V1028)) %>%                                           # pega o peso dessa pessoa no periodo 1
    ungroup() %>% 
    summarise(fui = ((sum(zzz * kkk, na.rm = T))/(sum(xxx * V1028, na.rm = T)))) 
  
  # fora da força de trabalho x desocupada 
  fd = trimestres %>% group_by(idind) %>% arrange(idind, Ano, Trimestre) %>% 
    mutate(xxx = ifelse(VD4001 == 2 & Trimestre == tri0, 1, 0),                # 
           xxx2 = ifelse(VD4002 == 2 & Trimestre == tri1, 1, 0),
           zzz = ifelse(VD4002 == 2 & Trimestre == tri1 & lag(xxx) == 1, 1, 0),    # 
           kkk = lag(V1028)) %>%                                           # pega o peso dessa pessoa no periodo 1
    ungroup() %>% 
    summarise(fd = ((sum(zzz * kkk, na.rm = T))/(sum(xxx * V1028, na.rm = T)))) 
  
  
  resultados = bind_rows(ufuf, uiui, dd, ff, ufui, uiuf, ufd, uid, uff, uif, duf, dui, df, fuf, fui, fd) %>% 
    summarize(ufuf = mean(ufuf, na.rm = T),
              uiui = mean(uiui, na.rm = T),
              dd = mean(dd, na.rm = T), 
              ff = mean(ff, na.rm = T), 
              ufui = mean(ufui, na.rm = T), 
              uiuf = mean(uiuf, na.rm = T), 
              ufd = mean(ufd, na.rm = T), 
              uid = mean(uid, na.rm = T), 
              uff = mean(uff, na.rm = T), 
              uif = mean(uif, na.rm = T),
              duf = mean(duf, na.rm = T),
              dui = mean(dui, na.rm = T), 
              df = mean(df, na.rm = T),
              fuf = mean(fuf, na.rm = T), 
              fui = mean(fui, na.rm = T), 
              fd = mean(fd, na.rm = T)) %>% 
    mutate(quarter = tri1, year = ano) 
  
  write.csv(resultados, file = paste0("input/transicao_csv/homens/transicao_", ano, "_", tri1, ".csv"))
}




geradora_magica_matriz_transicao_informalidade_anos = function(ano0, ano1, tri0, tri1, painel) {
  
  trimestres1 = painel78 %>% dplyr::filter(Ano == ano0 & Trimestre == tri0)
  trimestres2 = painel78 %>% dplyr::filter(Ano == ano1 & Trimestre == tri1)
  trimestres = bind_rows(trimestres1, trimestres2)
  
  # filtros de caracteristicas que queremos (mulheres, homens, raça e educação)
  trimestres = trimestres %>% filter(V2007 == 1) # 2= mulheres, 1= homens
  
  #criar coluna de contribuinte nos casos de ocupação com 2 possibilidades
  trimestres = trimestres %>%  mutate(contribuinte = ifelse((VD4009 == 8 | VD4009 == 9) & VD4012 == 1, 1, (ifelse((VD4009 == 8 | VD4009 == 9) & VD4012 == 2, 2, 0))))
  
  # ocupada formal x ocupada formal 
  ufuf = trimestres %>% group_by(idind) %>% arrange(idind, Ano, Trimestre) %>% 
    mutate(xxx = ifelse(VD4002 == 1 & (VD4009 == 1 | VD4009 == 3 | VD4009 == 5 | VD4009 == 7 | contribuinte == 1) & Trimestre == tri0, 1, 0),                # se estiver ocupada no 1 tri
           xxx2 = ifelse(VD4002 == 1 & (VD4009 == 1 | VD4009 == 3 | VD4009 == 5 | VD4009 == 7 | contribuinte == 1) & Trimestre == tri1, 1, 0),
           zzz = ifelse(VD4002 == 1 & (VD4009 == 1 | VD4009 == 3 | VD4009 == 5 | VD4009 == 7 | contribuinte == 1) & Trimestre == tri1 & lag(xxx) == 1, 1, 0),    # se ela continuou ocupada
           kkk = lag(V1028)) %>%                                           # pega o peso dessa pessoa no periodo 1
    ungroup() %>% 
    summarise(ufuf = ((sum(zzz * kkk, na.rm = T))/(sum(xxx * V1028, na.rm = T)))) 
  
  #ocupada informal x ocupada informal
  uiui = trimestres %>% group_by(idind) %>% arrange(idind, Ano, Trimestre) %>% 
    mutate(xxx = ifelse(VD4002 == 1 & (VD4009 == 2 | VD4009 == 4 | VD4009 == 6 | VD4009 == 10 | contribuinte == 2) & Trimestre == tri0, 1, 0),                # se estiver ocupada no 1 tri
           xxx2 = ifelse(VD4002 == 1 & (VD4009 == 2 | VD4009 == 4 | VD4009 == 6 | VD4009 == 10 | contribuinte == 2) & Trimestre == tri1, 1, 0),
           zzz = ifelse(VD4002 == 1 & (VD4009 == 2 | VD4009 == 4 | VD4009 == 6 | VD4009 == 10 | contribuinte == 2) & Trimestre == tri1 & lag(xxx) == 1, 1, 0),    # se ela continuou ocupada
           kkk = lag(V1028)) %>%                                           # pega o peso dessa pessoa no periodo 1
    ungroup() %>% 
    summarise(uiui = ((sum(zzz * kkk, na.rm = T))/(sum(xxx * V1028, na.rm = T))))   
  
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
    mutate(xxx = ifelse(VD4001 == 2 & Trimestre == tri0, 1, 0),                # se estiver desocupada no 1 tri
           xxx2 = ifelse(VD4001 == 2 & Trimestre == tri1, 1, 0),
           zzz = ifelse(VD4001 == 2 & Trimestre == tri1 & lag(xxx) == 1, 1, 0),    # se ela continuou desocupada
           kkk = lag(V1028)) %>%                                           # pega o peso dessa pessoa no periodo 1
    ungroup() %>% 
    summarise(ff = ((sum(zzz * kkk, na.rm = TRUE))/(sum(xxx * V1028, na.rm = TRUE)))) 
  
  # ocupada formal x ocupada informal
  ufui = trimestres %>% group_by(idind) %>% arrange(idind, Ano, Trimestre) %>% 
    mutate(xxx = ifelse(VD4002 == 1 & (VD4009 == 1 | VD4009 == 3 | VD4009 == 5 | VD4009 == 7 | contribuinte == 1) & Trimestre == tri0, 1, 0),                # se estiver ocupada no 1 tri
           xxx2 = ifelse(VD4002 == 1 & (VD4009 == 2 | VD4009 == 4 | VD4009 == 6 | VD4009 == 10 | contribuinte == 2) & Trimestre == tri1, 1, 0),
           zzz = ifelse(VD4002 == 1 & (VD4009 == 2 | VD4009 == 4 | VD4009 == 6 | VD4009 == 10 | contribuinte == 2) & Trimestre == tri1 & lag(xxx) == 1, 1, 0),    # se ela continuou ocupada
           kkk = lag(V1028)) %>%                                           # pega o peso dessa pessoa no periodo 1
    ungroup() %>% 
    summarise(ufui = ((sum(zzz * kkk, na.rm = T))/(sum(xxx * V1028, na.rm = T)))) 
  
  # ocupada informal x ocupada formal
  uiuf = trimestres %>% group_by(idind) %>% arrange(idind, Ano, Trimestre) %>% 
    mutate(xxx = ifelse(VD4002 == 1 & (VD4009 == 2 | VD4009 == 4 | VD4009 == 6 | VD4009 == 10 | contribuinte == 2) & Trimestre == tri0, 1, 0),                # se estiver ocupada no 1 tri
           xxx2 = ifelse(VD4002 == 1 & (VD4009 == 1 | VD4009 == 3 | VD4009 == 5 | VD4009 == 7 | contribuinte == 1) & Trimestre == tri1, 1, 0),
           zzz = ifelse(VD4002 == 1 & (VD4009 == 1 | VD4009 == 3 | VD4009 == 5 | VD4009 == 7 | contribuinte == 1) & Trimestre == tri1 & lag(xxx) == 1, 1, 0),    # se ela continuou ocupada
           kkk = lag(V1028)) %>%                                           # pega o peso dessa pessoa no periodo 1
    ungroup() %>% 
    summarise(uiuf = ((sum(zzz * kkk, na.rm = T))/(sum(xxx * V1028, na.rm = T)))) 
  
  
  # ocupada formal x desocupada 
  ufd = trimestres %>% group_by(idind) %>% arrange(idind, Ano, Trimestre) %>% 
    mutate(xxx = ifelse(VD4002 == 1 & (VD4009 == 1 | VD4009 == 3 | VD4009 == 5 | VD4009 == 7 | contribuinte == 1) & Trimestre == tri0, 1, 0),                # 
           xxx2 = ifelse(VD4002 == 2 & Trimestre == tri1, 1, 0),
           zzz = ifelse(VD4002 == 2 & Trimestre == tri1 & lag(xxx) == 1, 1, 0),    # 
           kkk = lag(V1028)) %>%                                           # pega o peso dessa pessoa no periodo 1
    ungroup() %>% 
    summarise(ufd = ((sum(zzz * kkk, na.rm = T))/(sum(xxx * V1028, na.rm = T)))) 
  
  # ocupada informal x desocupada 
  uid = trimestres %>% group_by(idind) %>% arrange(idind, Ano, Trimestre) %>% 
    mutate(xxx = ifelse(VD4002 == 1 & (VD4009 == 2 | VD4009 == 4 | VD4009 == 6 | VD4009 == 10 | contribuinte == 2) & Trimestre == tri0, 1, 0),                # 
           xxx2 = ifelse(VD4002 == 2 & Trimestre == tri1, 1, 0),
           zzz = ifelse(VD4002 == 2 & Trimestre == tri1 & lag(xxx) == 1, 1, 0),    # 
           kkk = lag(V1028)) %>%                                           # pega o peso dessa pessoa no periodo 1
    ungroup() %>% 
    summarise(uid = ((sum(zzz * kkk, na.rm = T))/(sum(xxx * V1028, na.rm = T)))) 
  
  
  # ocupado formal x fora da força de trabalho 
  uff = trimestres %>% group_by(idind) %>% arrange(idind, Ano, Trimestre) %>% 
    mutate(xxx = ifelse(VD4002 == 1 & (VD4009 == 1 | VD4009 == 3 | VD4009 == 5 | VD4009 == 7 | contribuinte == 1) & Trimestre == tri0, 1, 0),                # 
           xxx2 = ifelse(VD4001 == 2 & Trimestre == tri1, 1, 0),
           zzz = ifelse(VD4001 == 2 & Trimestre == tri1 & lag(xxx) == 1, 1, 0),    # 
           kkk = lag(V1028)) %>%                                           # pega o peso dessa pessoa no periodo 1
    ungroup() %>% 
    summarise(uff = ((sum(zzz * kkk, na.rm = T))/(sum(xxx * V1028, na.rm = T)))) 
  
  # ocupado informal x fora da força de trabalho 
  uif = trimestres %>% group_by(idind) %>% arrange(idind, Ano, Trimestre) %>% 
    mutate(xxx = ifelse(VD4002 == 1 & (VD4009 == 2 | VD4009 == 4 | VD4009 == 6 | VD4009 == 10 | contribuinte == 2) & Trimestre == tri0, 1, 0),                # 
           xxx2 = ifelse(VD4001 == 2 & Trimestre == tri1, 1, 0),
           zzz = ifelse(VD4001 == 2 & Trimestre == tri1 & lag(xxx) == 1, 1, 0),    # 
           kkk = lag(V1028)) %>%                                           # pega o peso dessa pessoa no periodo 1
    ungroup() %>% 
    summarise(uif = ((sum(zzz * kkk, na.rm = T))/(sum(xxx * V1028, na.rm = T)))) 
  
  # desocupada x ocupada formal
  duf = trimestres %>% group_by(idind) %>% arrange(idind, Ano, Trimestre) %>% 
    mutate(xxx = ifelse(VD4002 == 2 & Trimestre == tri0, 1, 0),                # 
           xxx2 = ifelse(VD4002 == 1 & (VD4009 == 1 | VD4009 == 3 | VD4009 == 5 | VD4009 == 7 | contribuinte == 1) & Trimestre == tri1, 1, 0),
           zzz = ifelse(VD4002 == 1 & (VD4009 == 1 | VD4009 == 3 | VD4009 == 5 | VD4009 == 7 | contribuinte == 1) & Trimestre == tri1 & lag(xxx) == 1, 1, 0),    # 
           kkk = lag(V1028)) %>%                                           # pega o peso dessa pessoa no periodo 1
    ungroup() %>% 
    summarise(duf = ((sum(zzz * kkk, na.rm = T))/(sum(xxx * V1028, na.rm = T)))) 
  
  # desocupada x ocupada informal
  dui = trimestres %>% group_by(idind) %>% arrange(idind, Ano, Trimestre) %>% 
    mutate(xxx = ifelse(VD4002 == 2 & Trimestre == tri0, 1, 0),                # 
           xxx2 = ifelse(VD4002 == 1 & (VD4009 == 2 | VD4009 == 4 | VD4009 == 6 | VD4009 == 10 | contribuinte == 2) & Trimestre == tri1, 1, 0),
           zzz = ifelse(VD4002 == 1 & (VD4009 == 2 | VD4009 == 4 | VD4009 == 6 | VD4009 == 10 | contribuinte == 2) & Trimestre == tri1 & lag(xxx) == 1, 1, 0),    # 
           kkk = lag(V1028)) %>%                                           # pega o peso dessa pessoa no periodo 1
    ungroup() %>% 
    summarise(dui = ((sum(zzz * kkk, na.rm = T))/(sum(xxx * V1028, na.rm = T)))) 
  
  
  # desocupada x fora da força de trabalho
  df = trimestres %>% group_by(idind) %>% arrange(idind, Ano, Trimestre) %>% 
    mutate(xxx = ifelse(VD4002 == 2 & Trimestre == tri0, 1, 0),                # 
           xxx2 = ifelse(VD4001 == 2 & Trimestre == tri1, 1, 0),
           zzz = ifelse(VD4001 == 2 & Trimestre == tri1 & lag(xxx) == 1, 1, 0),    # 
           kkk = lag(V1028)) %>%                                           # pega o peso dessa pessoa no periodo 1
    ungroup() %>% 
    summarise(df = ((sum(zzz * kkk, na.rm = T))/(sum(xxx * V1028, na.rm = T)))) 
  
  # fora da força de trabalho x ocupada formal
  fuf = trimestres %>% group_by(idind) %>% arrange(idind, Ano, Trimestre) %>% 
    mutate(xxx = ifelse(VD4001 == 2 & Trimestre == tri0, 1, 0),                # 
           xxx2 = ifelse(VD4002 == 1 & (VD4009 == 1 | VD4009 == 3 | VD4009 == 5 | VD4009 == 7 | contribuinte == 1) & Trimestre == tri1, 1, 0),
           zzz = ifelse(VD4002 == 1 & (VD4009 == 1 | VD4009 == 3 | VD4009 == 5 | VD4009 == 7 | contribuinte == 1) & Trimestre == tri1 & lag(xxx) == 1, 1, 0),    # 
           kkk = lag(V1028)) %>%                                           # pega o peso dessa pessoa no periodo 1
    ungroup() %>% 
    summarise(fuf = ((sum(zzz * kkk, na.rm = T))/(sum(xxx * V1028, na.rm = T)))) 
  
  # fora da força de trabalho x ocupada informal
  fui = trimestres %>% group_by(idind) %>% arrange(idind, Ano, Trimestre) %>% 
    mutate(xxx = ifelse(VD4001 == 2 & Trimestre == tri0, 1, 0),                # 
           xxx2 = ifelse(VD4002 == 1 & (VD4009 == 2 | VD4009 == 4 | VD4009 == 6 | VD4009 == 10 | contribuinte == 2) & Trimestre == tri1, 1, 0),
           zzz = ifelse(VD4002 == 1 & (VD4009 == 2 | VD4009 == 4 | VD4009 == 6 | VD4009 == 10 | contribuinte == 2) & Trimestre == tri1 & lag(xxx) == 1, 1, 0),    # 
           kkk = lag(V1028)) %>%                                           # pega o peso dessa pessoa no periodo 1
    ungroup() %>% 
    summarise(fui = ((sum(zzz * kkk, na.rm = T))/(sum(xxx * V1028, na.rm = T)))) 
  
  # fora da força de trabalho x desocupada 
  fd = trimestres %>% group_by(idind) %>% arrange(idind, Ano, Trimestre) %>% 
    mutate(xxx = ifelse(VD4001 == 2 & Trimestre == tri0, 1, 0),                # 
           xxx2 = ifelse(VD4002 == 2 & Trimestre == tri1, 1, 0),
           zzz = ifelse(VD4002 == 2 & Trimestre == tri1 & lag(xxx) == 1, 1, 0),    # 
           kkk = lag(V1028)) %>%                                           # pega o peso dessa pessoa no periodo 1
    ungroup() %>% 
    summarise(fd = ((sum(zzz * kkk, na.rm = T))/(sum(xxx * V1028, na.rm = T)))) 
  
  
  resultados = bind_rows(ufuf, uiui, dd, ff, ufui, uiuf, ufd, uid, uff, uif, duf, dui, df, fuf, fui, fd) %>% 
    summarize(ufuf = mean(ufuf, na.rm = T),
              uiui = mean(uiui, na.rm = T),
              dd = mean(dd, na.rm = T), 
              ff = mean(ff, na.rm = T), 
              ufui = mean(ufui, na.rm = T), 
              uiuf = mean(uiuf, na.rm = T), 
              ufd = mean(ufd, na.rm = T), 
              uid = mean(uid, na.rm = T), 
              uff = mean(uff, na.rm = T), 
              uif = mean(uif, na.rm = T),
              duf = mean(duf, na.rm = T),
              dui = mean(dui, na.rm = T), 
              df = mean(df, na.rm = T),
              fuf = mean(fuf, na.rm = T), 
              fui = mean(fui, na.rm = T), 
              fd = mean(fd, na.rm = T)) %>% 
    mutate(quarter = tri1, year = ano1) 
  
  write.csv(resultados, file = paste0("input/transicao_csv/homens/transicao_", ano1, "_", tri1, ".csv"))
}
