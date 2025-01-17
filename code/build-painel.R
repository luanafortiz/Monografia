library(tidyverse)
library(haven)
library(dplyr)


setwd("C:/Users/luana/OneDrive/Documentos/Monografia/")
# painel 7

painel7 = read_dta(file = "input/PNAD_painel_7_rs.dta")
painel8 = read_dta(file = "input/PNAD_painel_8_rs.dta")
#%>% slice_head(prop = 0.1)

variaveis_escolhidas = c("Ano", "Trimestre", "UF", "Capital", 
                         "V1008", # número de seleção do domicilio
                         "V1014", # grupo de amostra
                         "V1022", # urbana ou rural
                         "V1028", # peso com pos estratificação 
                         "VD2002", # condição no domicílio (pai, mae, avó..)
                         "V2007", # sexo
                         "V2009", # idade
                         "V2010", # cor ou raça
                         #caracteristicas de educação dos +5 anos
                         "V3001", # sabe ler e escrever?
                         "V3002", # frequenta a escola?
                         "V3002A", # a escola que frequenta é de rede pública/privada/nao se aplica
                         "VD3004", # nível de instrução mais elevado
                         #trabalho
                         "VD4001", # na força de trabalho (sim ou nao)
                         "VD4002", # ocupadas ou desocupadas
                         "VD4003", # força de trabalho potencial
                         "VD4004", # subocupada efetivamente (1 se sim, NA )
                         "VD4004A", # subocupada habitualmente
                         "VD4005", # pop desalentada (fora da força de trabalho)
                         "VD4008", # nesse trabalho era ... (trabalhador domestico, setor publico,... )
                         "VD4009", # nesse trabalho era.. com mais detalhes sobre carteira assinada
                         "V4029", # carteira assinada (sim ou nao)
                         "VD4012", # contribuinte do instituto de previdencia 
                         "VD4019", # rendimento mensal habitual 
                         "VD4020", # rendimento mensal efetivo
                         "VD4023", # motivo por nao ter procurado emprego ou nao estava disponivel
                         "VD4030", # o mesmo ^ mas atual
                         #horas
                         "VD4031", # todos os trabalhos - horas habitualmente trabalhadas por semana
                         "VD4032", # trabalho principal - horas efetivamente trabalhadas por semana
                         "VD4033", # trabalho secundário - horas habitualmente trabalhadas por semana
                         "VD4034", # trabalho secundário - horas efetivamente trabalhadas por semana
                         "VD4035", # todos os trabalhos - horas efetivamente trabalhadas por semana
                         "idind")

# selecionando as variáveis importantes
painel7 = painel7 %>% select(all_of(variaveis_escolhidas))
painel8 = painel8 %>% select(all_of(variaveis_escolhidas))


# juntar os dois paineis 
painel78 = bind_rows(painel7,painel8)












# filtrar trimestres desejados (1 e 2 2019)
trimestres = painel7 %>% dplyr::filter(Ano == 2019 & (Trimestre == 1 | Trimestre == 2)) 

    # matriz para mulheres
trimestres = trimestres %>% filter(V2007 == 1)

          # ocupada x ocupada 
          uu = trimestres %>% group_by(idind) %>% arrange(idind, Ano, Trimestre) %>% 
            mutate(xxx = ifelse(VD4002 == 1 & Trimestre == 1, 1, 0),                # se estiver ocupada no 1 tri
                   xxx2 = ifelse(VD4002 == 1 & Trimestre == 2, 1, 0),
                   zzz = ifelse(VD4002 == 1 & Trimestre == 2 & lag(xxx) == 1, 1, 0),    # se ela continuou ocupada
                   kkk = lag(V1028)) %>%                                           # pega o peso dessa pessoa no periodo 1
            ungroup() %>% 
            summarise(uu = ((sum(zzz * kkk, na.rm = T))/(sum(xxx * V1028, na.rm = T)))) 
    
          # desocupada x desocupada 
          dd = trimestres %>% group_by(idind) %>% arrange(idind, Ano, Trimestre) %>% 
            mutate(xxx = ifelse(VD4002 == 2 & Trimestre == 1, 1, 0),                # se estiver desocupada no 1 tri
                   xxx2 = ifelse(VD4002 == 2 & Trimestre == 2, 1, 0),
                   zzz = ifelse(VD4002 == 2 & Trimestre == 2 & lag(xxx) == 1, 1, 0),    # se ela continuou desocupada
                   kkk = lag(V1028)) %>%                                           # pega o peso dessa pessoa no periodo 1
            ungroup() %>% 
            summarise(dd = ((sum(zzz * kkk, na.rm = T))/(sum(xxx * V1028, na.rm = T)))) 
          
          # ocupada x desocupada 
          ud = trimestres %>% group_by(idind) %>% arrange(idind, Ano, Trimestre) %>% 
            mutate(xxx = ifelse(VD4002 == 1 & Trimestre == 1, 1, 0),                # 
                   xxx2 = ifelse(VD4002 == 2 & Trimestre == 2, 1, 0),
                   zzz = ifelse(VD4002 == 2 & Trimestre == 2 & lag(xxx) == 1, 1, 0),    # 
                   kkk = lag(V1028)) %>%                                           # pega o peso dessa pessoa no periodo 1
            ungroup() %>% 
            summarise(ud = ((sum(zzz * kkk, na.rm = T))/(sum(xxx * V1028, na.rm = T)))) 
          
          # desocupada x ocupada 
          du = trimestres %>% group_by(idind) %>% arrange(idind, Ano, Trimestre) %>% 
            mutate(xxx = ifelse(VD4002 == 2 & Trimestre == 1, 1, 0),                # 
                   xxx2 = ifelse(VD4002 == 1 & Trimestre == 2, 1, 0),
                   zzz = ifelse(VD4002 == 1 & Trimestre == 2 & lag(xxx) == 1, 1, 0),    # 
                   kkk = lag(V1028)) %>%                                           # pega o peso dessa pessoa no periodo 1
            ungroup() %>% 
            summarise(du = ((sum(zzz * kkk, na.rm = T))/(sum(xxx * V1028, na.rm = T)))) 
          

resultados = bind_rows(dd, ud, du, uu) %>% summarize(dd = mean(dd, na.rm = T), ud = mean(ud, na.rm = T), du = mean(du, na.rm = T), uu = mean(uu, na.rm = T)) %>% 
  mutate(quarter = 1, year = 2019) 

write.csv(resultados, file = "input/transicao_csv/transicao_2019_1.csv")

          
          
          
          
          
          
          
          
          
          
    # dessas mulheres, pretas 1º tri 2019
    mp_1tri19_ocupadas = m_1tri19 %>% mutate(ocupadas_p = ifelse(VD4002 == 1 & V2010 == 2, 1, 0)) %>% 
      ungroup() %>% summarise(sum(ocupadas_p, na.rm = T)) %>% as.integer()
    
    # dessas mulheres, brancas 1º tri 2019
    mb_1tri19_ocupadas = m_1tri19 %>% mutate(ocupadas_b = ifelse(VD4002 == 1 & V2010 == 1, 1, 0)) %>% 
      ungroup() %>% summarise(sum(ocupadas_b, na.rm = T)) %>% as.integer()
 
    #amarelas, pardas...
    
    
# filtrar homens em trimestre 1 2019
h_1tri19 = painel7 %>% dplyr::filter(V2007 == 1 & Ano == 2019 & Trimestre == 1) 

    # só homens ocupados 1º tri 2019
    h_1tri19_ocupadas = h_1tri19 %>% mutate(h_ocupadas = ifelse(VD4002 == 1, 1, 0)) %>% 
      ungroup() %>% summarise(sum(h_ocupadas, na.rm = T)) %>% as.integer()
    
    