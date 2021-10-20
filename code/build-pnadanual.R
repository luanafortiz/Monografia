
library(tidyverse)
library(haven)
library(dplyr)




# pnad continua anual 2019

pnadc2019anual = read_dta(file = "input/PNADC_anual_2019_visita5.dta") 


variaveis_escolhidas = c("Ano", "Trimestre", "UF", "Capital", 
                         "V1032", # peso com pos estratificação 
                         "V1022", # urbana ou rural 
                         "VD3004", # nível ode instrução mais alto alcançado
                         # características dos moradores
                         "V2003", # número de ordem da pessoa em domicílio 
                         "V2007", # sexo
                         "VD2002", # condição no domicílio (mãe, pai, filho...)
                         "V2009", # idade
                         "V2010", # cor ou raça
                         # mercado de trabalho
                         "VD4001", # força de trabalho ou não 
                         "VD4002", # ocupada X descocupada
                         "VD4005", # desalentada
                         "VD4009", # posição no mercado de trabalho e carteira assinada
                         "VD4012", # contribuição pra previdencia 
                         "VD4030", # motivo por nao ter procurado trabalho naquela semana 
                         # trabalho de pessoas entre 5 e 13 anos
                         #"S06001", # trabalhou em alguma atividade remunerada em dinheiro?
                         # horas dessas crianças?
                         # horas
                         "V4039C", # horas efetivamente trabalhados no trabalho principal 
                         "VD4031", # horas HABITUALMENTE trabalhadas naquela semana em todos os trabalhos
                         "VD4035", # horas EFETIVAMENTE trabalhadas naquela semana em todos os trabalhos
                         "V4121B", # total de horas dedicadas a cuidados e afazeres domésticos
                         # cuidado e afazeres domésticos
                         "VD4039", # cuidou ou não de moradores do domicílio
                         "VD4040", # cuidado de moradores de 0 a 5 anos 
                         "VD4041", # cuidado de moradores de 6 a 14 anos
                         "VD4042", # cuidado de moradores de 15 a 59 anos 
                         "VD4043", # cuidado de moradores maior de 60 anos
                         "VD4049", # realizou tarefas domésticas
                         # se recebeu auxílio de programas sociais
                         "V5001A", # BPC
                         "V5002A", # bolsa família
                         "V5003A" # outro
                         )

pnadc2019anual = pnadc2019anual %>% select(all_of(variaveis_escolhidas))


# horas com trabalho 
total_horas_trabalho = pnadc2019anual %>% 
  filter(VD4002 == 1) %>% 
  group_by(UF) %>% 
  mutate(pessoas_ocupadas = sum(V1032), 
         horas_trabalho = sum(V4039C*V1032),
         media_horas_trabalho = horas_trabalho/pessoas_ocupadas) %>% 
  summarise(pessoas_ocupadas = mean(pessoas_ocupadas),
            horas_trabalho = mean(horas_trabalho),
            media_horas_trabalho = mean(media_horas_trabalho)) %>% 
  saveRDS(file = "tmp/horas_trabalho_2019.rds")



# horas com trabalho mulheres
total_horas_trabalho_mulheres = pnadc2019anual %>% 
  filter(VD4002 == 1, V2007 == 2) %>% 
  group_by(UF) %>% 
  mutate(pessoas_ocupadas = sum(V1032), 
         horas_trabalho = sum(V4039C*V1032),
         media_horas_trabalho = horas_trabalho/pessoas_ocupadas) %>% 
  summarise(pessoas_ocupadas = mean(pessoas_ocupadas),
            horas_trabalho = mean(horas_trabalho),
            media_horas_trabalho = mean(media_horas_trabalho)) %>% 
  saveRDS(file = "tmp/horas_trabalho_mulheres_2019.rds")


#horas com trabalho mulheres brancas
total_horas_trabalho_mulheres_brancas = pnadc2019anual %>% 
  filter(VD4002 == 1, V2007 == 2, V2010 == 1) %>% 
  group_by(UF) %>% 
  mutate(pessoas_ocupadas = sum(V1032), 
         horas_trabalho = sum(V4039C*V1032),
         media_horas_trabalho = horas_trabalho/pessoas_ocupadas) %>% 
  summarise(pessoas_ocupadas = mean(pessoas_ocupadas),
            horas_trabalho = mean(horas_trabalho),
            media_horas_trabalho = mean(media_horas_trabalho)) %>% 
  saveRDS(file = "tmp/horas_trabalho_mulheres_brancas_2019.rds")


#horas com trabalho mulheres não brancas
total_horas_trabalho_mulheres_nao_brancas = pnadc2019anual %>% 
  filter(VD4002 == 1, V2007 == 2, !V2010 == 1) %>% 
  group_by(UF) %>% 
  mutate(pessoas_ocupadas = sum(V1032), 
         horas_trabalho = sum(V4039C*V1032),
         media_horas_trabalho = horas_trabalho/pessoas_ocupadas) %>% 
  summarise(pessoas_ocupadas = mean(pessoas_ocupadas),
            horas_trabalho = mean(horas_trabalho),
            media_horas_trabalho = mean(media_horas_trabalho)) %>% 
  saveRDS(file = "tmp/horas_trabalho_mulheres_nao_brancas_2019.rds")


# horas com trabalho homens
total_horas_trabalho_homens = pnadc2019anual %>% 
  filter(VD4002 == 1, V2007 == 1) %>% 
  group_by(UF) %>% 
  mutate(pessoas_ocupadas = sum(V1032), 
         horas_trabalho = sum(V4039C*V1032),
         media_horas_trabalho = horas_trabalho/pessoas_ocupadas) %>% 
  summarise(pessoas_ocupadas = mean(pessoas_ocupadas),
            horas_trabalho = mean(horas_trabalho),
            media_horas_trabalho = mean(media_horas_trabalho)) %>% 
  saveRDS(file = "tmp/horas_trabalho_homens_2019.rds")


# horas com trabalho homens brancos
total_horas_trabalho_homens_brancos = pnadc2019anual %>% 
  filter(VD4002 == 1, V2007 == 1, V2010 == 1) %>% 
  group_by(UF) %>% 
  mutate(pessoas_ocupadas = sum(V1032), 
         horas_trabalho = sum(V4039C*V1032),
         media_horas_trabalho = horas_trabalho/pessoas_ocupadas) %>% 
  summarise(pessoas_ocupadas = mean(pessoas_ocupadas),
            horas_trabalho = mean(horas_trabalho),
            media_horas_trabalho = mean(media_horas_trabalho)) %>% 
  saveRDS(file = "tmp/horas_trabalho_homens_brancos_2019.rds")


# horas com trabalho homens não brancos
total_horas_trabalho_homens_nao_brancos = pnadc2019anual %>% 
  filter(VD4002 == 1, V2007 == 1, !V2010 == 1) %>% 
  group_by(UF) %>% 
  mutate(pessoas_ocupadas = sum(V1032), 
         horas_trabalho = sum(V4039C*V1032),
         media_horas_trabalho = horas_trabalho/pessoas_ocupadas) %>% 
  summarise(pessoas_ocupadas = mean(pessoas_ocupadas),
            horas_trabalho = mean(horas_trabalho),
            media_horas_trabalho = mean(media_horas_trabalho)) %>% 
  saveRDS(file = "tmp/horas_trabalho_homens_nao_brancos_2019.rds")


# horas cuidado e afazeres
total_horas_afazeres = pnadc2019anual %>% 
  filter(!is.na(V4121B)) %>% 
  group_by(UF) %>% 
  mutate(pessoas_afazeres = sum(V1032), 
         horas_afazeres = sum(V4121B*V1032),
         media_horas_afazeres = horas_afazeres/pessoas_afazeres) %>% 
  summarise(pessoas_afazeres = mean(pessoas_afazeres),
            horas_afazeres = mean(horas_afazeres),
            media_horas_afazeres = mean(media_horas_afazeres)) %>% 
  saveRDS(file = "tmp/horas_afazeres_2019.rds")


# horas cuidado e afazeres mulheres
total_horas_afazeres_mulheres = pnadc2019anual %>% 
  filter(!is.na(V4121B), V2007 == 2) %>% 
  group_by(UF) %>% 
  mutate(pessoas_afazeres = sum(V1032), 
         horas_afazeres = sum(V4121B*V1032),
         media_horas_afazeres = horas_afazeres/pessoas_afazeres) %>% 
  summarise(pessoas_afazeres = mean(pessoas_afazeres),
            horas_afazeres = mean(horas_afazeres),
            media_horas_afazeres = mean(media_horas_afazeres)) %>% 
  saveRDS(file = "tmp/horas_afazeres_mulheres_2019.rds")
 

# horas cuidado e afazeres mulheres brancas
total_horas_afazeres_mulheres_brancas = pnadc2019anual %>% 
  filter(!is.na(V4121B), V2007 == 2, V2010 == 1) %>% 
  group_by(UF) %>% 
  mutate(pessoas_afazeres = sum(V1032), 
         horas_afazeres = sum(V4121B*V1032),
         media_horas_afazeres = horas_afazeres/pessoas_afazeres) %>% 
  summarise(pessoas_afazeres = mean(pessoas_afazeres),
            horas_afazeres = mean(horas_afazeres),
            media_horas_afazeres = mean(media_horas_afazeres)) %>% 
  saveRDS(file = "tmp/horas_afazeres_mulheres_brancas_2019.rds")


# horas cuidado e afazeres mulheres não brancas
total_horas_afazeres_mulheres_brancas = pnadc2019anual %>% 
  filter(!is.na(V4121B), V2007 == 2, !V2010 == 1) %>% 
  group_by(UF) %>% 
  mutate(pessoas_afazeres = sum(V1032), 
         horas_afazeres = sum(V4121B*V1032),
         media_horas_afazeres = horas_afazeres/pessoas_afazeres) %>% 
  summarise(pessoas_afazeres = mean(pessoas_afazeres),
            horas_afazeres = mean(horas_afazeres),
            media_horas_afazeres = mean(media_horas_afazeres)) %>% 
  saveRDS(file = "tmp/horas_afazeres_mulheres_nao_brancas_2019.rds")


# horas cuidado e afazeres homens
total_horas_afazeres_homens = pnadc2019anual %>% 
  filter(!is.na(V4121B), V2007 == 1) %>% 
  group_by(UF) %>% 
  mutate(pessoas_afazeres = sum(V1032), 
         horas_afazeres = sum(V4121B*V1032),
         media_horas_afazeres = horas_afazeres/pessoas_afazeres) %>% 
  summarise(pessoas_afazeres = mean(pessoas_afazeres),
            horas_afazeres = mean(horas_afazeres),
            media_horas_afazeres = mean(media_horas_afazeres)) %>% 
  saveRDS(file = "tmp/horas_afazeres_homens_2019.rds")



# horas cuidado e afazeres homens brancos
total_horas_afazeres_homens = pnadc2019anual %>% 
  filter(!is.na(V4121B), V2007 == 1, V2010 == 1) %>% 
  group_by(UF) %>% 
  mutate(pessoas_afazeres = sum(V1032), 
         horas_afazeres = sum(V4121B*V1032),
         media_horas_afazeres = horas_afazeres/pessoas_afazeres) %>% 
  summarise(pessoas_afazeres = mean(pessoas_afazeres),
            horas_afazeres = mean(horas_afazeres),
            media_horas_afazeres = mean(media_horas_afazeres)) %>% 
  saveRDS(file = "tmp/horas_afazeres_homens_brancos_2019.rds")


# horas cuidado e afazeres homens não brancos
total_horas_afazeres_homens = pnadc2019anual %>% 
  filter(!is.na(V4121B), V2007 == 1, !V2010 == 1) %>% 
  group_by(UF) %>% 
  mutate(pessoas_afazeres = sum(V1032), 
         horas_afazeres = sum(V4121B*V1032),
         media_horas_afazeres = horas_afazeres/pessoas_afazeres) %>% 
  summarise(pessoas_afazeres = mean(pessoas_afazeres),
            horas_afazeres = mean(horas_afazeres),
            media_horas_afazeres = mean(media_horas_afazeres)) %>% 
  saveRDS(file = "tmp/horas_afazeres_homens_nao_brancos_2019.rds")













populacao = pnadc2019 %>% 
  group_by(UF) %>% 
  mutate(pop = sum(V1032)) %>% 
  summarise(pop = mean(pop))

mulheres = pnadc2019 %>% 
  filter(V2007 == 2) %>% 
  group_by(UF) %>% 
  mutate(mulheres = sum(V1032)) %>% 
  summarise(mulheres = mean(mulheres))

mulheres_afazeres = pnadc2019 %>% 
  filter(V2007 == 2, V4120 == 1) %>% 
  group_by(UF) %>% 
  mutate(mulheres_afazeres = sum(V1032)) %>% 
  summarise(mulheres_afazeres = mean(mulheres_afazeres)) %>% 
  saveRDS(file = "tmp/mulheres_afazeres.rds")

base_principal = merge(populacao, mulheres, by = c("UF"))

base_principal = merge(base_principal, mulheres_afazeres, by = c("UF"))

base_principal= base_principal %>% mutate(proporcao_mulheres_afazeres = (mulheres_afazeres/mulheres)*100)
