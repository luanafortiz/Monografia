
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


# coluna de formal ou informal
pnadc2019anual = pnadc2019anual %>% 
  mutate(formalidade = case_when(VD4009 == 1 ~ "formal",
                                 VD4009 == 2 ~ "informal",
                                 VD4009 == 3 ~ "formal",
                                 VD4009 == 4 ~ "informal",
                                 VD4009 == 5 ~ "formal",
                                 VD4009 == 6 ~ "informal",
                                 VD4009 == 7 ~ "formal",
                                 VD4009 == 8 ~ "autonomo",
                                 VD4009 == 9 ~ "autonomo",
                                 VD4009 == 10 ~ "informal"
                                 )) %>% 
  mutate(autonomo_formal = ifelse(formalidade == "autonomo" & VD4012 == 1,1,2)) %>% 
  mutate(dummy_formalidade = ifelse(VD4002 == 1 & (formalidade == "formal" | (formalidade == "autonomo" & autonomo_formal == 1)), 1,
                                    ifelse(VD4002 == 1, 2, 0)))




                            # trabalho 

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



# horas com trabalho mulheres formais
total_horas_trabalho_mulheres = pnadc2019anual %>% 
  filter(VD4002 == 1, V2007 == 2, dummy_formalidade == 1) %>% 
  group_by(UF) %>% 
  mutate(pessoas_ocupadas = sum(V1032), 
         horas_trabalho = sum(V4039C*V1032),
         media_horas_trabalho = horas_trabalho/pessoas_ocupadas) %>% 
  summarise(pessoas_ocupadas = mean(pessoas_ocupadas),
            horas_trabalho = mean(horas_trabalho),
            media_horas_trabalho = mean(media_horas_trabalho)) %>% 
  saveRDS(file = "tmp/horas_trabalho_mulheres_formal_2019.rds")


# horas com trabalho mulheres informais
total_horas_trabalho_mulheres = pnadc2019anual %>% 
  filter(VD4002 == 1, V2007 == 2, dummy_formalidade == 2) %>% 
  group_by(UF) %>% 
  mutate(pessoas_ocupadas = sum(V1032), 
         horas_trabalho = sum(V4039C*V1032),
         media_horas_trabalho = horas_trabalho/pessoas_ocupadas) %>% 
  summarise(pessoas_ocupadas = mean(pessoas_ocupadas),
            horas_trabalho = mean(horas_trabalho),
            media_horas_trabalho = mean(media_horas_trabalho)) %>% 
  saveRDS(file = "tmp/horas_trabalho_mulheres_informal_2019.rds")


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


                              # formalidade


# horas com trabalho homens formal
total_horas_trabalho_homens = pnadc2019anual %>% 
  filter(VD4002 == 1, V2007 == 1, dummy_formalidade == 1) %>% 
  group_by(UF) %>% 
  mutate(pessoas_ocupadas = sum(V1032), 
         horas_trabalho = sum(V4039C*V1032),
         media_horas_trabalho = horas_trabalho/pessoas_ocupadas) %>% 
  summarise(pessoas_ocupadas = mean(pessoas_ocupadas),
            horas_trabalho = mean(horas_trabalho),
            media_horas_trabalho = mean(media_horas_trabalho)) %>% 
  saveRDS(file = "tmp/horas_trabalho_homens_formal_2019.rds")


# horas com trabalho homens informal
total_horas_trabalho_homens = pnadc2019anual %>% 
  filter(VD4002 == 1, V2007 == 1, dummy_formalidade == 2) %>% 
  group_by(UF) %>% 
  mutate(pessoas_ocupadas = sum(V1032), 
         horas_trabalho = sum(V4039C*V1032),
         media_horas_trabalho = horas_trabalho/pessoas_ocupadas) %>% 
  summarise(pessoas_ocupadas = mean(pessoas_ocupadas),
            horas_trabalho = mean(horas_trabalho),
            media_horas_trabalho = mean(media_horas_trabalho)) %>% 
  saveRDS(file = "tmp/horas_trabalho_homens_informal_2019.rds")

                                      #


                              # cuidado e afazeres 


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

                                      # formalidade

# horas cuidado e afazeres mulheres formal
total_horas_afazeres_mulheres = pnadc2019anual %>% 
  filter(!is.na(V4121B), V2007 == 2, dummy_formalidade == 1) %>% 
  group_by(UF) %>% 
  mutate(pessoas_afazeres = sum(V1032), 
         horas_afazeres = sum(V4121B*V1032),
         media_horas_afazeres = horas_afazeres/pessoas_afazeres) %>% 
  summarise(pessoas_afazeres = mean(pessoas_afazeres),
            horas_afazeres = mean(horas_afazeres),
            media_horas_afazeres = mean(media_horas_afazeres)) %>% 
  saveRDS(file = "tmp/horas_afazeres_mulheres_formal_2019.rds")


# horas cuidado e afazeres mulheres informal
total_horas_afazeres_mulheres = pnadc2019anual %>% 
  filter(!is.na(V4121B), V2007 == 2, dummy_formalidade == 2) %>% 
  group_by(UF) %>% 
  mutate(pessoas_afazeres = sum(V1032), 
         horas_afazeres = sum(V4121B*V1032),
         media_horas_afazeres = horas_afazeres/pessoas_afazeres) %>% 
  summarise(pessoas_afazeres = mean(pessoas_afazeres),
            horas_afazeres = mean(horas_afazeres),
            media_horas_afazeres = mean(media_horas_afazeres)) %>% 
  saveRDS(file = "tmp/horas_afazeres_mulheres_informal_2019.rds")

                                          #

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

                                # formalidade

# horas cuidado e afazeres homens formal
total_horas_afazeres_homens = pnadc2019anual %>% 
  filter(!is.na(V4121B), V2007 == 1, dummy_formalidade == 1) %>% 
  group_by(UF) %>% 
  mutate(pessoas_afazeres = sum(V1032), 
         horas_afazeres = sum(V4121B*V1032),
         media_horas_afazeres = horas_afazeres/pessoas_afazeres) %>% 
  summarise(pessoas_afazeres = mean(pessoas_afazeres),
            horas_afazeres = mean(horas_afazeres),
            media_horas_afazeres = mean(media_horas_afazeres)) %>% 
  saveRDS(file = "tmp/horas_afazeres_homens_formal_2019.rds")



# horas cuidado e afazeres homens informal
total_horas_afazeres_homens = pnadc2019anual %>% 
  filter(!is.na(V4121B), V2007 == 1, dummy_formalidade == 2) %>% 
  group_by(UF) %>% 
  mutate(pessoas_afazeres = sum(V1032), 
         horas_afazeres = sum(V4121B*V1032),
         media_horas_afazeres = horas_afazeres/pessoas_afazeres) %>% 
  summarise(pessoas_afazeres = mean(pessoas_afazeres),
            horas_afazeres = mean(horas_afazeres),
            media_horas_afazeres = mean(media_horas_afazeres)) %>% 
  saveRDS(file = "tmp/horas_afazeres_homens_informal_2019.rds")

                                        #
                                  

                               # faixa etaria da pessoa cuidada 

# horas com trabalho mulheres com crianças de 0 a 14 anos
total_horas_trabalho_mulheres = pnadc2019anual %>% 
  filter(VD4002 == 1, V2007 == 2, (VD4040 == 1 | VD4041 == 1)) %>% 
  group_by(UF) %>% 
  mutate(pessoas_ocupadas = sum(V1032), 
         horas_trabalho = sum(V4039C*V1032),
         media_horas_trabalho = horas_trabalho/pessoas_ocupadas) %>% 
  summarise(pessoas_ocupadas = mean(pessoas_ocupadas),
            horas_trabalho = mean(horas_trabalho),
            media_horas_trabalho = mean(media_horas_trabalho)) %>% 
  saveRDS(file = "tmp/horas_trabalho_mulheres_0_14_2019.rds")


# horas com trabalho mulheres com crianças de 15 a 59 anos
total_horas_trabalho_mulheres = pnadc2019anual %>% 
  filter(VD4002 == 1, V2007 == 2, VD4042 == 1) %>% 
  group_by(UF) %>% 
  mutate(pessoas_ocupadas = sum(V1032), 
         horas_trabalho = sum(V4039C*V1032),
         media_horas_trabalho = horas_trabalho/pessoas_ocupadas) %>% 
  summarise(pessoas_ocupadas = mean(pessoas_ocupadas),
            horas_trabalho = mean(horas_trabalho),
            media_horas_trabalho = mean(media_horas_trabalho)) %>% 
  saveRDS(file = "tmp/horas_trabalho_mulheres_15_59_2019.rds")


# horas com trabalho mulheres com crianças de idosos
total_horas_trabalho_mulheres = pnadc2019anual %>% 
  filter(VD4002 == 1, V2007 == 2, VD4043 == 1) %>% 
  group_by(UF) %>% 
  mutate(pessoas_ocupadas = sum(V1032), 
         horas_trabalho = sum(V4039C*V1032),
         media_horas_trabalho = horas_trabalho/pessoas_ocupadas) %>% 
  summarise(pessoas_ocupadas = mean(pessoas_ocupadas),
            horas_trabalho = mean(horas_trabalho),
            media_horas_trabalho = mean(media_horas_trabalho)) %>% 
  saveRDS(file = "tmp/horas_trabalho_mulheres_60_2019.rds")


# horas com trabalho homens com crianças de 0 a 14 anos
total_horas_trabalho_homens = pnadc2019anual %>% 
  filter(VD4002 == 1, V2007 == 1, (VD4040 == 1 | VD4041 == 1)) %>% 
  group_by(UF) %>% 
  mutate(pessoas_ocupadas = sum(V1032), 
         horas_trabalho = sum(V4039C*V1032),
         media_horas_trabalho = horas_trabalho/pessoas_ocupadas) %>% 
  summarise(pessoas_ocupadas = mean(pessoas_ocupadas),
            horas_trabalho = mean(horas_trabalho),
            media_horas_trabalho = mean(media_horas_trabalho)) %>% 
  saveRDS(file = "tmp/horas_trabalho_homens_0_14_2019.rds")


# horas com trabalho homens com crianças de 15 a 59 anos
total_horas_trabalho_homens = pnadc2019anual %>% 
  filter(VD4002 == 1, V2007 == 1, VD4042 == 1) %>% 
  group_by(UF) %>% 
  mutate(pessoas_ocupadas = sum(V1032), 
         horas_trabalho = sum(V4039C*V1032),
         media_horas_trabalho = horas_trabalho/pessoas_ocupadas) %>% 
  summarise(pessoas_ocupadas = mean(pessoas_ocupadas),
            horas_trabalho = mean(horas_trabalho),
            media_horas_trabalho = mean(media_horas_trabalho)) %>% 
  saveRDS(file = "tmp/horas_trabalho_homens_15_59_2019.rds")


# horas com trabalho homens com crianças de idosos
total_horas_trabalho_homens = pnadc2019anual %>% 
  filter(VD4002 == 1, V2007 == 1, VD4043 == 1) %>% 
  group_by(UF) %>% 
  mutate(pessoas_ocupadas = sum(V1032), 
         horas_trabalho = sum(V4039C*V1032),
         media_horas_trabalho = horas_trabalho/pessoas_ocupadas) %>% 
  summarise(pessoas_ocupadas = mean(pessoas_ocupadas),
            horas_trabalho = mean(horas_trabalho),
            media_horas_trabalho = mean(media_horas_trabalho)) %>% 
  saveRDS(file = "tmp/horas_trabalho_homens_60_2019.rds")



# horas cuidado e afazeres mulheres de 0 a 14 anos
total_horas_afazeres_mulheres = pnadc2019anual %>% 
  filter(!is.na(V4121B), V2007 == 2, (VD4040 == 1 | VD4041 == 1)) %>% 
  group_by(UF) %>% 
  mutate(pessoas_afazeres = sum(V1032), 
         horas_afazeres = sum(V4121B*V1032),
         media_horas_afazeres = horas_afazeres/pessoas_afazeres) %>% 
  summarise(pessoas_afazeres = mean(pessoas_afazeres),
            horas_afazeres = mean(horas_afazeres),
            media_horas_afazeres = mean(media_horas_afazeres)) %>% 
  saveRDS(file = "tmp/horas_afazeres_mulheres_0_14_2019.rds")


# horas cuidado e afazeres mulheres de 15 a 59
total_horas_afazeres_mulheres = pnadc2019anual %>% 
  filter(!is.na(V4121B), V2007 == 2, VD4042 == 1) %>% 
  group_by(UF) %>% 
  mutate(pessoas_afazeres = sum(V1032), 
         horas_afazeres = sum(V4121B*V1032),
         media_horas_afazeres = horas_afazeres/pessoas_afazeres) %>% 
  summarise(pessoas_afazeres = mean(pessoas_afazeres),
            horas_afazeres = mean(horas_afazeres),
            media_horas_afazeres = mean(media_horas_afazeres)) %>% 
  saveRDS(file = "tmp/horas_afazeres_mulheres_15_59_2019.rds")


# horas cuidado e afazeres mulheres de idosos
total_horas_afazeres_mulheres = pnadc2019anual %>% 
  filter(!is.na(V4121B), V2007 == 2, VD4043 == 1) %>% 
  group_by(UF) %>% 
  mutate(pessoas_afazeres = sum(V1032), 
         horas_afazeres = sum(V4121B*V1032),
         media_horas_afazeres = horas_afazeres/pessoas_afazeres) %>% 
  summarise(pessoas_afazeres = mean(pessoas_afazeres),
            horas_afazeres = mean(horas_afazeres),
            media_horas_afazeres = mean(media_horas_afazeres)) %>% 
  saveRDS(file = "tmp/horas_afazeres_mulheres_60_2019.rds")



# horas cuidado e afazeres homens de 0 a 14 anos
total_horas_afazeres_homens = pnadc2019anual %>% 
  filter(!is.na(V4121B), V2007 == 1, (VD4040 == 1 | VD4041 == 1)) %>% 
  group_by(UF) %>% 
  mutate(pessoas_afazeres = sum(V1032), 
         horas_afazeres = sum(V4121B*V1032),
         media_horas_afazeres = horas_afazeres/pessoas_afazeres) %>% 
  summarise(pessoas_afazeres = mean(pessoas_afazeres),
            horas_afazeres = mean(horas_afazeres),
            media_horas_afazeres = mean(media_horas_afazeres)) %>% 
  saveRDS(file = "tmp/horas_afazeres_homens_0_14_2019.rds")


# horas cuidado e afazeres homens de 15 a 59
total_horas_afazeres_homens = pnadc2019anual %>% 
  filter(!is.na(V4121B), V2007 == 1, VD4042 == 1) %>% 
  group_by(UF) %>% 
  mutate(pessoas_afazeres = sum(V1032), 
         horas_afazeres = sum(V4121B*V1032),
         media_horas_afazeres = horas_afazeres/pessoas_afazeres) %>% 
  summarise(pessoas_afazeres = mean(pessoas_afazeres),
            horas_afazeres = mean(horas_afazeres),
            media_horas_afazeres = mean(media_horas_afazeres)) %>% 
  saveRDS(file = "tmp/horas_afazeres_homens_15_59_2019.rds")


# horas cuidado e afazeres homens de idosos
total_horas_afazeres_homens = pnadc2019anual %>% 
  filter(!is.na(V4121B), V2007 == 1, VD4043 == 1) %>% 
  group_by(UF) %>% 
  mutate(pessoas_afazeres = sum(V1032), 
         horas_afazeres = sum(V4121B*V1032),
         media_horas_afazeres = horas_afazeres/pessoas_afazeres) %>% 
  summarise(pessoas_afazeres = mean(pessoas_afazeres),
            horas_afazeres = mean(horas_afazeres),
            media_horas_afazeres = mean(media_horas_afazeres)) %>% 
  saveRDS(file = "tmp/horas_afazeres_homens_60_2019.rds")





#######################################################################################



############################## pnad continua anual 2018 ###############################


pnadc2018anual = read_dta(file = "input/PNADC_anual_2018_visita5.dta") 


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

pnadc2018anual = pnadc2018anual %>% select(all_of(variaveis_escolhidas))


# coluna de formal ou informal
pnadc2018anual = pnadc2018anual %>% 
  mutate(formalidade = case_when(VD4009 == 1 ~ "formal",
                                 VD4009 == 2 ~ "informal",
                                 VD4009 == 3 ~ "formal",
                                 VD4009 == 4 ~ "informal",
                                 VD4009 == 5 ~ "formal",
                                 VD4009 == 6 ~ "informal",
                                 VD4009 == 7 ~ "formal",
                                 VD4009 == 8 ~ "autonomo",
                                 VD4009 == 9 ~ "autonomo",
                                 VD4009 == 10 ~ "informal"
                                 )) %>% 
  mutate(autonomo_formal = ifelse(formalidade == "autonomo" & VD4012 == 1,1,2)) %>% 
  mutate(dummy_formalidade = ifelse(VD4002 == 1 & (formalidade == "formal" | (formalidade == "autonomo" & autonomo_formal == 1)), 1,
                                    ifelse(VD4002 == 1, 2, 0)))




############## trabalho ############## 

# horas com trabalho 
total_horas_trabalho = pnadc2018anual %>% 
  filter(VD4002 == 1) %>% 
  group_by(UF) %>% 
  mutate(pessoas_ocupadas = sum(V1032), 
         horas_trabalho = sum(V4039C*V1032),
         media_horas_trabalho = horas_trabalho/pessoas_ocupadas) %>% 
  summarise(pessoas_ocupadas = mean(pessoas_ocupadas),
            horas_trabalho = mean(horas_trabalho),
            media_horas_trabalho = mean(media_horas_trabalho)) %>% 
  saveRDS(file = "tmp/horas_trabalho_2018.rds")



# horas com trabalho mulheres
total_horas_trabalho_mulheres = pnadc2018anual %>% 
  filter(VD4002 == 1, V2007 == 2) %>% 
  group_by(UF) %>% 
  mutate(pessoas_ocupadas = sum(V1032), 
         horas_trabalho = sum(V4039C*V1032),
         media_horas_trabalho = horas_trabalho/pessoas_ocupadas) %>% 
  summarise(pessoas_ocupadas = mean(pessoas_ocupadas),
            horas_trabalho = mean(horas_trabalho),
            media_horas_trabalho = mean(media_horas_trabalho)) %>% 
  saveRDS(file = "tmp/horas_trabalho_mulheres_2018.rds")


#horas com trabalho mulheres brancas
total_horas_trabalho_mulheres_brancas = pnadc2018anual %>% 
  filter(VD4002 == 1, V2007 == 2, V2010 == 1) %>% 
  group_by(UF) %>% 
  mutate(pessoas_ocupadas = sum(V1032), 
         horas_trabalho = sum(V4039C*V1032),
         media_horas_trabalho = horas_trabalho/pessoas_ocupadas) %>% 
  summarise(pessoas_ocupadas = mean(pessoas_ocupadas),
            horas_trabalho = mean(horas_trabalho),
            media_horas_trabalho = mean(media_horas_trabalho)) %>% 
  saveRDS(file = "tmp/horas_trabalho_mulheres_brancas_2018.rds")


#horas com trabalho mulheres não brancas
total_horas_trabalho_mulheres_nao_brancas = pnadc2018anual %>% 
  filter(VD4002 == 1, V2007 == 2, !V2010 == 1) %>% 
  group_by(UF) %>% 
  mutate(pessoas_ocupadas = sum(V1032), 
         horas_trabalho = sum(V4039C*V1032),
         media_horas_trabalho = horas_trabalho/pessoas_ocupadas) %>% 
  summarise(pessoas_ocupadas = mean(pessoas_ocupadas),
            horas_trabalho = mean(horas_trabalho),
            media_horas_trabalho = mean(media_horas_trabalho)) %>% 
  saveRDS(file = "tmp/horas_trabalho_mulheres_nao_brancas_2018.rds")



# horas com trabalho mulheres formais
total_horas_trabalho_mulheres = pnadc2018anual %>% 
  filter(VD4002 == 1, V2007 == 2, dummy_formalidade == 1) %>% 
  group_by(UF) %>% 
  mutate(pessoas_ocupadas = sum(V1032), 
         horas_trabalho = sum(V4039C*V1032),
         media_horas_trabalho = horas_trabalho/pessoas_ocupadas) %>% 
  summarise(pessoas_ocupadas = mean(pessoas_ocupadas),
            horas_trabalho = mean(horas_trabalho),
            media_horas_trabalho = mean(media_horas_trabalho)) %>% 
  saveRDS(file = "tmp/horas_trabalho_mulheres_formal_2018.rds")


# horas com trabalho mulheres informais
total_horas_trabalho_mulheres = pnadc2018anual %>% 
  filter(VD4002 == 1, V2007 == 2, dummy_formalidade == 2) %>% 
  group_by(UF) %>% 
  mutate(pessoas_ocupadas = sum(V1032), 
         horas_trabalho = sum(V4039C*V1032),
         media_horas_trabalho = horas_trabalho/pessoas_ocupadas) %>% 
  summarise(pessoas_ocupadas = mean(pessoas_ocupadas),
            horas_trabalho = mean(horas_trabalho),
            media_horas_trabalho = mean(media_horas_trabalho)) %>% 
  saveRDS(file = "tmp/horas_trabalho_mulheres_informal_2018.rds")


# horas com trabalho homens
total_horas_trabalho_homens = pnadc2018anual %>% 
  filter(VD4002 == 1, V2007 == 1) %>% 
  group_by(UF) %>% 
  mutate(pessoas_ocupadas = sum(V1032), 
         horas_trabalho = sum(V4039C*V1032),
         media_horas_trabalho = horas_trabalho/pessoas_ocupadas) %>% 
  summarise(pessoas_ocupadas = mean(pessoas_ocupadas),
            horas_trabalho = mean(horas_trabalho),
            media_horas_trabalho = mean(media_horas_trabalho)) %>% 
  saveRDS(file = "tmp/horas_trabalho_homens_2018.rds")


# horas com trabalho homens brancos
total_horas_trabalho_homens_brancos = pnadc2018anual %>% 
  filter(VD4002 == 1, V2007 == 1, V2010 == 1) %>% 
  group_by(UF) %>% 
  mutate(pessoas_ocupadas = sum(V1032), 
         horas_trabalho = sum(V4039C*V1032),
         media_horas_trabalho = horas_trabalho/pessoas_ocupadas) %>% 
  summarise(pessoas_ocupadas = mean(pessoas_ocupadas),
            horas_trabalho = mean(horas_trabalho),
            media_horas_trabalho = mean(media_horas_trabalho)) %>% 
  saveRDS(file = "tmp/horas_trabalho_homens_brancos_2018.rds")


# horas com trabalho homens não brancos
total_horas_trabalho_homens_nao_brancos = pnadc2018anual %>% 
  filter(VD4002 == 1, V2007 == 1, !V2010 == 1) %>% 
  group_by(UF) %>% 
  mutate(pessoas_ocupadas = sum(V1032), 
         horas_trabalho = sum(V4039C*V1032),
         media_horas_trabalho = horas_trabalho/pessoas_ocupadas) %>% 
  summarise(pessoas_ocupadas = mean(pessoas_ocupadas),
            horas_trabalho = mean(horas_trabalho),
            media_horas_trabalho = mean(media_horas_trabalho)) %>% 
  saveRDS(file = "tmp/horas_trabalho_homens_nao_brancos_2018.rds")


# formalidade


# horas com trabalho homens formal
total_horas_trabalho_homens = pnadc2018anual %>% 
  filter(VD4002 == 1, V2007 == 1, dummy_formalidade == 1) %>% 
  group_by(UF) %>% 
  mutate(pessoas_ocupadas = sum(V1032), 
         horas_trabalho = sum(V4039C*V1032),
         media_horas_trabalho = horas_trabalho/pessoas_ocupadas) %>% 
  summarise(pessoas_ocupadas = mean(pessoas_ocupadas),
            horas_trabalho = mean(horas_trabalho),
            media_horas_trabalho = mean(media_horas_trabalho)) %>% 
  saveRDS(file = "tmp/horas_trabalho_homens_formal_2018.rds")


# horas com trabalho homens informal
total_horas_trabalho_homens = pnadc2018anual %>% 
  filter(VD4002 == 1, V2007 == 1, dummy_formalidade == 2) %>% 
  group_by(UF) %>% 
  mutate(pessoas_ocupadas = sum(V1032), 
         horas_trabalho = sum(V4039C*V1032),
         media_horas_trabalho = horas_trabalho/pessoas_ocupadas) %>% 
  summarise(pessoas_ocupadas = mean(pessoas_ocupadas),
            horas_trabalho = mean(horas_trabalho),
            media_horas_trabalho = mean(media_horas_trabalho)) %>% 
  saveRDS(file = "tmp/horas_trabalho_homens_informal_2018.rds")


############## 


############## cuidado e afazeres ############## 


# horas cuidado e afazeres
total_horas_afazeres = pnadc2018anual %>% 
  filter(!is.na(V4121B)) %>% 
  group_by(UF) %>% 
  mutate(pessoas_afazeres = sum(V1032), 
         horas_afazeres = sum(V4121B*V1032),
         media_horas_afazeres = horas_afazeres/pessoas_afazeres) %>% 
  summarise(pessoas_afazeres = mean(pessoas_afazeres),
            horas_afazeres = mean(horas_afazeres),
            media_horas_afazeres = mean(media_horas_afazeres)) %>% 
  saveRDS(file = "tmp/horas_afazeres_2018.rds")


# horas cuidado e afazeres mulheres
total_horas_afazeres_mulheres = pnadc2018anual %>% 
  filter(!is.na(V4121B), V2007 == 2) %>% 
  group_by(UF) %>% 
  mutate(pessoas_afazeres = sum(V1032), 
         horas_afazeres = sum(V4121B*V1032),
         media_horas_afazeres = horas_afazeres/pessoas_afazeres) %>% 
  summarise(pessoas_afazeres = mean(pessoas_afazeres),
            horas_afazeres = mean(horas_afazeres),
            media_horas_afazeres = mean(media_horas_afazeres)) %>% 
  saveRDS(file = "tmp/horas_afazeres_mulheres_2018.rds")


# horas cuidado e afazeres mulheres brancas
total_horas_afazeres_mulheres_brancas = pnadc2018anual %>% 
  filter(!is.na(V4121B), V2007 == 2, V2010 == 1) %>% 
  group_by(UF) %>% 
  mutate(pessoas_afazeres = sum(V1032), 
         horas_afazeres = sum(V4121B*V1032),
         media_horas_afazeres = horas_afazeres/pessoas_afazeres) %>% 
  summarise(pessoas_afazeres = mean(pessoas_afazeres),
            horas_afazeres = mean(horas_afazeres),
            media_horas_afazeres = mean(media_horas_afazeres)) %>% 
  saveRDS(file = "tmp/horas_afazeres_mulheres_brancas_2018.rds")


# horas cuidado e afazeres mulheres não brancas
total_horas_afazeres_mulheres_brancas = pnadc2018anual %>% 
  filter(!is.na(V4121B), V2007 == 2, !V2010 == 1) %>% 
  group_by(UF) %>% 
  mutate(pessoas_afazeres = sum(V1032), 
         horas_afazeres = sum(V4121B*V1032),
         media_horas_afazeres = horas_afazeres/pessoas_afazeres) %>% 
  summarise(pessoas_afazeres = mean(pessoas_afazeres),
            horas_afazeres = mean(horas_afazeres),
            media_horas_afazeres = mean(media_horas_afazeres)) %>% 
  saveRDS(file = "tmp/horas_afazeres_mulheres_nao_brancas_2018.rds")

# formalidade

# horas cuidado e afazeres mulheres formal
total_horas_afazeres_mulheres = pnadc2018anual %>% 
  filter(!is.na(V4121B), V2007 == 2, dummy_formalidade == 1) %>% 
  group_by(UF) %>% 
  mutate(pessoas_afazeres = sum(V1032), 
         horas_afazeres = sum(V4121B*V1032),
         media_horas_afazeres = horas_afazeres/pessoas_afazeres) %>% 
  summarise(pessoas_afazeres = mean(pessoas_afazeres),
            horas_afazeres = mean(horas_afazeres),
            media_horas_afazeres = mean(media_horas_afazeres)) %>% 
  saveRDS(file = "tmp/horas_afazeres_mulheres_formal_2018.rds")


# horas cuidado e afazeres mulheres informal
total_horas_afazeres_mulheres = pnadc2018anual %>% 
  filter(!is.na(V4121B), V2007 == 2, dummy_formalidade == 2) %>% 
  group_by(UF) %>% 
  mutate(pessoas_afazeres = sum(V1032), 
         horas_afazeres = sum(V4121B*V1032),
         media_horas_afazeres = horas_afazeres/pessoas_afazeres) %>% 
  summarise(pessoas_afazeres = mean(pessoas_afazeres),
            horas_afazeres = mean(horas_afazeres),
            media_horas_afazeres = mean(media_horas_afazeres)) %>% 
  saveRDS(file = "tmp/horas_afazeres_mulheres_informal_2018.rds")

#

# horas cuidado e afazeres homens
total_horas_afazeres_homens = pnadc2018anual %>% 
  filter(!is.na(V4121B), V2007 == 1) %>% 
  group_by(UF) %>% 
  mutate(pessoas_afazeres = sum(V1032), 
         horas_afazeres = sum(V4121B*V1032),
         media_horas_afazeres = horas_afazeres/pessoas_afazeres) %>% 
  summarise(pessoas_afazeres = mean(pessoas_afazeres),
            horas_afazeres = mean(horas_afazeres),
            media_horas_afazeres = mean(media_horas_afazeres)) %>% 
  saveRDS(file = "tmp/horas_afazeres_homens_2018.rds")



# horas cuidado e afazeres homens brancos
total_horas_afazeres_homens = pnadc2018anual %>% 
  filter(!is.na(V4121B), V2007 == 1, V2010 == 1) %>% 
  group_by(UF) %>% 
  mutate(pessoas_afazeres = sum(V1032), 
         horas_afazeres = sum(V4121B*V1032),
         media_horas_afazeres = horas_afazeres/pessoas_afazeres) %>% 
  summarise(pessoas_afazeres = mean(pessoas_afazeres),
            horas_afazeres = mean(horas_afazeres),
            media_horas_afazeres = mean(media_horas_afazeres)) %>% 
  saveRDS(file = "tmp/horas_afazeres_homens_brancos_2018.rds")


# horas cuidado e afazeres homens não brancos
total_horas_afazeres_homens = pnadc2018anual %>% 
  filter(!is.na(V4121B), V2007 == 1, !V2010 == 1) %>% 
  group_by(UF) %>% 
  mutate(pessoas_afazeres = sum(V1032), 
         horas_afazeres = sum(V4121B*V1032),
         media_horas_afazeres = horas_afazeres/pessoas_afazeres) %>% 
  summarise(pessoas_afazeres = mean(pessoas_afazeres),
            horas_afazeres = mean(horas_afazeres),
            media_horas_afazeres = mean(media_horas_afazeres)) %>% 
  saveRDS(file = "tmp/horas_afazeres_homens_nao_brancos_2018.rds")

# formalidade

# horas cuidado e afazeres homens formal
total_horas_afazeres_homens = pnadc2018anual %>% 
  filter(!is.na(V4121B), V2007 == 1, dummy_formalidade == 1) %>% 
  group_by(UF) %>% 
  mutate(pessoas_afazeres = sum(V1032), 
         horas_afazeres = sum(V4121B*V1032),
         media_horas_afazeres = horas_afazeres/pessoas_afazeres) %>% 
  summarise(pessoas_afazeres = mean(pessoas_afazeres),
            horas_afazeres = mean(horas_afazeres),
            media_horas_afazeres = mean(media_horas_afazeres)) %>% 
  saveRDS(file = "tmp/horas_afazeres_homens_formal_2018.rds")



# horas cuidado e afazeres homens informal
total_horas_afazeres_homens = pnadc2018anual %>% 
  filter(!is.na(V4121B), V2007 == 1, dummy_formalidade == 2) %>% 
  group_by(UF) %>% 
  mutate(pessoas_afazeres = sum(V1032), 
         horas_afazeres = sum(V4121B*V1032),
         media_horas_afazeres = horas_afazeres/pessoas_afazeres) %>% 
  summarise(pessoas_afazeres = mean(pessoas_afazeres),
            horas_afazeres = mean(horas_afazeres),
            media_horas_afazeres = mean(media_horas_afazeres)) %>% 
  saveRDS(file = "tmp/horas_afazeres_homens_informal_2018.rds")

#


# faixa etaria da pessoa cuidada 

# horas com trabalho mulheres com crianças de 0 a 14 anos
total_horas_trabalho_mulheres = pnadc2018anual %>% 
  filter(VD4002 == 1, V2007 == 2, (VD4040 == 1 | VD4041 == 1)) %>% 
  group_by(UF) %>% 
  mutate(pessoas_ocupadas = sum(V1032), 
         horas_trabalho = sum(V4039C*V1032),
         media_horas_trabalho = horas_trabalho/pessoas_ocupadas) %>% 
  summarise(pessoas_ocupadas = mean(pessoas_ocupadas),
            horas_trabalho = mean(horas_trabalho),
            media_horas_trabalho = mean(media_horas_trabalho)) %>% 
  saveRDS(file = "tmp/horas_trabalho_mulheres_0_14_2018.rds")


# horas com trabalho mulheres com de 15 a 59 anos
total_horas_trabalho_mulheres = pnadc2018anual %>% 
  filter(VD4002 == 1, V2007 == 2, VD4042 == 1) %>% 
  group_by(UF) %>% 
  mutate(pessoas_ocupadas = sum(V1032), 
         horas_trabalho = sum(V4039C*V1032),
         media_horas_trabalho = horas_trabalho/pessoas_ocupadas) %>% 
  summarise(pessoas_ocupadas = mean(pessoas_ocupadas),
            horas_trabalho = mean(horas_trabalho),
            media_horas_trabalho = mean(media_horas_trabalho)) %>% 
  saveRDS(file = "tmp/horas_trabalho_mulheres_15_59_2018.rds")


# horas com trabalho mulheres com crianças de idosos
total_horas_trabalho_mulheres = pnadc2018anual %>% 
  filter(VD4002 == 1, V2007 == 2, VD4043 == 1) %>% 
  group_by(UF) %>% 
  mutate(pessoas_ocupadas = sum(V1032), 
         horas_trabalho = sum(V4039C*V1032),
         media_horas_trabalho = horas_trabalho/pessoas_ocupadas) %>% 
  summarise(pessoas_ocupadas = mean(pessoas_ocupadas),
            horas_trabalho = mean(horas_trabalho),
            media_horas_trabalho = mean(media_horas_trabalho)) %>% 
  saveRDS(file = "tmp/horas_trabalho_mulheres_60_2018.rds")


# horas com trabalho homens com crianças de 0 a 14 anos
total_horas_trabalho_homens = pnadc2018anual %>% 
  filter(VD4002 == 1, V2007 == 1, (VD4040 == 1 | VD4041 == 1)) %>% 
  group_by(UF) %>% 
  mutate(pessoas_ocupadas = sum(V1032), 
         horas_trabalho = sum(V4039C*V1032),
         media_horas_trabalho = horas_trabalho/pessoas_ocupadas) %>% 
  summarise(pessoas_ocupadas = mean(pessoas_ocupadas),
            horas_trabalho = mean(horas_trabalho),
            media_horas_trabalho = mean(media_horas_trabalho)) %>% 
  saveRDS(file = "tmp/horas_trabalho_homens_0_14_2018.rds")


# horas com trabalho homens com crianças de 15 a 59 anos
total_horas_trabalho_homens = pnadc2018anual %>% 
  filter(VD4002 == 1, V2007 == 1, VD4042 == 1) %>% 
  group_by(UF) %>% 
  mutate(pessoas_ocupadas = sum(V1032), 
         horas_trabalho = sum(V4039C*V1032),
         media_horas_trabalho = horas_trabalho/pessoas_ocupadas) %>% 
  summarise(pessoas_ocupadas = mean(pessoas_ocupadas),
            horas_trabalho = mean(horas_trabalho),
            media_horas_trabalho = mean(media_horas_trabalho)) %>% 
  saveRDS(file = "tmp/horas_trabalho_homens_15_59_2018.rds")


# horas com trabalho homens com crianças de idosos
total_horas_trabalho_homens = pnadc2018anual %>% 
  filter(VD4002 == 1, V2007 == 1, VD4043 == 1) %>% 
  group_by(UF) %>% 
  mutate(pessoas_ocupadas = sum(V1032), 
         horas_trabalho = sum(V4039C*V1032),
         media_horas_trabalho = horas_trabalho/pessoas_ocupadas) %>% 
  summarise(pessoas_ocupadas = mean(pessoas_ocupadas),
            horas_trabalho = mean(horas_trabalho),
            media_horas_trabalho = mean(media_horas_trabalho)) %>% 
  saveRDS(file = "tmp/horas_trabalho_homens_60_2018.rds")



# horas cuidado e afazeres mulheres de 0 a 14 anos
total_horas_afazeres_mulheres = pnadc2018anual %>% 
  filter(!is.na(V4121B), V2007 == 2, (VD4040 == 1 | VD4041 == 1)) %>% 
  group_by(UF) %>% 
  mutate(pessoas_afazeres = sum(V1032), 
         horas_afazeres = sum(V4121B*V1032),
         media_horas_afazeres = horas_afazeres/pessoas_afazeres) %>% 
  summarise(pessoas_afazeres = mean(pessoas_afazeres),
            horas_afazeres = mean(horas_afazeres),
            media_horas_afazeres = mean(media_horas_afazeres)) %>% 
  saveRDS(file = "tmp/horas_afazeres_mulheres_0_14_2018.rds")


# horas cuidado e afazeres mulheres de 15 a 59
total_horas_afazeres_mulheres = pnadc2018anual %>% 
  filter(!is.na(V4121B), V2007 == 2, VD4042 == 1) %>% 
  group_by(UF) %>% 
  mutate(pessoas_afazeres = sum(V1032), 
         horas_afazeres = sum(V4121B*V1032),
         media_horas_afazeres = horas_afazeres/pessoas_afazeres) %>% 
  summarise(pessoas_afazeres = mean(pessoas_afazeres),
            horas_afazeres = mean(horas_afazeres),
            media_horas_afazeres = mean(media_horas_afazeres)) %>% 
  saveRDS(file = "tmp/horas_afazeres_mulheres_15_59_2018.rds")


# horas cuidado e afazeres mulheres de idosos
total_horas_afazeres_mulheres = pnadc2018anual %>% 
  filter(!is.na(V4121B), V2007 == 2, VD4043 == 1) %>% 
  group_by(UF) %>% 
  mutate(pessoas_afazeres = sum(V1032), 
         horas_afazeres = sum(V4121B*V1032),
         media_horas_afazeres = horas_afazeres/pessoas_afazeres) %>% 
  summarise(pessoas_afazeres = mean(pessoas_afazeres),
            horas_afazeres = mean(horas_afazeres),
            media_horas_afazeres = mean(media_horas_afazeres)) %>% 
  saveRDS(file = "tmp/horas_afazeres_mulheres_60_2018.rds")



# horas cuidado e afazeres homens de 0 a 14 anos
total_horas_afazeres_homens = pnadc2018anual %>% 
  filter(!is.na(V4121B), V2007 == 1, (VD4040 == 1 | VD4041 == 1)) %>% 
  group_by(UF) %>% 
  mutate(pessoas_afazeres = sum(V1032), 
         horas_afazeres = sum(V4121B*V1032),
         media_horas_afazeres = horas_afazeres/pessoas_afazeres) %>% 
  summarise(pessoas_afazeres = mean(pessoas_afazeres),
            horas_afazeres = mean(horas_afazeres),
            media_horas_afazeres = mean(media_horas_afazeres)) %>% 
  saveRDS(file = "tmp/horas_afazeres_homens_0_14_2018.rds")


# horas cuidado e afazeres homens de 15 a 59
total_horas_afazeres_homens = pnadc2018anual %>% 
  filter(!is.na(V4121B), V2007 == 1, VD4042 == 1) %>% 
  group_by(UF) %>% 
  mutate(pessoas_afazeres = sum(V1032), 
         horas_afazeres = sum(V4121B*V1032),
         media_horas_afazeres = horas_afazeres/pessoas_afazeres) %>% 
  summarise(pessoas_afazeres = mean(pessoas_afazeres),
            horas_afazeres = mean(horas_afazeres),
            media_horas_afazeres = mean(media_horas_afazeres)) %>% 
  saveRDS(file = "tmp/horas_afazeres_homens_15_59_2018.rds")


# horas cuidado e afazeres homens de idosos
total_horas_afazeres_homens = pnadc2018anual %>% 
  filter(!is.na(V4121B), V2007 == 1, VD4043 == 1) %>% 
  group_by(UF) %>% 
  mutate(pessoas_afazeres = sum(V1032), 
         horas_afazeres = sum(V4121B*V1032),
         media_horas_afazeres = horas_afazeres/pessoas_afazeres) %>% 
  summarise(pessoas_afazeres = mean(pessoas_afazeres),
            horas_afazeres = mean(horas_afazeres),
            media_horas_afazeres = mean(media_horas_afazeres)) %>% 
  saveRDS(file = "tmp/horas_afazeres_homens_60_2018.rds")






#######################################################################################



############################## pnad continua anual 2017 ###############################


pnadc2017anual = read_dta(file = "input/PNADC_anual_2017_visita5.dta") 


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

pnadc2017anual = pnadc2017anual %>% select(all_of(variaveis_escolhidas))


# coluna de formal ou informal
pnadc2017anual = pnadc2017anual %>% 
  mutate(formalidade = case_when(VD4009 == 1 ~ "formal",
                                 VD4009 == 2 ~ "informal",
                                 VD4009 == 3 ~ "formal",
                                 VD4009 == 4 ~ "informal",
                                 VD4009 == 5 ~ "formal",
                                 VD4009 == 6 ~ "informal",
                                 VD4009 == 7 ~ "formal",
                                 VD4009 == 8 ~ "autonomo",
                                 VD4009 == 9 ~ "autonomo",
                                 VD4009 == 10 ~ "informal"
                                 )) %>% 
  mutate(autonomo_formal = ifelse(formalidade == "autonomo" & VD4012 == 1,1,2)) %>% 
  mutate(dummy_formalidade = ifelse(VD4002 == 1 & (formalidade == "formal" | (formalidade == "autonomo" & autonomo_formal == 1)), 1,
                                    ifelse(VD4002 == 1, 2, 0)))




############## trabalho ############## 

# horas com trabalho 
total_horas_trabalho = pnadc2017anual %>% 
  filter(VD4002 == 1) %>% 
  group_by(UF) %>% 
  mutate(pessoas_ocupadas = sum(V1032), 
         horas_trabalho = sum(V4039C*V1032),
         media_horas_trabalho = horas_trabalho/pessoas_ocupadas) %>% 
  summarise(pessoas_ocupadas = mean(pessoas_ocupadas),
            horas_trabalho = mean(horas_trabalho),
            media_horas_trabalho = mean(media_horas_trabalho)) %>% 
  saveRDS(file = "tmp/horas_trabalho_2017.rds")



# horas com trabalho mulheres
total_horas_trabalho_mulheres = pnadc2017anual %>% 
  filter(VD4002 == 1, V2007 == 2) %>% 
  group_by(UF) %>% 
  mutate(pessoas_ocupadas = sum(V1032), 
         horas_trabalho = sum(V4039C*V1032),
         media_horas_trabalho = horas_trabalho/pessoas_ocupadas) %>% 
  summarise(pessoas_ocupadas = mean(pessoas_ocupadas),
            horas_trabalho = mean(horas_trabalho),
            media_horas_trabalho = mean(media_horas_trabalho)) %>% 
  saveRDS(file = "tmp/horas_trabalho_mulheres_2017.rds")


#horas com trabalho mulheres brancas
total_horas_trabalho_mulheres_brancas = pnadc2017anual %>% 
  filter(VD4002 == 1, V2007 == 2, V2010 == 1) %>% 
  group_by(UF) %>% 
  mutate(pessoas_ocupadas = sum(V1032), 
         horas_trabalho = sum(V4039C*V1032),
         media_horas_trabalho = horas_trabalho/pessoas_ocupadas) %>% 
  summarise(pessoas_ocupadas = mean(pessoas_ocupadas),
            horas_trabalho = mean(horas_trabalho),
            media_horas_trabalho = mean(media_horas_trabalho)) %>% 
  saveRDS(file = "tmp/horas_trabalho_mulheres_brancas_2017.rds")


#horas com trabalho mulheres não brancas
total_horas_trabalho_mulheres_nao_brancas = pnadc2017anual %>% 
  filter(VD4002 == 1, V2007 == 2, !V2010 == 1) %>% 
  group_by(UF) %>% 
  mutate(pessoas_ocupadas = sum(V1032), 
         horas_trabalho = sum(V4039C*V1032),
         media_horas_trabalho = horas_trabalho/pessoas_ocupadas) %>% 
  summarise(pessoas_ocupadas = mean(pessoas_ocupadas),
            horas_trabalho = mean(horas_trabalho),
            media_horas_trabalho = mean(media_horas_trabalho)) %>% 
  saveRDS(file = "tmp/horas_trabalho_mulheres_nao_brancas_2017.rds")



# horas com trabalho mulheres formais
total_horas_trabalho_mulheres = pnadc2017anual %>% 
  filter(VD4002 == 1, V2007 == 2, dummy_formalidade == 1) %>% 
  group_by(UF) %>% 
  mutate(pessoas_ocupadas = sum(V1032), 
         horas_trabalho = sum(V4039C*V1032),
         media_horas_trabalho = horas_trabalho/pessoas_ocupadas) %>% 
  summarise(pessoas_ocupadas = mean(pessoas_ocupadas),
            horas_trabalho = mean(horas_trabalho),
            media_horas_trabalho = mean(media_horas_trabalho)) %>% 
  saveRDS(file = "tmp/horas_trabalho_mulheres_formal_2017.rds")


# horas com trabalho mulheres informais
total_horas_trabalho_mulheres = pnadc2017anual %>% 
  filter(VD4002 == 1, V2007 == 2, dummy_formalidade == 2) %>% 
  group_by(UF) %>% 
  mutate(pessoas_ocupadas = sum(V1032), 
         horas_trabalho = sum(V4039C*V1032),
         media_horas_trabalho = horas_trabalho/pessoas_ocupadas) %>% 
  summarise(pessoas_ocupadas = mean(pessoas_ocupadas),
            horas_trabalho = mean(horas_trabalho),
            media_horas_trabalho = mean(media_horas_trabalho)) %>% 
  saveRDS(file = "tmp/horas_trabalho_mulheres_informal_2017.rds")


# horas com trabalho homens
total_horas_trabalho_homens = pnadc2017anual %>% 
  filter(VD4002 == 1, V2007 == 1) %>% 
  group_by(UF) %>% 
  mutate(pessoas_ocupadas = sum(V1032), 
         horas_trabalho = sum(V4039C*V1032),
         media_horas_trabalho = horas_trabalho/pessoas_ocupadas) %>% 
  summarise(pessoas_ocupadas = mean(pessoas_ocupadas),
            horas_trabalho = mean(horas_trabalho),
            media_horas_trabalho = mean(media_horas_trabalho)) %>% 
  saveRDS(file = "tmp/horas_trabalho_homens_2017.rds")


# horas com trabalho homens brancos
total_horas_trabalho_homens_brancos = pnadc2017anual %>% 
  filter(VD4002 == 1, V2007 == 1, V2010 == 1) %>% 
  group_by(UF) %>% 
  mutate(pessoas_ocupadas = sum(V1032), 
         horas_trabalho = sum(V4039C*V1032),
         media_horas_trabalho = horas_trabalho/pessoas_ocupadas) %>% 
  summarise(pessoas_ocupadas = mean(pessoas_ocupadas),
            horas_trabalho = mean(horas_trabalho),
            media_horas_trabalho = mean(media_horas_trabalho)) %>% 
  saveRDS(file = "tmp/horas_trabalho_homens_brancos_2017.rds")


# horas com trabalho homens não brancos
total_horas_trabalho_homens_nao_brancos = pnadc2017anual %>% 
  filter(VD4002 == 1, V2007 == 1, !V2010 == 1) %>% 
  group_by(UF) %>% 
  mutate(pessoas_ocupadas = sum(V1032), 
         horas_trabalho = sum(V4039C*V1032),
         media_horas_trabalho = horas_trabalho/pessoas_ocupadas) %>% 
  summarise(pessoas_ocupadas = mean(pessoas_ocupadas),
            horas_trabalho = mean(horas_trabalho),
            media_horas_trabalho = mean(media_horas_trabalho)) %>% 
  saveRDS(file = "tmp/horas_trabalho_homens_nao_brancos_2017.rds")


# formalidade


# horas com trabalho homens formal
total_horas_trabalho_homens = pnadc2017anual %>% 
  filter(VD4002 == 1, V2007 == 1, dummy_formalidade == 1) %>% 
  group_by(UF) %>% 
  mutate(pessoas_ocupadas = sum(V1032), 
         horas_trabalho = sum(V4039C*V1032),
         media_horas_trabalho = horas_trabalho/pessoas_ocupadas) %>% 
  summarise(pessoas_ocupadas = mean(pessoas_ocupadas),
            horas_trabalho = mean(horas_trabalho),
            media_horas_trabalho = mean(media_horas_trabalho)) %>% 
  saveRDS(file = "tmp/horas_trabalho_homens_formal_2017.rds")


# horas com trabalho homens informal
total_horas_trabalho_homens = pnadc2017anual %>% 
  filter(VD4002 == 1, V2007 == 1, dummy_formalidade == 2) %>% 
  group_by(UF) %>% 
  mutate(pessoas_ocupadas = sum(V1032), 
         horas_trabalho = sum(V4039C*V1032),
         media_horas_trabalho = horas_trabalho/pessoas_ocupadas) %>% 
  summarise(pessoas_ocupadas = mean(pessoas_ocupadas),
            horas_trabalho = mean(horas_trabalho),
            media_horas_trabalho = mean(media_horas_trabalho)) %>% 
  saveRDS(file = "tmp/horas_trabalho_homens_informal_2017.rds")


############## 


############## cuidado e afazeres ############## 


# horas cuidado e afazeres
total_horas_afazeres = pnadc2017anual %>% 
  filter(!is.na(V4121B)) %>% 
  group_by(UF) %>% 
  mutate(pessoas_afazeres = sum(V1032), 
         horas_afazeres = sum(V4121B*V1032),
         media_horas_afazeres = horas_afazeres/pessoas_afazeres) %>% 
  summarise(pessoas_afazeres = mean(pessoas_afazeres),
            horas_afazeres = mean(horas_afazeres),
            media_horas_afazeres = mean(media_horas_afazeres)) %>% 
  saveRDS(file = "tmp/horas_afazeres_2017.rds")


# horas cuidado e afazeres mulheres
total_horas_afazeres_mulheres = pnadc2017anual %>% 
  filter(!is.na(V4121B), V2007 == 2) %>% 
  group_by(UF) %>% 
  mutate(pessoas_afazeres = sum(V1032), 
         horas_afazeres = sum(V4121B*V1032),
         media_horas_afazeres = horas_afazeres/pessoas_afazeres) %>% 
  summarise(pessoas_afazeres = mean(pessoas_afazeres),
            horas_afazeres = mean(horas_afazeres),
            media_horas_afazeres = mean(media_horas_afazeres)) %>% 
  saveRDS(file = "tmp/horas_afazeres_mulheres_2017.rds")


# horas cuidado e afazeres mulheres brancas
total_horas_afazeres_mulheres_brancas = pnadc2017anual %>% 
  filter(!is.na(V4121B), V2007 == 2, V2010 == 1) %>% 
  group_by(UF) %>% 
  mutate(pessoas_afazeres = sum(V1032), 
         horas_afazeres = sum(V4121B*V1032),
         media_horas_afazeres = horas_afazeres/pessoas_afazeres) %>% 
  summarise(pessoas_afazeres = mean(pessoas_afazeres),
            horas_afazeres = mean(horas_afazeres),
            media_horas_afazeres = mean(media_horas_afazeres)) %>% 
  saveRDS(file = "tmp/horas_afazeres_mulheres_brancas_2017.rds")


# horas cuidado e afazeres mulheres não brancas
total_horas_afazeres_mulheres_brancas = pnadc2017anual %>% 
  filter(!is.na(V4121B), V2007 == 2, !V2010 == 1) %>% 
  group_by(UF) %>% 
  mutate(pessoas_afazeres = sum(V1032), 
         horas_afazeres = sum(V4121B*V1032),
         media_horas_afazeres = horas_afazeres/pessoas_afazeres) %>% 
  summarise(pessoas_afazeres = mean(pessoas_afazeres),
            horas_afazeres = mean(horas_afazeres),
            media_horas_afazeres = mean(media_horas_afazeres)) %>% 
  saveRDS(file = "tmp/horas_afazeres_mulheres_nao_brancas_2017.rds")

# formalidade

# horas cuidado e afazeres mulheres formal
total_horas_afazeres_mulheres = pnadc2017anual %>% 
  filter(!is.na(V4121B), V2007 == 2, dummy_formalidade == 1) %>% 
  group_by(UF) %>% 
  mutate(pessoas_afazeres = sum(V1032), 
         horas_afazeres = sum(V4121B*V1032),
         media_horas_afazeres = horas_afazeres/pessoas_afazeres) %>% 
  summarise(pessoas_afazeres = mean(pessoas_afazeres),
            horas_afazeres = mean(horas_afazeres),
            media_horas_afazeres = mean(media_horas_afazeres)) %>% 
  saveRDS(file = "tmp/horas_afazeres_mulheres_formal_2017.rds")


# horas cuidado e afazeres mulheres informal
total_horas_afazeres_mulheres = pnadc2017anual %>% 
  filter(!is.na(V4121B), V2007 == 2, dummy_formalidade == 2) %>% 
  group_by(UF) %>% 
  mutate(pessoas_afazeres = sum(V1032), 
         horas_afazeres = sum(V4121B*V1032),
         media_horas_afazeres = horas_afazeres/pessoas_afazeres) %>% 
  summarise(pessoas_afazeres = mean(pessoas_afazeres),
            horas_afazeres = mean(horas_afazeres),
            media_horas_afazeres = mean(media_horas_afazeres)) %>% 
  saveRDS(file = "tmp/horas_afazeres_mulheres_informal_2017.rds")

#

# horas cuidado e afazeres homens
total_horas_afazeres_homens = pnadc2017anual %>% 
  filter(!is.na(V4121B), V2007 == 1) %>% 
  group_by(UF) %>% 
  mutate(pessoas_afazeres = sum(V1032), 
         horas_afazeres = sum(V4121B*V1032),
         media_horas_afazeres = horas_afazeres/pessoas_afazeres) %>% 
  summarise(pessoas_afazeres = mean(pessoas_afazeres),
            horas_afazeres = mean(horas_afazeres),
            media_horas_afazeres = mean(media_horas_afazeres)) %>% 
  saveRDS(file = "tmp/horas_afazeres_homens_2017.rds")



# horas cuidado e afazeres homens brancos
total_horas_afazeres_homens = pnadc2017anual %>% 
  filter(!is.na(V4121B), V2007 == 1, V2010 == 1) %>% 
  group_by(UF) %>% 
  mutate(pessoas_afazeres = sum(V1032), 
         horas_afazeres = sum(V4121B*V1032),
         media_horas_afazeres = horas_afazeres/pessoas_afazeres) %>% 
  summarise(pessoas_afazeres = mean(pessoas_afazeres),
            horas_afazeres = mean(horas_afazeres),
            media_horas_afazeres = mean(media_horas_afazeres)) %>% 
  saveRDS(file = "tmp/horas_afazeres_homens_brancos_2017.rds")


# horas cuidado e afazeres homens não brancos
total_horas_afazeres_homens = pnadc2017anual %>% 
  filter(!is.na(V4121B), V2007 == 1, !V2010 == 1) %>% 
  group_by(UF) %>% 
  mutate(pessoas_afazeres = sum(V1032), 
         horas_afazeres = sum(V4121B*V1032),
         media_horas_afazeres = horas_afazeres/pessoas_afazeres) %>% 
  summarise(pessoas_afazeres = mean(pessoas_afazeres),
            horas_afazeres = mean(horas_afazeres),
            media_horas_afazeres = mean(media_horas_afazeres)) %>% 
  saveRDS(file = "tmp/horas_afazeres_homens_nao_brancos_2017.rds")

# formalidade

# horas cuidado e afazeres homens formal
total_horas_afazeres_homens = pnadc2017anual %>% 
  filter(!is.na(V4121B), V2007 == 1, dummy_formalidade == 1) %>% 
  group_by(UF) %>% 
  mutate(pessoas_afazeres = sum(V1032), 
         horas_afazeres = sum(V4121B*V1032),
         media_horas_afazeres = horas_afazeres/pessoas_afazeres) %>% 
  summarise(pessoas_afazeres = mean(pessoas_afazeres),
            horas_afazeres = mean(horas_afazeres),
            media_horas_afazeres = mean(media_horas_afazeres)) %>% 
  saveRDS(file = "tmp/horas_afazeres_homens_formal_2017.rds")



# horas cuidado e afazeres homens informal
total_horas_afazeres_homens = pnadc2017anual %>% 
  filter(!is.na(V4121B), V2007 == 1, dummy_formalidade == 2) %>% 
  group_by(UF) %>% 
  mutate(pessoas_afazeres = sum(V1032), 
         horas_afazeres = sum(V4121B*V1032),
         media_horas_afazeres = horas_afazeres/pessoas_afazeres) %>% 
  summarise(pessoas_afazeres = mean(pessoas_afazeres),
            horas_afazeres = mean(horas_afazeres),
            media_horas_afazeres = mean(media_horas_afazeres)) %>% 
  saveRDS(file = "tmp/horas_afazeres_homens_informal_2017.rds")

#


# faixa etaria da pessoa cuidada 

# horas com trabalho mulheres com crianças de 0 a 14 anos
total_horas_trabalho_mulheres = pnadc2017anual %>% 
  filter(VD4002 == 1, V2007 == 2, (VD4040 == 1 | VD4041 == 1)) %>% 
  group_by(UF) %>% 
  mutate(pessoas_ocupadas = sum(V1032), 
         horas_trabalho = sum(V4039C*V1032),
         media_horas_trabalho = horas_trabalho/pessoas_ocupadas) %>% 
  summarise(pessoas_ocupadas = mean(pessoas_ocupadas),
            horas_trabalho = mean(horas_trabalho),
            media_horas_trabalho = mean(media_horas_trabalho)) %>% 
  saveRDS(file = "tmp/horas_trabalho_mulheres_0_14_2017.rds")


# horas com trabalho mulheres com crianças de 15 a 59 anos
total_horas_trabalho_mulheres = pnadc2017anual %>% 
  filter(VD4002 == 1, V2007 == 2, VD4042 == 1) %>% 
  group_by(UF) %>% 
  mutate(pessoas_ocupadas = sum(V1032), 
         horas_trabalho = sum(V4039C*V1032),
         media_horas_trabalho = horas_trabalho/pessoas_ocupadas) %>% 
  summarise(pessoas_ocupadas = mean(pessoas_ocupadas),
            horas_trabalho = mean(horas_trabalho),
            media_horas_trabalho = mean(media_horas_trabalho)) %>% 
  saveRDS(file = "tmp/horas_trabalho_mulheres_15_59_2017.rds")


# horas com trabalho mulheres com crianças de idosos
total_horas_trabalho_mulheres = pnadc2017anual %>% 
  filter(VD4002 == 1, V2007 == 2, VD4043 == 1) %>% 
  group_by(UF) %>% 
  mutate(pessoas_ocupadas = sum(V1032), 
         horas_trabalho = sum(V4039C*V1032),
         media_horas_trabalho = horas_trabalho/pessoas_ocupadas) %>% 
  summarise(pessoas_ocupadas = mean(pessoas_ocupadas),
            horas_trabalho = mean(horas_trabalho),
            media_horas_trabalho = mean(media_horas_trabalho)) %>% 
  saveRDS(file = "tmp/horas_trabalho_mulheres_60_2017.rds")


# horas com trabalho homens com crianças de 0 a 14 anos
total_horas_trabalho_homens = pnadc2017anual %>% 
  filter(VD4002 == 1, V2007 == 1, (VD4040 == 1 | VD4041 == 1)) %>% 
  group_by(UF) %>% 
  mutate(pessoas_ocupadas = sum(V1032), 
         horas_trabalho = sum(V4039C*V1032),
         media_horas_trabalho = horas_trabalho/pessoas_ocupadas) %>% 
  summarise(pessoas_ocupadas = mean(pessoas_ocupadas),
            horas_trabalho = mean(horas_trabalho),
            media_horas_trabalho = mean(media_horas_trabalho)) %>% 
  saveRDS(file = "tmp/horas_trabalho_homens_0_14_2017.rds")


# horas com trabalho homens com crianças de 15 a 59 anos
total_horas_trabalho_homens = pnadc2017anual %>% 
  filter(VD4002 == 1, V2007 == 1, VD4042 == 1) %>% 
  group_by(UF) %>% 
  mutate(pessoas_ocupadas = sum(V1032), 
         horas_trabalho = sum(V4039C*V1032),
         media_horas_trabalho = horas_trabalho/pessoas_ocupadas) %>% 
  summarise(pessoas_ocupadas = mean(pessoas_ocupadas),
            horas_trabalho = mean(horas_trabalho),
            media_horas_trabalho = mean(media_horas_trabalho)) %>% 
  saveRDS(file = "tmp/horas_trabalho_homens_15_59_2017.rds")


# horas com trabalho homens com crianças de idosos
total_horas_trabalho_homens = pnadc2017anual %>% 
  filter(VD4002 == 1, V2007 == 1, VD4043 == 1) %>% 
  group_by(UF) %>% 
  mutate(pessoas_ocupadas = sum(V1032), 
         horas_trabalho = sum(V4039C*V1032),
         media_horas_trabalho = horas_trabalho/pessoas_ocupadas) %>% 
  summarise(pessoas_ocupadas = mean(pessoas_ocupadas),
            horas_trabalho = mean(horas_trabalho),
            media_horas_trabalho = mean(media_horas_trabalho)) %>% 
  saveRDS(file = "tmp/horas_trabalho_homens_60_2017.rds")



# horas cuidado e afazeres mulheres de 0 a 14 anos
total_horas_afazeres_mulheres = pnadc2017anual %>% 
  filter(!is.na(V4121B), V2007 == 2, (VD4040 == 1 | VD4041 == 1)) %>% 
  group_by(UF) %>% 
  mutate(pessoas_afazeres = sum(V1032), 
         horas_afazeres = sum(V4121B*V1032),
         media_horas_afazeres = horas_afazeres/pessoas_afazeres) %>% 
  summarise(pessoas_afazeres = mean(pessoas_afazeres),
            horas_afazeres = mean(horas_afazeres),
            media_horas_afazeres = mean(media_horas_afazeres)) %>% 
  saveRDS(file = "tmp/horas_afazeres_mulheres_0_14_2017.rds")


# horas cuidado e afazeres mulheres de 15 a 59
total_horas_afazeres_mulheres = pnadc2017anual %>% 
  filter(!is.na(V4121B), V2007 == 2, VD4042 == 1) %>% 
  group_by(UF) %>% 
  mutate(pessoas_afazeres = sum(V1032), 
         horas_afazeres = sum(V4121B*V1032),
         media_horas_afazeres = horas_afazeres/pessoas_afazeres) %>% 
  summarise(pessoas_afazeres = mean(pessoas_afazeres),
            horas_afazeres = mean(horas_afazeres),
            media_horas_afazeres = mean(media_horas_afazeres)) %>% 
  saveRDS(file = "tmp/horas_afazeres_mulheres_15_59_2017.rds")


# horas cuidado e afazeres mulheres de idosos
total_horas_afazeres_mulheres = pnadc2017anual %>% 
  filter(!is.na(V4121B), V2007 == 2, VD4043 == 1) %>% 
  group_by(UF) %>% 
  mutate(pessoas_afazeres = sum(V1032), 
         horas_afazeres = sum(V4121B*V1032),
         media_horas_afazeres = horas_afazeres/pessoas_afazeres) %>% 
  summarise(pessoas_afazeres = mean(pessoas_afazeres),
            horas_afazeres = mean(horas_afazeres),
            media_horas_afazeres = mean(media_horas_afazeres)) %>% 
  saveRDS(file = "tmp/horas_afazeres_mulheres_60_2017.rds")



# horas cuidado e afazeres homens de 0 a 14 anos
total_horas_afazeres_homens = pnadc2017anual %>% 
  filter(!is.na(V4121B), V2007 == 1, (VD4040 == 1 | VD4041 == 1)) %>% 
  group_by(UF) %>% 
  mutate(pessoas_afazeres = sum(V1032), 
         horas_afazeres = sum(V4121B*V1032),
         media_horas_afazeres = horas_afazeres/pessoas_afazeres) %>% 
  summarise(pessoas_afazeres = mean(pessoas_afazeres),
            horas_afazeres = mean(horas_afazeres),
            media_horas_afazeres = mean(media_horas_afazeres)) %>% 
  saveRDS(file = "tmp/horas_afazeres_homens_0_14_2017.rds")


# horas cuidado e afazeres homens de 15 a 59
total_horas_afazeres_homens = pnadc2017anual %>% 
  filter(!is.na(V4121B), V2007 == 1, VD4042 == 1) %>% 
  group_by(UF) %>% 
  mutate(pessoas_afazeres = sum(V1032), 
         horas_afazeres = sum(V4121B*V1032),
         media_horas_afazeres = horas_afazeres/pessoas_afazeres) %>% 
  summarise(pessoas_afazeres = mean(pessoas_afazeres),
            horas_afazeres = mean(horas_afazeres),
            media_horas_afazeres = mean(media_horas_afazeres)) %>% 
  saveRDS(file = "tmp/horas_afazeres_homens_15_59_2017.rds")


# horas cuidado e afazeres homens de idosos
total_horas_afazeres_homens = pnadc2017anual %>% 
  filter(!is.na(V4121B), V2007 == 1, VD4043 == 1) %>% 
  group_by(UF) %>% 
  mutate(pessoas_afazeres = sum(V1032), 
         horas_afazeres = sum(V4121B*V1032),
         media_horas_afazeres = horas_afazeres/pessoas_afazeres) %>% 
  summarise(pessoas_afazeres = mean(pessoas_afazeres),
            horas_afazeres = mean(horas_afazeres),
            media_horas_afazeres = mean(media_horas_afazeres)) %>% 
  saveRDS(file = "tmp/horas_afazeres_homens_60_2017.rds")





#######################################################################################



############################## pnad continua anual 2016 ###############################


pnadc2016anual = read_dta(file = "input/PNADC_anual_2016_visita5.dta") 


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

pnadc2016anual = pnadc2016anual %>% select(all_of(variaveis_escolhidas))


# coluna de formal ou informal
pnadc2016anual = pnadc2016anual %>% 
  mutate(formalidade = case_when(VD4009 == 1 ~ "formal",
                                 VD4009 == 2 ~ "informal",
                                 VD4009 == 3 ~ "formal",
                                 VD4009 == 4 ~ "informal",
                                 VD4009 == 5 ~ "formal",
                                 VD4009 == 6 ~ "informal",
                                 VD4009 == 7 ~ "formal",
                                 VD4009 == 8 ~ "autonomo",
                                 VD4009 == 9 ~ "autonomo",
                                 VD4009 == 10 ~ "informal"
                                 )) %>% 
  mutate(autonomo_formal = ifelse(formalidade == "autonomo" & VD4012 == 1,1,2)) %>% 
  mutate(dummy_formalidade = ifelse(VD4002 == 1 & (formalidade == "formal" | (formalidade == "autonomo" & autonomo_formal == 1)), 1,
                                    ifelse(VD4002 == 1, 2, 0)))




############## trabalho ############## 

# horas com trabalho 
total_horas_trabalho = pnadc2016anual %>% 
  filter(VD4002 == 1) %>% 
  group_by(UF) %>% 
  mutate(pessoas_ocupadas = sum(V1032), 
         horas_trabalho = sum(V4039C*V1032, na.rm = T),
         media_horas_trabalho = horas_trabalho/pessoas_ocupadas) %>% 
  summarise(pessoas_ocupadas = mean(pessoas_ocupadas),
            horas_trabalho = mean(horas_trabalho),
            media_horas_trabalho = mean(media_horas_trabalho)) %>% 
  saveRDS(file = "tmp/horas_trabalho_2016.rds")



# horas com trabalho mulheres
total_horas_trabalho_mulheres = pnadc2016anual %>% 
  filter(VD4002 == 1, V2007 == 2) %>% 
  group_by(UF) %>% 
  mutate(pessoas_ocupadas = sum(V1032), 
         horas_trabalho = sum(V4039C*V1032, na.rm = T),
         media_horas_trabalho = horas_trabalho/pessoas_ocupadas) %>% 
  summarise(pessoas_ocupadas = mean(pessoas_ocupadas, na.rm = T),
            horas_trabalho = mean(horas_trabalho, na.rm = T),
            media_horas_trabalho = mean(media_horas_trabalho)) %>% 
  saveRDS(file = "tmp/horas_trabalho_mulheres_2016.rds")


#horas com trabalho mulheres brancas
total_horas_trabalho_mulheres_brancas = pnadc2016anual %>% 
  filter(VD4002 == 1, V2007 == 2, V2010 == 1) %>% 
  group_by(UF) %>% 
  mutate(pessoas_ocupadas = sum(V1032), 
         horas_trabalho = sum(V4039C*V1032, na.rm = T),
         media_horas_trabalho = horas_trabalho/pessoas_ocupadas) %>% 
  summarise(pessoas_ocupadas = mean(pessoas_ocupadas),
            horas_trabalho = mean(horas_trabalho),
            media_horas_trabalho = mean(media_horas_trabalho)) %>% 
  saveRDS(file = "tmp/horas_trabalho_mulheres_brancas_2016.rds")


#horas com trabalho mulheres não brancas
total_horas_trabalho_mulheres_nao_brancas = pnadc2016anual %>% 
  filter(VD4002 == 1, V2007 == 2, !V2010 == 1) %>% 
  group_by(UF) %>% 
  mutate(pessoas_ocupadas = sum(V1032), 
         horas_trabalho = sum(V4039C*V1032, na.rm = T),
         media_horas_trabalho = horas_trabalho/pessoas_ocupadas) %>% 
  summarise(pessoas_ocupadas = mean(pessoas_ocupadas),
            horas_trabalho = mean(horas_trabalho),
            media_horas_trabalho = mean(media_horas_trabalho)) %>% 
  saveRDS(file = "tmp/horas_trabalho_mulheres_nao_brancas_2016.rds")



# horas com trabalho mulheres formais
total_horas_trabalho_mulheres = pnadc2016anual %>% 
  filter(VD4002 == 1, V2007 == 2, dummy_formalidade == 1) %>% 
  group_by(UF) %>% 
  mutate(pessoas_ocupadas = sum(V1032), 
         horas_trabalho = sum(V4039C*V1032, na.rm = T),
         media_horas_trabalho = horas_trabalho/pessoas_ocupadas) %>% 
  summarise(pessoas_ocupadas = mean(pessoas_ocupadas),
            horas_trabalho = mean(horas_trabalho),
            media_horas_trabalho = mean(media_horas_trabalho)) %>% 
  saveRDS(file = "tmp/horas_trabalho_mulheres_formal_2016.rds")


# horas com trabalho mulheres informais
total_horas_trabalho_mulheres = pnadc2016anual %>% 
  filter(VD4002 == 1, V2007 == 2, dummy_formalidade == 2) %>% 
  group_by(UF) %>% 
  mutate(pessoas_ocupadas = sum(V1032), 
         horas_trabalho = sum(V4039C*V1032, na.rm = T),
         media_horas_trabalho = horas_trabalho/pessoas_ocupadas) %>% 
  summarise(pessoas_ocupadas = mean(pessoas_ocupadas),
            horas_trabalho = mean(horas_trabalho),
            media_horas_trabalho = mean(media_horas_trabalho)) %>% 
  saveRDS(file = "tmp/horas_trabalho_mulheres_informal_2016.rds")


# horas com trabalho homens
total_horas_trabalho_homens = pnadc2016anual %>% 
  filter(VD4002 == 1, V2007 == 1) %>% 
  group_by(UF) %>% 
  mutate(pessoas_ocupadas = sum(V1032), 
         horas_trabalho = sum(V4039C*V1032, na.rm = T),
         media_horas_trabalho = horas_trabalho/pessoas_ocupadas) %>% 
  summarise(pessoas_ocupadas = mean(pessoas_ocupadas),
            horas_trabalho = mean(horas_trabalho),
            media_horas_trabalho = mean(media_horas_trabalho)) %>% 
  saveRDS(file = "tmp/horas_trabalho_homens_2016.rds")


# horas com trabalho homens brancos
total_horas_trabalho_homens_brancos = pnadc2016anual %>% 
  filter(VD4002 == 1, V2007 == 1, V2010 == 1) %>% 
  group_by(UF) %>% 
  mutate(pessoas_ocupadas = sum(V1032), 
         horas_trabalho = sum(V4039C*V1032, na.rm = T),
         media_horas_trabalho = horas_trabalho/pessoas_ocupadas) %>% 
  summarise(pessoas_ocupadas = mean(pessoas_ocupadas),
            horas_trabalho = mean(horas_trabalho),
            media_horas_trabalho = mean(media_horas_trabalho)) %>% 
  saveRDS(file = "tmp/horas_trabalho_homens_brancos_2016.rds")


# horas com trabalho homens não brancos
total_horas_trabalho_homens_nao_brancos = pnadc2016anual %>% 
  filter(VD4002 == 1, V2007 == 1, !V2010 == 1) %>% 
  group_by(UF) %>% 
  mutate(pessoas_ocupadas = sum(V1032), 
         horas_trabalho = sum(V4039C*V1032, na.rm = T),
         media_horas_trabalho = horas_trabalho/pessoas_ocupadas) %>% 
  summarise(pessoas_ocupadas = mean(pessoas_ocupadas),
            horas_trabalho = mean(horas_trabalho),
            media_horas_trabalho = mean(media_horas_trabalho)) %>% 
  saveRDS(file = "tmp/horas_trabalho_homens_nao_brancos_2016.rds")


# formalidade


# horas com trabalho homens formal
total_horas_trabalho_homens = pnadc2016anual %>% 
  filter(VD4002 == 1, V2007 == 1, dummy_formalidade == 1) %>% 
  group_by(UF) %>% 
  mutate(pessoas_ocupadas = sum(V1032), 
         horas_trabalho = sum(V4039C*V1032, na.rm = T),
         media_horas_trabalho = horas_trabalho/pessoas_ocupadas) %>% 
  summarise(pessoas_ocupadas = mean(pessoas_ocupadas),
            horas_trabalho = mean(horas_trabalho),
            media_horas_trabalho = mean(media_horas_trabalho)) %>% 
  saveRDS(file = "tmp/horas_trabalho_homens_formal_2016.rds")


# horas com trabalho homens informal
total_horas_trabalho_homens = pnadc2016anual %>% 
  filter(VD4002 == 1, V2007 == 1, dummy_formalidade == 2) %>% 
  group_by(UF) %>% 
  mutate(pessoas_ocupadas = sum(V1032), 
         horas_trabalho = sum(V4039C*V1032, na.rm = T),
         media_horas_trabalho = horas_trabalho/pessoas_ocupadas) %>% 
  summarise(pessoas_ocupadas = mean(pessoas_ocupadas),
            horas_trabalho = mean(horas_trabalho),
            media_horas_trabalho = mean(media_horas_trabalho)) %>% 
  saveRDS(file = "tmp/horas_trabalho_homens_informal_2016.rds")


############## 


############## cuidado e afazeres ############## 


# horas cuidado e afazeres
total_horas_afazeres = pnadc2016anual %>% 
  filter(!is.na(V4121B)) %>% 
  group_by(UF) %>% 
  mutate(pessoas_afazeres = sum(V1032), 
         horas_afazeres = sum(V4121B*V1032),
         media_horas_afazeres = horas_afazeres/pessoas_afazeres) %>% 
  summarise(pessoas_afazeres = mean(pessoas_afazeres),
            horas_afazeres = mean(horas_afazeres),
            media_horas_afazeres = mean(media_horas_afazeres)) %>% 
  saveRDS(file = "tmp/horas_afazeres_2016.rds")


# horas cuidado e afazeres mulheres
total_horas_afazeres_mulheres = pnadc2016anual %>% 
  filter(!is.na(V4121B), V2007 == 2) %>% 
  group_by(UF) %>% 
  mutate(pessoas_afazeres = sum(V1032), 
         horas_afazeres = sum(V4121B*V1032),
         media_horas_afazeres = horas_afazeres/pessoas_afazeres) %>% 
  summarise(pessoas_afazeres = mean(pessoas_afazeres),
            horas_afazeres = mean(horas_afazeres),
            media_horas_afazeres = mean(media_horas_afazeres)) %>% 
  saveRDS(file = "tmp/horas_afazeres_mulheres_2016.rds")


# horas cuidado e afazeres mulheres brancas
total_horas_afazeres_mulheres_brancas = pnadc2016anual %>% 
  filter(!is.na(V4121B), V2007 == 2, V2010 == 1) %>% 
  group_by(UF) %>% 
  mutate(pessoas_afazeres = sum(V1032), 
         horas_afazeres = sum(V4121B*V1032),
         media_horas_afazeres = horas_afazeres/pessoas_afazeres) %>% 
  summarise(pessoas_afazeres = mean(pessoas_afazeres),
            horas_afazeres = mean(horas_afazeres),
            media_horas_afazeres = mean(media_horas_afazeres)) %>% 
  saveRDS(file = "tmp/horas_afazeres_mulheres_brancas_2016.rds")


# horas cuidado e afazeres mulheres não brancas
total_horas_afazeres_mulheres_brancas = pnadc2016anual %>% 
  filter(!is.na(V4121B), V2007 == 2, !V2010 == 1) %>% 
  group_by(UF) %>% 
  mutate(pessoas_afazeres = sum(V1032), 
         horas_afazeres = sum(V4121B*V1032),
         media_horas_afazeres = horas_afazeres/pessoas_afazeres) %>% 
  summarise(pessoas_afazeres = mean(pessoas_afazeres),
            horas_afazeres = mean(horas_afazeres),
            media_horas_afazeres = mean(media_horas_afazeres)) %>% 
  saveRDS(file = "tmp/horas_afazeres_mulheres_nao_brancas_2016.rds")

# formalidade

# horas cuidado e afazeres mulheres formal
total_horas_afazeres_mulheres = pnadc2016anual %>% 
  filter(!is.na(V4121B), V2007 == 2, dummy_formalidade == 1) %>% 
  group_by(UF) %>% 
  mutate(pessoas_afazeres = sum(V1032), 
         horas_afazeres = sum(V4121B*V1032),
         media_horas_afazeres = horas_afazeres/pessoas_afazeres) %>% 
  summarise(pessoas_afazeres = mean(pessoas_afazeres),
            horas_afazeres = mean(horas_afazeres),
            media_horas_afazeres = mean(media_horas_afazeres)) %>% 
  saveRDS(file = "tmp/horas_afazeres_mulheres_formal_2016.rds")


# horas cuidado e afazeres mulheres informal
total_horas_afazeres_mulheres = pnadc2016anual %>% 
  filter(!is.na(V4121B), V2007 == 2, dummy_formalidade == 2) %>% 
  group_by(UF) %>% 
  mutate(pessoas_afazeres = sum(V1032), 
         horas_afazeres = sum(V4121B*V1032),
         media_horas_afazeres = horas_afazeres/pessoas_afazeres) %>% 
  summarise(pessoas_afazeres = mean(pessoas_afazeres),
            horas_afazeres = mean(horas_afazeres),
            media_horas_afazeres = mean(media_horas_afazeres)) %>% 
  saveRDS(file = "tmp/horas_afazeres_mulheres_informal_2016.rds")

#

# horas cuidado e afazeres homens
total_horas_afazeres_homens = pnadc2016anual %>% 
  filter(!is.na(V4121B), V2007 == 1) %>% 
  group_by(UF) %>% 
  mutate(pessoas_afazeres = sum(V1032), 
         horas_afazeres = sum(V4121B*V1032),
         media_horas_afazeres = horas_afazeres/pessoas_afazeres) %>% 
  summarise(pessoas_afazeres = mean(pessoas_afazeres),
            horas_afazeres = mean(horas_afazeres),
            media_horas_afazeres = mean(media_horas_afazeres)) %>% 
  saveRDS(file = "tmp/horas_afazeres_homens_2016.rds")



# horas cuidado e afazeres homens brancos
total_horas_afazeres_homens = pnadc2016anual %>% 
  filter(!is.na(V4121B), V2007 == 1, V2010 == 1) %>% 
  group_by(UF) %>% 
  mutate(pessoas_afazeres = sum(V1032), 
         horas_afazeres = sum(V4121B*V1032),
         media_horas_afazeres = horas_afazeres/pessoas_afazeres) %>% 
  summarise(pessoas_afazeres = mean(pessoas_afazeres),
            horas_afazeres = mean(horas_afazeres),
            media_horas_afazeres = mean(media_horas_afazeres)) %>% 
  saveRDS(file = "tmp/horas_afazeres_homens_brancos_2016.rds")


# horas cuidado e afazeres homens não brancos
total_horas_afazeres_homens = pnadc2016anual %>% 
  filter(!is.na(V4121B), V2007 == 1, !V2010 == 1) %>% 
  group_by(UF) %>% 
  mutate(pessoas_afazeres = sum(V1032), 
         horas_afazeres = sum(V4121B*V1032),
         media_horas_afazeres = horas_afazeres/pessoas_afazeres) %>% 
  summarise(pessoas_afazeres = mean(pessoas_afazeres),
            horas_afazeres = mean(horas_afazeres),
            media_horas_afazeres = mean(media_horas_afazeres)) %>% 
  saveRDS(file = "tmp/horas_afazeres_homens_nao_brancos_2016.rds")

# formalidade

# horas cuidado e afazeres homens formal
total_horas_afazeres_homens = pnadc2016anual %>% 
  filter(!is.na(V4121B), V2007 == 1, dummy_formalidade == 1) %>% 
  group_by(UF) %>% 
  mutate(pessoas_afazeres = sum(V1032), 
         horas_afazeres = sum(V4121B*V1032),
         media_horas_afazeres = horas_afazeres/pessoas_afazeres) %>% 
  summarise(pessoas_afazeres = mean(pessoas_afazeres),
            horas_afazeres = mean(horas_afazeres),
            media_horas_afazeres = mean(media_horas_afazeres)) %>% 
  saveRDS(file = "tmp/horas_afazeres_homens_formal_2016.rds")



# horas cuidado e afazeres homens informal
total_horas_afazeres_homens = pnadc2016anual %>% 
  filter(!is.na(V4121B), V2007 == 1, dummy_formalidade == 2) %>% 
  group_by(UF) %>% 
  mutate(pessoas_afazeres = sum(V1032), 
         horas_afazeres = sum(V4121B*V1032),
         media_horas_afazeres = horas_afazeres/pessoas_afazeres) %>% 
  summarise(pessoas_afazeres = mean(pessoas_afazeres),
            horas_afazeres = mean(horas_afazeres),
            media_horas_afazeres = mean(media_horas_afazeres)) %>% 
  saveRDS(file = "tmp/horas_afazeres_homens_informal_2016.rds")

#


# faixa etaria da pessoa cuidada 

# horas com trabalho mulheres com crianças de 0 a 14 anos
total_horas_trabalho_mulheres = pnadc2016anual %>% 
  filter(VD4002 == 1, V2007 == 2, (VD4040 == 1 | VD4041 == 1)) %>% 
  group_by(UF) %>% 
  mutate(pessoas_ocupadas = sum(V1032), 
         horas_trabalho = sum(V4039C*V1032, na.rm = T),
         media_horas_trabalho = horas_trabalho/pessoas_ocupadas) %>% 
  summarise(pessoas_ocupadas = mean(pessoas_ocupadas),
            horas_trabalho = mean(horas_trabalho),
            media_horas_trabalho = mean(media_horas_trabalho)) %>% 
  saveRDS(file = "tmp/horas_trabalho_mulheres_0_14_2016.rds")


# horas com trabalho mulheres com crianças de 15 a 59 anos
total_horas_trabalho_mulheres = pnadc2016anual %>% 
  filter(VD4002 == 1, V2007 == 2, VD4042 == 1) %>% 
  group_by(UF) %>% 
  mutate(pessoas_ocupadas = sum(V1032), 
         horas_trabalho = sum(V4039C*V1032, na.rm = T),
         media_horas_trabalho = horas_trabalho/pessoas_ocupadas) %>% 
  summarise(pessoas_ocupadas = mean(pessoas_ocupadas),
            horas_trabalho = mean(horas_trabalho),
            media_horas_trabalho = mean(media_horas_trabalho)) %>% 
  saveRDS(file = "tmp/horas_trabalho_mulheres_15_59_2016.rds")


# horas com trabalho mulheres com crianças de idosos
total_horas_trabalho_mulheres = pnadc2016anual %>% 
  filter(VD4002 == 1, V2007 == 2, VD4043 == 1) %>% 
  group_by(UF) %>% 
  mutate(pessoas_ocupadas = sum(V1032), 
         horas_trabalho = sum(V4039C*V1032, na.rm = T),
         media_horas_trabalho = horas_trabalho/pessoas_ocupadas) %>% 
  summarise(pessoas_ocupadas = mean(pessoas_ocupadas),
            horas_trabalho = mean(horas_trabalho),
            media_horas_trabalho = mean(media_horas_trabalho)) %>% 
  saveRDS(file = "tmp/horas_trabalho_mulheres_60_2016.rds")


# horas com trabalho homens com crianças de 0 a 14 anos
total_horas_trabalho_homens = pnadc2016anual %>% 
  filter(VD4002 == 1, V2007 == 1, (VD4040 == 1 | VD4041 == 1)) %>% 
  group_by(UF) %>% 
  mutate(pessoas_ocupadas = sum(V1032), 
         horas_trabalho = sum(V4039C*V1032, na.rm = T),
         media_horas_trabalho = horas_trabalho/pessoas_ocupadas) %>% 
  summarise(pessoas_ocupadas = mean(pessoas_ocupadas),
            horas_trabalho = mean(horas_trabalho),
            media_horas_trabalho = mean(media_horas_trabalho)) %>% 
  saveRDS(file = "tmp/horas_trabalho_homens_0_14_2016.rds")


# horas com trabalho homens com crianças de 15 a 59 anos
total_horas_trabalho_homens = pnadc2016anual %>% 
  filter(VD4002 == 1, V2007 == 1, VD4042 == 1) %>% 
  group_by(UF) %>% 
  mutate(pessoas_ocupadas = sum(V1032), 
         horas_trabalho = sum(V4039C*V1032, na.rm = T),
         media_horas_trabalho = horas_trabalho/pessoas_ocupadas) %>% 
  summarise(pessoas_ocupadas = mean(pessoas_ocupadas),
            horas_trabalho = mean(horas_trabalho),
            media_horas_trabalho = mean(media_horas_trabalho)) %>% 
  saveRDS(file = "tmp/horas_trabalho_homens_15_59_2016.rds")


# horas com trabalho homens com crianças de idosos
total_horas_trabalho_homens = pnadc2016anual %>% 
  filter(VD4002 == 1, V2007 == 1, VD4043 == 1) %>% 
  group_by(UF) %>% 
  mutate(pessoas_ocupadas = sum(V1032), 
         horas_trabalho = sum(V4039C*V1032, na.rm = T),
         media_horas_trabalho = horas_trabalho/pessoas_ocupadas) %>% 
  summarise(pessoas_ocupadas = mean(pessoas_ocupadas),
            horas_trabalho = mean(horas_trabalho),
            media_horas_trabalho = mean(media_horas_trabalho)) %>% 
  saveRDS(file = "tmp/horas_trabalho_homens_60_2016.rds")



# horas cuidado e afazeres mulheres de 0 a 14 anos
total_horas_afazeres_mulheres = pnadc2016anual %>% 
  filter(!is.na(V4121B), V2007 == 2, (VD4040 == 1 | VD4041 == 1)) %>% 
  group_by(UF) %>% 
  mutate(pessoas_afazeres = sum(V1032), 
         horas_afazeres = sum(V4121B*V1032),
         media_horas_afazeres = horas_afazeres/pessoas_afazeres) %>% 
  summarise(pessoas_afazeres = mean(pessoas_afazeres),
            horas_afazeres = mean(horas_afazeres),
            media_horas_afazeres = mean(media_horas_afazeres)) %>% 
  saveRDS(file = "tmp/horas_afazeres_mulheres_0_14_2016.rds")


# horas cuidado e afazeres mulheres de 15 a 59
total_horas_afazeres_mulheres = pnadc2016anual %>% 
  filter(!is.na(V4121B), V2007 == 2, VD4042 == 1) %>% 
  group_by(UF) %>% 
  mutate(pessoas_afazeres = sum(V1032), 
         horas_afazeres = sum(V4121B*V1032),
         media_horas_afazeres = horas_afazeres/pessoas_afazeres) %>% 
  summarise(pessoas_afazeres = mean(pessoas_afazeres),
            horas_afazeres = mean(horas_afazeres),
            media_horas_afazeres = mean(media_horas_afazeres)) %>% 
  saveRDS(file = "tmp/horas_afazeres_mulheres_15_59_2016.rds")


# horas cuidado e afazeres mulheres de idosos
total_horas_afazeres_mulheres = pnadc2016anual %>% 
  filter(!is.na(V4121B), V2007 == 2, VD4043 == 1) %>% 
  group_by(UF) %>% 
  mutate(pessoas_afazeres = sum(V1032), 
         horas_afazeres = sum(V4121B*V1032),
         media_horas_afazeres = horas_afazeres/pessoas_afazeres) %>% 
  summarise(pessoas_afazeres = mean(pessoas_afazeres),
            horas_afazeres = mean(horas_afazeres),
            media_horas_afazeres = mean(media_horas_afazeres)) %>% 
  saveRDS(file = "tmp/horas_afazeres_mulheres_60_2016.rds")



# horas cuidado e afazeres homens de 0 a 14 anos
total_horas_afazeres_homens = pnadc2016anual %>% 
  filter(!is.na(V4121B), V2007 == 1, (VD4040 == 1 | VD4041 == 1)) %>% 
  group_by(UF) %>% 
  mutate(pessoas_afazeres = sum(V1032), 
         horas_afazeres = sum(V4121B*V1032),
         media_horas_afazeres = horas_afazeres/pessoas_afazeres) %>% 
  summarise(pessoas_afazeres = mean(pessoas_afazeres),
            horas_afazeres = mean(horas_afazeres),
            media_horas_afazeres = mean(media_horas_afazeres)) %>% 
  saveRDS(file = "tmp/horas_afazeres_homens_0_14_2016.rds")


# horas cuidado e afazeres homens de 15 a 59
total_horas_afazeres_homens = pnadc2016anual %>% 
  filter(!is.na(V4121B), V2007 == 1, VD4042 == 1) %>% 
  group_by(UF) %>% 
  mutate(pessoas_afazeres = sum(V1032), 
         horas_afazeres = sum(V4121B*V1032),
         media_horas_afazeres = horas_afazeres/pessoas_afazeres) %>% 
  summarise(pessoas_afazeres = mean(pessoas_afazeres),
            horas_afazeres = mean(horas_afazeres),
            media_horas_afazeres = mean(media_horas_afazeres)) %>% 
  saveRDS(file = "tmp/horas_afazeres_homens_15_59_2016.rds")


# horas cuidado e afazeres homens de idosos
total_horas_afazeres_homens = pnadc2016anual %>% 
  filter(!is.na(V4121B), V2007 == 1, VD4043 == 1) %>% 
  group_by(UF) %>% 
  mutate(pessoas_afazeres = sum(V1032), 
         horas_afazeres = sum(V4121B*V1032),
         media_horas_afazeres = horas_afazeres/pessoas_afazeres) %>% 
  summarise(pessoas_afazeres = mean(pessoas_afazeres),
            horas_afazeres = mean(horas_afazeres),
            media_horas_afazeres = mean(media_horas_afazeres)) %>% 
  saveRDS(file = "tmp/horas_afazeres_homens_60_2016.rds")





