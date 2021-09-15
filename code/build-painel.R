library(tidyverse)
library(haven)
library(dplyr)

# teste
painel1 <- read_dta(file = "PNAD_painel_1_rs.dta")

painel1 <- painel1 %>% group_by(idind) %>% arrange(Ano, Trimestre)

painel1 <- painel1 %>% filter(UF==33) %>% group_by(idind) %>% arrange(Ano, Trimestre)


# painel 7

painel7 = read_dta(file = "input/PNAD_painel_7_rs.dta")

variaveis_escolhidas = c("Ano", "Trimestre", "UF", "Capital", 
                         "V1008", # número de seleção do domicilio
                         "V1014", # grupo de amostra
                         "V1022", # urbana ou rural
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

painel7 = painel7 %>% select(variaveis_escolhidas)
