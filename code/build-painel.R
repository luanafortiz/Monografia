library(tidyverse)
library(haven)

painel1 <- read_dta(file = "PNAD_painel_1_rs.dta")


painel1 <- painel1 %>% group_by(idind) %>% arrange(Ano, Trimestre)

painel1 <- painel1 %>% filter(UF==33) %>% group_by(idind) %>% arrange(Ano, Trimestre)

