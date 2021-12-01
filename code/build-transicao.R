# build transição

library(readxl)
library(Rcpp)
library(tidyverse)
library(haven)
library(dplyr)


setwd("C:/Users/luana/OneDrive/Documentos/Monografia/output/transicao")

# todos
dados_todos = read_excel("C:/Users/luana/OneDrive/Documentos/Monografia/output/transicao/transicao_todos.xlsx", col_names = T) %>% 
  mutate(ufuf = as.numeric(ufuf),
         uiuf = as.numeric(uiuf),
         duf = as.numeric(duf),
         fuf = as.numeric(fuf), 
         ufui = as.numeric(ufui),
         uiui = as.numeric(uiui),
         dui = as.numeric(dui),
         fui = as.numeric(fui),
         ufd = as.numeric(ufd),
         uid = as.numeric(uid),
         dd = as.numeric(dd),
         fd = as.numeric(fd),
         uff = as.numeric(uff),
         uif = as.numeric(uif),
         df = as.numeric(df),
         ff = as.numeric(ff))



# mulheres
dados_mulheres = read_excel("C:/Users/luana/OneDrive/Documentos/Monografia/output/transicao/transicao_mulheres.xlsx", col_names = T)  %>% 
  mutate(ufuf = as.numeric(ufuf),
         uiuf = as.numeric(uiuf),
         duf = as.numeric(duf),
         fuf = as.numeric(fuf), 
         ufui = as.numeric(ufui),
         uiui = as.numeric(uiui),
         dui = as.numeric(dui),
         fui = as.numeric(fui),
         ufd = as.numeric(ufd),
         uid = as.numeric(uid),
         dd = as.numeric(dd),
         fd = as.numeric(fd),
         uff = as.numeric(uff),
         uif = as.numeric(uif),
         df = as.numeric(df),
         ff = as.numeric(ff))

# homens
dados_homens = read_excel("C:/Users/luana/OneDrive/Documentos/Monografia/output/transicao/transicao_homens.xlsx", col_names = T)  %>% 
  mutate(ufuf = as.numeric(ufuf),
         uiuf = as.numeric(uiuf),
         duf = as.numeric(duf),
         fuf = as.numeric(fuf), 
         ufui = as.numeric(ufui),
         uiui = as.numeric(uiui),
         dui = as.numeric(dui),
         fui = as.numeric(fui),
         ufd = as.numeric(ufd),
         uid = as.numeric(uid),
         dd = as.numeric(dd),
         fd = as.numeric(fd),
         uff = as.numeric(uff),
         uif = as.numeric(uif),
         df = as.numeric(df),
         ff = as.numeric(ff))


# checando
soma_coluna = dados_todos %>% filter(yearq == 20191)
xxx = soma_coluna$ufuf + soma_coluna$uiuf + soma_coluna$duf + soma_coluna$fuf
yyy = soma_coluna$ufui + soma_coluna$uiui + soma_coluna$dui + soma_coluna$fui
www = soma_coluna$ufd + soma_coluna$uid + soma_coluna$dd + soma_coluna$fd
zzz = soma_coluna$uff + soma_coluna$uif + soma_coluna$df + soma_coluna$ff



ggplot(dados_todos, aes(x = yearq, y = ufuf)) +   #, fill = tipo
  geom_bar(stat = 'identity', position = 'stack', width = 0.6, ) +
  labs(title = "ufuf", 
       subtitle = "xxxxx", x = " ", y = "Horas", fill = " ") +
  theme(strip.background = element_blank(),
        strip.placement = "outside",
        panel.background = element_rect(fill = "white"),
        axis.line = element_line(colour = "black"),
        axis.line.y = element_blank(),
        panel.grid = element_line(colour = "grey90"),
        legend.key = element_blank(),
        plot.subtitle = element_text(size = 13),
        plot.title = element_text(size = 16),
        text = element_text(size = 16),
        axis.text.x = element_text(size = 10))






