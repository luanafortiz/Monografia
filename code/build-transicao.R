# build transição

library(readxl)
library(Rcpp)
library(tidyverse)
library(haven)
library(dplyr)
library(devtools)
# devtools::install_github("thomasp85/patchwork")
library(patchwork)
library(scales)

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


# maes
dados_maes= read_excel("C:/Users/luana/OneDrive/Documentos/Monografia/output/transicao/transicao_mulheres_maes.xlsx", col_names = T) %>% 
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

# pais
dados_pais = read_excel("C:/Users/luana/OneDrive/Documentos/Monografia/output/transicao/transicao_homens_pais.xlsx", col_names = T) %>% 
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


# ggplot(dados_todos, aes(x = yearq, y = ufuf)) +   
  # geom_bar(stat = 'identity', position = 'stack', width = 0.6, ) +
  # labs(title = "ufuf", 
  #      subtitle = "xxxxx", x = " ", y = "Horas", fill = " ") +
  # theme(strip.background = element_blank(),
  #       strip.placement = "outside",
  #       panel.background = element_rect(fill = "white"),
  #       axis.line = element_line(colour = "black"),
  #       axis.line.y = element_blank(),
  #       panel.grid = element_line(colour = "grey90"),
  #       legend.key = element_blank(),
  #       plot.subtitle = element_text(size = 13),
  #       plot.title = element_text(size = 16),
  #       text = element_text(size = 16),
  #       axis.text.x = element_text(size = 10))


## giovanni

dados = dados_todos %>% mutate(tri = paste0(year," - ", quarter,"º Tri"))

ufuf = ggplot(dados, aes(x = tri, y = ufuf))+
  geom_col() + xlab(NULL) + ylab(NULL) + ylim(0,0.8) +
  labs(title = "Transições do mercado formal para o mercado formal entre trimestres") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1, size = 8),
        strip.background = element_blank(),
        strip.placement = "outside",
        panel.background = element_rect(fill = "white"),
        axis.line = element_line(colour = "black"),
        axis.line.y = element_blank(),
        panel.grid = element_line(colour = "grey90"),
        legend.key = element_blank(),
        plot.subtitle = element_text(size = 13),
        plot.title = element_text(size = 13),
        text = element_text(size = 16)) +
  scale_y_continuous(labels = scales::number_format(accuracy = 0.01))


ufui = ggplot(dados, aes(x = tri, y = ufui))+
  geom_col() + xlab(NULL) + ylab(NULL) + ylim(0,0.10) +
  labs(title = "Transições do mercado formal para o mercado informal entre trimestres") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1, size = 8),
        strip.background = element_blank(),
        strip.placement = "outside",
        panel.background = element_rect(fill = "white"),
        axis.line = element_line(colour = "black"),
        axis.line.y = element_blank(),
        panel.grid = element_line(colour = "grey90"),
        legend.key = element_blank(),
        plot.subtitle = element_text(size = 13),
        plot.title = element_text(size = 13),
        text = element_text(size = 16)) +
  scale_y_continuous(labels = scales::number_format(accuracy = 0.01))


ufd = ggplot(dados, aes(x = tri, y = ufd))+
  geom_col() + xlab(NULL) + ylab(NULL) + ylim(0,0.04) +
  labs(title = "Transições do mercado formal para desocupado entre trimestres") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1, size = 8),
        strip.background = element_blank(),
        strip.placement = "outside",
        panel.background = element_rect(fill = "white"),
        axis.line = element_line(colour = "black"),
        axis.line.y = element_blank(),
        panel.grid = element_line(colour = "grey90"),
        legend.key = element_blank(),
        plot.subtitle = element_text(size = 13),
        plot.title = element_text(size = 13),
        text = element_text(size = 16)) +
  scale_y_continuous(labels = scales::number_format(accuracy = 0.01))



uff = ggplot(dados, aes(x = tri, y = uff))+
  geom_col() + xlab(NULL) + ylab(NULL) + ylim(0,0.04) +
  labs(title = "Transições do mercado formal para fora da força de trabalho entre trimestres") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1, size = 8),
        strip.background = element_blank(),
        strip.placement = "outside",
        panel.background = element_rect(fill = "white"),
        axis.line = element_line(colour = "black"),
        axis.line.y = element_blank(),
        panel.grid = element_line(colour = "grey90"),
        legend.key = element_blank(),
        plot.subtitle = element_text(size = 13),
        plot.title = element_text(size = 13),
        text = element_text(size = 16)) +
  scale_y_continuous(labels = scales::number_format(accuracy = 0.01))


uiuf = ggplot(dados, aes(x = tri, y = uiuf))+
  geom_col() + xlab(NULL) + ylab(NULL) + ylim(0,0.15) +
  labs(title = "Transições do mercado informal para mercado formal entre trimestres") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1, size = 8),
        strip.background = element_blank(),
        strip.placement = "outside",
        panel.background = element_rect(fill = "white"),
        axis.line = element_line(colour = "black"),
        axis.line.y = element_blank(),
        panel.grid = element_line(colour = "grey90"),
        legend.key = element_blank(),
        plot.subtitle = element_text(size = 13),
        plot.title = element_text(size = 13),
        text = element_text(size = 16)) +
  scale_y_continuous(labels = scales::number_format(accuracy = 0.01))


uiui = ggplot(dados, aes(x = tri, y = uiui))+
  geom_col() + xlab(NULL) + ylab(NULL) + ylim(0,0.7) +
  labs(title = "Transições do mercado informal para mercado informal entre trimestres") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1, size = 8),
        strip.background = element_blank(),
        strip.placement = "outside",
        panel.background = element_rect(fill = "white"),
        axis.line = element_line(colour = "black"),
        axis.line.y = element_blank(),
        panel.grid = element_line(colour = "grey90"),
        legend.key = element_blank(),
        plot.subtitle = element_text(size = 13),
        plot.title = element_text(size = 13),
        text = element_text(size = 16)) +
  scale_y_continuous(labels = scales::number_format(accuracy = 0.01))


uid = ggplot(dados, aes(x = tri, y = uid))+
  geom_col() + xlab(NULL) + ylab(NULL) + ylim(0,0.15) +
  labs(title = "Transições do mercado informal para desocupado entre trimestres") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1, size = 8),
        strip.background = element_blank(),
        strip.placement = "outside",
        panel.background = element_rect(fill = "white"),
        axis.line = element_line(colour = "black"),
        axis.line.y = element_blank(),
        panel.grid = element_line(colour = "grey90"),
        legend.key = element_blank(),
        plot.subtitle = element_text(size = 13),
        plot.title = element_text(size = 13),
        text = element_text(size = 16)) +
  scale_y_continuous(labels = scales::number_format(accuracy = 0.01))



uif = ggplot(dados, aes(x = tri, y = uif))+
  geom_col() + xlab(NULL) + ylab(NULL) + ylim(0,0.2) +
  labs(title = "Transições do mercado informal para fora da força de trabalho entre trimestres") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1, size = 8),
        strip.background = element_blank(),
        strip.placement = "outside",
        panel.background = element_rect(fill = "white"),
        axis.line = element_line(colour = "black"),
        axis.line.y = element_blank(),
        panel.grid = element_line(colour = "grey90"),
        legend.key = element_blank(),
        plot.subtitle = element_text(size = 13),
        plot.title = element_text(size = 13),
        text = element_text(size = 16)) +
  scale_y_continuous(labels = scales::number_format(accuracy = 0.01))


duf = ggplot(dados, aes(x = tri, y = duf))+
  geom_col() + xlab(NULL) + ylab(NULL) + ylim(0,0.15) +
  labs(title = "Transições de desocupado para mercado formal entre trimestres") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1, size = 8),
        strip.background = element_blank(),
        strip.placement = "outside",
        panel.background = element_rect(fill = "white"),
        axis.line = element_line(colour = "black"),
        axis.line.y = element_blank(),
        panel.grid = element_line(colour = "grey90"),
        legend.key = element_blank(),
        plot.subtitle = element_text(size = 13),
        plot.title = element_text(size = 13),
        text = element_text(size = 16)) +
  scale_y_continuous(labels = scales::number_format(accuracy = 0.01))


dui = ggplot(dados, aes(x = tri, y = dui))+
  geom_col() + xlab(NULL) + ylab(NULL) + ylim(0,0.25) +
  labs(title = "Transições de desocupado para mercado informal entre trimestres") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1, size = 8),
        strip.background = element_blank(),
        strip.placement = "outside",
        panel.background = element_rect(fill = "white"),
        axis.line = element_line(colour = "black"),
        axis.line.y = element_blank(),
        panel.grid = element_line(colour = "grey90"),
        legend.key = element_blank(),
        plot.subtitle = element_text(size = 13),
        plot.title = element_text(size = 13),
        text = element_text(size = 16)) +
  scale_y_continuous(labels = scales::number_format(accuracy = 0.01))


dd = ggplot(dados, aes(x = tri, y = dd))+
  geom_col() + xlab(NULL) + ylab(NULL) + ylim(0,0.5) +
  labs(title = "Transições de desocupado para desocupado entre trimestres") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1, size = 8),
        strip.background = element_blank(),
        strip.placement = "outside",
        panel.background = element_rect(fill = "white"),
        axis.line = element_line(colour = "black"),
        axis.line.y = element_blank(),
        panel.grid = element_line(colour = "grey90"),
        legend.key = element_blank(),
        plot.subtitle = element_text(size = 13),
        plot.title = element_text(size = 13),
        text = element_text(size = 16)) +
  scale_y_continuous(labels = scales::number_format(accuracy = 0.01))



df = ggplot(dados, aes(x = tri, y = df))+
  geom_col() + xlab(NULL) + ylab(NULL) + ylim(0,0.25) +
  labs(title = "Transições de desocupado para fora da força de trabalho entre trimestres") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1, size = 8),
        strip.background = element_blank(),
        strip.placement = "outside",
        panel.background = element_rect(fill = "white"),
        axis.line = element_line(colour = "black"),
        axis.line.y = element_blank(),
        panel.grid = element_line(colour = "grey90"),
        legend.key = element_blank(),
        plot.subtitle = element_text(size = 13),
        plot.title = element_text(size = 13),
        text = element_text(size = 16)) +
  scale_y_continuous(labels = scales::number_format(accuracy = 0.01))



fuf = ggplot(dados, aes(x = tri, y = fuf))+
  geom_col() + xlab(NULL) + ylab(NULL) + ylim(0,0.05) +
  labs(title = "Transições de fora da força de trabalho para mercado formal entre trimestres") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1, size = 8),
        strip.background = element_blank(),
        strip.placement = "outside",
        panel.background = element_rect(fill = "white"),
        axis.line = element_line(colour = "black"),
        axis.line.y = element_blank(),
        panel.grid = element_line(colour = "grey90"),
        legend.key = element_blank(),
        plot.subtitle = element_text(size = 13),
        plot.title = element_text(size = 13),
        text = element_text(size = 16)) +
  scale_y_continuous(labels = scales::number_format(accuracy = 0.01))


fui = ggplot(dados, aes(x = tri, y = fui))+
  geom_col() + xlab(NULL) + ylab(NULL) + ylim(0,0.1) +
  labs(title = "Transições de fora da força de trabalho para mercado informal entre trimestres") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1, size = 8),
        strip.background = element_blank(),
        strip.placement = "outside",
        panel.background = element_rect(fill = "white"),
        axis.line = element_line(colour = "black"),
        axis.line.y = element_blank(),
        panel.grid = element_line(colour = "grey90"),
        legend.key = element_blank(),
        plot.subtitle = element_text(size = 13),
        plot.title = element_text(size = 13),
        text = element_text(size = 16)) +
  scale_y_continuous(labels = scales::number_format(accuracy = 0.01))


fd = ggplot(dados, aes(x = tri, y = fd))+
  geom_col() + xlab(NULL) + ylab(NULL) + ylim(0,0.15) +
  labs(title = "Transições de fora da força de trabalho para desocupado entre trimestres") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1, size = 8),
        strip.background = element_blank(),
        strip.placement = "outside",
        panel.background = element_rect(fill = "white"),
        axis.line = element_line(colour = "black"),
        axis.line.y = element_blank(),
        panel.grid = element_line(colour = "grey90"),
        legend.key = element_blank(),
        plot.subtitle = element_text(size = 13),
        plot.title = element_text(size = 13),
        text = element_text(size = 16)) +
  scale_y_continuous(labels = scales::number_format(accuracy = 0.01))



ff = ggplot(dados, aes(x = tri, y = ff))+
  geom_col() + xlab(NULL) + ylab(NULL) + ylim(0,0.8) +
  labs(title = "Transições de fora da força de trabalho para fora da força de trabalho entre trimestres") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1, size = 8),
        strip.background = element_blank(),
        strip.placement = "outside",
        panel.background = element_rect(fill = "white"),
        axis.line = element_line(colour = "black"),
        axis.line.y = element_blank(),
        panel.grid = element_line(colour = "grey90"),
        legend.key = element_blank(),
        plot.subtitle = element_text(size = 13),
        plot.title = element_text(size = 13),
        text = element_text(size = 16)) +
  scale_y_continuous(labels = scales::number_format(accuracy = 0.01))





## matrizona

matrizona = (ufuf | ufui | ufd | uff)/(uiuf | uiui | uid | uif)/(duf | dui | dd | df)/(fuf | fui | fd | ff) 


para_formal = (ufuf | uiuf)/(duf | fuf)


para_informal = (ufui | uiui)/(dui | fui)


para_desocupado = (ufd | uid)/(dd | fd)


para_fora = (uff | uif)/(df | ff)

######################################################################################################################33


taxas = read_excel("C:/Users/luana/OneDrive/Documentos/Monografia/desocupados.xlsx", col_names = T) %>% 
  mutate(Mulheres = as.numeric(Mulheres),
         Homens = as.numeric(Homens),
         Total = as.numeric(Total))
  
ggplot(taxas, aes(x = date, y = Mulheres, group = 1)) +
  geom_line(aes(y = Mulheres, color = "Mulheres"), size = 1) +
  geom_line(aes(y = Homens, color = "Homens"), size = 1) +
  geom_line(aes(y = Total, color = "Todos"), size = 1) +
  scale_colour_manual("", values=c("Mulheres" = "red","Homens" = "blue","Todos" = "green")) +
  labs(title = "Evolução da Taxa de Desocupação entre 2012 e 2021, desagregada por sexo", x = NULL, y = NULL) +
    theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1, size = 8),
        strip.background = element_blank(),
        strip.placement = "outside",
        panel.background = element_rect(fill = "white"),
        axis.line = element_line(colour = "black"),
        axis.line.y = element_blank(),
        panel.grid = element_line(colour = "grey90"),
        legend.key = element_blank(),
        plot.subtitle = element_text(size = 13),
        plot.title = element_text(size = 16),
        text = element_text(size = 16)) 
  


ggsave("desocupado.png", scale = 1, plot = last_plot(), path = "C:/Users/luana/OneDrive/Documentos/Monografia/" )



# checando
soma_coluna = dados_todos %>% filter(yearq == "2019_1")
xxx = soma_coluna$ufuf + soma_coluna$ufui + soma_coluna$ufd + soma_coluna$uff
yyy = soma_coluna$uiuf + soma_coluna$uiui + soma_coluna$uid + soma_coluna$uif
www = soma_coluna$duf + soma_coluna$dui + soma_coluna$dd + soma_coluna$df
zzz = soma_coluna$fuf + soma_coluna$fui + soma_coluna$fd + soma_coluna$ff





