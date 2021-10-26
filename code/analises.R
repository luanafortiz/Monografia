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
  geom_bar(stat = 'identity', position = 'stack') +
  labs(title = "Alocação de horas", x = " ", y = "Horas", fill = " ") +
  facet_wrap(~ano, ncol = 4, strip.position = 'bottom') +
  theme(strip.background = element_blank(),
        strip.placement = "outside",
        panel.background = element_rect(fill = "white"),
        axis.line = element_line(colour = "black"),
        panel.grid = element_line(colour = "grey90"),
        legend.key = element_blank(),
        text = element_text(size = 17)) +
  scale_fill_manual(values = c("grey85","grey50"),
                     labels = c("afazeres" = "Cuidados e \n afazeres domésticos",
                     "trabalho" = "Trabalho \n remunerado"))

ggsave("sexo.pdf", path = "C:\\Users\\luana\\OneDrive\\Documentos\\Monografia\\output", plot = last_plot())









ggplot(base, aes(x = id, y = xxx)) + 
  geom_bar(stat='identity', width = 0.4) +
  labs(x = "Sexo", y = "media horas") +
  theme_bw() 

png(filename = "output/media_afazeres_por_sexo.PNG", units = "px", res = 300, width = 2000, height = 2000)
plot(figure)
dev.off()
