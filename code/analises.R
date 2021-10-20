library(ggplot2)
library(tidyverse)




homens = readRDS(file = "tmp/horas_afazeres_homens_2019.rds") %>% 
  mutate(id = "Homens")
  
mulheres = readRDS(file = "tmp/horas_afazeres_mulheres_2019.rds") %>% 
  mutate(id = "Mulheres")


base = rbind(mulheres, homens) %>%
  group_by(id) %>% 
  summarise(xxx = weighted.mean(media_horas_afazeres, pessoas_afazeres))


figure = ggplot(base, aes(x = id, y = xxx)) + 
  geom_bar(stat='identity', width = 0.4) +
  labs(x = "Sexo", y = "Média de horas gastas com afazeres domésticos") +
  theme_bw() 

png(filename = "output/media_afazeres_por_sexo.png", units = "px", res = 300, width = 2000, height = 2000)
plot(figure)
dev.off()
