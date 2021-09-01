#inicio do projeto de monografia

library(tidyverse)
install.packages("PNADcIBGE")
library(PNADcIBGE)

setwd("C:\\Users\\luana\\OneDrive\\Área de Trabalho\\PUC\\Monografia\\PNADC")
microdata_file <- file.path("PNADC_012012.txt")
chave_file <- file.path("input_PNADC_trimestral.txt")
xxx <- PNADcIBGE::read_pnadc(microdata = microdata_file, input_txt = chave_file)
