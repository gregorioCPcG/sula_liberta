library(readxl)
dados <- read_excel("dados.xlsx")
library (dplyr)
library (ggplot2)
library (shiny)
library(tidyverse)
options(scipen = 1000)


dados$origem <- as.factor(dados$origem_competicao) 

summary(dados)

dados$origem <- relevel(dados$origem, "L")
modS <- lm(resultado ~ origem, data = dados)
summary(modS)
exp(modS$coefficients)

library(huxtable)
huxreg(modS)
dados$liga <- as.factor(dados$pais)
dados$liga <- relevel(dados$liga, "BRA")
mod2 <- lm(resultado ~ origem + liga, data = dados)
summary(mod2)


huxreg(modS, mod2)


library(coefplot)

coefplot(mod2, intercept = FALSE)
