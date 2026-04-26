## Bibliotecas ##

library(rpart)
library(caTools)
library(ggplot2)
library(tidyverse)

## Preparação de dados e variáveis ##

seeds <- c(28062005, 16071998, 11052006, 14072001, 123, 321, 123456)

dados <- lista

predicoes <- vector("list", length(seeds))
modelos <- vector("list", length(seeds))

## Aplicação do modelo ##

for (i in seq_along(predicoes)) {
  
  set.seed(seeds[i])
  
  split <- dados_separados(lista[[i]], seeds[i])
  
  treino <- as.data.frame(split[[1]])
  teste <- as.data.frame(split[[2]])
  
  treino$V1 <- as.factor(treino$V1)
  teste$V1 <- as.factor(teste$V1)
  
  modelo <- rpart(formula = V1 ~ V2 + V3,
                data = treino,
                method = "class")
  teste$pred <- predict(modelo, newdata = teste, type = "class")
  
  teste <- teste %>%
    mutate(Acerto = (V1 == pred))
  
  predicoes[[i]] <- teste
  
  modelos[[i]] <- modelo
}

## Teste de precisão

exemplo <- predicoes[[1]]

exemplo %>% 
  pull(Acerto) %>%
  mean()



