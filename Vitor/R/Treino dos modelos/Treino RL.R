library(mvtnorm)
library(tidyverse)
library(caret)

# =============================== Treino do RM ================================

predicoes_RL <- vector("list", length(seeds))

for (i in seq_along(predicoes_RL)) {
  
  set.seed(seeds[i])
  
  split <- dados_separados(lista[[i]], seeds[i])
  
  treino <- as.data.frame(split[[1]])
  teste <- as.data.frame(split[[2]])
  
  treino$V1 <- as.factor(treino$V1)
  teste$V1 <- as.factor(teste$V1)
  
  rmmodel <- glm(
    formula = V1 ~ V2 + V3, 
    data = treino, 
    family = binomial
  )
  
  teste$prob <- predict(rmmodel, newdata = teste, type = "response")
  teste$pred <- ifelse(teste$prob > 0.5, 1, 0)
  teste$pred <- as.factor(teste$pred)
  
  teste <- teste  %>% mutate(acertos = case_when(
    pred == V1 ~ 1,
    TRUE ~ 0
  ))
  
  predicoes_RL[[i]] <- teste
  
}

mean(unlist(lapply(predicoes_RL, function(x) mean(x$acertos))))

save(predicoes_RL, file = "Joao_Victor/dados/predicoes_RL.RData")
