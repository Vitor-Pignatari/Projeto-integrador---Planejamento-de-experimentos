# =============================== Treino do RM ================================

predicoes <- vector("list", length(seeds))

for (i in seq_along(predicoes)) {
  
  set.seed(seeds[i])
  
  split <- dados_separados(lista[[i]], seeds[i])
  
  treino <- as.data.frame(split[[1]])
  teste <- as.data.frame(split[[2]])
  
  treino$V1 <- as.factor(treino$V1)
  teste$V1 <- as.factor(teste$V1)
  
  rmmodel <- randomMachinesClassifier(
    formula = V1 ~ V2 + V3, 
    data = treino, 
    B = 10
  )
  
  teste$pred <- predict.randomMachinesClassifierClass(rmc_model = rmmodel, newdata = teste)
  
  teste <- teste  %>% mutate(acertos = case_when(
    pred == V1 ~ 1,
    TRUE ~ 0
  ))
  
  predicoes[[i]] <- teste
  
}

mean(unlist(lapply(predicoes, function(x) mean(x$acertos))))