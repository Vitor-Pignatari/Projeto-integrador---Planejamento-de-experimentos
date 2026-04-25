# ============================= Bibliotecas ==============================

library(mvtnorm)
library(tidyverse)

# ============================= Auxiliadores ============================



# ============================= Primeira distribuição ====================

mu1 <- c(60, 50)

Sigma1 <- matrix(c(25, 0,
                   0, 25), nrow = 2)

classe1 <- rmvnorm(500, mean = mu1, sigma = Sigma1)
classe1 <- cbind(1, classe1)

# ============================= Segunda distribuição =====================

mu2 <- c(45, 60) # Matriz de médias

Sigma2 <- matrix(
  c(36, 0,
   0, 36),
  nrow = 2)

classe2 <- rmvnorm(500, mean = mu2, sigma = Sigma2)
classe2 <- cbind(0, classe2)

# ============================ Plotagem de exemplo ========================

plot(classe1[,2], classe1[,3],
     col = 'red',
     xlim = c(20, 100),
     ylim = c(20, 100))

points(classe2[,2], classe2[,3], col = 'blue')

# ========================== 1° dado de simulação ===================

set.seed(28062005)
classe1 <- rmvnorm(2500, mean = mu1, sigma = Sigma1)
classe1 <- cbind(1, classe1)
classe2 <- rmvnorm(2500, mean = mu2, sigma = Sigma2)
classe2 <- cbind(0, classe2)

classe <- rbind(classe1,classe2)

cores <- ifelse(classe[,1] == 0,
                adjustcolor("blue", alpha.f = 0.3),
                adjustcolor("red", alpha.f = 0.3))

plot(classe[,2], classe[,3],
     col = cores,
     pch = 16,
     xlim = c(20, 100),
     ylim = c(20, 100))

# ========================== 2° dado de simulação ===================

set.seed(16071998)
classe1 <- rmvnorm(2500, mean = mu1, sigma = Sigma1)
classe1 <- cbind(1, classe1)
classe2 <- rmvnorm(2500, mean = mu2, sigma = Sigma2)
classe2 <- cbind(0, classe2)

classe <- rbind(classe1,classe2)

cores <- ifelse(classe[,1] == 0,
                adjustcolor("blue", alpha.f = 0.3),
                adjustcolor("red", alpha.f = 0.3))

plot(classe[,2], classe[,3],
     col = cores,
     pch = 16,
     xlim = c(20, 100),
     ylim = c(20, 100))

# ========================== 3° dado de simulação ===================

set.seed(11052006)
classe1 <- rmvnorm(2500, mean = mu1, sigma = Sigma1)
classe1 <- cbind(1, classe1)
classe2 <- rmvnorm(2500, mean = mu2, sigma = Sigma2)
classe2 <- cbind(0, classe2)

classe <- rbind(classe1,classe2)

cores <- ifelse(classe[,1] == 0,
                adjustcolor("blue", alpha.f = 0.3),
                adjustcolor("red", alpha.f = 0.3))

plot(classe[,2], classe[,3],
     col = cores,
     pch = 16,
     xlim = c(20, 100),
     ylim = c(20, 100))

# ========================== 4° dado de simulação ===================

set.seed(14072001)
classe1 <- rmvnorm(2500, mean = mu1, sigma = Sigma1)
classe1 <- cbind(1, classe1)
classe2 <- rmvnorm(2500, mean = mu2, sigma = Sigma2)
classe2 <- cbind(0, classe2)

classe <- rbind(classe1,classe2)

cores <- ifelse(classe[,1] == 0,
                adjustcolor("blue", alpha.f = 0.3),
                adjustcolor("red", alpha.f = 0.3))

plot(classe[,2], classe[,3],
     col = cores,
     pch = 16,
     xlim = c(20, 100),
     ylim = c(20, 100))

# ========================== 5° dado de simulação ===================

set.seed(123)
classe1 <- rmvnorm(2500, mean = mu1, sigma = Sigma1)
classe1 <- cbind(1, classe1)
classe2 <- rmvnorm(2500, mean = mu2, sigma = Sigma2)
classe2 <- cbind(0, classe2)

classe <- rbind(classe1,classe2)

cores <- ifelse(classe[,1] == 0,
                adjustcolor("blue", alpha.f = 0.3),
                adjustcolor("red", alpha.f = 0.3))

plot(classe[,2], classe[,3],
     col = cores,
     pch = 16,
     xlim = c(20, 100),
     ylim = c(20, 100))

# ========================== 6° dado de simulação ===================

set.seed(321)
classe1 <- rmvnorm(2500, mean = mu1, sigma = Sigma1)
classe1 <- cbind(1, classe1)
classe2 <- rmvnorm(2500, mean = mu2, sigma = Sigma2)
classe2 <- cbind(0, classe2)

classe <- rbind(classe1,classe2)

cores <- ifelse(classe[,1] == 0,
                adjustcolor("blue", alpha.f = 0.3),
                adjustcolor("red", alpha.f = 0.3))

plot(classe[,2], classe[,3],
     col = cores,
     pch = 16,
     xlim = c(20, 100),
     ylim = c(20, 100))

# ========================== 7° dado de simulação ===================

set.seed(123456)
classe1 <- rmvnorm(2500, mean = mu1, sigma = Sigma1)
classe1 <- cbind(1, classe1)
classe2 <- rmvnorm(2500, mean = mu2, sigma = Sigma2)
classe2 <- cbind(0, classe2)

classe <- rbind(classe1,classe2)

cores <- ifelse(classe[,1] == 0,
                adjustcolor("blue", alpha.f = 0.3),
                adjustcolor("red", alpha.f = 0.3))

plot(classe[,2], classe[,3],
     col = cores,
     pch = 16,
     xlim = c(20, 100),
     ylim = c(20, 100))


# ============================= Bibliotecas ==============================

library(mvtnorm)
library(tidyverse)
library(caret)

# ============================= Primeira distribuição ====================

mu1 <- c(60, 50)

Sigma1 <- matrix(c(25, 0,
                   0, 25), nrow = 2)

# ============================= Segunda distribuição =====================

mu2 <- c(45, 60) # Matriz de médias

Sigma2 <- matrix(
  c(36, 0,
    0, 36),
  nrow = 2)

# ============================ Dados ========================

seeds <- c(28062005, 16071998, 11052006, 14072001, 123, 321, 123456)

lista <- vector("list", length(seeds))

for (i in seq_along(seeds)) {
  
  set.seed(seeds[i])
  classe1 <- rmvnorm(2500, mean = mu1, sigma = Sigma1)
  classe1 <- cbind(1, classe1)
  classe2 <- rmvnorm(2500, mean = mu2, sigma = Sigma2)
  classe2 <- cbind(0, classe2)
  
  classe <- rbind(classe1,classe2)
  
  cores <- ifelse(classe[,1] == 0,
                  adjustcolor("blue", alpha.f = 0.3),
                  adjustcolor("red", alpha.f = 0.3))
  
  plot(classe[,2], classe[,3],
       col = cores,
       pch = 16,
       xlim = c(20, 100),
       ylim = c(20, 100))
  
  lista[[i]] <- classe
  
}

# usem a mesma seed que gerou os dados para cada conjunto

dados_separados <- function(dados, seed) {
  
  set.seed(seed)
  idx <- createDataPartition(dados[,1], p = 0.7, list = FALSE)
  
  treino <- dados[idx, ]
  teste  <- dados[-idx, ]
  
  split <- list(treino, teste)
  
  return(split)
}

# =============================== Treino do RM ================================

# split <- dados_separados(lista[[1]], seeds[1])
# 
# treino <- as.data.frame(split[[1]])
# teste <- as.data.frame(split[[2]])
# 
# treino$V1 <- as.factor(treino$V1)
# teste$V1 <- as.factor(teste$V1)
# 
# rmmodel <- randomMachinesClassifier(
#   formula = V1 ~ V2 + V3, 
#   data = treino, 
#   B = 10
# )
# 
# verif <- predict.randomMachinesClassifierClass(rmc_model = rmmodel, newdata = teste)
# 
# verif <- as.data.frame(verif) %>% cbind(teste$V1)
# 
# names(verif) <- c('pred', 'obs')
# 
# verif <- verif %>% mutate(acertos = case_when(
#   pred == obs ~ 1,
#   TRUE ~ 0
# ))
# 
# mean(verif$acertos)

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