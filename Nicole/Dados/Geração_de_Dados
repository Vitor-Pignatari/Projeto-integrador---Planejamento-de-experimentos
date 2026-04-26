# ============================= Bibliotecas ==============================

library(mvtnorm)
library(tidyverse)
library(caTools)

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
  idx <- sample.split(dados[,1], SplitRatio = 0.7)
  
  treino <- dados[idx, ]
  teste  <- dados[-idx, ]
  
  split <- list(treino, teste)
  
  return(split)
}

split <- dados_separados(lista[[1]], seeds[1])



