# UFPR - DEPARTAMENTO DE ESTATÍSTICA ~ 2026/1
# CE311 - PLANEJAMENTO DE EXPERIMENTOS
# ============== PROJETO INTEGRADOR 1 - EXPERIMENTOS NA PRÁTICA ================
# EXPERIMENTO 01
## ============= 1. PROFESSOR ==================================================
#   . Amanda Merian Freitas Mendes

## ============= 2. ALUNOS =====================================================
#   . João Victor Pietchaki Gonçalves
#   . Leonardo Eizo Sakai
#   . Nicole Rodrigues do Nasscimento
#   . Vitor Gabriel Pignatari

## ============= 3. TÍTULO =====================================================
#   . Estudo comparativo do efeito preditivo de modelos de classificação binária para dados simulados.



## ============================= 4. SIMULAÇÃO DE DADOS =========================
## 
### ============================ 4.1 Bibliotecas ===============================

library(mvtnorm)
library(tidyverse)
library(caret)

### ============================ 4.2 Dados =====================================
#### =========================== 4.2.1 Primeira distribuição ===================

mu1 <- c(60, 50)

Sigma1 <- matrix(
  c(25, 0,
    0, 25),
  nrow = 2)

#### =========================== 4.2.2 Segunda distribuição ====================

mu2 <- c(45, 60) # Matriz de médias

Sigma2 <- matrix(
  c(36, 0,
    0, 36),
  nrow = 2)

#### =========================== 4.2.3 Amostra Aleatória =======================
seeds <- c(28062005, 16071998, 11052006, 14072001, 123, 321, 123456)

lista <- vector("list", length(seeds))

for (i in seq_along(seeds)) {
  
  set.seed(seeds[i])
  classe1 <- rmvnorm(2500, mean = mu1, sigma = Sigma1)
  classe1 <- cbind(1, classe1)
  classe2 <- rmvnorm(2500, mean = mu2, sigma = Sigma2)
  classe2 <- cbind(0, classe2)
  
  classe <- rbind(classe1,classe2)
  
  # cores <- ifelse(classe[,1] == 0,
  #                 adjustcolor("blue", alpha.f = 0.3),
  #                 adjustcolor("red", alpha.f = 0.3))
  # 
  # plot(classe[,2], classe[,3],
  #      col = cores,
  #      pch = 16,
  #      xlim = c(20, 100),
  #      ylim = c(20, 100))
  
  lista[[i]] <- classe
  
}

# Salva lista com as amostras simuladas
dados <- lista
save(dados, file = "dados/dados.Rdata")

# Limpa memória do ambiente
rm(mu1, Sigma1, mu2, Sigma2, classe1, classe2, classe, lista, i, dados, cores)

#### =========================== 4.2.4 Dados de Treino e Teste =================

dados_separados <- function(dados, seed) {
  
  set.seed(seed)
  idx <- createDataPartition(dados[,1], p = 0.7, list = FALSE)
  
  treino <- dados[idx, ]
  teste  <- dados[-idx, ]
  
  split <- list(treino, teste)
  
  return(split)
}


## ============================= 5. AJUSTE DOS MODELOS E PREDIÇÃO ==============
load("dados/dados.Rdata")

### ============================ 5.0. Bibliotecas ==============================

# Carrega pacotes necessários
# install.packages(c("rpart", "caTools", "class", "caret"))
library(rpart)
library(caTools)
library(class)
library(caret)

### ============================ 5.1. Regressão Logística (RL) =================

# Pré-aloca memória para armazenar vetor de predições
predicoes_RL <- vector("list", length(seeds))
names(predicoes_RL) <- seeds

# Para cada amostra, treina o modelo e testa em novos dados
for (i in seq_along(predicoes_RL)) {
  
  # Define semente para reprodutibilidade
  set.seed(seeds[i])
  
  # Separa dados de treino e teste
  split <- dados_separados(dados[[i]], seeds[i])
  
  # Transforma vetores em data frames
  treino <- as.data.frame(split[[1]])
  teste <- as.data.frame(split[[2]])
  
  # Cria fator para a variável resposta binária
  treino$V1 <- as.factor(treino$V1)
  teste$V1 <- as.factor(teste$V1)
  
  # Treina o modelo
  modelo <- glm(
    formula = V1 ~ V2 + V3, 
    data = treino, 
    family = binomial
  )
  
  # Testa em novos dados
  teste$prob <- predict(modelo, newdata = teste, type = "response")
  teste$pred <- ifelse(teste$prob > 0.5, 1, 0)
  teste$pred <- as.factor(teste$pred)
  
  # Para cada teste, verifica se o modelo predisse corretamente
  teste <- teste  %>% mutate(acertos = case_when(
    pred == V1 ~ 1,
    TRUE ~ 0
  ))
  
  # Para cada amostra, armazena teste
  predicoes_RL[[i]] <- teste
  
}

# Salva lista com as predições para cada amostra
save(predicoes_RL, file = "dados/predicoes_RL.RData")

# Calcula acurácia geral do modelo
mean(unlist(lapply(predicoes_RL, function(x) mean(x$acertos))))

# Limpa memória do ambiente
rm(i, modelo, split, teste, treino, predicoes_RL)

### ============================ 5.2. Árvores de Decisão (AD) ==================

# Pré-aloca memória para armazenar vetor de predições e de modelos
predicoes_AD <- vector("list", length(seeds))
names(predicoes_AD) <- seeds
modelos <- vector("list", length(seeds))
names(modelos) <- seeds

# Para cada amostra, treina o modelo e testa em novos dados
for (i in seq_along(predicoes_AD)) {
  
  # Define semente para reprodutibilidade
  set.seed(seeds[i])
  
  # Separa dados de treino e teste
  split <- dados_separados(dados[[i]], seeds[i])
  
  # Transforma vetores em data frames
  treino <- as.data.frame(split[[1]])
  teste <- as.data.frame(split[[2]])
  
  # Cria fator para a variável resposta binária
  treino$V1 <- as.factor(treino$V1)
  teste$V1 <- as.factor(teste$V1)
  
  # Treina o modelo
  modelo <- rpart(formula = V1 ~ V2 + V3,
                  data = treino,
                  method = "class")
  
  # Testa em novos dados
  teste$pred <- predict(modelo, newdata = teste, type = "class")
  
  # Para cada teste, verifica se o modelo predisse corretamente
  teste <- teste %>%
    mutate(acertos = (V1 == pred))
  
  # Para cada amostra, armazena teste
  predicoes_AD[[i]] <- teste
  modelos[[i]] <- modelo
}

# Salva lista com as predições para cada amostra
save(predicoes_AD, file = "dados/predicoes_AD.RData")

# Calcula acurácia geral do modelo
mean(unlist(lapply(predicoes_AD, function(x) mean(x$acertos))))
# exemplo <- predicoes_AD[[1]]
# exemplo %>%
#   pull(acertos) %>%
#   mean()

# Limpa memória do ambiente
rm(i, modelo, predicoes_AD, split, teste, treino)

### ============================ 5.3. K-Nearest Neighbors (KNN) ================

# Pré-aloca memória para armazenar vetor de predições
predicoes_KNN <- vector("list", length(seeds))
names(predicoes_KNN) <- seeds
k_escolhido <- numeric(length(seeds))

# Para cada amostra, treina o modelo e testa em novos dados
for (i in seq_along(predicoes_KNN)) {
  
  # Define semente para reprodutibilidade
  set.seed(seeds[i])
  
  # Separa dados de treino e teste
  split <- dados_separados(dados[[i]], seeds[i])
  
  # Transforma vetores em data frames
  treino <- as.data.frame(split[[1]])
  teste  <- as.data.frame(split[[2]])
  
  # Cria fator para a variável resposta binária
  treino$V1 <- as.factor(treino$V1)
  teste$V1  <- as.factor(teste$V1)
  
  # Validação Cruzada
  controle <- trainControl(method = "cv", number = 5)
  
  # Grid de k
  grid <- expand.grid(k = c(3, 5, 7, 9, 11))
  
  # Treina o modelo com seleção automática do melhor k
  modelo_knn <- train(
    V1 ~ V2 + V3,
    data = treino,
    method = "knn",
    trControl = controle,
    tuneGrid = grid,
    preProcess = c("center", "scale")
  )
  
  # Melhor k
  melhor_k <- modelo_knn$bestTune$k
  k_escolhido[i] <- melhor_k
  
  # Testa em novos dados
  pred <- predict(modelo_knn, newdata = teste)
  teste$pred <- pred
  
  # Para cada teste, verifica se o modelo predisse corretamente
  teste <- teste %>%
    mutate(acertos = case_when(
      pred == V1 ~ 1,
      TRUE ~ 0
    ))
  
  # Para cada amostra, armazena teste
  predicoes_KNN[[i]] <- teste
}

# Salva lista com as predições para cada amostra
save(predicoes_KNN, file = "dados/predicoes_KNN.RData")

# Calcula acurácia geral do modelo
mean(unlist(lapply(predicoes_KNN, function(x) mean(x$acertos))))

# Distribuição dos k escolhidos
table(k_escolhido)

# Limpa memória do ambiente
rm(controle, grid, i, k_escolhido, melhor_k, modelo_knn, modelos, pred, predicoes_KNN, split, teste, treino)

### ============================ 5.4. Random Machines (RM) ================

# # Pré-aloca memória para armazenar vetor de predições
# predicoes_RM <- vector("list", length(seeds))
# names(predicoes_RM) <- seeds
# 
# # Para cada amostra, treina o modelo e testa em novos dados
# for (i in seq_along(predicoes_RM)) {
#   
#   # Define semente para reprodutibilidade
#   set.seed(seeds[i])
#   
#   # Separa dados de treino e teste
#   split <- dados_separados(dados[[i]], seeds[i])
#   
#   # Transforma vetores em data frames
#   treino <- as.data.frame(split[[1]])
#   teste <- as.data.frame(split[[2]])
#   
#   # Cria fator para a variável resposta binária
#   treino$V1 <- as.factor(treino$V1)
#   teste$V1 <- as.factor(teste$V1)
#   
#   # Treina o modelo
#   rmmodel <- randomMachinesClassifier(
#     formula = V1 ~ V2 + V3, 
#     data = treino, 
#     B = 10
#   )
#   
#   # Testa em novos dados
#   teste$pred <- predict.randomMachinesClassifierClass(rmc_model = rmmodel, newdata = teste)
#   
#   # Para cada teste, verifica se o modelo predisse corretamente
#   teste <- teste  %>% mutate(acertos = case_when(
#     pred == V1 ~ 1,
#     TRUE ~ 0
#   ))
#   
#   # Para cada amostra, armazena teste
#   predicoes_RM[[i]] <- teste
#   
# }
# 
# # Salva lista com as predições para cada amostra
# # save(predicoes_RM, file = "dados/predicoes_RM.RData")
# 
# # Calcula acurácia geral do modelo
# mean(unlist(lapply(predicoes_RM, function(x) mean(x$acertos))))
# 
# Limpa memória do ambiente
# rm(i, modelo, predicoes_RM, split, teste, treino)



## ============================= 6. DIC ========================================

### ============================ 6.1. Tabela de Dados ===========================

# Carrega as predições para cada modelo (RL, AD, KNN e RM)
load("dados/predicoes_RL.RData")
load("dados/predicoes_AD.RData")
load("dados/predicoes_KNN.RData")
load("dados/predicoes_RM.RData")
predicoes_RM <- predicoes
rm(predicoes)
predicoes <- list(
  RL = predicoes_RL
  ,AD = predicoes_AD
  ,KNN = predicoes_KNN
  ,RM = predicoes_RM
)

# Cria vetor para representar as unidades experimentais
UEs <- rep(seeds, times = length(predicoes))

# Cria vetor para representar os tratamentos
tratamentos <- rep(names(predicoes), each = length(seeds))

# Obtém vetor de acurácias para cada modelo e cada réplica
acuracias <- unlist(
  lapply(predicoes, function(modelo_lista) {
    sapply(modelo_lista, function(df) {
      mean(df$acertos)
    })
  })
)

# Cria tabela bruta de dados do DIC
DIC <- data.frame(
  UE = UEs,
  Tratamento = tratamentos,
  Acuracia = acuracias
)

# Tabela bruta de dados do DIC - pivotada no formato wide
tabela_bruta <- DIC %>%
  pivot_wider(names_from = Tratamento, values_from = Acuracia) %>%
  select(-UE) %>% as.matrix()
rownames(tabela_bruta) <- seeds
tabela_bruta <- tabela_bruta %>% t()
dimnames(tabela_bruta) <- list(
  Modelo = rownames(tabela_bruta),
  Réplica = colnames(tabela_bruta)
)
tabela_bruta

### ============================ 6.2. Análise Exploratória de Dados (EDA) ======
for (model in DIC$Tratamento %>% unique()) {
  print(paste("Análise para o modelo:", model))
  subset_model <- DIC %>% filter(Tratamento == model)
  
  # Estatísticas descritivas
  print(summary(subset_model$Acuracia))
  
  # Boxplot
  boxplot(subset_model$Acuracia, main = paste("Boxplot -", model), ylab = "Acurácia")
  
  # Histograma
  hist(subset_model$Acuracia, main = paste("Histograma -", model), xlab = "Acurácia", breaks = 10)
  
}

### ============================ 6.2. DIC ======================================

#### ============================ 6.2.1. Ajuste do Modelo Linear ===============

modelo_aov <- aov(Acuracia ~ Tratamento, data = DIC)
summary(modelo_aov)
boxplot(Acuracia ~ Tratamento, data = DIC)


#### ============================ 6.2.2 Checagem de Adequação do Modelo ========

##### ============================ 6.2.2.1 Teste de Normalidade ================

# Carregas pacotes necessários
# install.packages(c("tseries", "magrittr"))
library(tseries)
library(magrittr)

normal_JB_teste <- DIC %>% pivot_wider(names_from = Tratamento, values_from = Acuracia) %>%
  select(-UE) %>% as.matrix %>%
  apply(2, function(x) {return(jarque.bera.test(x))})

if(sum(unlist(lapply(normal_JB_teste, function(x) if(x$p.value <= 0.05){TRUE}else{FALSE}))) == 0) {
  print("Todos os tratamentos seguem a normalidade.")
} else {
  print("Algum tratamento não segue a normalidade.")
}

##### ============================ 6.2.2.2 Independência =======================
modelo_aov$residuals %>% plot(main = "Resíduos do modelo", ylab = "Resíduos", xlab = "Índice")

##### ============================ 6.2.2.2 Homogeneidaade de Variâncias ========
bartlett_teste <- bartlett.test(Acuracia ~ Tratamento, data = DIC)
if(bartlett_teste$p.value > 0.05) {
  print("As variâncias são homogêneas.")
} else {
  print("As variâncias não são homogêneas.")
}


### ============================ 6.2.3. Parâmetros Estimados ===================

# Calculando estatísticas do Modelo Linear para o DIC
mu <- DIC$Acuracia %>% mean()

QMres <- summary(modelo_aov)[[1]]["Residuals", "Mean Sq"]
gl <- df.residual(modelo_aov)

t_value <- qt(0.975, df = df.residual(modelo_aov))

chisq_value <- qchisq(0.05, summary(modelo_aov)[[1]]["Residuals", "Df"])

resumo <- DIC %>%
  group_by(Tratamento) %>%
  summarise(
    mu_i_inf = mean(Acuracia) - t_value * sqrt(QMres / n()),
    mu_i = mean(Acuracia),
    mu_i_sup = mean(Acuracia) + t_value * sqrt(QMres / n()),
    mu = mu,
    sigma2 = QMres,
    sigma2_sup = gl * QMres / chisq_value
  )

ggplot(resumo, aes(x = Tratamento, y = mu_i)) +
  geom_point(size = 3) +
  geom_errorbar(aes(ymin = mu_i_inf, ymax = mu_i_sup), width = 0.2) +
  geom_hline(yintercept = unique(DIC$Acuracia) %>% mean(), linetype = "dashed") +
  labs(
    title = "Estimativas Pontuais e Intervalares para a Acurácia por Tratamento",
    y = "Acurácia",
    x = "Tratamento"
  ) +
  theme_minimal()

### ============================ 6.3. Teste de Tukey ==========================
TukeyHSD(modelo_aov)

# install.packages("agricolae")
library(agricolae)
tukey <- HSD.test(modelo_aov, "Tratamento", group = TRUE)
tukey$groups
