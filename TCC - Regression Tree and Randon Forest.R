#TCC - Arvore de Regressão e Randon Forest

# Instalação de pacotes
pacotes <- c(
  'tidyverse',  # Pacote básico de datawrangling
  'rpart',      # Biblioteca de árvores
  'rpart.plot', # Conjunto com Rpart, plota a parvore
  'gtools',     # funções auxiliares como quantcut,
  'Rmisc',      # carrega a função sumarySE para a descritiva
  'scales',     # importa paletas de cores
  'viridis',    # Escalas 'viridis' para o ggplot2
  'caret',      # Funções úteis para machine learning
  'AMR',
  'randomForest',
  'fastDummies',
  'rattle',
  'xgboost',
  "plotly",
  "tidyverse",
  "ggrepel",
  "fastDummies",
  "knitr",
  "kableExtra",
  "splines",
  "reshape2",
  "PerformanceAnalytics",
  "metan",
  "correlation",
  "see",
  "ggraph",
  "nortest",
  "rgl",
  "car",
  "olsrr",
  "jtools",
  "ggstance",
  "magick",
  "cowplot",
  "beepr",
  "Rcpp",
  "dplyr",
  "nlme",
  "lmtest",
  "fastDummies",
  "msm",
  "lmeInfo",
  "MLmetrics",
  "Metrics",
  "gridExtra",
  "data.table",
  "ggseas",
  "knitr",
  "zoo")


if(sum(as.numeric(!pacotes %in% installed.packages())) != 0){
  instalador <- pacotes[!pacotes %in% installed.packages()]
  for(i in 1:length(instalador)) {
    install.packages(instalador, dependencies = T)
    break()}
  sapply(pacotes, require, character = T) 
} else {
  sapply(pacotes, require, character = T) 
}

#Observando valores dos Itens em função do tempo de separação

ggplot(Base_Separação, aes(Itens,Tempo_Separação_Real)) + 
  geom_point(aes(colour='Observado')) +
  scale_color_viridis(discrete=TRUE, begin=0, end=.85, name = "Valor") +
  theme(legend.position="bottom",
        legend.spacing.x = unit(0, 'cm'))

#Criando a base de teste e de treino
set.seed(0)
n <- sample(1:2,
            size=nrow(Base_Separação),
            replace=TRUE, # Amostragem com reposição (de c(1,2))
            prob=c(0.8,0.2)) # A probabilidade de ser 1 é 80%, de ser 2 é 20%

# Dividir amostras de treino e teste #

# Amostra de treino: n==1 (os 80%)
treino <- Base_Separação[n==1,]

# Amostra de teste: n==2 (os 20%)
teste <- Base_Separação[n==2,]

set.seed(0)

#Construindo a árvore de regressão com 3 folhas

?rpart

separação_tree3 <- rpart(Tempo_Separação_Real ~ Itens, 
              data=treino,
              control=rpart.control(maxdepth = 3, cp=0))#cp = 0 para deixar a árvore crescer até o maxdepth

# Plotando a árvore de 3 folhas

paleta = scales::viridis_pal(begin=.75, end=1)(20)
rpart.plot::rpart.plot(separação_tree3,
                       box.palette = paleta) # Paleta de cores

# Valores preditos para a árvore de 3 folhas
teste['Preditos3'] <- predict(separação_tree3, teste)

# Valores esperados e observados para a árvore de 3 folhas

Árvore_3 <-ggplot(teste, aes(Itens,Tempo_Separação_Real)) + 
  geom_point(alpha=.7, size=.5, aes(colour='Observado')) +
  geom_line(size = 2, aes(Itens,Preditos3, colour='Esperado')) + #Ploting
  scale_color_viridis(discrete=TRUE, begin=0, end=0.8, name = "Dado: ") +
  theme_bw() +
  theme(legend.position="bottom") +
  # guides(colour = guide_legend(label.position = "bottom")) +
  labs(title="Valores observados vs esperados") +
  scale_y_continuous(name= "Tempo de Separação") +
  scale_x_continuous(name= "Itens")
Árvore_3
#Avaliando resultado do modelo de 3 folhas para a base de teste

MAPE_Arvore3 <- MAPE(teste$Preditos3,teste$Tempo_Separação_Real)
MAPE_Arvore3

RMSE_Arvore3 <- rmse(teste$Tempo_Separação_Real,teste$Preditos3)
RMSE_Arvore3

#gráfico dos resíduos da árvore de 3 folhas

residuos3 <- teste$Tempo_Separação_Real-predict(separação_tree3,teste)
residuos3
teste %>%
  mutate(residuos3 = residuos3) %>%
  ggplot(aes(x = residuos3)) +
  geom_histogram(aes(y = ..density..), 
                 color = "grey10", 
                 fill = "orange", 
                 bins = 50,
                 alpha = 0.6) +
  stat_function(fun = dnorm, 
                args = list(mean = mean(residuos3),
                            sd = sd(residuos3)),
                aes(color = "Curva Normal Teórica"),
                size = 2) +
  scale_color_manual("Legenda:",
                     values = "grey10") +
  labs(x = "Resíduos",
       y = "Frequência") +
  theme(panel.background = element_rect("white"),
        panel.grid = element_line("grey95"),
        panel.border = element_rect(NA),
        legend.position = "bottom")

#Construindo a árvore de regressão com 4 folhas

separação_tree4 <- rpart(Tempo_Separação_Real ~ Itens, 
                         data=treino,
                         control=rpart.control(maxdepth = 4, cp=0))

# Plotando a árvore de 4 folhas

paleta = scales::viridis_pal(begin=.75, end=1)(20)
rpart.plot::rpart.plot(separação_tree4,
                       box.palette = paleta) # Paleta de cores

# Valores preditos para a árvore de 4 folhas
teste['Preditos4'] <- predict(separação_tree4, teste)

# Valores esperados e observados para a árvore de 4 folhas

Árvore_4 <- ggplot(teste, aes(Itens,Tempo_Separação_Real)) + 
  geom_point(alpha=.7, size=.5, aes(colour='Observado')) +
  geom_line(size = 2, aes(Itens,Preditos4, colour='Esperado')) + #Ploting
  scale_color_viridis(discrete=TRUE, begin=0, end=0.8, name = "Dado: ") +
  theme_bw() +
  theme(legend.position="bottom") +
  # guides(colour = guide_legend(label.position = "bottom")) +
  labs(title="Valores observados vs esperados") +
  scale_y_continuous(name= "Tempo de Separação") +
  scale_x_continuous(name= "Itens")
Árvore_4

#Avaliando resultado do modelo de 4 folhas

MAPE_Arvore4 <- MAPE(teste$Preditos4,teste$Tempo_Separação_Real)
MAPE_Arvore4

RMSE_Arvore4 <- rmse(teste$Tempo_Separação_Real,teste$Preditos4)
RMSE_Arvore4

#gráfico dos resíduos da árvore de 4 folhas

residuos4 <- teste$Tempo_Separação_Real-predict(separação_tree4,teste)
residuos4
teste %>%
  mutate(residuos4 = residuos4) %>%
  ggplot(aes(x = residuos4)) +
  geom_histogram(aes(y = ..density..), 
                 color = "grey10", 
                 fill = "orange", 
                 bins = 50,
                 alpha = 0.6) +
  stat_function(fun = dnorm, 
                args = list(mean = mean(residuos4),
                            sd = sd(residuos4)),
                aes(color = "Curva Normal Teórica"),
                size = 2) +
  scale_color_manual("Legenda:",
                     values = "grey10") +
  labs(x = "Resíduos",
       y = "Frequência") +
  theme(panel.background = element_rect("white"),
        panel.grid = element_line("grey95"),
        panel.border = element_rect(NA),
        legend.position = "bottom")

#Construindo a árvore de regressão com 5 folhas

separação_tree5 <- rpart(Tempo_Separação_Real ~ Itens, 
                         data=treino,
                         control=rpart.control(maxdepth = 5, cp=0))

# Plotando a árvore de 5 folhas

paleta = scales::viridis_pal(begin=.75, end=1)(20)
rpart.plot::rpart.plot(separação_tree5,
                       box.palette = paleta) # Paleta de cores

# Valores preditos para a árvore de 5 folhas
teste['Preditos5'] <- predict(separação_tree5, teste)

# Valores esperados e observados para a árvore de 5 folhas

Árvore_5 <-ggplot(teste, aes(Itens,Tempo_Separação_Real)) + 
  geom_point(alpha=.7, size=.5, aes(colour='Observado')) +
  geom_line(size = 2, aes(Itens,Preditos5, colour='Esperado')) + #Ploting
  scale_color_viridis(discrete=TRUE, begin=0, end=0.8, name = "Dado: ") +
  theme_bw() +
  theme(legend.position="bottom") +
  # guides(colour = guide_legend(label.position = "bottom")) +
  labs(title="Valores observados vs esperados") +
  scale_y_continuous(name= "Tempo de Separação") +
  scale_x_continuous(name= "Itens")
Árvore_5
#Avaliando resultado do modelo de 5 folhas para a base de teste


MAPE_Arvore5 <- MAPE(teste$Preditos5,teste$Tempo_Separação_Real)
MAPE_Arvore5

RMSE_Arvore5 <- rmse(teste$Tempo_Separação_Real,teste$Preditos5)
RMSE_Arvore5

#gráfico dos resíduos da árvore de 5 folhas

residuos5 <- teste$Tempo_Separação_Real-predict(separação_tree5,teste)
residuos5
teste %>%
  mutate(residuos5 = residuos5) %>%
  ggplot(aes(x = residuos5)) +
  geom_histogram(aes(y = ..density..), 
                 color = "grey10", 
                 fill = "orange", 
                 bins = 50,
                 alpha = 0.6) +
  stat_function(fun = dnorm, 
                args = list(mean = mean(residuos5),
                            sd = sd(residuos5)),
                aes(color = "Curva Normal Teórica"),
                size = 2) +
  scale_color_manual("Legenda:",
                     values = "grey10") +
  labs(x = "Resíduos",
       y = "Frequência") +
  theme(panel.background = element_rect("white"),
        panel.grid = element_line("grey95"),
        panel.border = element_rect(NA),
        legend.position = "bottom")

#Aplicando a técnica de Random Forest para predição dos dados

?randomForest

separação_rf100 <- randomForest::randomForest(
  Tempo_Separação_Real ~ Itens, 
  data = treino, 
  ntree = 100,
  importance = T)

separação_rf50 <- randomForest::randomForest(
  Tempo_Separação_Real ~ Itens, 
  data = treino, 
  ntree = 50,
  importance = T)

teste['Preditos_RF100'] <- predict(separação_rf100, teste)

teste['Preditos_RF50'] <- predict(separação_rf50, teste)

# Valores esperados e observados para a primeira random forest

ggplot(teste, aes(Itens,Tempo_Separação_Real)) + 
  geom_point(alpha=.7, size=.5, aes(colour='Observado')) +
  geom_line(size=2,aes(Itens,Preditos_RF100, colour='Esperado')) + #Ploting
  scale_color_viridis(discrete=TRUE, begin=0, end=.8, name = "Dado: ") +
  theme_bw() +
  theme(legend.position="bottom") +
  # guides(colour = guide_legend(label.position = "bottom")) +
  labs(title="Valores observados vs esperados") +
  scale_y_continuous(name= "Tempo de Separação") +
  scale_x_continuous(name= "Embalagem Grande")

MAPE_RF100 <- MAPE(teste$Preditos_RF100,teste$Tempo_Separação_Real)
MAPE_RF100

RMSE_RF100 <- rmse(teste$Tempo_Separação_Real,teste$Preditos_RF100)
RMSE_RF100

#gráfico dos resíduos da random forest de 100 folhas

residuosRF100 <- teste$Tempo_Separação_Real-predict(separação_rf100,teste)
residuosRF100
ResRF100<-teste %>%
  mutate(residuosRF100 = residuosRF100) %>%
  ggplot(aes(x = residuosRF100)) +
  geom_histogram(aes(y = ..density..), 
                 color = "grey10", 
                 fill = "Purple", 
                 bins = 50,
                 alpha = 0.6) +
  stat_function(fun = dnorm, 
                args = list(mean = mean(residuosRF100),
                            sd = sd(residuosRF100)),
                aes(color = "Curva Normal Teórica"),
                size = 2) +
  scale_color_manual("Legenda:",
                     values = "grey10") +
  labs(x = "Resíduos",
       y = "Frequência") +
  theme(panel.background = element_rect("white"),
        panel.grid = element_line("grey95"),
        panel.border = element_rect(NA),
        legend.position = "bottom")
ResRF100
# Valores esperados e observados para a segunda random forest

ggplot(teste, aes(Itens,Tempo_Separação_Real)) + 
  geom_point(alpha=.7, size=.5, aes(colour='Observado')) +
  geom_line(size= 2,aes(Itens,Preditos_RF50, colour='Esperado')) + #Ploting
  scale_color_viridis(discrete=TRUE, begin=0, end=.8, name = "Dado: ") +
  theme_bw() +
  theme(legend.position="bottom") +
  # guides(colour = guide_legend(label.position = "bottom")) +
  labs(title="Valores observados vs esperados") +
  scale_y_continuous(name= "Tempo de Separação") +
  scale_x_continuous(name= "Itens")

MAPE_RF50 <- MAPE(teste$Preditos_RF50,teste$Tempo_Separação_Real)
MAPE_RF50

RMSE_RF50 <- rmse(teste$Tempo_Separação_Real,teste$Preditos_RF50)
RMSE_RF50

#gráfico dos resíduos da random forest de 100 folhas

residuosRF50 <- teste$Tempo_Separação_Real-predict(separação_rf50,teste)
residuosRF50
ResRF50<-teste %>%
  mutate(residuosRF50 = residuosRF50) %>%
  ggplot(aes(x = residuosRF50)) +
  geom_histogram(aes(y = ..density..), 
                 color = "grey10", 
                 fill = "Purple", 
                 bins = 50,
                 alpha = 0.6) +
  stat_function(fun = dnorm, 
                args = list(mean = mean(residuosRF50),
                            sd = sd(residuosRF50)),
                aes(color = "Curva Normal Teórica"),
                size = 2) +
  scale_color_manual("Legenda:",
                     values = "grey10") +
  labs(x = "Resíduos",
       y = "Frequência") +
  theme(panel.background = element_rect("white"),
        panel.grid = element_line("grey95"),
        panel.border = element_rect(NA),
        legend.position = "bottom")
ResRF50
library(cowplot)
plot_grid(ResRF100,ResRF50)

