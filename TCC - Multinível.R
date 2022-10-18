#TCC - Regressão Multinível


#INSTALAÇÃO E CARREGAMENTO DE PACOTES NECESSÁRIOS

#Pacotes utilizados
pacotes <- c("plotly","tidyverse","ggrepel","fastDummies","knitr","kableExtra",
             "splines","reshape2","PerformanceAnalytics","metan","correlation",
             "see","ggraph","nortest","rgl","car","olsrr","jtools","ggstance",
             "magick","cowplot","beepr","Rcpp", "dplyr","nlme","lmtest",
             "fastDummies","msm","lmeInfo","MLmetrics","brms", "Metrics",
             "gridExtra","data.table","ggseas","knitr","zoo")

if(sum(as.numeric(!pacotes %in% installed.packages())) != 0){
  instalador <- pacotes[!pacotes %in% installed.packages()]
  for(i in 1:length(instalador)) {
    install.packages(instalador, dependencies = T)
    break()}
  sapply(pacotes, require, character = T) 
} else {
  sapply(pacotes, require, character = T) 
}

#Gráfico de Itens x Tempo_Separação_Real por Almoxarife

ggplotly(
  Base_Separação %>%
    ggplot(aes(x = Itens, y = Tempo_Separação_Real, color = Almoxarife)) +
    geom_smooth(method = "lm", formula = y ~ x, se = F) +
    geom_point() +
    guides(color = F)+
    scale_colour_viridis_d() +
    labs(x = "Quantidade de Itens",
         y = "Tempo de Separação da carga") +
    theme_bw()
)

#Procedimento Stepwise

#ESTIMAÇÃO DO MODELO NULO SEPARAÇÃO

#Estimação do modelo nulo (função lme do pacote nlme)

modelo_nulo_separação <- lme(fixed = Tempo_Separação_Real ~ 1, 
                        random = ~ 1 | Almoxarife,
                        data = Base_Separação,
                        method = "REML")

#Parâmetros do modelo

summary(modelo_nulo_separação)

export_summs(modelo_nulo_separação)

#COMPARAÇÃO DO SEPARAÇÃO NULO COM UM OLS NULO

modelo_ols_nulo <- lm(formula = Tempo_Separação_Real ~ 1, 
                      data = Base_Separação)

#Parâmetros do modelo OLS nulo

summary(modelo_ols_nulo)

export_summs(modelo_ols_nulo)

#Para comparar os LLs dos modelos, vamos utilizar a função lrtest do pacote lmtest

lmtest::lrtest(modelo_ols_nulo, modelo_nulo_separação)

#Comparação entre os LLs dos modelos

data.frame(OLS_Nulo = logLik(modelo_ols_nulo),
           Separação_Nulo = logLik(modelo_nulo_separação)) %>%
  dplyr::rename("OLS Nulo" = 1,
         "Separação Nulo" = 2) %>%
  melt() %>%
  ggplot(aes(x = value, y = (variable), fill = factor(variable))) +
  geom_bar(stat = "Identity") +
  geom_label(aes(label = (round(value,1))), hjust = 0.5, color = "Black", size = 7) +
  labs(title = "Comparação do LL", 
       y = "Modelo Proposto", 
       x = "LogLik") +
  coord_flip() +
  scale_fill_manual("Legenda:",
                    values = c("grey","Blue")) +
  theme(legend.title = element_blank(), 
        panel.background = element_rect("white"),
        legend.position = "none",
        axis.line = element_line())

#Estimação do modelo com Interceptos Aleatórios

modelo_intercept_separação <- lme(fixed = Tempo_Separação_Real ~ Itens,
                             random = ~ 1 | Almoxarife,
                             data = Base_Separação,
                             method = "REML")

#Parâmetros do modelo

summary(modelo_intercept_separação)

export_summs(modelo_intercept_separação)

#Comparação entre os LLs dos modelos

data.frame(OLS_Nulo = logLik(modelo_ols_nulo),
           Separação_Nulo = logLik(modelo_nulo_separação),
           Separação_Intercept_Aleat = logLik(modelo_intercept_separação)) %>%
  dplyr::rename("OLS Nulo" = 1,
         "Separação Nulo" = 2,
         "Separação com Interceptos Aleatórios" = 3) %>% melt() %>%
  ggplot(aes(x = value, y = (variable), fill = factor(variable))) +
  geom_bar(stat = "Identity") +
  geom_label(aes(label = (round(value,1))),vjust = 0.5, color = "Black", size = 7) +
  labs(title = "Comparação do LL", 
       y = "Modelo Proposto", 
       x = "LogLik") +
  coord_flip() +
  scale_fill_manual("Legenda:",
                    values = c("grey","Blue","bisque4")) +
  theme(legend.title = element_blank(), 
        panel.background = element_rect("white"),
        legend.position = "none",
        axis.line = element_line())

#Estimação do modelo com Interceptos e Inclinações Aleatórios

modelo_intercept_inclin_separação <- lme(fixed = Tempo_Separação_Real ~ Itens,
                                    random = ~ Itens | Almoxarife,
                                    data = Base_Separação,
                                    method = "REML")

summary(modelo_intercept_inclin_separação)
?RMSE
export_summs(modelo_intercept_inclin_separação)
??predict

#Valores preditos na base TREINO!!! Isso pode?
Base_Separação$Preditos_Multinivel <- stats::predict(modelo_intercept_inclin_separação, Base_Separação)
Base_Separação['Preditos_Multinivel']


MAPE_Multinivel <- MAPE(Base_Separação$Preditos_Multinivel,Base_Separação$Tempo_Separação_Real)
MAPE_Multinivel

RMSE_Multinivel <- RMSE(Base_Separação$Preditos_Multinivel,Base_Separação$Tempo_Separação_Real)
RMSE_Multinivel

#Histograma dos resíduos do modelo multinível

residuosMulti<- Base_Separação$Tempo_Separação_Real-predict(modelo_intercept_inclin_separação,Base_Separação)

ResMulti<-Base_Separação %>%
  mutate(residuos = residuosMulti) %>%
  ggplot(aes(x = residuosMulti)) +
  geom_histogram(aes(y = ..density..), 
                 color = "grey10", 
                 fill = "green", 
                 bins = 50,
                 alpha = 0.6) +
  stat_function(fun = dnorm, 
                args = list(mean = mean(residuosMulti),
                            sd = sd(residuosMulti)),
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
ResMulti
#Comparação entre os LLs do modelos
data.frame(OLS_Nulo = logLik(modelo_ols_nulo),
           Separação_Nulo = logLik(modelo_nulo_separação),
           Separação_Intercept_Aleat = logLik(modelo_intercept_separação),
           Separação_Intercept_Inclin_Aleat = logLik(modelo_intercept_inclin_separação)) %>%
  melt() %>%
  ggplot(aes(x = value, y = (variable), fill = factor(variable))) +
  geom_bar(stat = "identity") +
  geom_label(aes(label = (round(value,3))), vjust = 0.5, color = "Black", size = 7) +
  labs(title = "Comparação do LL", 
       y = "Modelo Proposto", 
       x = "LogLik") +
  coord_flip() +
  scale_fill_manual("Legenda:",
                    values = c("grey","blue","bisque4","bisque3")) +
  theme(legend.title = element_blank(), 
        panel.background = element_rect("white"),
        legend.position = "none",
        axis.line = element_line())

#Gerando os fitted values do modelo Final Separação
Base_Separação$separação_fitted <- predict(modelo_intercept_inclin_separação,
                                           Base_Separação)

#Valores previstos do Tempo de Separação em função da variável Itens para o 
#modelo de Separação com interceptos e inclinações aleatórios
Base_Separação %>%
  mutate(fitted_Almoxarife = predict(modelo_intercept_inclin_separação, level = 1)) %>% 
  ggplot() +
  geom_point(aes(x = Itens, y = fitted_Almoxarife)) +
  geom_smooth(aes(x = Itens, y = fitted_Almoxarife, color = factor(Almoxarife)), 
              method = "lm", se = F) +
  scale_colour_viridis_d() +
  labs(x = "Quantidade de Embalagens Grandes",
       y = "Tempo de Separação Real (Fitted Values)") +
  theme_bw()


#COMPARAÇÃO COM UM MODELO MLE

#Elaborando um modelo MLE para fins de comparação
modelo_MLE <- gls(model = Tempo_Separação_Real ~ Itens,
                 data = Base_Separação,
                 method = "REML")

#Parâmetros
summary(modelo_MLE)

#Comparando os LL dos modelos elaborados
data.frame(MV = logLik(modelo_MLE),
           GLMM = logLik(modelo_intercept_inclin_separação)) %>%
  melt() %>%
  ggplot(aes(x = value, y = (variable), fill = factor(variable))) +
  geom_bar(stat = "identity") +
  geom_label(aes(label = (round(value,1))), vjust = 0.5, color = "black", size = 7) +
  labs(title = "Comparação do Log-likelyhood", 
       y = "Modelo Proposto", 
       x = "LogLik") +
  coord_flip() +
  scale_fill_manual("Legenda:",
                    values = c("darkorchid","deepskyblue1")) +
  theme(legend.title = element_blank(), 
        panel.background = element_rect("white"),
        legend.position = "none",
        axis.line = element_line())

#LR Test
lmtest::lrtest(modelo_MLE, modelo_intercept_inclin_separação)
?lrtest
#Comparando a aderência dos fitted values dos modelos estimados
#Gerando os fitted values do modelo MLE
Base_Separação$MLE_fitted <- predict(modelo_MLE,Base_Separação)

#Gerando gráfico de observado x esperado no modelo multinível

ggplot(Base_Separação, aes(Itens,Tempo_Separação_Real)) + 
  geom_point(alpha=.7, size=.5, aes(colour='Observado')) +
  geom_point(aes(Itens,predict(modelo_intercept_inclin_separação, Base_Separação), colour='Esperado')) + #Ploting
  scale_color_viridis(discrete=TRUE, begin=0, end=.8, name = "Dado: ") +
  theme_bw() +
  theme(legend.position="bottom") +
  # guides(colour = guide_legend(label.position = "bottom")) +
  labs(title="Valores observados vs esperados") +
  scale_y_continuous(name= "Tempo de Separação") +
  scale_x_continuous(name= "Itens")


##########################Resumo dos Resultados#############################

Modelo <- c("MV",
            "MV_Box-Cox",
            "Árvore de Regressão 3 folhas",
            "Árvore de Regressão 4 folhas", 
            "Árvore de Regressão 5 folhas",
            "Random Forest 100 árvores",
            "Random Forest 50 árvores",
            "Multinível")

MAPE <- round(c(MAPE_ML, MAPE_ML_BC, MAPE_Arvore3, MAPE_Arvore4, MAPE_Arvore5, MAPE_RF100, MAPE_RF50, MAPE_Multinivel),3)

RMSE <- round(c(RMSE_ML, RMSE_ML_BC, RMSE_Arvore3, RMSE_Arvore4, RMSE_Arvore5, RMSE_RF100, RMSE_RF50, RMSE_Multinivel),3)

df_resultados <- data.frame(Modelo, MAPE, RMSE)

df_resultados %>%
  kable() %>%
  kable_styling(bootstrap_options = "striped", 
                full_width = T, 
                font_size = 12)

