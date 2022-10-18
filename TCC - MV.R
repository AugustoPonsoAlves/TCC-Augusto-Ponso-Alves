#TCC - MLE

#INSTALAÇÃO E CARREGAMENTO DE PACOTES NECESSÁRIOS

#Pacotes utilizados
pacotes <- c("plotly","tidyverse","ggrepel","fastDummies","knitr","kableExtra",
             "splines","reshape2","PerformanceAnalytics","metan","correlation",
             "see","ggraph","nortest","rgl","car","olsrr","jtools","ggstance",
             "magick","cowplot","beepr","Rcpp", "dplyr", "MLmetrics", "stats4", 
             "bbmle", "nlme", "DAAG", "factoextra", "Metrics", "BetterReg",
             "gridExtra","data.table","ggseas","knitr","zoo", "dgof")

if(sum(as.numeric(!pacotes %in% installed.packages())) != 0){
  instalador <- pacotes[!pacotes %in% installed.packages()]
  for(i in 1:length(instalador)) {
    install.packages(instalador, dependencies = T)
    break()}
  sapply(pacotes, require, character = T) 
} else {
  sapply(pacotes, require, character = T) 
}

#Teste de Breusch-Pagan para diagnóstico de heterocedasticidade

?ols_test_breusch_pagan
modelo_OLS <- lm(formula = Tempo_Separação_Real ~  Itens,
                 data = Base_Separação)


ols_test_breusch_pagan(modelo_OLS)

#Calculando o modelo ML para o Tempo de Separação pelo método da máxima verossimilhança restrita.
?gls
modelo_ML <- gls(model = Tempo_Separação_Real ~  Itens,
                 data = Base_Separação,
                 method = "REML")

#Shapiro-Francia: n > 30
?sf.test
sf.test(modelo_ML$residuals) #função sf.test do pacote nortest

#Shapiro-Francia para n>5000
sf.test_bigdata <- function (x) 
{
  DNAME <- deparse(substitute(x))
  x <- sort(x[complete.cases(x)])
  n <- length(x)
  if ((n < 5 || n > 50000)) #alterado para 50000
    stop("sample size must be between 5 and 5000")
  y <- qnorm(ppoints(n, a = 3/8))
  W <- cor(x, y)^2
  u <- log(n)
  v <- log(u)
  mu <- -1.2725 + 1.0521 * (v - u)
  sig <- 1.0308 - 0.26758 * (v + 2/u)
  z <- (log(1 - W) - mu)/sig
  pval <- pnorm(z, lower.tail = FALSE)
  RVAL <- list(statistic = c(W = W), p.value = pval, method = "Shapiro-Francia normality test", 
               data.name = DNAME)
  class(RVAL) <- "htest"
  return(RVAL)
}
sf.test_bigdata(modelo_ML$residuals)

??ks.test

dgof::ks.test(modelo_ML$residuals,"pnorm",mean(modelo_ML$residuals),sd(modelo_ML$residuals))

??ad.test

ad.test(modelo_ML$residuals)

shapiro.test(modelo_ML$residuals)

summary(modelo_ML$residuals)

#Histograma dos Resíduos do modelo_ML

Resíduos_ML <- Base_Separação %>%
  mutate(resíduos = modelo_ML$residuals) %>%
  ggplot(aes(x = resíduos)) +
  geom_histogram(aes(y = ..density..), 
                 color = "grey10", 
                 fill = "blue", 
                 bins = 50,
                 alpha = 0.6) +
  stat_function(fun = dnorm, 
                args = list(mean = mean(modelo_ML$residuals),
                            sd = sd(modelo_ML$residuals)),
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

Resíduos_ML

??checkresiduals

forecast::checkresiduals(modelo_ML$residuals)

#Para calcular o lambda de Box-Cox
lambda_BC <- powerTransform(Base_Separação$Tempo_Separação_Real) #função powerTransform do pacote car#
lambda_BC


#Inserindo o lambda de Box-Cox na base de dados para a estimação de um novo modelo
Base_Separação$Tempo_Separação_Real_bc <- (((Base_Separação$Tempo_Separação_Real ^ lambda_BC$lambda) - 1) / 
                           lambda_BC$lambda)

#Estimando um novo modelo OLS com variável dependente transformada por Box-Cox
modelo_ML_bc <- gls(model = Tempo_Separação_Real_bc ~ Itens,
                data = Base_Separação,
                method = "REML")

Base_Separação$yhat_linear <- predict(modelo_ML,Base_Separação)
Base_Separação$yhat_modelo_bc <- (((predict(modelo_ML_bc,Base_Separação)*(lambda_BC$lambda))+
                                     1))^(1/(lambda_BC$lambda))
#Parâmetros do modelo
summary(modelo_ML_bc)


export_summs(modelo_ML,modelo_ML_bc)

MAPE_ML <- MAPE(Base_Separação$yhat_linear,Base_Separação$Tempo_Separação_Real)
MAPE_ML

RMSE_ML <- rmse(Base_Separação$Tempo_Separação_Real,Base_Separação$yhat_linear)
RMSE_ML 

MAPE_ML_BC <- MAPE(Base_Separação$yhat_modelo_bc, Base_Separação$Tempo_Separação_Real)
MAPE_ML_BC

RMSE_ML_BC <- rmse(Base_Separação$Tempo_Separação_Real,Base_Separação$yhat_modelo_bc)
RMSE_ML_BC 

sf.test_bigdata(modelo_ML_bc$residuals)# O QUE FAZER?

??ks.test

dgof::ks.test(modelo_ML_bc$residuals,"pnorm",mean(modelo_ML_bc$residuals),sd(modelo_ML_bc$residuals))

??ad.test

ad.test(modelo_ML_bc$residuals)

shapiro.test(modelo_ML_bc$residuals)

#Histograma dos resíduos do modelo_ML_bc

Resíduos_MLBoxCox <- Base_Separação %>%
  mutate(residuos = modelo_ML_bc$residuals) %>%
  ggplot(aes(x = residuos)) +
  geom_histogram(aes(y = ..density..), 
                 color = "grey10", 
                 fill = "blue", 
                 bins = 50,
                 alpha = 0.6) +
  stat_function(fun = dnorm, 
                args = list(mean = mean(modelo_ML_bc$residuals),
                            sd = sd(modelo_ML_bc$residuals)),
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

Resíduos_MLBoxCox

plot_grid(Resíduos_ML, Resíduos_MLBoxCox)


#Diagnóstico de multicolinearidade (Variance Inflation Factor e Tolerance)

#Modelo contém menos que 2 termos, portanto não há risco de multicolinearidade

#H0 do teste: ausência de heterocedasticidade.
#H1 do teste: heterocedasticidade, ou seja, correlação entre resíduos e uma ou mais
#variáveis explicativas, o que indica omissão de variável relevante!

#Gráfico do modelo ML

ggplot(Base_Separação, aes(Itens,Tempo_Separação_Real)) + 
  geom_point(alpha=.7, size=.5, aes(colour='Observado')) +
  geom_line(aes(Itens,yhat_linear, colour='Esperado'), size=2) + #Ploting
  scale_color_viridis(discrete=TRUE, begin=0, end=.8, name = "Dado: ") +
  theme_bw() +
  theme(legend.position="bottom") +
  # guides(colour = guide_legend(label.position = "bottom")) +
  labs(title="Valores observados vs esperados - Modelo [MV]") +
  scale_y_continuous(name= "Tempo de Separação") +
  scale_x_continuous(name= "Itens")

#Gráfico do modelo ML_bc

ggplot(Base_Separação, aes(Itens,Tempo_Separação_Real)) + 
  geom_point(alpha=.7, size=.5, aes(colour='Observado')) +
  geom_line(aes(Itens,yhat_modelo_bc, colour='Esperado'), size=2) + #Ploting
  scale_color_viridis(discrete=TRUE, begin=0, end=.8, name = "Dado: ") +
  theme_bw() +
  theme(legend.position="bottom") +
  # guides(colour = guide_legend(label.position = "bottom")) +
  labs(title="Valores observados vs esperados - Modelo [MV]Box-Cox") +
  scale_y_continuous(name= "Tempo de Separação") +
  scale_x_continuous(name= "Itens")

