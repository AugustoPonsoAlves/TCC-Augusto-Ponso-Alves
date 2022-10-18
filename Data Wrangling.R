#Data Wrangling

#INSTALAÇÃO E CARREGAMENTO DE PACOTES NECESSÁRIOS

#Pacotes utilizados
pacotes <- c("plotly","tidyverse","ggrepel","fastDummies","knitr","kableExtra",
             "splines","reshape2","PerformanceAnalytics","metan","correlation",
             "see","ggraph","nortest","rgl","car","olsrr","jtools","ggstance",
             "magick","cowplot","beepr","Rcpp", "dplyr", "MLmetrics", "stats4", 
             "bbmle", "nlme", "DAAG", "factoextra", "Metrics", "BetterReg","fpp2",
             "gridExtra","data.table","ggseas","knitr","zoo","dgof")

if(sum(as.numeric(!pacotes %in% installed.packages())) != 0){
  instalador <- pacotes[!pacotes %in% installed.packages()]
  for(i in 1:length(instalador)) {
    install.packages(instalador, dependencies = T)
    break()}
  sapply(pacotes, require, character = T) 
} else {
  sapply(pacotes, require, character = T) 
}

#Carregando o arquivo Base_Separação_V9.xlsx

library(readxl)

Base_Separação <- read_excel("Base_Separação_V9.xlsx")

Base_Separação %>% head

#Renomeando as variáveis

Base_Separação <- dplyr::rename(Base_Separação, 
                                Hora_do_Carregamento = "Hora do carregamento",
                                Codigo_do_Transporte = "Código do Transporte",
                                Emb_Grande = "Emb. Grande",
                                Emb_Média = "Emb. Média",
                                Emb_Pequena = "Emb. Pequena",
                                Tempo_Separação = "Tempo Separação",
                                Tempo_Separação_Real = "Tempo Separação Real",
                                Itens_Hora = "Itens por hora")

Base_Separação %>% head

#Substituindo missing values por zero

Base_Separação$Emb_Grande <- base::replace(Base_Separação$Emb_Grande, is.na(Base_Separação$Emb_Grande), 0)

Base_Separação$Emb_Média <- base::replace(Base_Separação$Emb_Média, is.na(Base_Separação$Emb_Média), 0)

Base_Separação$Emb_Pequena <- base::replace(Base_Separação$Emb_Pequena, is.na(Base_Separação$Emb_Pequena), 0)

Base_Separação$Tempo_Separação_Real <- base::replace(Base_Separação$Tempo_Separação_Real, is.na(Base_Separação$Tempo_Separação_Real), 0)

Base_Separação %>% head

#Criando Variavel de contagem das observações

Index <- 1: length(Base_Separação$Tempo_Separação_Real)

#Incluindo Variável na base de dados
?mutate
Base_Separação <- dplyr::mutate(Base_Separação, Index)

#Visualizando disposição dos dados e outliers

ggplotly(
  ggplot(Base_Separação, aes(x = Index, y = Tempo_Separação_Real)) +
    geom_point(color = "#39568CFF", size = 0.5) +
    xlab("Index") +
    ylab("Tempo de Separação") +
    scale_color_manual("Legenda:",
                       values = "grey50") +
    theme_classic()
)

#Reordenando as variáveis

Base_Separação <- select(Base_Separação, Index, Itens,Tempo_Separação_Real, Itens_Hora, Emb_Grande, Emb_Média, Emb_Pequena, Almoxarife, everything())

#Elaborando um histogram da variável resposta para verificar sua distribuição.

hist(Base_Separação$Itens_Hora, main = paste("Histograma da variável", "Itens por hora"), xlab = "Itens por hora", ylab = "Frequência", breaks = 100)
hist(Base_Separação$Itens, main = paste("Histograma da variável", "Itens"), xlab = "Itens", ylab = "Frequência", breaks = 100)
hist(Base_Separação$Tempo_Separação_Real, main = paste("Histograma da variável", "Tempo Separação Real"), xlab = "Tempo de Separação", ylab = "Frequência", breaks = 50)

#Kernel density estimation (KDE) - função densidade de probabilidade da
#variável independente Itens por hora.

ggplotly(
  ggplot(Base_Separação, aes(x = Itens_Hora)) +
    geom_density(aes(x = Itens_Hora), 
                 position = "identity", color = "black", size = 1) +
    geom_histogram(aes(y = ..density..), color = "white", fill = "deepskyblue",
                   bins = 50) +
    theme_classic()
)


#Limpando outliers através da variável Itens por hora
#set.seed(0)
#boxplot(Base_Separação$Itens_Hora)

#outliers <- boxplot.stats(Base_Separação$Itens_Hora)$out

#Base_Separação <- Base_Separação[-which(Base_Separação$Itens_Hora %in% outliers),]

#boxplot(Base_Separação$Itens_Hora)

#outliers2 <- boxplot.stats(Base_Separação$Itens_Hora)$out

#Base_Separação <- Base_Separação[-which(Base_Separação$Itens_Hora %in% outliers2),]

#boxplot(Base_Separação$Itens_Hora)

#outliers3 <- boxplot.stats(Base_Separação$Itens_Hora)$out
#Base_Separação <- Base_Separação[-which(Base_Separação$Itens_Hora %in% outliers3),]

#boxplot(Base_Separação$Itens_Hora)

#outliers4 <- boxplot.stats(Base_Separação$Itens_Hora)$out

#Base_Separação <- Base_Separação[-which(Base_Separação$Itens_Hora %in% outliers4),]

#boxplot(Base_Separação$Itens_Hora)

#outliers5 <- boxplot.stats(Base_Separação$Itens_Hora)$out

#Base_Separação <- Base_Separação[-which(Base_Separação$Itens_Hora %in% outliers5),]

#p1<-boxplot(Base_Separação$Itens_Hora)


#Limpando outliers da variável resposta
#set.seed(0)
#boxplot(Base_Separação$Tempo_Separação_Real)

#outliers6 <- boxplot.stats(Base_Separação$Tempo_Separação_Real)$out

#Base_Separação <- Base_Separação[-which(Base_Separação$Tempo_Separação_Real %in% outliers6),]

#boxplot(Base_Separação$Tempo_Separação_Real)

#outliers7 <- boxplot.stats(Base_Separação$Tempo_Separação_Real)$out

#Base_Separação <- Base_Separação[-which(Base_Separação$Tempo_Separação_Real %in% outliers7),]

#boxplot(Base_Separação$Tempo_Separação_Real)

#outliers8 <- boxplot.stats(Base_Separação$Tempo_Separação_Real)$out

#Base_Separação <- Base_Separação[-which(Base_Separação$Tempo_Separação_Real %in% outliers8),]

#p2<-boxplot(Base_Separação$Tempo_Separação_Real)
#p2
?filter

#Regras de negócio

Base_Separação <- dplyr::filter(Base_Separação, Itens > 0)
Base_Separação <- dplyr::filter(Base_Separação, Itens < 300)
Base_Separação <- dplyr::filter(Base_Separação, Tempo_Separação_Real > 0)
Base_Separação <- dplyr::filter(Base_Separação, Tempo_Separação_Real < 3)
Base_Separação <- dplyr::filter(Base_Separação, Itens_Hora < 900)
Base_Separação <- dplyr::filter(Base_Separação, Itens_Hora > 120)

#Graficos Boxplot
Base_Separação_Long1 <- melt(Base_Separação[,3:4], id.vars = "Tempo_Separação_Real")
ggplot(Base_Separação_Long1) +
  geom_boxplot(aes(x = variable, y = value, fill = variable))

boxplot(Base_Separação$Itens_Hora)

#Kernel density estimation (KDE) após filtração dos dados

ggplotly(
  ggplot(Base_Separação, aes(x = Itens_Hora)) +
    geom_density(aes(x = Itens_Hora), 
                 position = "identity", color = "black", size = 1) +
    geom_histogram(aes(y = ..density..), color = "white", fill = "darkGreen",
                   bins = 50) +
    theme_classic()
)

hist(Base_Separação$Tempo_Separação_Real, main = paste("Histograma da variável", "Tempo Separação Real"), xlab = "Tempo Separação", ylab = "Frequência", breaks = 50)

#Transformando Almoxarife em categorias

Base_Separação$Almoxarife <- as.factor(Base_Separação$Almoxarife)

summary(Base_Separação$Itens_Hora)

Base_Separação %>%
  corr_plot(Itens, Tempo_Separação_Real,
            shape.point = 21,
            col.point = "black",
            fill.point = "#FDE725FF",
            size.point = 2,
            alpha.point = 0.6,
            maxsize = 4,
            minsize = 2,
            smooth = TRUE,
            col.smooth = "black",
            col.sign = "#440154FF",
            upper = "corr",
            lower = "scatter",
            diag.type = "density",
            col.diag = "#440154FF",
            pan.spacing = 0,
            lab.position = "bl")

