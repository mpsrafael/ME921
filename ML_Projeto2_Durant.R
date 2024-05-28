## Trabalho 1 - Aprendizado de M?quinas N?o Supervisionado

library(rvest)
library(httr)
library(tidyverse)
library(GGally)
library(cluster)
library(factoextra)
library(mclust)
library(covRobust)


# Link do site contendo os dados
url = "http://bkref.com/pi/shareit/SOIaR"

# Obter os dados com base na url acima
resposta = GET(url)
conteudo_html = content(resposta, "text")
webpage = read_html(conteudo_html)
tabela = html_nodes(webpage, "table")
dados = html_table(tabela)

# Seleção e manipulação dos dados
colnames(dados[[1]])[3] = 'Date'
colnames(dados[[1]])[9] = 'W-L'

dados = dados[[1]] %>% select(`Date`, Series, Tm, Opp, G, `W-L`, MP, `FG%`, `3P%`, `FT%`, ORB, DRB, TRB, AST, STL, BLK, TOV, PF, PTS)

dados$`W-L` = substr(dados$`W-L`, 1, 1)
dados$Date = substr(dados$Date, 1, 4)


data = dados %>% select(PTS, TRB, AST, `3P%`, `FG%`, `FT%`) %>% filter(!PTS %in% c("Did Not Dress", "Inactive")) %>% drop_na()

for(i in 1:ncol(data)){
  data[[i]] = as.numeric(data[[i]])
}

rownames(data) = 1:nrow(data)


data_aux = data %>% mutate(
  `3P%` = data$`3P%`*100,
  `FT%` = data$`FT%`*100,
  `FG%` = data$`FG%`*100
)

ggpairs(data_aux, lower = list(continuous = "smooth"))
summary(data_aux)

## Completo
data_c = data %>% select(PTS, TRB, AST, `3P%`, `FG%`, `FT%`) %>% drop_na()
cluster_comp = Mclust(scale(data_c))
plot(cluster_comp, what = "BIC")

## "VEE" 2 Componentes
modelo_c1 = Mclust(scale(data_c), 2, modelNames = "VEE")
plot(modelo_c1, what = "classification")
summary(modelo_c1)
## "EII" 4 Componentes
modelo_c2 = Mclust(scale(data_c), 4, modelNames = "EII")
plot(modelo_c2, what = "classification")
summary(modelo_c2)

## Sem ruído
data_out = data %>% select(PTS, TRB, AST, `3P%`, `FG%`, `FT%`) %>% drop_na() 
nnve.out = cov.nnve(data_out)
cluster_srui = Mclust(scale(data_out), initialization = list(noise = (nnve.out$classification == 0)))
plot(cluster_srui, what = "BIC")

## "EII" 2 Componentes
modelo_r1 = Mclust(scale(data_out),2,modelNames = "EII", initialization = list(noise = (nnve.out$classification == 0)))
plot(modelo_r1, what = "classification")
summary(modelo_r1)
## "EEI" 2 Componentes
modelo_r2 = Mclust(scale(data_out),2,modelNames = "EEI", initialization = list(noise = (nnve.out$classification == 0)))
plot(modelo_r2, what = "classification")
summary(modelo_r2)

## Modelo de cluster escolhido: EEI com 2 componentes sem ruídos (outliers)
data_out$cluster = modelo_r2$classification

data_out = data_out %>% mutate(
  cluster = factor(modelo_r2$classification,
                   labels = c("Outliers", "C2", "C1"))
)
pivot_longer(data_out, -cluster, names_to = "Variaveis", values_to = "Valores") %>% 
  filter(Variaveis %in% c("PTS", "TRB", "AST"))%>% 
  ggplot(aes(x = Variaveis, y = Valores, fill = cluster))+
  geom_boxplot() + coord_flip()

pivot_longer(data_out, -cluster, names_to = "Variaveis", values_to = "Valores") %>% 
  filter(!Variaveis %in% c("PTS", "TRB", "AST"))%>% 
  ggplot(aes(x = Variaveis, y = Valores, fill = cluster))+
  geom_boxplot() + coord_flip()
