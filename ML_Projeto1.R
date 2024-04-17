## Trabalho 1 - Aprendizado de Máquinas Não Supervisionados

library(rvest)
library(httr)
library(tidyverse)
library(GGally)
library(cluster)
library(factoextra)

# URL que contém o conjunto de dados a ser trabalhado
url = "https://www.pff.com/news/draft-nfl-combine-results-2023"

# Obter o conteúdo HTML da página com as tabelas
resposta = GET(url)
conteudo_html = content(resposta, "text")
webpage = read_html(conteudo_html)
tabela = html_nodes(webpage, "table")
dados = html_table(tabela)

# Unir as 4 tabelas presentes na página e renomear as colunas
for(i in 1:4){
  nomes_colunas = dados[[i]][1,]
  dados[[i]] = dados[[i]][-1,]
  colnames(dados[[i]]) = nomes_colunas
  dados[[i]] = dados[[i]] %>% dplyr::select(Position, Name, School, Height, Weight)
}

dados = rbind(dados[[1]], dados[[2]], dados[[3]], dados[[4]])

# Converter a coluna Altura de pés/polegadas para cm.
for(i in 1:length(dados$Height)){
  dados$Height[i] = paste(substring(dados$Height[i], c(1,2),c(1,3)), collapse="'")
}

dados$Position[dados$Position == 'WO'] = 'WR'

dados = dados %>% #filter(Position %in% c('QB', 'RB', 'WR')) %>%  
  separate(Height, c('feet', 'inches'), "'", convert = TRUE) %>% 
  mutate(Height = round((12*feet + inches)*2.54, digits = 1),
         Weight = round(as.integer(Weight) * 0.45, digits = 1))

dados = dados %>% dplyr::select(Position, Name, School, Height, Weight)

dados_a = dados %>% filter(Position %in% c('QB', 'WR', 'TE', 'RB', 'OL'))
dados_d = dados %>% filter(Position %in% c('LB', 'DB', 'DL'))


# Gráfico de correlação e distribuição das variáveis 
ggpairs(dados_a[c(4,5)], lower = list(continuous = "smooth"), )
ggpairs(dados_d[c(4,5)], lower = list(continuous = "smooth"))


ataque = dados_a %>% drop_na() %>% dplyr::select(Height, Weight)
ataque = unique(ataque)
defesa = dados_d %>% drop_na() %>% dplyr::select(Height, Weight)
defesa = unique(defesa)
rownames(ataque) = 1:nrow(ataque)
rownames(defesa) = 1:nrow(defesa)

cluster_a = hclust(dist(ataque), method = 'average')
cluster_d = hclust(dist(defesa), method = 'average')

plot(cluster_a, ylab = 'h')
abline(h = 9, col = "Red", lty = 2)
plot(cluster_d, ylab = 'h')
abline(h = 9, col = "Red", lty = 2)

groups <- cutree(cluster_a, h = 9)
plot(ataque, col = groups, xlab = 'Altura (cm)', ylab = 'Peso (Kg)')
for(i in unique(groups)){
  LLL <- ataque[groups == i,]
  xts <- chull(LLL)
  xts <- c(xts,xts[1])
  lines(LLL[xts,], col = i)
}

# gráfico defesa bem robusto, juntando clusteres.
groups <- cutree(cluster_d, h = 9)
plot(defesa, col = groups, xlab = 'Altura (cm)', ylab = 'Peso (Kg)')
for(i in unique(groups)){
  LLL <- defesa[groups == i,]
  xts <- chull(LLL)
  xts <- c(xts,xts[1])
  lines(LLL[xts,], col = i)
}
