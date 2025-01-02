# Carregar os pacotes necessários
library(rio)      # Para importar/exportar dados
library(sf)       # Para manipular dados espaciais
library(vegan)    # Para análise ecológica
library(tidyverse) # Para manipulação e visualização de dados
library(broom)    # Para organizar saídas de modelos em formato limpo

# Função auxiliar para capitalizar a primeira letra de uma string
# Isso pode ser útil ao manipular texto em dados
proper <- function(x) paste0(toupper(substr(x, 1, 1)), tolower(substring(x, 2)))

# Tema personalizado para visualizações no ggplot2
mathe_theme_normal <- theme(
  axis.title = element_text(face = "bold", size = 9, colour = "black"), # Títulos dos eixos
  axis.text = element_text(vjust = 0.6, colour = "black", size = 8),   # Texto dos eixos
  legend.text = element_text(size = 8, colour = "black"),             # Texto da legenda
  legend.title = element_text(size = 11, colour = "black"),           # Título da legenda
  panel.background = element_rect(fill = "white"),                    # Fundo do painel
  panel.grid.major = element_line(size = 0.5, linetype = 'solid', colour = "gray100"), # Grades principais
  panel.grid.minor = element_line(size = 0.25, linetype = 'dashed', colour = "gray80"), # Grades menores
  plot.background = element_rect(fill = "white")                     # Fundo do gráfico
)
theme_set(mathe_theme_normal) # Define este tema como padrão

# Importar o conjunto de dados de espécies
data <- import("Tabelas exportadas/dat1_spread_frag_species.csv")

# Remover colunas desnecessárias para a análise
data_frag <- data %>% select(Frag, Fragmento) # Guardar as colunas de fragmentos separadamente
data <- data %>% select(-Frag, -Fragmento)    # Excluir estas colunas do conjunto principal

# Gerar curva de acumulação de espécies usando a função specaccum do pacote vegan
spec_accum_result <- specaccum(data)

# Extrair valores de riqueza de espécies da curva
crescent_values <- spec_accum_result$richness                # Valores crescentes
decrescent_values <- rev(crescent_values)                   # Valores decrescentes (invertidos)
effort <- 1:length(crescent_values)                         # Esforço de amostragem (número de amostras)

# Combinar dados em um único data frame para plotagem
df <- data.frame(
  Effort = c(effort, effort),
  Richness = c(crescent_values, decrescent_values),
  Curva = rep(c("Crescente", "Decrescente"), each = length(crescent_values))
)

# Plotar a curva de acumulação de espécies
ggplot(df, aes(x = Effort, y = Richness, colour = Curva)) +
  geom_line(size = 1) +
  geom_point(size = 2) +
  labs(
    x = "Esforço de Amostragem",
    y = "Riqueza de Espécies"
  ) +
  scale_x_continuous(breaks = scales::pretty_breaks(n = 10)) +
  scale_colour_manual(values = c("Crescente" = "blue", "Decrescente" = "red")) +
  theme(legend.position = "bottom") # Legenda na parte inferior

# Salvar o gráfico como uma imagem PNG
ggsave("Graficos/Curva de acumulação de espécies.PNG", width = 5, height = 5, bg = "white")

# Calcular os índices de diversidade de Shannon e Simpson
data1 <- data %>% as.data.frame()                  # Converter os dados para um data frame
shannon <- diversity(data1, index = "shannon")    # Índice de diversidade de Shannon

data <- bind_cols(data, data_frag)                # Reunir dados fragmentados com os índices

# Adicionar os índices de diversidade ao conjunto de dados
data_ind <- data %>% 
  mutate(
    H = diversity(data1, index = "shannon"),  # Índice de Shannon
    S = diversity(data1, index = "simpson")   # Índice de Simpson
  ) %>% 
  select(Fragment = Frag, Shannon = H, Simpson = S) # Selecionar as colunas de interesse

# Exportar os índices de diversidade como um arquivo Excel
data_ind %>% export("Tabelas exportadas/data_species_diversity.csv")
