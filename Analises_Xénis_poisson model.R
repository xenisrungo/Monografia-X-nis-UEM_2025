# Carregar as bibliotecas necessárias
library(tidyverse)  # Para manipulação e visualização de dados
library(rio)        # Para importar/exportar dados
library(broom)      # Para organizar saídas de modelos em formato limpo
library(caret)      # Para técnicas de treino e validação de modelos

# Definir um tema personalizado para gráficos com ggplot2
mathe_theme_normal = theme(
  axis.title = element_text(face = "bold", size = 9, colour = "black"),  # Estilo dos títulos dos eixos
  axis.text = element_text(vjust = 0.6, colour = "black", size = 8),    # Estilo do texto dos eixos
  legend.text = element_text(size = 8, colour = "black"),               # Estilo do texto da legenda
  legend.title = element_text(size = 11, colour = "black"),             # Estilo do título da legenda
  panel.background = element_rect(fill = "white",                       # Fundo do painel
                                  colour = "grey9",
                                  size = 0.5, linetype = "solid"),
  panel.grid.major = element_line(size = 0.5, linetype = 'solid',       # Grades principais
                                  colour = "gray100"),
  panel.grid.minor = element_line(size = 0.25, linetype = 'dashed',     # Grades menores
                                  colour = "gray80"),
  plot.background = element_rect(fill = "white")                       # Fundo do gráfico
)
theme_set(mathe_theme_normal)  # Definir o tema como padrão

# Importar os dados
data <- import("Tabelas exportadas/esp_frag_sp.csv")

# Somar o número total de espécies
sum(data$sp)

# Verificar a distribuição dos dados
# 1. Scatterplot de Área vs Número de Espécies
ggplot(data, aes(x = Area, y = sp)) +
  geom_point() +
  labs(
    title = "Gráfico de Dispersão: Área vs Espécies de Aves",
    x = "Área do Fragmento (metros quadrados)",
    y = "Número de Espécies de Aves"
  )

# Gráfico de colunas: Fragmento vs Número de Espécies
ggplot(data, aes(x = as.factor(Frag), y = sp)) +
  geom_col() +
  labs(
    x = "Fragmento",
    y = "Número de Espécies"
  )
ggsave("Graficos/Fragmento vs Sp.PNG", height = 5, width = 5)

# Gráfico com rótulos indicando o número de espécies em cada fragmento
ggplot(data, aes(x = as.factor(Frag), y = sp)) +
  geom_col() +
  labs(
    x = "Fragmento",
    y = "Número de Espécies"
  ) +
  geom_text(aes(label = sp, x = as.factor(Frag), y = sp + 1))  # Adicionar rótulos
ggsave("Graficos/Fragmento vs Sp_labels.PNG", height = 5, width = 5)

# 2. Verificar assimetria usando histogramas
hist(data$Area, main = "Histograma da Área", xlab = "Área", breaks = 10)
hist(data$sp, main = "Histograma da Riqueza de Espécies", xlab = "Espécies (sp)", breaks = 10)

# 3. Aplicar transformação logarítmica na área
data$log_Area <- log(data$Area)

# Gráfico de dispersão com a área transformada (log)
ggplot(data, aes(x = log_Area, y = sp)) +
  geom_point() +
  geom_smooth(method = "lm", colour = "blue", se = FALSE) +  # Adicionar linha de regressão
  geom_text(aes(label = Frag), vjust = -0.5, size = 3) +     # Adicionar rótulos dos fragmentos
  labs(
    x = "Log (Área do Fragmento)",
    y = "Número de Espécies"
  )
ggsave("Graficos/Correlation_graph2a.PNG", height = 4, width = 4)

# 4. Análise de correlação
cor_result <- cor(data$log_Area, data$sp)  # Coeficiente de correlação
cat("Coeficiente de Correlação (Área transformada vs Número de Espécies):", cor_result, "\n")

# Ajustar um modelo de regressão de Poisson
poisson_model <- glm(sp ~ log_Area, family = poisson(link = "log"), data = data)

# Organizar e exportar os resultados do modelo
poisson_model %>% tidy(exponentiate = T, conf.int = T) %>% 
  export("Tabelas exportadas/GLM outputs.xlsx")

# Adicionar previsões e diferença entre valores observados e estimados
data <- data %>% 
  mutate(
    predicted_sp = predict(poisson_model, type = "response") %>% round(0),  # Previsões
    diff = sp - predicted_sp  # Diferença entre observado e previsto
  )
data %>% select(Fragmento = Frag, Spo = sp, Spe = predicted_sp, diff) %>% 
  export("Tabelas exportadas/riqueza observada e estimada.xlsx")

# Testar o modelo com validação cruzada LOOCV
control <- trainControl(method = "LOOCV")  # Configurar validação cruzada
model <- train(sp ~ log_Area, data = data, method = "glm", 
               family = poisson(link = "log"), trControl = control)  # Treinar o modelo
model
