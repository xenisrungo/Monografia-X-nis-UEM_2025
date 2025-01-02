# Load necessary libraries
library(tidyverse)
library(rio)
library(broom)
library(caret)

mathe_theme_normal=theme(axis.title = element_text(face = "bold",size = 9,colour="black"),
                         axis.text = element_text(vjust=0.6,colour = "black",size = 8),
                         legend.text = element_text(size = 8,colour="black"),
                         legend.title = element_text(size=11,colour="black"),
                         panel.background = element_rect(fill = "white",
                                                         colour = "grey9",
                                                         size = 0.5, linetype = "solid"),
                         panel.grid.major = element_line(size = 0.5, linetype = 'solid',
                                                         colour = "gray100"),
                         panel.grid.minor = element_line(size = 0.25, linetype = 'dashed',
                                                         colour = "gray80"),
                         plot.background = element_rect(fill = "white"))
theme_set(mathe_theme_normal)
#load data
data <- import("Tabelas exportadas/esp_frag_sp.xlsx") %>% 
  select(Frag,sp,Area)

sum(data$sp)

#Check the data distribution
# 1. Scatterplot of Area vs sp
ggplot(data, aes(x = Area, y = sp)) +
  geom_point() +
  labs(title = "Scatterplot of Area vs Bird Species",
       x = "Fragment Area (sq metres)",
       y = "Number of Bird Species")

ggplot(data, aes(x = as.factor(Frag), y = sp)) +
  geom_col() +
  labs(x = "Fragmento",
       y = "Número de espécies")
ggsave("Graficos/Fragmento vs Sp.PNG",height = 5,width = 5)

ggplot(data, aes(x = as.factor(Frag), y = sp)) +
  geom_col() +
  labs(x = "Fragmento",
       y = "Número de espécies")+
  geom_text(aes(label = sp,x = as.factor(Frag), y = sp+1))
ggsave("Graficos/Fragmento vs Sp_labels.PNG",height = 5,width = 5)

# 2. Check for skewness using histograms
hist(data$Area, main = "Histogram of Area", xlab = "Area", breaks = 10)
hist(data$sp, main = "Histogram of Species Richness", xlab = "Species (sp)", breaks = 10)

# 3. Log-transform the area
data$log_Area <- log(data$Area)

# Scatterplot with log-transformed Area
ggplot(data, aes(x = log_Area, y = sp)) +
  geom_point() +
  geom_smooth(method = "lm", colour = "blue", se = FALSE)+
  geom_text(aes(label = Frag), vjust = -0.5, size = 3)+
  labs(
       x = "Log(Fragment Area)",
       y = "Number of Species")
ggsave("Graficos/Correlation_graph2a.PNG",height = 4,width = 4)

# 4. Correlation analysis
cor_result <- cor(data$log_Area, data$sp)
cat("Correlation coefficient (log-transformed Area vs sp):", cor_result, "\n")

# Fit the Poisson regression model
poisson_model <- glm(sp ~ log_Area, family = poisson(link = "log"), data = data)

poisson_model %>% tidy(exponentiate = T,conf.int = T)
poisson_model %>% tidy(exponentiate = T,conf.int = T) %>% export("Tabelas exportadas/GLM outputs.xlsx")

data$predicted_sp <- predict(poisson_model, data = test_data, type = "response")
data <- data %>% 
  mutate(predicted_sp=predict(poisson_model, data = test_data, type = "response") %>% round(0)) %>% 
  mutate(diff=sp-predicted_sp);data
data%>%select(Fragmento=Frag,Spo=sp,Spe=predicted_sp,diff) %>%  export("Tabelas exportadas/riqueza obervada e estimada.xlsx")


#Model test
# Define LOOCV method
control <- trainControl(method = "LOOCV")

# Train the model (e.g., linear regression)
model <- train(sp ~ log_Area, data = data, method = "glm", 
               family = poisson(link = "log"), trControl = control);model

