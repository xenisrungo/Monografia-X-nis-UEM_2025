#Load packages 
library(rio)
library(sf)
library(vegan)
library(tidyverse)
library(broom)
#detach("package:vegan", unload = TRUE)
#Proper function in R: 
proper=function(x) paste0(toupper(substr(x, 1, 1)), tolower(substring(x, 2)))

mathe_theme_normal=theme(axis.title = element_text(face = "bold",size = 9,colour="black"),
                         axis.text = element_text(vjust=0.6,colour = "black",size = 8),
                         legend.text = element_text(size = 8,colour="black"),
                         legend.title = element_text(size=11,colour="black"),
                         panel.background = element_rect(fill = "white"),
                         panel.grid.major = element_line(size = 0.5, linetype = 'solid',
                                                         colour = "gray100"),
                         panel.grid.minor = element_line(size = 0.25, linetype = 'dashed',
                                                         colour = "gray80"),
                         plot.background = element_rect(fill = "white"))
theme_set(mathe_theme_normal)
#Species accumulation
data <- import("Tabelas exportadas/dat1_spread_frag_species.xlsx")
data <- data %>% select(-Frag,-Fragmento)


# Generate species accumulation curve
spec_accum_result <- specaccum(data)

# Prepare data for ggplot
crescent_values <- spec_accum_result$richness
decrescent_values <- rev(crescent_values)
effort <- 1:length(crescent_values)

# Combine data into a single data frame
df <- data.frame(
  Effort = c(effort, effort),
  Richness = c(crescent_values, decrescent_values),
  Curva = rep(c("Crescente", "Decrescente"), each = length(crescent_values))
)

# Plot with ggplot
ggplot(df, aes(x = Effort, y = Richness, colour = Curva)) +
  geom_line(size = 1) +
  geom_point(size = 2) +
  labs(
    x = "Esforço amostral",
    y = "Riqueza específica"
  ) +
  scale_x_continuous(breaks = scales::pretty_breaks(n = 10)) +
  scale_colour_manual(values = c("Crescente" = "blue", "Decrescente" = "red")) +
  theme(legend.position = "bottom")


ggsave("Graficos/Curva de acumulação de espécies.PNG",width = 5,height = 5,bg="white")


#Índice de Shannon and Simpson
data1 <- data %>% select(-Frag,-Fragmento) %>%
  as.data.frame()
shannon <- diversity(data1, index = "shannon")

data_ind <- data %>% 
  mutate(H=diversity(data1, index = "shannon"),
         S=diversity(data1, index = "simpson")) %>% 
  select(Fragmento=Frag,H,S);data_ind

data_ind %>% export("Tabelas exportadas/data_species_diversity.xlsx")
