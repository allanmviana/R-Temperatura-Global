# Mudanças na Temperatura Global
library(tmap)
library(rWBclimate)
library(tidyr)
library(sf)
library(rworldmap)
library(rgeos)
library(RColorBrewer)

setwd("D:/Projetos em R/Temperatura Global")
codigos <- read.csv("ISO3 Codes.csv") #Pegando o código ISO3 de todos os países disponíveis

hist_data <- get_historical_temp(codigos[,1],"year") #Dados históricos de temperatura
hist_data$data <- round(hist_data$data, 3) #arrendondando as temperaturas pra 3 dígitos
df <- pivot_wider(hist_data, names_from = year, values_from = data) #transformando anos em colunas
colnames(df) <- paste("a", colnames(df), sep = "_")
names(df)[names(df)==names(df[1])] <- 'iso_a3' #trocando o nome da coluna 

pal <- brewer.pal(10, "Set3")[c(10, 8, 4, 5)]

# Função que troca os NA pela informação do último ano
for (y in 3:ncol(df)) {
  for (x in 2:nrow(df)) {
    if (is.na(df[x,y])) {
        df[x,y] <- df[x, y-1]      
    }
    x <- x+1
  }
  y <- y+1
}

teste <- merge(World, df, by = "iso_a3")

tm_shape(teste) +
  tm_polygons(c("a_1901","a_2012"),
              n = 100,
              style = "pretty")

teste$dif <- (teste$a_2012 - teste$a_1901)

tm_shape(teste) +
  tm_polygons("dif",
              n = 100,
              style = "cont",
              midpoint = 0,
              palette = c("blue", "white", "red"))
