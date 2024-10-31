library(tidyverse)
library(dplyr)
#Análisis exploratorio
#Importar archivo como df
df<-read.csv("Titanicv2.csv")
#Explorar variables y datos
head(df)
#Visualizar el df
View(df)
#Datos nulos
colSums(is.na(df))
#Eliminar datos nulos
df<-drop_na(df)
#Confirmar eliminación de datos nulos
colSums(is.na(df))
#Visualización de estructura
str(df)
#Resumen de estadísticos
summary(df)
#Gráfico de barras por sexo
df %>%drop_na() %>%  ggplot(aes(x=Sex,fill=Sex)) + geom_bar()
#Histograma por edad
df %>%drop_na() %>%  ggplot(aes(x=Age)) + geom_histogram()
#Diagramas de caja por Sexo,Edad y Tipo de Sobreviviente/Deceso
df %>%drop_na() %>%   ggplot(aes(x=Sex, y=Age,fill=Survived)) + geom_boxplot(alpha=0.5) 
#Gráfico de barras de Sobrevivientes por clases sociales
df %>% 
  filter(Survived=="Yes") %>%   
  ggplot(aes(x=Pclass,fill=Pclass)) + 
  geom_bar()
#Histograma de sobrevivientes por edad
df %>% 
  filter(Survived=="Yes") %>%   
  ggplot(aes(x=Age,fill=Age)) + 
  geom_histogram()
#Diagrama de dispersión de sobrevivientes por edad y clase social
df %>% 
  filter(Survived=="Yes") %>%   
  ggplot(aes(x = Age, y = Pclass)) + 
  geom_point()
##Gráfico de barras de Sobrevivientes por lugar de embarcación 
df %>% 
  filter(Survived=="Yes") %>%   
  ggplot(aes(x=Embarked,fill=Embarked)) + 
  geom_bar()
#Diagrama de dispersión no sobrevivientes por edad y clase social
df %>% 
  filter(Survived=="No") %>%   
  ggplot(aes(x = Age, y = Pclass)) + 
  geom_point()
##Gráfico de barras de no Sobrevivientes por lugar de embarcación 
df %>% 
  filter(Survived=="No") %>%   
  ggplot(aes(x=Embarked,fill=Embarked)) + 
  geom_bar()
#Histograma de no sobrevivientes por edad
df %>% 
  filter(Survived=="No") %>%   
  ggplot(aes(x=Age,fill=Age)) + 
  geom_histogram()

#Resumen de sobrevivientes
Resumen_Sobrevivientes<-
  df %>% 
  select(Sex,Pclass,Age,Survived) %>% 
  filter(Survived=="Yes") %>% 
  drop_na() %>% #quitando nulos
  group_by(Pclass) %>% 
  summarise( Edad.promedio=mean(Age),  
             Edad.mediana=median(Age),
             Edad.maxima=max(Age),              
             Edad.minima=min(Age),
             Total=n()
  ) %>%
  ungroup()
view(Resumen_Sobrevivientes)

#Resumen de no sobrevivientes
Resumen_nosobrevivientes<-
  df %>% 
  select(Sex,Pclass,Age,Survived) %>% 
  filter(Survived=="No") %>% 
  drop_na() %>% #quitando nulos
  group_by(Pclass) %>% 
  summarise( Edad.promedio=mean(Age),  
             Edad.mediana=median(Age),
             Edad.maxima=max(Age),              
             Edad.minima=min(Age),
             Total=n()
  ) %>%
  ungroup()
view(Resumen_ nosobrevivientes)