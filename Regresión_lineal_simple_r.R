---
title: "Regresion lineal simple"
author: "Gabriel Tehozol Hernández"
date: "15 de febrero 2023"
output: 
   html_document:
      toc: True # Tabla de contenido
      toc_depth: 2 # Niveles de la tabla de contenido
      toc_float: True # Tabla de contenido presente en todo el documento
      collapsed: True # Nivel principal de cada sección del documento
      smooth_scroll: True # Reflejar tabla de contenido
      theme: journal # Tema del documento 
      highlight: kate # Manera en la que se va a presentar el código
      df_print: paged # Como apareceran los dataframes
      code_folding: show # Por si el lector quiere ver la lineas de cód.
---
# Modelo convencional
En este markdown realizaremos un modelo de regresión simple

## Importamos librerías
```{r, warning=FALSE, message=FALSE}
library(caTools)
library(tidyverse)
library(car)
library(boot)
library(QuantPsyc)
library(ggplot2)
```

## Obtenemos datos
En esta ocasión vamos a oobtener los datos de una base de datos radicada en un repositorio en GitHub, definimos la variable donde hallaremos la liga de la base y posteriormente la leemos y observamos los primeros 5 datos
```{r}
liga <- "https://raw.githubusercontent.com/AnnaShestova/salary-years-simple-linear-regression/master/Salary_Data.csv"
datos <- read.csv(liga)
head(datos)
str(datos)
```
## Tratamiento de datos 

Renombramos las columnas donde: 
* Añexp = Años de experiencia 
* Sal = Salario
```{r}
names(datos) <- c("Añexp", "Sal")
head(datos)
```

## Relizamos el modelo
```{r}
modelo = lm(Sal~Añexp, data = datos, na.action=na.exclude)
summary(modelo)
```

## Interpretación del modelo
Tenemos betas significativas ya que el estadistico "t", es menor a cero.
A su vez, el ajuste del modelo dado por R^2 nos aroja 0.95, es decir, hay un ajuste considerable para efectos del ejemplo.

Podemos interpretar que por cada año adicional de experiencia, el salario aumentará en 9450 unidades monetarias. Es decir, cuando carecemos de años de experiencia el salario sería de 25792.2 unidades monetarias

##correlación de pearson
Para obtener la correlación de Pearson, podemos sacar la raiz cuadrada de la R^2
```{r}
sqrt(0.957)
```
## Gráficamos 
Realizamos una gráfica de puntos y gráficamos la regresión
```{r}
grafica = ggplot(data = datos, mapping = aes(Añexp, Sal)) + 
   geom_point() + 
   geom_smooth(method = 'lm', colour = 'Red')
grafica
```

# Modelo de aprendizaje autómatico (Machine Learning) 
## Entrenamiento del modelo
Se separan los datos que se usaran para entrenar el modelo y lo que posteriormente serán usados para probarlo
```{r}
set.seed(123)
split = sample.split(datos$Sal, SplitRatio = 2/3)
dato_entreamiento = subset(datos, split == TRUE)
dato_prueba = subset(datos, split == FALSE)
```

## Definimos el modelo
Definimos un modelo de regresión lineal simple
```{r}
modelo_ml = lm(Sal~Añexp, data = dato_entreamiento)
preds = predict(modelo_ml, newdata = dato_prueba)
summary(modelo_ml)
```
## Interpretación modelo lm de aprendizaje autómatico
Nuevamente, tenemos betas significativas ya que el estadistico "t", es menor a cero.
A su vez, el ajuste del modelo dado por R^2 nos aroja 0.96, es decir, se ha mejorado e alcance del modelo a través del aprendizaje autómatico.

Podemos interpretar que por cada año adicional de experiencia, el salario aumentará en 9365 unidades monetarias. Es decir, cuando carecemos de años de experiencia el salario sería de 25592 unidades monetarias

## Graficamos el modelo
```{r}
ggplot() +
  geom_point(aes(x = dato_entreamiento$Añexp, y = dato_entreamiento$Sal),
             colour = 'red') +
  geom_line(aes(x = dato_entreamiento$Añexp, y = predict(modelo_ml, newdata = dato_entreamiento)),
            colour = 'blue') +
  geom_smooth(method = 'lm') +
  ggtitle('Impacto de la experiencia laboral en el salario (datos entrenamiento)') +
  xlab('Años de experiencia') +
  ylab('Salario')
```

```{r}
ggplot() +
  geom_point(aes(x = dato_prueba$Añexp, y = dato_prueba$Sal),
             colour = 'red') +
  geom_line(aes(x = dato_entreamiento$Añexp, y = predict(modelo_ml, newdata = dato_entreamiento)),
            colour = 'blue') +
  geom_smooth(method = 'lm') +
  ggtitle('Impacto de la experiencia laboral en el salario (datos prueba)') +
  xlab('Años de experiencia') +
  ylab('Salario')
```
Es posible observar de manera gráfica, que el modelo de datos entrenado se adapta a los datos prueba, lo que comprueba la eficacia de los modelos de aprendizaje automático.