# Capsula 1. Regresión Lineal (OLS)
#Para correr cada línea CTRL + Enter


rm(list=ls())
# 1. Cargar librerías
#En R se debe **instalar** librerías (solo 1 vez, con `install.packages("librería")`), y luego **cargarlas** cada vez que es necesario usarlas (con `library(librería)`).

install.packages("dplyr") # Para manipular bases de datos
install.packages("sjPlot") #Para graficar
install.packages("broom") #Para transformar modelos en dataframe
install.packages("writexl") #Para exportar modelos en .xlsx

library(dplyr)
library(sjPlot)
library(broom)
library(writexl)

# 2. Cargar datos
#En esta cápsula trabajaremos con los datos de **Supermarket sales** publicado por Aung Pyae (https://www.kaggle.com/datasets/aungpyaeap/supermarket-sales?resource=download). 
#Para ello, utilizaremos la función `read.csv` de la librería `haven`

data <- read.csv("input/supermarket_sales - Sheet1.csv")

## Explorar datos
dim(data) 
View(data)

# 3. Selección de variables
#Mediante la función `select` de `dplyr`, seleccionamos cada una de nuestras variables de interés y creamos una nueva base con el nombre `ene_proc`, donde "proc" hace referencia a base procesada:
proc <- select(data, #base original
               tipo_cliente = 4, #Tipo de cliente
               genero = 5, #Género
               p_unitario = 7, #Precio unitario
               unidades = 8, #Unidades vendidas
               puntaje = 17, #Puntaje del cliente
               ing_bruto = 16) #Ingresos brutos

## Correlación entre variables cuantitativas

cor(proc %>% select(ing_bruto, p_unitario, unidades, puntaje))

### Correlación alta y positiva entre ingresos brutos y precio unitario y 
### unidades vendidas
### Correlación positiva y cercana a cero entre p_unitario y unidades
### Correlación negativa y cercana a cero entre puntaje del cliente e ingresos 
### brutos, precio unitario y unidades vendidas

sjPlot::plot_scatter(proc, #Datos
                     ing_bruto, #Eje x
                     p_unitario, #Eje y
                     fit.line = "lm") #Incorporar recta OLS
sjPlot::plot_scatter(proc, 
                     ing_bruto, 
                     unidades,
                     fit.line = "lm")
sjPlot::plot_scatter(proc, 
                     ing_bruto, 
                     puntaje,
                     fit.line = "lm")


proc %>% 
  group_by(tipo_cliente) %>%#Agrupamos por tipo de cliente
  summarise(media = mean(ing_bruto)) %>% #Para estimar diferencias promedio
  ungroup() #Desagrupamos

proc %>% 
  group_by(genero) %>% 
  summarise(media = mean(ing_bruto)) %>% 
  ungroup()

# 4. Estimación de modelos ------------------------------------------------

## Modelo nulo
nulo = lm(ing_bruto ~ 1, proc)

summary(nulo)

## Modelo simple ---------------------------------------------------------

### Predictores cuantitativos

m01 = lm(ing_bruto ~ p_unitario, proc)
summary(m01)
m02 = lm(ing_bruto ~ unidades, proc)
summary(m02)
m03 = lm(ing_bruto ~ puntaje, proc)
summary(m03)

### Predictores categóricos

m04 = lm(ing_bruto ~ tipo_cliente, proc)
summary(m04)

m05 = lm(ing_bruto ~ genero, proc)
summary(m05)


## Modelo múltiple ---------------------------------------------------------

m06 = lm(ing_bruto ~ p_unitario + unidades + puntaje + tipo_cliente + genero, proc)
summary(m06)

## Interacciones -----------------------------------------------------------

m07 = lm(ing_bruto ~ p_unitario*unidades + puntaje + tipo_cliente + genero, proc)
summary(m07)

m08 = lm(ing_bruto ~ p_unitario + puntaje + tipo_cliente + unidades*genero, proc)
summary(m08)

## Transformaciones funcionales --------------------------------------------

proc$p_unitario2 = (proc$p_unitario)^2
sjPlot::plot_scatter(proc, 
                     ing_bruto, 
                     p_unitario2,
                     fit.line = "lm")

m09 = lm(ing_bruto ~ p_unitario2 + unidades + tipo_cliente + genero, proc)
summary(m09)

proc$p_unitario_log = log(proc$p_unitario)
sjPlot::plot_scatter(proc, 
                     ing_bruto, 
                     p_unitario_log,
                     fit.line = "lm")

m10 = lm(ing_bruto ~ p_unitario_log + unidades + tipo_cliente + genero, proc)
summary(m10)


### Presentación gráfica ----------------------------------------------------

sjPlot::plot_model(m10,
                   title = "Coeficientes de regresion del modelo 10")

# Exportar modelo ---------------------------------------------------------

coef <- tidy(m10) 
coef = coef %>% 
  mutate_at(vars(2:5),
  ~(round(., digits = 3))) %>% 
  rename(Predictores = 1,
         "Coef." = 2,
         "Error Estandar" = 3,
         "Valor t" = 4,
         "P-valor" = 5)

writexl::write_xlsx(coef, "output/modelo10.xlsx")

ajuste = glance(m10) 
ajuste = ajuste %>% 
  mutate_all(~(round(., digits = 3))) %>% 
  select("R2" = 1,
         "R2 ajustado" = 2,
         AIC, BIC,
         "Devianza" = 10)

writexl::write_xlsx(ajuste, "output/ajuste_m10.xlsx")


# Análisis preliminar de supuestos ----------------------------------------

### Valores predichos (X) y Residuos (Y)

mod10 = augment(m10) #Crear objeto con valores predichos y residuos
plot_scatter(mod10, 
             .fitted,
             .resid)

### Valores observados en dependiente (X) y Residuos

plot_scatter(mod10, 
              ing_bruto,
              .resid)

### Valores observados en unidades (X) y residuos (Y)

plot_scatter(mod10, 
             unidades,
             .resid)

### Unidades (X) y valores predichos (Y)

plot_scatter(mod10,
             unidades, 
             .fitted)

