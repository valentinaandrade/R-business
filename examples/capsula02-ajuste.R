# Capsula 2. Bondad de ajuste y análisis de supuestos para regresión lineal

# 1. Cargar librerías

install.packages("sjPlot") #Para graficar
install.packages("dplyr") #Para seleccionar variables
install.packages("performance")#Para analizar supuestos

library(sjPlot)
library(dplyr)
library(performance)

# 2. Cargar datos 

data <- read.csv("input/supermarket_sales - Sheet1.csv")

# 3. Selección de variables
#Mediante la función `select` de `dplyr`, seleccionamos cada una de nuestras variables de interés y creamos una nueva base con el nombre `ene_proc`, donde "proc" hace referencia a base procesada:
proc <- select(data, #base original
               tipo_cliente = 4, #Tipo de cliente
               genero = 5, #Género
               p_unitario = 7, #Precio unitario
               unidades = 8, #Unidades vendidas
               puntaje = 17, #Puntaje del cliente
               ing_bruto = 16) #Ingresos brutos


# 4. Crear modelos --------------------------------------------------------
m01 = lm(ing_bruto ~ p_unitario, proc)
m02 = lm(ing_bruto ~ unidades, proc)
m03 = lm(ing_bruto ~ p_unitario + unidades + puntaje + genero + tipo_cliente, proc)

summary(m01)
summary(m02)
summary(m03)

# 5. Medidas de ajuste ----------------------------------------------------

# R2 y R2 ajustado

summary(m01)$r.squared
summary(m02)$r.squared
summary(m03)$r.squared

summary(m01)$adj.r.squared
summary(m02)$adj.r.squared
summary(m03)$adj.r.squared

# 6. Análisis de supuestos ---------------------------------------------------

## Linealidad --------------------------------------------------------------

sjPlot::plot_scatter(proc, 
                     ing_bruto, 
                     p_unitario,
                     fit.line = "lm")

check_model(m03, check = c("ncv", "linearity"))

#Existe patrón, no hay linealidad
#Relación (no) lineal entre residuos y predichos

proc$p_unitario_log = log(proc$p_unitario)
sjPlot::plot_scatter(proc, 
                     ing_bruto, 
                     p_unitario_log,
                     fit.line = "lm")

m04 = lm(ing_bruto ~ p_unitario_log + unidades + puntaje + genero + tipo_cliente, proc)
check_model(m04, check = c("ncv", "linearity"))

## Test homogeneidad de varianza -------------------------------------------
check_heteroscedasticity(m03) #Se rechaza la hipotesis nula de que los errores 
# no varían homogéneamente

## Normalidad de residuos --------------------------------------------------
check_normality(m03) #Test Saphiro-Wilk
# Los residuos no se distribuyen normalmente

qqnorm(residuals(m03), pch = 1)
qqline(residuals(m03), col = "steelblue", lwd = 2)

## Multicolinealidad -------------------------------------------------------
plot(check_collinearity(m03)) #Sin problemas de multicolinealidad

# Independencia de residuos -----------------------------------------------
check_autocorrelation(m03) #Durbin-Watson
# Residuos independientes y no autocorrelacionados

## Casos influyentes -------------------------------------------------------
check_outliers(m03)
plot(check_outliers(m03))
# Sin problemas de casos influyentes
