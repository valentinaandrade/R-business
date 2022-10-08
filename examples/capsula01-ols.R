# Capsula 1. Procesamiento de datos
#Para correr cada línea CTRL + Enter

# 1. Cargar librerías
#En R se debe **instalar** librerías (solo 1 vez, con `install.packages("librería")`), y luego **cargarlas** cada vez que es necesario usarlas (con `library(librería)`).

install.packages("haven") # Para cargar bases de datos en formato SPSS, STATA y csv
install.packages("dplyr") # Para manipular bases de datos
install.packages("car") # Para recodificar variables

library(haven)
library(dplyr)
library(car)

# 2. Cargar datos
#En esta cápsula trabajaremos con los datos de **ene 2017**. 
#Para ello, utilizaremos la función `read_sav` de la librería `haven`

ene <- read_sav(file = "ENE 2020 MAM.sav")

#STATA (formato .dta) ocupar read_dta
#CSV (formato .csv) ocupar read_csv

## Explorar datos
dim(ene) 
View(ene)

# 3. Selección de variables
#Mediante la función `select` de `dplyr`, seleccionamos cada una de nuestras variables de interés y creamos una nueva base con el nombre `ene_proc`, donde "proc" hace referencia a base procesada:
ene_proc <- select(ene, #base original
                   c2_1_2, # horas semana
                   sexo, tramo_edad, cine) #sexo, edad, nivel educación (CINE)

#Para saber las variables que contiene una base utilizamos `names`
names(ene_proc)

# 4. Procesamiento de variables

## 4.1 Descriptivos

summary(ene_proc)

## 4.2 Recodificación 
#Para recodificar utilizamos la función `recode`, de la librería `car`

# Para cine
ene_proc$cine <- car::recode(ene_proc$cine, c("c(999,888)=NA"))

#Para sexo
ene_proc$sexo <- as.numeric(ene_proc$sexo)
ene_proc$sexo <- car::recode(ene_proc$sexo, c("'1'='Hombre';'2'='Mujer'"))

#Para horas
ene_proc$c2_1_2 <- car::recode(ene_proc$c2_1_2, c("c(999,888)=NA"))

## Etiquetamiento
ene_proc <- rename(ene_proc,
                   "horas"=c2_1_2, #horas
                   "n_educ" =cine) #nivel educacional

# 5. Creación de variables
#Imaginemos que queremos recodificar la variable edad en **tramos de edad**
ene_proc$tramo_edad <- as.numeric(ene_proc$tramo_edad)
ene_proc$t_edad <- car::recode(ene_proc$tramo_edad,
                               c("1:4='Jovenes';5:9='Adultos';10:12='Adultos mayores'"))
#Para verificar
table(ene_proc$t_edad)

# 6. Guardar base de datos procesada
summary(ene_proc)

save(ene_proc,file = "ene.RData")


# 7. ¡Ahora te toca a ti!
## Realiza este mismo ejercicio seleccionando y codificando variables que sean de tu interés en ENE
