# Tutorial 1 - Parte 2 ----------------------------------------------------

# 1. Cargar librerias --------------------------------------------------------
install.packages("sjPlot") # se instala solo la primera vez
install.packages("forcats") # se instala solo la primera vez
install.packages("readxl") # se instala solo la primera vez
library(sjPlot)
library(forcats)
library(readxl)
# 2. Cargar datos ---------------------------------------------------------
data <- read_excel("input/Datos Cardio.xlsx")

# 3. Explorar datos -------------------------------------------------------
dim(data)
str(data)
summary(data)

# 4. Crear objeto glm -----------------------------------------------------

modelo1 <- glm(compra ~ edad, data = data, family = binomial(link = "logit"))
modelo2 <- glm(compra ~ as_factor(sexo), data = data, family = binomial(link = "logit"))
modelo3 <- glm(compra ~ edad + as_factor(sexo), data = data, family = binomial(link = "logit"))

# 5. Explorar objetos -----------------------------------------------------
summary(modelo1)
summary(modelo2)
summary(modelo3)

# 6. Tablas ---------------------------------------------------------------
tab_model(modelo1, modelo2, modelo3, transform = "exp", show.aic = T)

plot_model(modelo3, type = "est")
ggsave("figura1.png", plot = last_plot())
plot_model(modelo3, type = "pred", terms = c("edad"), show.data = T)
ggsave("figura2.png", plot = last_plot())
plot_model(modelo3, type = "pred", terms = c("edad", "sexo"), show.data = T)
ggsave("figura3.png", plot = last_plot())



