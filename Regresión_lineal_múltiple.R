#--------------------
# Regresión Lineal Múltiple
# Ejemplo ilustrativo
# Datos para ejemplificar una regresión múltiple 
# relacionada con los componentes de democracia en México, 
# correspondientes al periodo de 2000 a 2021
#--------------------

library(tidyverse)
library(readxl)
library(ggcorrplot)

datos <- read_excel("datos_regresion.xlsx")

glimpse(datos)

# calcular la matriz de correlación

corr_datos <- datos %>% select(anio, homicidio_doloso, homicidio_culposo,
                               secuestro, extorsion, robo_violencia, 
                               robo_vehiculo, robo_casa, robo_negocio,
                               robo_transeunte, violacion, lesiones_dolosas,
                               feminicidio, dem_poliarquia) %>% # feminicidio: imputación por la media
  cor(use ="pairwise") %>% round(1)

ggcorrplot(corr_datos,type="lower",lab=T,show.legend=T) #upper

# Gráfico de columnas para variable dependiente dem_poliarquia

ggplot(datos, aes(x = anio, y = dem_poliarquia)) +
  geom_col(fill = "coral") +
  labs(title = "Componente electoral de la democracia en México (2000-2021)",
       x = "Años", y = "Democracia electoral") +
  theme_minimal()

# Gráfico para cada variable independiente

variables_independientes <- c("homicidio_doloso", "homicidio_culposo", "secuestro", "extorsion", 
                              "robo_violencia", "robo_vehiculo", "robo_casa", "robo_negocio",
                              "robo_transeunte", "violacion", "lesiones_dolosas", "feminicidio")

for (var in variables_independientes) {
  grafica_var <- ggplot(datos, aes(x = datos$anio, y = datos[[var]])) +
    geom_col(fill = "coral") +
    labs(title = paste("Tasa de", gsub("_", " ", var), "en el tiempo (años)"),
         x = "Años", y = gsub("_", " ", var)) +
    theme_minimal() +
    theme(
      title = element_text(size = 9),
      axis.title.x = element_text(size = 7),
      axis.title.y = element_text(size = 7)
    )
  
  print(grafica_var)
  
  ggsave(paste0(var, "_por_años.png"), width = 6, height = 4)
}



# Modelo lm

modelo <- lm(dem_poliarquia ~ 1 + homicidio_doloso +
             secuestro + extorsion + lesiones_dolosas + 
               feminicidio, data = datos)


summary(modelo)


anova(modelo)

step(object = modelo, direction = "both", trace = 1) # mejor modelo

modelo2 <- lm(dem_poliarquia ~ 1 + secuestro + extorsion, data = datos)

summary(modelo2)

# Datos para ejemplificar predicciones
# Las predicciones derivadas no deben considerarse válidas debido a la falta de significancia en los valores
# Este fragmento sirve sólo con fines ilustrativos

P1 <- data.frame(secuestro = seq(5:10), extorsion = seq(5:10))

predict(modelo2, P1)

