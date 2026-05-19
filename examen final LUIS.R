# =========================================
# EXAMEN FINAL - DISEÑOS EXPERIMENTALES
# Comparación de crecimiento de plántulas
# =========================================
# Limpiar entorno
rm(list=ls())
# -----------------------------
# Datos
# -----------------------------
cercado <- c(
  35.2, 36.1, 34.8, 37.0, 35.9,
  36.5, 34.9, 35.7, 36.2, 35.4,
  36.8, 35.1, 36.0, 35.6, 34.7,
  36.4, 35.8, 36.1, 35.3, 36.6,
  35.5, 36.3, 35.9, 36.0, 35.7
)
sin_cercado <- c(
  28.1, 29.0, 27.8, 28.5, 29.2,
  28.7, 27.9, 28.4, 29.1, 28.3,
  28.8, 27.7, 28.6, 28.9, 27.6,
  29.0, 28.2, 28.5, 27.8, 28.7,
  28.4, 29.1, 28.6, 27.9, 28.3
)
# -----------------------------
# Crear dataframe
# -----------------------------
altura <- c(cercado, sin_cercado)
tratamiento <- factor(c(
  rep("Cercado", 25),
  rep("Sin_cercado", 25)
))
datos <- data.frame(tratamiento, altura)
# -----------------------------
# Estadística descriptiva
# -----------------------------
aggregate(altura ~ tratamiento,
          data = datos,
          FUN = mean)
aggregate(altura ~ tratamiento,
          data = datos,
          FUN = sd)
# -----------------------------
# Boxplot
# -----------------------------
boxplot(altura ~ tratamiento,
        data = datos,
        col = c("lightblue", "lightgreen"),
        main = "Altura de plántulas",
        xlab = "Tratamiento",
        ylab = "Altura (cm)")
# -----------------------------
# Prueba de normalidad
# -----------------------------
shapiro.test(cercado)
shapiro.test(sin_cercado)
# -----------------------------
# Homogeneidad de varianzas
# -----------------------------
# Instalar paquete si no existe
install.packages("car")
library(car)
leveneTest(altura ~ tratamiento,
           data = datos)
# -----------------------------
# Prueba t independiente
# -----------------------------
t.test(cercado,
       sin_cercado,
       var.equal = TRUE)
# =========================================
# FIN DEL SCRIPT
# =========================================