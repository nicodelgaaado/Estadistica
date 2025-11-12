# ============================================================================
# ANÁLISIS ESTADÍSTICO COMPLETO - USO DE HERRAMIENTAS DE IA
# ============================================================================

# ----------------------------------------------------------------------------
# INSTALACIÓN Y CARGA DE PAQUETES
# ----------------------------------------------------------------------------

# Instalar paquetes necesarios
install.packages(c("tidyverse", "readr", "ggplot2", "dplyr", "tidyr", 
                   "moments", "fitdistrplus", "nortest", "car", "corrplot",
                   "lmtest", "MASS", "gridExtra", "scales", "RColorBrewer"))

# Cargar librerías
library(tidyverse)
library(readr)
library(ggplot2)
library(dplyr)
library(tidyr)
library(moments)
library(fitdistrplus)
library(nortest)
library(car)
library(corrplot)
library(lmtest)
library(MASS)
library(gridExtra)
library(scales)
library(RColorBrewer)

# ----------------------------------------------------------------------------
# IMPORTACIÓN Y LIMPIEZA DE DATOS
# ----------------------------------------------------------------------------

# Leer el dataset
datos <- read_csv("uso_de_la_ia.csv")

# Limpiar nombres de columnas (eliminar espacios)
names(datos) <- trimws(names(datos))

# Limpiar espacios en todas las columnas de texto
datos <- datos %>%
  mutate(across(where(is.character), trimws))

# Convertir tiempo_diario a numérico (reemplazar comas por puntos)
datos$tiempo_diario <- as.numeric(gsub(",", ".", datos$tiempo_diario))

# Visualizar estructura
cat("\n=== ESTRUCTURA DEL DATASET ===\n")
str(datos)
cat("\n=== PRIMERAS FILAS ===\n")
head(datos)
cat("\n=== RESUMEN ===\n")
summary(datos)

# ----------------------------------------------------------------------------
# 1. ESTADÍSTICA DESCRIPTIVA
# ----------------------------------------------------------------------------

cat("\n\n============================================\n")
cat("1. ESTADÍSTICA DESCRIPTIVA\n")
cat("============================================\n")

# Variable numérica: tiempo_diario (horas diarias de uso)
tiempo <- datos$tiempo_diario[!is.na(datos$tiempo_diario)]

# Eliminar outliers extremos (valores > 100 horas probablemente son errores)
tiempo_limpio <- tiempo[tiempo < 100]

cat("\n--- MEDIDAS DE TENDENCIA CENTRAL (Tiempo Diario) ---\n")
media_tiempo <- mean(tiempo_limpio, na.rm = TRUE)
mediana_tiempo <- median(tiempo_limpio, na.rm = TRUE)
moda_tiempo <- as.numeric(names(sort(table(tiempo_limpio), decreasing = TRUE)[1]))

cat("Media:", round(media_tiempo, 2), "horas\n")
cat("Mediana:", round(mediana_tiempo, 2), "horas\n")
cat("Moda:", moda_tiempo, "horas\n")

cat("\n--- MEDIDAS DE DISPERSIÓN (Tiempo Diario) ---\n")
varianza_tiempo <- var(tiempo_limpio, na.rm = TRUE)
sd_tiempo <- sd(tiempo_limpio, na.rm = TRUE)
rango_tiempo <- range(tiempo_limpio, na.rm = TRUE)
iqr_tiempo <- IQR(tiempo_limpio, na.rm = TRUE)
cv_tiempo <- (sd_tiempo / media_tiempo) * 100

cat("Varianza:", round(varianza_tiempo, 2), "\n")
cat("Desviación Estándar:", round(sd_tiempo, 2), "horas\n")
cat("Rango:", rango_tiempo[1], "-", rango_tiempo[2], "horas\n")
cat("Rango Intercuartílico (IQR):", round(iqr_tiempo, 2), "\n")
cat("Coeficiente de Variación:", round(cv_tiempo, 2), "%\n")
cat("Asimetría:", round(skewness(tiempo_limpio), 2), "\n")
cat("Curtosis:", round(kurtosis(tiempo_limpio), 2), "\n")

# VISUALIZACIONES
par(mfrow = c(2, 2))

# Histograma
hist(tiempo_limpio, 
     breaks = 20,
     main = "Distribución del Tiempo Diario de Uso de IA",
     xlab = "Horas diarias",
     ylab = "Frecuencia",
     col = "skyblue",
     border = "white")
abline(v = media_tiempo, col = "red", lwd = 2, lty = 2)
abline(v = mediana_tiempo, col = "blue", lwd = 2, lty = 2)
legend("topright", legend = c("Media", "Mediana"), 
       col = c("red", "blue"), lty = 2, lwd = 2)

# Boxplot
boxplot(tiempo_limpio,
        main = "Diagrama de Caja - Tiempo Diario",
        ylab = "Horas diarias",
        col = "lightgreen",
        horizontal = FALSE)

# Gráfico de densidad
plot(density(tiempo_limpio),
     main = "Densidad del Tiempo Diario",
     xlab = "Horas diarias",
     ylab = "Densidad",
     col = "darkblue",
     lwd = 2)
polygon(density(tiempo_limpio), col = rgb(0, 0, 1, 0.3), border = "darkblue")

# Q-Q plot
qqnorm(tiempo_limpio, main = "Q-Q Plot - Normalidad")
qqline(tiempo_limpio, col = "red", lwd = 2)

# Estadísticas por categorías
cat("\n--- FRECUENCIAS DE USO ---\n")
print(table(datos$frecuencia_uso))

cat("\n--- CONFIABILIDAD PERCIBIDA ---\n")
print(table(datos$ia_confiable))

cat("\n--- EXPERIENCIA NEGATIVA ---\n")
print(table(datos$experiencia_negativa))

# Gráficos de barras para variables categóricas
par(mfrow = c(2, 2))

# Frecuencia de uso
barplot(sort(table(datos$frecuencia_uso), decreasing = TRUE),
        main = "Frecuencia de Uso de IA",
        col = rainbow(length(unique(datos$frecuencia_uso))),
        las = 2,
        cex.names = 0.7)

# Herramientas más usadas
herramientas <- unlist(strsplit(datos$herramienta_ia_frecuente, ",|;"))
herramientas <- trimws(herramientas)
herramientas_top <- head(sort(table(herramientas), decreasing = TRUE), 10)
barplot(herramientas_top,
        main = "Top 10 Herramientas de IA",
        col = "coral",
        las = 2,
        cex.names = 0.6)

# Propósito de uso
barplot(sort(table(datos$proposito_uso_ia), decreasing = TRUE),
        main = "Propósito de Uso",
        col = "lightblue",
        las = 2,
        cex.names = 0.6)

# Confiabilidad
barplot(table(datos$ia_confiable),
        main = "Percepción de Confiabilidad",
        col = c("salmon", "lightgreen", "gold", "skyblue"),
        las = 2,
        cex.names = 0.7)

# ----------------------------------------------------------------------------
# 2. DISTRIBUCIONES DE PROBABILIDAD
# ----------------------------------------------------------------------------

cat("\n\n============================================\n")
cat("2. DISTRIBUCIONES DE PROBABILIDAD\n")
cat("============================================\n")

# Ajuste de distribución normal
fit_normal <- fitdist(tiempo_limpio, "norm")
cat("\n--- AJUSTE DISTRIBUCIÓN NORMAL ---\n")
print(summary(fit_normal))

# Ajuste de distribución exponencial
fit_exp <- tryCatch({
  fitdist(tiempo_limpio, "exp")
}, error = function(e) NULL)

if (!is.null(fit_exp)) {
  cat("\n--- AJUSTE DISTRIBUCIÓN EXPONENCIAL ---\n")
  print(summary(fit_exp))
}

# Ajuste de distribución gamma
fit_gamma <- fitdist(tiempo_limpio, "gamma")
cat("\n--- AJUSTE DISTRIBUCIÓN GAMMA ---\n")
print(summary(fit_gamma))

# Ajuste de distribución lognormal
fit_lnorm <- fitdist(tiempo_limpio, "lnorm")
cat("\n--- AJUSTE DISTRIBUCIÓN LOGNORMAL ---\n")
print(summary(fit_lnorm))

# Comparación visual de ajustes
par(mfrow = c(2, 2))
plot(fit_normal)
mtext("Ajuste Normal", side = 3, line = -2, outer = TRUE, cex = 1.2, font = 2)

par(mfrow = c(2, 2))
plot(fit_gamma)
mtext("Ajuste Gamma", side = 3, line = -2, outer = TRUE, cex = 1.2, font = 2)

par(mfrow = c(2, 2))
plot(fit_lnorm)
mtext("Ajuste Lognormal", side = 3, line = -2, outer = TRUE, cex = 1.2, font = 2)

# Criterios de información (AIC/BIC) - menor es mejor
cat("\n--- COMPARACIÓN DE MODELOS (AIC) ---\n")
cat("Normal:", fit_normal$aic, "\n")
if (!is.null(fit_exp)) cat("Exponencial:", fit_exp$aic, "\n")
cat("Gamma:", fit_gamma$aic, "\n")
cat("Lognormal:", fit_lnorm$aic, "\n")

# Pruebas de bondad de ajuste
cat("\n--- PRUEBAS DE BONDAD DE AJUSTE ---\n")

# Prueba de Shapiro-Wilk para normalidad
shapiro_test <- shapiro.test(tiempo_limpio)
cat("\nPrueba de Shapiro-Wilk (Normalidad):\n")
cat("Estadístico W:", shapiro_test$statistic, "\n")
cat("p-valor:", shapiro_test$p.value, "\n")
cat("Conclusión:", ifelse(shapiro_test$p.value > 0.05, 
                          "No se rechaza normalidad (α=0.05)", 
                          "Se rechaza normalidad (α=0.05)"), "\n")

# Prueba de Kolmogorov-Smirnov
ks_test <- ks.test(tiempo_limpio, "pnorm", 
                   mean = mean(tiempo_limpio), 
                   sd = sd(tiempo_limpio))
cat("\nPrueba de Kolmogorov-Smirnov:\n")
cat("Estadístico D:", ks_test$statistic, "\n")
cat("p-valor:", ks_test$p.value, "\n")

# Prueba de Anderson-Darling
ad_test <- ad.test(tiempo_limpio)
cat("\nPrueba de Anderson-Darling:\n")
cat("Estadístico A:", ad_test$statistic, "\n")
cat("p-valor:", ad_test$p.value, "\n")

# ----------------------------------------------------------------------------
# 3. INFERENCIA ESTADÍSTICA
# ----------------------------------------------------------------------------

cat("\n\n============================================\n")
cat("3. INFERENCIA ESTADÍSTICA\n")
cat("============================================\n")

# INTERVALOS DE CONFIANZA
cat("\n--- INTERVALOS DE CONFIANZA (95%) ---\n")

# IC para la media del tiempo diario
ic_media <- t.test(tiempo_limpio)$conf.int
cat("IC para tiempo diario promedio:", 
    round(ic_media[1], 2), "-", round(ic_media[2], 2), "horas\n")

# IC para proporción de uso diario
uso_diario <- sum(datos$frecuencia_uso == "Diariamente", na.rm = TRUE)
n_total <- sum(!is.na(datos$frecuencia_uso))
prop_diario <- uso_diario / n_total
ic_prop <- prop.test(uso_diario, n_total)$conf.int
cat("IC para proporción de uso diario:", 
    round(ic_prop[1], 3), "-", round(ic_prop[2], 3), "\n")

# PRUEBAS DE HIPÓTESIS

cat("\n--- PRUEBA T: ¿El tiempo promedio es diferente de 2.5 horas? ---\n")
t_test_2_5 <- t.test(tiempo_limpio, mu = 2.5)
cat("Hipótesis nula: μ = 2.5 horas\n")
cat("Estadístico t:", round(t_test_2_5$statistic, 3), "\n")
cat("p-valor:", round(t_test_2_5$p.value, 4), "\n")
cat("Conclusión:", ifelse(t_test_2_5$p.value < 0.05,
                          "Se rechaza H0: el tiempo promedio es significativamente diferente de 2.5h",
                          "No se rechaza H0: el tiempo promedio no difiere significativamente de 2.5h"), "\n")

# Comparación entre grupos: experiencia negativa vs sin experiencia negativa
cat("\n--- PRUEBA T: Comparación de tiempo de uso según experiencia negativa ---\n")
tiempo_con_exp_neg <- datos$tiempo_diario[datos$experiencia_negativa == "Si" & 
                                            datos$tiempo_diario < 100]
tiempo_sin_exp_neg <- datos$tiempo_diario[datos$experiencia_negativa == "No" & 
                                            datos$tiempo_diario < 100]

if (length(tiempo_con_exp_neg) > 1 && length(tiempo_sin_exp_neg) > 1) {
  t_test_exp <- t.test(tiempo_con_exp_neg, tiempo_sin_exp_neg)
  cat("Media con experiencia negativa:", round(mean(tiempo_con_exp_neg, na.rm = TRUE), 2), "h\n")
  cat("Media sin experiencia negativa:", round(mean(tiempo_sin_exp_neg, na.rm = TRUE), 2), "h\n")
  cat("Estadístico t:", round(t_test_exp$statistic, 3), "\n")
  cat("p-valor:", round(t_test_exp$p.value, 4), "\n")
  cat("Conclusión:", ifelse(t_test_exp$p.value < 0.05,
                            "Hay diferencia significativa en el tiempo de uso",
                            "No hay diferencia significativa en el tiempo de uso"), "\n")
}

# ANOVA: Comparación entre grupos de edad
cat("\n--- ANOVA: Tiempo de uso según rango de edad ---\n")
datos_anova <- datos %>%
  filter(!is.na(tiempo_diario) & tiempo_diario < 100 & !is.na(edad)) %>%
  mutate(edad_limpia = trimws(edad))

if (nrow(datos_anova) > 0) {
  cat("\nGrupos de edad encontrados:\n")
  print(table(datos_anova$edad_limpia))
  
  anova_edad <- aov(tiempo_diario ~ edad_limpia, data = datos_anova)
  cat("\nResumen ANOVA:\n")
  print(summary(anova_edad))
  
  # Prueba de Levene para homogeneidad de varianzas
  cat("\nPrueba de Levene (Homogeneidad de varianzas):\n")
  levene_test <- leveneTest(tiempo_diario ~ edad_limpia, data = datos_anova)
  print(levene_test)
  
  # Si ANOVA es significativo, realizar pruebas post-hoc
  if (summary(anova_edad)[[1]][["Pr(>F)"]][1] < 0.05) {
    cat("\nPrueba Post-hoc de Tukey:\n")
    tukey_result <- TukeyHSD(anova_edad)
    print(tukey_result)
  }
} else {
  cat("No hay datos suficientes para realizar ANOVA.\n")
}

# Prueba Chi-cuadrado: Independencia entre frecuencia de uso y confiabilidad
cat("\n--- PRUEBA CHI-CUADRADO: Frecuencia de uso vs Confiabilidad ---\n")
tabla_contingencia <- table(datos$frecuencia_uso, datos$ia_confiable)
cat("\nTabla de contingencia:\n")
print(tabla_contingencia)

chi_test <- chisq.test(tabla_contingencia)
cat("\nEstadístico Chi-cuadrado:", round(chi_test$statistic, 3), "\n")
cat("Grados de libertad:", chi_test$parameter, "\n")
cat("p-valor:", round(chi_test$p.value, 4), "\n")
cat("Conclusión:", ifelse(chi_test$p.value < 0.05,
                          "Hay asociación significativa entre frecuencia y confiabilidad",
                          "No hay asociación significativa"), "\n")

# ----------------------------------------------------------------------------
# 4. REGRESIÓN Y CORRELACIÓN
# ----------------------------------------------------------------------------

cat("\n\n============================================\n")
cat("4. REGRESIÓN Y CORRELACIÓN\n")
cat("============================================\n")

# Crear variables numéricas para análisis de correlación
datos_numericos <- datos %>%
  filter(!is.na(tiempo_diario) & tiempo_diario < 100) %>%
  mutate(
    edad_numerica = case_when(
      edad == "Menos de 18 anios" ~ 16,
      edad == "18 - 24 anios" ~ 21,
      edad == "25 - 34 anios" ~ 29.5,
      edad == "35 - 44 anios" ~ 39.5,
      edad == "45 anios o mas" ~ 50,
      TRUE ~ NA_real_
    ),
    frecuencia_numerica = case_when(
      frecuencia_uso == "Diariamente" ~ 7,
      frecuencia_uso == "Semanalmente" ~ 3.5,
      frecuencia_uso == "Ocasionalmente" ~ 1,
      frecuencia_uso == "Mensualmente" ~ 0.25,
      TRUE ~ NA_real_
    ),
    experiencia_numerica = case_when(
      tiempo_experiencia == "Mas de un anio" ~ 18,
      tiempo_experiencia == "Un anio" ~ 12,
      tiempo_experiencia == "Mas de seis meses" ~ 9,
      tiempo_experiencia == "Menos de seis meses" ~ 3,
      TRUE ~ NA_real_
    )
  ) %>%
  dplyr::select(tiempo_diario, edad_numerica, frecuencia_numerica, experiencia_numerica) %>%
  na.omit()

# Diagnóstico
cat("\n--- DIAGNÓSTICO DE DATOS NUMÉRICOS ---\n")
cat("Número de observaciones:", nrow(datos_numericos), "\n")
cat("Número de variables:", ncol(datos_numericos), "\n")
if (nrow(datos_numericos) > 0) {
  cat("\nPrimeras filas:\n")
  print(head(datos_numericos))
  cat("\nResumen estadístico:\n")
  print(summary(datos_numericos))
} else {
  cat("\n¡ADVERTENCIA! No hay datos después de la limpieza.\n")
  cat("Verificando valores únicos en las variables originales:\n")
  cat("\nValores únicos en 'frecuencia_uso':\n")
  print(unique(datos$frecuencia_uso))
  cat("\nValores únicos en 'tiempo_experiencia':\n")
  print(unique(datos$tiempo_experiencia))
  cat("\nValores únicos en 'edad':\n")
  print(unique(datos$edad))
}

# Matriz de correlación
cat("\n--- MATRIZ DE CORRELACIÓN (Pearson) ---\n")
if (nrow(datos_numericos) >= 3) {
  cor_matrix <- cor(datos_numericos, method = "pearson", use = "complete.obs")
  print(round(cor_matrix, 3))
} else {
  cat("No hay suficientes datos para calcular la matriz de correlación.\n")
  cat("Se necesitan al menos 3 observaciones.\n")
}

# Visualización de correlaciones
if (nrow(datos_numericos) >= 3) {
  par(mfrow = c(1, 1))
  corrplot(cor_matrix, 
           method = "color", 
           type = "upper",
           addCoef.col = "black",
           tl.col = "black",
           tl.srt = 45,
           title = "Matriz de Correlación",
           mar = c(0, 0, 2, 0))
} else {
  cat("No se puede generar el gráfico de correlación: datos insuficientes.\n")
}

# Pruebas de significancia de correlación
cat("\n--- PRUEBAS DE CORRELACIÓN ---\n")
if (nrow(datos_numericos) >= 3) {
  cor_test_freq <- cor.test(datos_numericos$tiempo_diario, 
                            datos_numericos$frecuencia_numerica)
  cat("\nCorrelación: Tiempo diario vs Frecuencia de uso\n")
  cat("Coeficiente de Pearson:", round(cor_test_freq$estimate, 3), "\n")
  cat("p-valor:", round(cor_test_freq$p.value, 4), "\n")
  
  cor_test_exp <- cor.test(datos_numericos$tiempo_diario, 
                           datos_numericos$experiencia_numerica)
  cat("\nCorrelación: Tiempo diario vs Experiencia (meses)\n")
  cat("Coeficiente de Pearson:", round(cor_test_exp$estimate, 3), "\n")
  cat("p-valor:", round(cor_test_exp$p.value, 4), "\n")
} else {
  cat("No hay suficientes datos para realizar pruebas de correlación.\n")
}

# REGRESIÓN LINEAL SIMPLE
cat("\n--- REGRESIÓN LINEAL: Tiempo diario ~ Frecuencia de uso ---\n")
modelo_lm <- lm(tiempo_diario ~ frecuencia_numerica, data = datos_numericos)
cat("\nResumen del modelo:\n")
print(summary(modelo_lm))

# Diagnóstico del modelo
par(mfrow = c(2, 2))
plot(modelo_lm)

# Intervalos de confianza para coeficientes
cat("\n--- INTERVALOS DE CONFIANZA PARA COEFICIENTES ---\n")
print(confint(modelo_lm))

# Gráfico de regresión
par(mfrow = c(1, 1))
plot(datos_numericos$frecuencia_numerica, datos_numericos$tiempo_diario,
     xlab = "Frecuencia de uso (días/semana)",
     ylab = "Tiempo diario (horas)",
     main = "Regresión Lineal: Tiempo vs Frecuencia",
     pch = 19, col = rgb(0, 0, 1, 0.5))
abline(modelo_lm, col = "red", lwd = 2)
grid()

# Regresión múltiple
cat("\n--- REGRESIÓN MÚLTIPLE ---\n")
modelo_multiple <- lm(tiempo_diario ~ frecuencia_numerica + experiencia_numerica + edad_numerica, 
                      data = datos_numericos)
cat("\nResumen del modelo múltiple:\n")
print(summary(modelo_multiple))

# Comparación de modelos
cat("\n--- COMPARACIÓN DE MODELOS (ANOVA) ---\n")
print(anova(modelo_lm, modelo_multiple))

# ----------------------------------------------------------------------------
# 5. SIMULACIÓN MONTE CARLO
# ----------------------------------------------------------------------------

cat("\n\n============================================\n")
cat("5. SIMULACIÓN MONTE CARLO\n")
cat("============================================\n")

# Parámetros de la población
mu <- mean(tiempo_limpio)
sigma <- sd(tiempo_limpio)

cat("\nParámetros poblacionales estimados:\n")
cat("Media:", round(mu, 2), "horas\n")
cat("Desviación estándar:", round(sigma, 2), "horas\n")

# Simulación 1: Distribución muestral de la media
cat("\n--- SIMULACIÓN 1: Distribución Muestral de la Media ---\n")
n_simulaciones <- 10000
n_muestra <- 30
medias_muestrales <- replicate(n_simulaciones, mean(rnorm(n_muestra, mu, sigma)))

cat("Media de las medias muestrales:", round(mean(medias_muestrales), 3), "\n")
cat("Error estándar teórico:", round(sigma / sqrt(n_muestra), 3), "\n")
cat("Error estándar simulado:", round(sd(medias_muestrales), 3), "\n")

# Visualización
par(mfrow = c(1, 2))
hist(medias_muestrales, 
     breaks = 50,
     main = "Distribución Muestral de la Media\n(n=30, 10000 simulaciones)",
     xlab = "Media muestral",
     ylab = "Frecuencia",
     col = "lightblue",
     border = "white")
abline(v = mu, col = "red", lwd = 2, lty = 2)

# Q-Q plot de las medias muestrales
qqnorm(medias_muestrales, main = "Q-Q Plot - Medias Muestrales")
qqline(medias_muestrales, col = "red", lwd = 2)

# Simulación 2: Estimación de probabilidades
cat("\n--- SIMULACIÓN 2: Estimación de Probabilidades ---\n")
datos_simulados <- rnorm(100000, mu, sigma)
datos_simulados <- datos_simulados[datos_simulados > 0] # Solo valores positivos

prob_menos_2 <- mean(datos_simulados < 2)
prob_mas_5 <- mean(datos_simulados > 5)
prob_entre_2_4 <- mean(datos_simulados >= 2 & datos_simulados <= 4)

cat("P(tiempo < 2 horas):", round(prob_menos_2, 3), "\n")
cat("P(tiempo > 5 horas):", round(prob_mas_5, 3), "\n")
cat("P(2 ≤ tiempo ≤ 4 horas):", round(prob_entre_2_4, 3), "\n")

# Simulación 3: Bootstrap para intervalos de confianza
cat("\n--- SIMULACIÓN 3: Bootstrap para IC de la Media ---\n")
n_bootstrap <- 10000
medias_bootstrap <- replicate(n_bootstrap, {
  muestra <- sample(tiempo_limpio, replace = TRUE)
  mean(muestra)
})

ic_bootstrap <- quantile(medias_bootstrap, c(0.025, 0.975))
cat("IC 95% Bootstrap:", round(ic_bootstrap[1], 2), "-", round(ic_bootstrap[2], 2), "\n")
cat("Comparar con IC paramétrico:", round(ic_media[1], 2), "-", round(ic_media[2], 2), "\n")

# Visualización del Bootstrap
par(mfrow = c(1, 1))
hist(medias_bootstrap,
     breaks = 50,
     main = "Distribución Bootstrap de la Media\n(10000 remuestreos)",
     xlab = "Media",
     ylab = "Frecuencia",
     col = "lightgreen",
     border = "white")
abline(v = ic_bootstrap, col = "red", lwd = 2, lty = 2)
abline(v = mean(tiempo_limpio), col = "blue", lwd = 2)
legend("topright", 
       legend = c("Media observada", "IC 95%"),
       col = c("blue", "red"), 
       lty = c(1, 2), 
       lwd = 2)

# Simulación 4: Predicción de escenarios futuros
cat("\n--- SIMULACIÓN 4: Predicción de Uso Futuro ---\n")
cat("\nEscenario: Si la población crece un 20% y el tiempo promedio aumenta 10%\n")

nuevos_usuarios <- round(nrow(datos) * 0.2)
nuevo_mu <- mu * 1.1
tiempos_futuros <- rnorm(nuevos_usuarios, nuevo_mu, sigma)
tiempos_futuros <- tiempos_futuros[tiempos_futuros > 0]

cat("Nuevos usuarios simulados:", nuevos_usuarios, "\n")
cat("Tiempo promedio esperado:", round(mean(tiempos_futuros), 2), "horas\n")
cat("IC 95% para el nuevo promedio:", 
    round(quantile(tiempos_futuros, 0.025), 2), "-",
    round(quantile(tiempos_futuros, 0.975), 2), "horas\n")

# Simulación 5: Análisis de sensibilidad
cat("\n--- SIMULACIÓN 5: Análisis de Sensibilidad ---\n")
cat("\nImpacto de cambios en parámetros sobre el tiempo total de uso:\n")

parametros <- expand.grid(
  cambio_usuarios = c(0.8, 1.0, 1.2, 1.5),
  cambio_tiempo = c(0.8, 1.0, 1.2, 1.5)
)

resultados_sensibilidad <- sapply(1:nrow(parametros), function(i) {
  n_sim <- round(nrow(datos) * parametros$cambio_usuarios[i])
  t_sim <- rnorm(n_sim, mu * parametros$cambio_tiempo[i], sigma)
  sum(t_sim[t_sim > 0])
})

parametros$tiempo_total <- resultados_sensibilidad
cat("\nResultados (tiempo total en horas):\n")
print(parametros)

# Visualización final
par(mfrow = c(1, 2))
barplot(height = parametros$tiempo_total[1:4],
        names.arg = c("-20%", "Base", "+20%", "+50%"),
        main = "Sensibilidad: Cambio en Usuarios\n(tiempo constante)",
        ylab = "Tiempo total (h)",
        col = "coral")

barplot(height = parametros$tiempo_total[c(1, 5, 9, 13)],
        names.arg = c("-20%", "Base", "+20%", "+50%"),
        main = "Sensibilidad: Cambio en Tiempo\n(usuarios constantes)",
        ylab = "Tiempo total (h)",
        col = "skyblue")

cat("\n\n============================================\n")
cat("ANÁLISIS COMPLETADO EXITOSAMENTE\n")
cat("============================================\n")

# Restaurar configuración gráfica
par(mfrow = c(1, 1))