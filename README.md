# Análisis Estadístico: Uso de Herramientas de Inteligencia Artificial

## Descripción

Análisis estadístico completo sobre patrones de uso de herramientas de IA mediante técnicas de estadística descriptiva, inferencial, modelado probabilístico y simulación Monte Carlo.

## Dataset

Archivo CSV con variables sobre tiempo de uso, frecuencia, edad, herramientas utilizadas, propósito de uso, percepción de confiabilidad y experiencia.

## Requisitos

### Paquetes de R:

```r
install.packages(c(
  "tidyverse", "readr", "ggplot2", "dplyr", "tidyr",
  "moments", "fitdistrplus", "nortest", "car", "corrplot",
  "lmtest", "MASS", "gridExtra", "scales", "RColorBrewer"
))
```

## Uso

1. Colocar el archivo `uso_de_la_ia.csv` en el directorio de trabajo
2. Ejecutar el script `Uso de la IA.R`

```r
source("Uso de la IA.R")
```

## Análisis Incluidos

### Estadística Descriptiva
Medidas de tendencia central, dispersión y forma. Visualizaciones con histogramas, boxplots y gráficos de densidad.

### Distribuciones de Probabilidad
Ajuste de distribuciones Normal, Exponencial, Gamma y Lognormal. Pruebas de bondad de ajuste.

### Inferencia Estadística
Intervalos de confianza, pruebas t, ANOVA y Chi-cuadrado.

### Regresión y Correlación
Análisis de correlación, regresión lineal simple y múltiple.

### Simulación Monte Carlo
Distribución muestral, bootstrap, predicción de escenarios y análisis de sensibilidad.

## Estructura

- `Uso de la IA.R`: Script principal con todo el análisis
- `uso_de_la_ia.csv`: Dataset con información de uso de IA