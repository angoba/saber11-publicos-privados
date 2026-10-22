# ==============================================================================
# ANÁLISIS DESCRIPTIVO DE BASE DE DATOS ICFES (SABER 11)
# ==============================================================================

# 1. Cargar librerías necesarias
library(data.table)
library(dplyr)
library(tidyr)
library(ggplot2)
library(skimr)

# 2. Cargar la base de datos 
datos <- fread("Saber_11__2020-2_20240925.csv", encoding = "UTF-8")

# A. Descriptivo de la base
#Dimensiones
dim(datos)
glimpse(datos)

#Resumen general de completitud y distribución
skim(datos)


#B. DESCRIPTIVO DE PUNTAJES (VARIABLES NUMÉRICAS)
# Identificación de variables de puntajes
vars_puntajes <- c("PUNT_GLOBAL", "PUNT_LECTURA_CRITICA", "PUNT_MATEMATICAS", 
                   "PUNT_C_NATURALES", "PUNT_SOCIALES_CIUDADANAS", "PUNT_INGLES")

# Filtrar solo las variables presentes en la base cargada
vars_presentes <- intersect(vars_puntajes, names(datos))

# Tabla de medidas de tendencia central y dispersión
resumen_puntajes <- datos %>%
  select(all_of(vars_presentes)) %>%
  summarise(across(everything(), list(
    Min      = ~min(.x, na.rm = TRUE),
    Q1       = ~quantile(.x, 0.25, na.rm = TRUE),
    Media    = ~mean(.x, na.rm = TRUE),
    Mediana  = ~median(.x, na.rm = TRUE),
    Q3       = ~quantile(.x, 0.75, na.rm = TRUE),
    Max      = ~max(.x, na.rm = TRUE),
    DesvEst  = ~sd(.x, na.rm = TRUE),
    RIC      = ~IQR(.x, na.rm = TRUE),
    N_Missing = ~sum(is.na(.x))
  ))) %>%
  pivot_longer(everything(), 
               names_to = c("Variable", "Estadistico"), 
               names_pattern = "(.*)_(.*)") %>%
  pivot_wider(names_from = Estadistico, values_from = value)

print(resumen_puntajes)


# C. DESCRIPTIVO DE VARIABLES CATEGÓRICAS (SOCIOECONÓMICAS)
# Frecuencia para Naturaleza del Colegio (Oficial / No Oficial)
if("COLE_NATURALEZA" %in% names(datos)){
  cat("\nDistribución por Naturaleza del Colegio:\n")
  tabla_naturaleza <- datos %>%
    count(COLE_NATURALEZA) %>%
    mutate(Porcentaje = round((n / sum(n)) * 100, 2))
  print(tabla_naturaleza)
}

# Frecuencia para Estrato Socioeconómico
if("FAMI_ESTRATOVIVIENDA" %in% names(datos)){
  cat("\nDistribución por Estrato Socioeconómico:\n")
  tabla_estrato <- datos %>%
    count(FAMI_ESTRATOVIVIENDA) %>%
    mutate(Porcentaje = round((n / sum(n)) * 100, 2))
  print(tabla_estrato)
}


#D. CRUCES Y ANÁLISIS BIVARIADO 
# Puntaje Global promedio según Naturaleza del Colegio
if(all(c("COLE_NATURALEZA", "PUNT_GLOBAL") %in% names(datos))){
  cat("\nPuntaje Global por Naturaleza de Colegio:\n")
  bivariado_colegio <- datos %>%
    group_by(COLE_NATURALEZA) %>%
    summarise(
      N              = n(),
      Promedio_Punt  = round(mean(PUNT_GLOBAL, na.rm = TRUE), 2),
      Mediana_Punt   = median(PUNT_GLOBAL, na.rm = TRUE),
      Desv_Est       = round(sd(PUNT_GLOBAL, na.rm = TRUE), 2),
      .groups        = "drop"
    )
  print(bivariado_colegio)
}


# E. VISUALIZACIÓN GRÁFICA
if("PUNT_GLOBAL" %in% names(datos)){
  # 1. Histograma del Puntaje Global
  p1 <- ggplot(datos, aes(x = PUNT_GLOBAL)) +
    geom_histogram(aes(y = ..density..), bins = 30, fill = "#1F78B4", color = "white", alpha = 0.8) +
    geom_density(color = "#E31A1C", linewidth = 1) +
    labs(
      title = "Distribución del Puntaje Global ICFES",
      subtitle = "Histograma y curva de densidad",
      x = "Puntaje Global",
      y = "Densidad"
    ) +
    theme_minimal()
  print(p1)
}

if(all(c("COLE_NATURALEZA", "PUNT_GLOBAL") %in% names(datos))){
  # 2. Diagrama de caja (Boxplot) por Naturaleza del Colegio
  p2 <- ggplot(datos, aes(x = COLE_NATURALEZA, y = PUNT_GLOBAL, fill = COLE_NATURALEZA)) +
    geom_boxplot(alpha = 0.7, show.legend = FALSE) +
    scale_fill_brewer(palette = "Set2") +
    labs(
      title = "Puntaje Global según Naturaleza del Colegio",
      x = "Naturaleza",
      y = "Puntaje Global"
    ) +
    theme_minimal()
  print(p2)
}