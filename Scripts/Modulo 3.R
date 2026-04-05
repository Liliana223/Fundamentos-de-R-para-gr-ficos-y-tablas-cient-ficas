# Fundamentos de R para gráficos y tablas científicas
# Prof. Astrid Liliana Vargas Sanchez
# Modulo 3
# Tablas científicas y casos prácticos

# =========================
# Cargar librerías
# =========================
# =========================
# Cargar librerías y datos
# =========================
library(dplyr)
library(gt)
library(kableExtra)
library(broom)
library(stargazer)
library(sjPlot)
library(readr)

datos <- read_csv("datos_limpios.csv")

# =========================
# Tabla descriptiva (dplyr)
# =========================

tabla_resumen <- datos %>%
  group_by(especie) %>% # Agrupa los datos por especie
  summarise( # Resume los datos de cada grupo
    media_petalo = mean(longitud_petalo), # Calcula el promedio de longitud_petalo por especie
    sd_petalo = sd(longitud_petalo), # Calcula la desviación estándar
    n = n(), # Cuenta cuántas observaciones hay
    .groups = "drop"
  )

tabla_gt <- tabla_resumen %>%
  gt() %>%
  cols_label(
    especie = "Especie",
    media_petalo = "Media del pétalo",
    sd_petalo = "Desviación estándar",
    n = "Número de muestras"
  )

tabla_resumen

# =========================
# Formato con gt
# =========================

tabla_gt <- tabla_resumen %>%
  gt() %>% # Convierte el data frame en una tabla visual profesional
  tab_header(
    title = "Tabla descriptiva",
    subtitle = "Longitud del pétalo por especie"
  ) %>%
  fmt_number(
    columns = c(media_petalo, sd_petalo), # Muestra los números con 2 decimales en esas columnas
    decimals = 2
  ) %>%
  cols_label(
    especie = "Especie",
    media_petalo = "Media del pétalo",
    sd_petalo = "Desviación estándar",
    n = "Número de muestras"
  )


tabla_gt

# =========================
# Formato con kableExtra
# =========================

tabla_resumen %>%
  kable(
    col.names = c(
      "Especie",
      "Media del pétalo",
      "Desviación estándar",
      "Número de muestras"
    ),
    caption = "Tabla descriptiva"
  ) %>%
  kable_styling(full_width = FALSE)

# =========================
# Modelo estadístico
# =========================

# Crear modelo (sin explicar detalles técnicos)
modelo <- lm(longitud_petalo ~ longitud_sepalo, data = datos)

# Mostrar resultados de forma visual y simple
library(sjPlot)
tab_model(modelo) # Ejemplo de tabla de resultados de un modelo estadístico

# =========================
# Exportar tablas
# =========================

# --------- Exportar gt ---------
gtsave(tabla_gt, "tabla_gt.html")

# --------- Exportar CSV ---------
write.csv(tabla_resumen, "tabla_resumen.csv", row.names = FALSE)

# =========================
# Casos practicos
# Ejercicio 1
# =========================
library(tidyverse)

# =========================
# Crear dataset de ejemplo
# =========================
set.seed(123)

datos <- tibble(
  edad = sample(20:70, 50, replace = TRUE),
  biomarcador = rnorm(50, mean = 50, sd = 10),
  grupo = sample(c("control", "tratamiento"), 50, replace = TRUE)
)

# Ver datos
head(datos)

# =========================
# Gráfico básico
# =========================
ggplot(datos, aes(x = edad, y = biomarcador, color = grupo)) +
  geom_point(size = 3)

# =========================
# Gráfico avanzado
# =========================
ggplot(datos, aes(x = edad, y = biomarcador, color = grupo)) + #aes(color = grupo): “Colorea los puntos según el grupo”, creacion de la leyenda
  geom_point(size = 3) +
  
  # Leyenda (nombre)
  labs(
    title = "Relación entre edad y biomarcador",
    x = "Edad (años)",
    y = "Nivel de biomarcador",
    color = "Grupo"   #  Título de la leyenda
  ) +
  
  # Escala de color
  scale_color_manual( #scale_color_manual(): “¿Qué color específico quiero para cada grupo?”
    values = c(
      "control" = "blue",
      "tratamiento" = "red"
    )
  )
# =========================
# Casos practicos
# Ejercicio 2
# =========================

library(ggplot2)

# Dataset de ejemplo
set.seed(123)

datos <- data.frame(
  edad = sample(20:70, 100, replace = TRUE),
  biomarcador = rnorm(100, 50, 10),
  grupo = sample(c("Control", "Tratamiento A", "Tratamiento B"), 100, replace = TRUE)
)

# Gráfico con facetas
ggplot(datos, aes(x = edad, y = biomarcador)) +
  geom_point(color = "black") +
  geom_smooth(method = "lm", color = "red") + # se usa para añadir una línea de tendencia a un gráfico.
  facet_wrap(~ grupo, scales = "free")+ # Crea un gráfico por cada grupo. Cada gráfico tenga su propia escala
  theme_bw() +
  labs(
    title = "Comparación del biomarcador por grupo",
    x = "Edad",
    y = "Nivel de biomarcador"
  )
# “Aquí usamos facetas para dividir el gráfico en varios paneles, uno por cada 
# grupo, lo que permite comparar fácilmente las tendencias.”

# Línea ascendente → relación positiva
# Línea descendente → relación negativa
# Línea plana → no hay relación

# =========================
# Casos practicos
# Ejercicio 3
# =========================

library(ggplot2)
library(patchwork)

# =========================
# Dataset de ejemplo
# =========================
set.seed(123)

datos <- data.frame(
  edad = sample(20:70, 50, replace = TRUE),
  biomarcador = rnorm(50, 50, 10),
  grupo = sample(c("control", "tratamiento"), 50, replace = TRUE)
)

# =========================
# Crear gráficos individuales
# =========================

# Gráfico 1: Dispersión
p1 <- ggplot(datos, aes(x = edad, y = biomarcador, color = grupo)) +
  geom_point() +
  theme_classic() +
  labs(title = "Dispersión")

# Gráfico 2: Boxplot
p2 <- ggplot(datos, aes(x = grupo, y = biomarcador, fill = grupo)) +
  geom_boxplot() +
  theme_classic() +
  labs(title = "Boxplot")

# Gráfico 3: Histograma
p3 <- ggplot(datos, aes(x = biomarcador, fill = grupo)) +
  geom_histogram(alpha = 0.6, position = "identity") +
  theme_classic() +
  labs(title = "Histograma")

# =========================
# Combinar gráficos
# =========================

# Forma horizontal
p1 + p2

# Forma vertical
p1 / p2

# Cuadrícula
(p1 | p2) / p3

# =========================
# Casos practicos
# Ejercicio 4
# =========================

library(ggplot2)
library(cowplot)

# =========================
# 2. Dataset de ejemplo
# =========================
set.seed(123)

datos <- data.frame(
  edad = sample(20:70, 100, replace = TRUE),
  biomarcador = rnorm(100, 50, 10),
  grupo = sample(c("Control", "Tratamiento A", "Tratamiento B"), 100, replace = TRUE)
)

# =========================
# 3. Crear gráficos individuales
# =========================

# Gráfico 1: Dispersión
p1 <- ggplot(datos, aes(x = edad, y = biomarcador, color = grupo)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  theme_classic() +
  labs(title = "A) Dispersión")

# Gráfico 2: Boxplot
p2 <- ggplot(datos, aes(x = grupo, y = biomarcador, fill = grupo)) +
  geom_boxplot() +
  theme_classic() +
  labs(title = "B) Boxplot")

# Gráfico 3: Histograma
p3 <- ggplot(datos, aes(x = biomarcador, fill = grupo)) +
  geom_histogram(alpha = 0.6, position = "identity") +
  theme_classic() +
  labs(title = "C) Histograma")

# =========================
# 4. Combinar gráficos con cowplot
# =========================

plot_grid(
  p1, p2, p3,
  ncol = 2,          # número de columnas
  labels = "AUTO"    # agrega etiquetas A, B, C automáticamente
)

# =========================
# Casos practicos
# Ejercicio 5
# =========================

library(tidyverse)
library(gt)
library(viridis)

# =========================
# Cargar datos
# =========================

datos <- read.csv("iris_extended.csv")

# =========================
# Limpieza de datos
# =========================

# Eliminar NA
datos <- datos %>%
  drop_na()

# Eliminar duplicados
datos <- datos %>%
  distinct()

# =========================
# 4. Tabla descriptiva
# =========================

tabla_gt <- datos %>%
  group_by(tipo_suelo) %>%
  summarise(
    media_area_sepalo = mean(area_sepalo),
    sd_area_sepalo = sd(area_sepalo),
    media_elevacion = mean(elevacion),
    n = n(),
    .groups = "drop"
  ) %>%
  gt() %>%
  tab_header(
    title = "Tabla descriptiva por tipo de suelo",
    subtitle = "Área del sépalo y elevación promedio"
  ) %>%
  cols_label(
    tipo_suelo = "Tipo de suelo",
    media_area_sepalo = "Media área del sépalo",
    sd_area_sepalo = "Desviación estándar",
    media_elevacion = "Elevación promedio",
    n = "Número de muestras"
  ) %>%
  fmt_number(
    columns = c(media_area_sepalo, sd_area_sepalo, media_elevacion),
    decimals = 2
  )

tabla_gt

# =========================
# 5. Boxplot
# =========================

grafico_box <- ggplot(datos, aes(x = tipo_suelo, y = area_sepalo, fill = tipo_suelo)) +
  geom_boxplot(alpha = 0.7) +
  labs(
    title = "Distribución del área del sépalo por tipo de suelo",
    x = "Tipo de suelo",
    y = "Área del sépalo"
  ) +
  theme_classic() +
  scale_fill_viridis_d()

grafico_box

# =========================
# Gráfico de dispersión
# =========================

grafico_disp <- ggplot(datos, aes(x = elevacion, y = area_sepalo, color = tipo_suelo)) +
  geom_point(alpha = 0.7) +
  labs(
    title = "Relación entre elevación y área del sépalo",
    x = "Elevación",
    y = "Área del sépalo",
    color = "Tipo de suelo"
  ) +
  theme_minimal() +
  scale_color_viridis_d()

grafico_disp

# =========================
# 7. Exportación
# =========================

# Guardar gráfico principal
ggsave(
  "grafico_boxplot_publicacion.png",
  plot = grafico_box,
  dpi = 300,
  width = 6,
  height = 4
)

# Guardar tabla
gtsave(tabla_gt, "tabla_suelo.html")

# Guardar datos
write.csv(datos, "datos_finales_mod6.csv", row.names = FALSE)