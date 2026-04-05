# Fundamentos de R para gráficos y tablas científicas
# Prof. Astrid Liliana Vargas Sanchez
# Modulo 2
# Gráficos con ggplot2 listos para publicar 

# =========================
# Cargar librerías
# =========================
library(readr)
library(ggplot2)
library(dplyr)

# =========================
# Cargar datos
# =========================
datos <- read_csv("datos_limpios.csv")

# Ver estructura
str(datos)

# =========================
# GRÁFICOS CON GGPLOT2
# =========================

# -------------------------
# Estructura básica
# -------------------------

ggplot(datos, aes(x = longitud_sepalo, y = longitud_petalo)) +
  geom_point()

# -------------------------
# Histograma
# -------------------------

ggplot(datos, aes(x = longitud_sepalo)) +
  geom_histogram(bins = 20)

# -------------------------
# Gráfico de barras
# -------------------------

resumen <- datos %>%
  group_by(especie) %>% #Agrupando por especie
  summarise( #Calculando:
    media = mean(longitud_petalo), #Media (mean)
    sd = sd(longitud_petalo) # Desviación estándar (sd)
  )

ggplot(resumen, aes(x = especie, y = media, fill = especie)) +
  geom_col() 

# -------------------------
# Diagrama de pastel
# -------------------------

resumen2 <- datos %>%
  group_by(especie) %>%
  summarise(
    media = mean(longitud_petalo)
  )

# Calcular proporciones
resumen2 <- resumen2 %>%
  mutate(
    proporcion = media / sum(media)
  )

ggplot(resumen2, aes(x = "", y = proporcion, fill = especie)) + # x = "". Hace que todo sea un solo círculo
  geom_col(width = 1) + # Crea barras (como base del pastel)
  coord_polar(theta = "y") + # Convierte las barras en un círculo (pastel)
  theme_void() # Quita ejes y fondo (más limpio)

# -------------------------
# Dispersión
# -------------------------

ggplot(datos, aes(x = longitud_sepalo, y = longitud_petalo, color = especie)) +
  geom_point()

# -------------------------
# Boxplot
# -------------------------

ggplot(datos, aes(x = especie, y = longitud_petalo, fill = especie)) +
  geom_boxplot()

# -------------------------
# Violín
# -------------------------

ggplot(datos, aes(x = especie, y = longitud_petalo, fill = especie)) +
  geom_violin()

# -------------------------
# Gráfico de líneas
# -------------------------

resumen_linea <- datos %>%
  group_by(especie) %>%
  summarise(media = mean(longitud_petalo))

ggplot(resumen_linea, aes(x = especie, y = media, group = 1)) +
  geom_line()

# -------------------------
# Mapa de calor (heatmap)
# -------------------------

heatmap_data <- datos %>%
  group_by(especie, tipo_suelo) %>%
  summarise(media = mean(longitud_petalo), .groups = "drop")

ggplot(heatmap_data, aes(x = especie, y = tipo_suelo, fill = media)) +
  geom_tile() 

# Sandy (arenoso)
# Loamy (franco / equilibrado) (Mezcla de arena, limo y arcilla)
# Clay (arcilloso)

# -------------------------
# Elementos esenciales
# -------------------------

ggplot(datos, aes(x = longitud_sepalo, y = longitud_petalo, color = especie)) +
  geom_point() +
  labs(
    title = "Relación entre longitud del sépalo y pétalo",
    x = "Longitud del sépalo (cm)",
    y = "Longitud del pétalo (cm)",
    color = "Especie" #  Título de la leyenda
  ) +

# Escala de color
scale_color_manual( #scale_color_manual(): “¿Qué color específico quiero para cada grupo?”
  values = c(
    "setosa" = "blue",
    "versicolor" = "red",
    "virginica" = "#9A32CD"
  )
)


# -------------------------
# Temas en R
# -------------------------

grafico_base <- ggplot(datos, aes(x = longitud_sepalo, y = longitud_petalo, color = especie)) +
  geom_point()

grafico_base
grafico_base + theme_classic()

# Otro ejemplo:

grafico_base2 <- ggplot(datos, aes(x = longitud_sepalo, y = longitud_petalo, color = especie)) +
  geom_point()+
  theme_dark()

grafico_base2

# -------------------------
# Paleta viridis
# -------------------------

library(viridis)

grafico_base +
  scale_color_viridis_d() +
  theme_minimal()

# Mapa de calor
heatmap_data <- datos %>%
  group_by(especie, tipo_suelo) %>%
  summarise(media = mean(longitud_petalo), .groups = "drop")

ggplot(heatmap_data, aes(x = especie, y = tipo_suelo, fill = media)) +
  geom_tile() 

# Sandy (arenoso)
# Loamy (franco / equilibrado) (Mezcla de arena, limo y arcilla)
# Clay (arcilloso)

library(viridis)

ggplot(heatmap_data, aes(x = especie, y = tipo_suelo, fill = media)) +
  geom_tile() +
  scale_fill_viridis_c() +
  theme_minimal()

# -------------------------
# Escala en grises
# -------------------------

grafico_base +
  scale_color_grey() +
  theme_bw()

# -------------------------
# Anotaciones
# -------------------------

grafico_base +
  annotate("text", x = 5, y = 6, label = "Tendencia general", size = 4)

# -------------------------
# Línea de referencia
# -------------------------

grafico_base +
  geom_hline(yintercept = 5, linetype = "dashed")


# =========================
# Composición de gráficos 
# =========================

# Patchwork

library(patchwork)

g1 <- ggplot(datos, aes(x = longitud_sepalo, y = longitud_petalo)) +
  geom_point()

g2 <- ggplot(datos, aes(x = especie, y = longitud_petalo)) +
  geom_boxplot()

g1 + g2

# Cowplot

library(cowplot)

# Gráfico 1: dispersión
g1 <- ggplot(datos, aes(x = longitud_sepalo, y = longitud_petalo, color = especie)) +
  geom_point() +
  theme_classic()+
  labs(
    title = "Dispersión",
    x = "Longitud del pétalo",
    y = "Longitud del sépalo"
    )

# Gráfico 2: boxplot
g2 <- ggplot(datos, aes(x = especie, y = longitud_petalo, fill = especie)) +
  geom_boxplot() +
  theme_classic()+
  labs(
    title = "Boxplot",
    y = "Longitud del pétalo"
    )

# Gráfico 3: histograma
g3 <- ggplot(datos, aes(x = longitud_sepalo, fill = especie)) +
  geom_histogram(alpha = 0.6, bins = 20) +
  theme_classic()+
  labs(
    title = "Histograma",
    x = "Longitud del sépalo",
    y = "Frecuencia"
    )

# Combinar gráficos
grafico_final <- plot_grid( # Organiza los gráficos como una figura
  g1, g2, g3,
  labels = c("A", "B", "C"),
  ncol = 2
)

# Mostrar resultado
grafico_final

# Centrar el histograma

# Parte superior (2 gráficos)
fila_superior <- plot_grid(g1, g2, labels = c("A", "B"), ncol = 2)

# Parte inferior (centrado)
fila_inferior <- plot_grid(NULL, g3, NULL, labels = c("", "C", ""), ncol = 3)

# Combinar todo
grafico_final <- plot_grid(fila_superior, fila_inferior, ncol = 1)

grafico_final

# -------------------------
# Guardar gráficos
# -------------------------

library(ggplot2)
library(viridis)

grafico_final <- ggplot(datos, aes(x = longitud_sepalo, y = longitud_petalo, color = especie)) +
  geom_point(size = 2, alpha = 0.8) +
  scale_color_viridis_d() +
  theme_classic() +
  labs(
    title = "Relación entre longitud del sépalo y del pétalo",
    x = "Longitud del sépalo (cm)",
    y = "Longitud del pétalo (cm)",
    color = "Especie"
  )

grafico_final

ggsave(
  "grafico_publicacion.png",
  plot = grafico_final,
  dpi = 300, # Pixeles por pulgada. Se recomienda 300 para artículos científicos
  width = 6,
  height = 4
)